use rust_decimal::Decimal;
use serde_json::json;
use serde_json::Value;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::error::Error;
use std::str::FromStr;

use crate::coins_registry::CoinRegistry;
use crate::coins_registry::ensure_coin_in_registry;
use crate::coins_registry::find_decimals;
use crate::coins_registry::normalize_struct_tag;
use crate::config::SUI_RPC_CHAINBASE;
use crate::parser::PoolSummary;
use crate::parser::{AuctionInfo, EventSummary};
use chrono::{DateTime, NaiveDateTime, Utc};
use rust_decimal::prelude::ToPrimitive;

fn short_symbol(typename: &str) -> &str {
    // e.g. "0x...::sui::SUI" -> "SUI"
    typename.rsplit("::").next().unwrap_or(typename)
}

pub fn readable_deadline_ms(ms: u64) -> String {
    // Treat known sentinels as "no deadline"
    if ms == u64::MAX || ms >= i64::MAX as u64 {
        return "∞ (no effective deadline)".to_string();
    }

    // Safe conversion if it fits in i64 milliseconds
    if let Some(ndt) = NaiveDateTime::from_timestamp_millis(ms as i64) {
        let dt: DateTime<Utc> = DateTime::<Utc>::from_naive_utc_and_offset(ndt, Utc);
        format!("{} ({} ms from epoch)", dt.to_rfc3339(), ms)
    } else {
        // Extremely out-of-range or otherwise invalid
        "unknown deadline (invalid ms)".to_string()
    }
}
// Price math helpers ---------------------------------------------------------

const TWO64: f64 = 18446744073709551616.0; // 2^64
const TWO96: f64 = 79228162514264337593543950336.0; // 2^96

#[inline]
fn pow10_i(exp: i32) -> f64 {
    // handles negative exponents as well
    10f64.powi(exp)
}

/// Parse a u128 decimal string into f64 (lossy but fine for logging)
fn parse_u128_str(s: &str) -> Option<f64> {
    let v = s.parse::<rust_decimal::Decimal>().ok()?;
    v.to_f64()
}

/// Price (Y per X) from sqrt_price encoded as Q64.64, scaled by token decimals.
/// dx = decimals of X (token0), dy = decimals of Y (token1)
pub fn price_from_sqrt_q64_64_scaled(sqrt_str: &str, dx: u8, dy: u8) -> Option<f64> {
    let sp = parse_u128_str(sqrt_str)?;
    let base = (sp / TWO64).powi(2); // (sqrtP / 2^64)^2
    let scale = pow10_i(dx as i32 - dy as i32); // 10^(dx - dy)
    Some(base * scale)
}

/// Approx price from sqrt_price assuming Q64.64: price ≈ (sqrtP / 2^64)^2
fn price_from_sqrt_q64_64(sqrt_str: &str) -> Option<f64> {
    let sp = parse_u128_str(sqrt_str)?;
    let r = sp / TWO64;
    Some(r * r)
}

/// Approx price from sqrt_price assuming Q64.96: price ≈ (sqrtP / 2^96)^2
/// SEEMS UNUSED ON THE SUI BLOCKCHAIN
fn _price_from_sqrt_q64_96(sqrt_str: &str) -> Option<f64> {
    let sp = parse_u128_str(sqrt_str)?;
    let r = sp / TWO96;
    Some(r * r)
}

/// Spot (mid) price Y per X from reserves (no decimals scaling here)
fn spot_from_reserves_unscaled(rx: &Option<String>, ry: &Option<String>) -> Option<f64> {
    let x = rx
        .as_deref()?
        .parse::<rust_decimal::Decimal>()
        .ok()?
        .to_f64()?;
    let y = ry
        .as_deref()?
        .parse::<rust_decimal::Decimal>()
        .ok()?
        .to_f64()?;
    if x > 0.0 { Some(y / x) } else { None }
}

fn spot_from_reserves_scaled(
    rx: &Option<String>,
    ry: &Option<String>,
    cx_full: &str,
    cy_full: &str,
    registry: &CoinRegistry,
) -> Option<f64> {
    use rust_decimal::prelude::ToPrimitive;
    let x = rx
        .as_deref()?
        .parse::<rust_decimal::Decimal>()
        .ok()?
        .to_f64()?;
    let y = ry
        .as_deref()?
        .parse::<rust_decimal::Decimal>()
        .ok()?
        .to_f64()?;

    let dx = registry.get(cx_full).map(|m| m.decimals).unwrap_or(0);
    let dy = registry.get(cy_full).map(|m| m.decimals).unwrap_or(0);

    let scale = 10f64.powi((dy as i32) - (dx as i32));
    if x > 0.0 { Some((y * scale) / x) } else { None }
}

/// Percent change helper
fn pct_change(a: f64, b: f64) -> f64 {
    if a == 0.0 { 0.0 } else { (b / a - 1.0) * 100.0 }
}

//Contains an async for getting a unknown coin symbol
pub async fn log_auction_info_with_registry(
    info: &AuctionInfo,
    registry: &mut CoinRegistry,
    coins_path: &str,
)->Result<String, Box<dyn Error + Send + Sync>> {
    println!("\n================= 🚀 New Auction Started =================");
    println!("TxDigest   : {}", info.tx_digest);
    println!("Gas Price  : {} microunits", info.gas_price);
    println!("Deadline   : {}", readable_deadline_ms(info.deadline_ms));
    if let Some(gu) = info.gas_usage {
        println!("Gas Usage  : {}", gu);
    }
    let pool_index = build_pool_index(&info.mutated_pools, registry);
    println!("\n--- 📊 Mutated Pools ---");
    for (i, pool) in info.mutated_pools.iter().enumerate() {
        let cx_full = pool.token_x.as_deref();
        let cy_full = pool.token_y.as_deref();

        if let Some(cx) = cx_full {
            let _ = ensure_coin_in_registry(registry, coins_path, SUI_RPC_CHAINBASE, cx).await;
        }
        if let Some(cy) = cy_full {
            let _ = ensure_coin_in_registry(registry, coins_path, SUI_RPC_CHAINBASE, cy).await;
        }

        // Resolve display symbols from registry (fallback to short_symbol)
        let sym_x = cx_full
            .and_then(|cx| {
                registry.get(&normalize_struct_tag(cx).unwrap_or_else(|| cx.to_string()))
            })
            .map(|m| m.symbol.as_str())
            .unwrap_or_else(|| cx_full.map(short_symbol).unwrap_or("?"));
        let sym_y = cy_full
            .and_then(|cy| {
                registry.get(&normalize_struct_tag(cy).unwrap_or_else(|| cy.to_string()))
            })
            .map(|m| m.symbol.as_str())
            .unwrap_or_else(|| cy_full.map(short_symbol).unwrap_or("?"));

        // CORRECT: pass full coin types to scaled spot
        let spot_scaled = match (cx_full, cy_full) {
            (Some(cx), Some(cy)) => spot_from_reserves_scaled(
                &pool.reserve_x,
                &pool.reserve_y,
                &normalize_struct_tag(cx).unwrap_or_else(|| cx.to_string()),
                &normalize_struct_tag(cy).unwrap_or_else(|| cy.to_string()),
                registry,
            ),
            _ => None,
        };

        // spot price (Y per X) from reserves
        let spot_unscaled = spot_from_reserves_unscaled(&pool.reserve_x, &pool.reserve_y);
        // sqrt-based prices
        let p64 = pool.sqrt_price.as_deref();

        let dec_x = pool
            .token_x
            .as_deref()
            .and_then(|cx| find_decimals(registry, cx));
        let dec_y = pool
            .token_y
            .as_deref()
            .and_then(|cy| find_decimals(registry, cy));

        let res_x_fmt = scale_reserve(pool.reserve_x.as_deref(), dec_x);
        let res_y_fmt = scale_reserve(pool.reserve_y.as_deref(), dec_y);

        let p64_scaled = price_from_sqrt_q64_64_scaled(
            p64.unwrap_or("0"),
            dec_x.unwrap_or(9),
            dec_y.unwrap_or(9),
        );

        println!("Pool #{i}: {}", pool.id);
        println!("  Pair       : {} / {}", sym_x, sym_y);
        println!("  Type       : {}", pool.object_type);
        println!("  Reserves   : x={} | y={}", res_x_fmt, res_y_fmt);

        println!(
            "  Sqrt Price : {}",
            pool.sqrt_price.as_deref().unwrap_or("?")
        );
        if let Some(px) = p64_scaled {
            println!("  Price≈(Y/X): Q64.64={:.10}  (approx)", px)
        } else {
            println!("  Price≈(Y/X): ? Missing sqrt_price");
        }

        if let Some(s) = spot_scaled {
            println!("  Spot(Y/X)  : {:.10} (from reserves, scaled)", s);
        } else if let Some(us) = spot_unscaled {
            println!("  Spot(Y/X)  : {:.10} (from reserves, unscaled)", us);
        }
        println!("  Tick Index : {:?}", pool.tick_index);
        println!("  Fee (ppm?) : {:?}", pool.swap_fee_rate);
        println!("----------------------------------------------------------");
    }

    println!("\n--- 📜 Events ---");
    for (i, evt) in info.events.iter().enumerate() {
        log_event_scaled(i, evt, &pool_index);
    }
    println!(
        "{}",
        summarize_auction_actions(info, &pool_index, /*verbose=*/ false)
    );
    println!("===========================================================\n");
    Ok(info.tx_digest.clone())
}

fn scale_reserve(raw: Option<&str>, decimals: Option<u8>) -> String {
    if let (Some(r), Some(d)) = (raw, decimals) {
        if let Ok(val) = Decimal::from_str(r) {
            let scaled = val / Decimal::from(10u64.pow(d as u32));
            return format!("{}", scaled.normalize()); // normalized string, e.g. 123.45
        }
    }
    // fallback to raw (or "?")
    raw.unwrap_or("?").to_string()
}

fn log_event_scaled(
    i: usize,
    evt: &EventSummary,
    pool_index: &HashMap<String, (String, String, String, String, u8, u8)>,
) {
    println!("Event #{i}: {}", evt.event_type);

    let (sym_x, sym_y, dec_x, dec_y) = if let Some(pid) = &evt.pool_id {
        print!("  Pool ID     : {pid}\n");
        match pool_index.get(pid) {
            Some((_cx, _cy, sx, sy, dx, dy)) => (sx.as_str(), sy.as_str(), *dx, *dy),
            None => {
                // Pool unknown (not in mutated set); fall back to raw amounts
                ("X", "Y", 9, 9)
            }
        }
    } else {
        ("X", "Y", 9, 9)
    };

    if let Some(sender) = &evt.sender {
        println!("  Sender      : {sender}");
    }

    // Decimals-aware amounts
    println!("  Amount X    : {}", fmt_amt(&evt.amount_x, dec_x, sym_x));
    println!("  Amount Y    : {}", fmt_amt(&evt.amount_y, dec_y, sym_y));

    if let Some(xfy) = evt.x_for_y {
        println!("  Direction   : {}", if xfy { "X → Y" } else { "Y → X" });
    }

    // Price movement from sqrtP (Q64.96 on most Sui CLMMs; our logger shows Q64.64 + Q64.96 approx)
    if let (Some(b), Some(a)) = (
        evt.sqrt_price_before.as_deref(),
        evt.sqrt_price_after.as_deref(),
    ) {
        let b64 = price_from_sqrt_q64_64(b);
        let a64 = price_from_sqrt_q64_64(a);
        if let (Some(pb), Some(pa)) = (b64, a64) {
            println!(
                "  Price Q64.64: before={:.10} after={:.10} Δ={:+.5}%",
                pb,
                pa,
                pct_change(pb, pa)
            );
        } else {
            println!("  Price (Q64.64): ? (decode warning)");
        }
    }

    if let Some(fee) = &evt.fee_amount {
        println!(
            "  Fee Amount  : {}",
            fmt_amt(&Some(fee.clone()), dec_y, sym_y)
        );
        // WARNING: Many CLMMs charge fees in the **output** asset; this varies by implementation.
        // If unsure, keep raw alongside scaled.
    }
    if let Some(pfee) = &evt.protocol_fee {
        println!(
            "  ProtocolFee : {}",
            fmt_amt(&Some(pfee.clone()), dec_y, sym_y)
        );
    }
    println!("----------------------------------------------------------");
}

fn build_pool_index<'a>(
    pools: &'a [PoolSummary],
    registry: &'a CoinRegistry,
) -> HashMap<String, (String, String, String, String, u8, u8)> {
    let mut map = HashMap::new();
    for p in pools {
        let cx_full = p.token_x.clone().unwrap_or_default();
        let cy_full = p.token_y.clone().unwrap_or_default();
        let cx_norm = normalize_struct_tag(&cx_full).unwrap_or(cx_full.clone());
        let cy_norm = normalize_struct_tag(&cy_full).unwrap_or(cy_full.clone());

        let sym_x = registry
            .get(&cx_norm)
            .map(|m| m.symbol.clone())
            .unwrap_or_else(|| short_symbol(&cx_norm).to_string());
        let sym_y = registry
            .get(&cy_norm)
            .map(|m| m.symbol.clone())
            .unwrap_or_else(|| short_symbol(&cy_norm).to_string());

        let dec_x = find_decimals(registry, &cx_norm).unwrap_or(9); // sane fallback
        let dec_y = find_decimals(registry, &cy_norm).unwrap_or(9);

        map.insert(
            p.id.clone(),
            (cx_norm, cy_norm, sym_x, sym_y, dec_x as u8, dec_y as u8),
        );
    }
    map
}

/// scale a raw amount (string) by `dec` decimals -> Decimal
fn scale_amount(raw: &str, dec: u8) -> Option<Decimal> {
    let i = Decimal::from_str_exact(raw).ok()?;
    let factor = Decimal::new(1, 0) / Decimal::new(10i64.pow(dec as u32), 0);
    Some(i * factor)
}

/// format scaled amount with symbol
fn fmt_amt(raw_opt: &Option<String>, dec: u8, sym: &str) -> String {
    match raw_opt.as_deref() {
        Some(s) => match scale_amount(s, dec) {
            Some(v) => format!("{} {}", v.normalize(), sym),
            None => "-".to_string(),
        },
        None => "-".to_string(),
    }
}

fn summarize_auction_actions(
    info: &AuctionInfo,
    pool_index: &HashMap<String, (String, String, String, String, u8, u8)>,
    verbose: bool,
) -> String {
    use std::fmt::Write;
    let mut line = String::new();
    let mut details = String::new();

    // Collect a few recognizable actions
    for e in &info.events {
        let (sx, sy, dx, dy) = e
            .pool_id
            .as_ref()
            .and_then(|pid| pool_index.get(pid))
            .map(|(_, _, sx, sy, dx, dy)| (sx.as_str(), sy.as_str(), *dx, *dy))
            .unwrap_or(("X", "Y", 9, 9));

        let ax = e
            .amount_x
            .as_deref()
            .and_then(|s| scale_amount(s, dx))
            .map(|d| d.normalize().to_string());
        let ay = e
            .amount_y
            .as_deref()
            .and_then(|s| scale_amount(s, dy))
            .map(|d| d.normalize().to_string());

        match e.event_type.as_str() {
            // Momentum / CLMM swap
            t if t.ends_with("::trade::SwapEvent") => {
                if let (Some(ax), Some(ay)) = (ax.as_ref(), ay.as_ref()) {
                    // Direction: x_for_y true => X -> Y
                    if e.x_for_y.unwrap_or(true) {
                        let _ = write!(line, "Swap {} {sx} → {} {sy}; ", ax, ay);
                    } else {
                        let _ = write!(line, "Swap {} {sy} → {} {sx}; ", ay, ax);
                    }
                } else {
                    let _ = write!(line, "Swap X↔Y; ");
                }

                if verbose {
                    let _ = writeln!(
                        details,
                        "• SwapEvent: traded across pool; amounts are raw pool X and Y \
                         (scaled here). Fees are taken by pool/protocol per its fee schedule."
                    );
                }
            }
            // Flash-swap repayment
            t if t.ends_with("::trade::RepayFlashSwapEvent") => {
                let _ = write!(line, "Repay flash swap debt; ");
                if verbose {
                    let _ = writeln!(
                        details,
                        "• RepayFlashSwapEvent: settles temporary borrow from flash_swap; \
                         `amount_*_debt` and `paid_*` fields show exact repayments."
                    );
                }
            }
            // Cetus-like generic swaps
            t if t.ends_with("::events::AssetSwap")
                || t.ends_with("::bluefin::BluefinSwapEvent") =>
            {
                let _ = write!(line, "Swap on DEX (Cetus/Bluefin); ");
                if verbose {
                    let _ = writeln!(
                        details,
                        "• AssetSwap/BluefinSwapEvent: swap routed by that DEX; \
                         decoding exact token sides may require their event layout."
                    );
                }
            }
            // Settlement
            t if t.ends_with("::settle::Swap") => {
                let _ = write!(line, "Settlement swap; ");
                if verbose {
                    let _ = writeln!(
                        details,
                        "• settle::Swap: off-pool settlement step (often part of aggregator flows)."
                    );
                }
            }
            // Liquidity mgmt
            t if t.ends_with("::pool::BurnEvent")
                || t.ends_with("::position_manager::DecreaseLiquidityEvent") =>
            {
                let _ = write!(line, "Decrease LP & collect; ");
                if verbose {
                    let _ = writeln!(
                        details,
                        "• Burn/DecreaseLiquidity: position liquidity burned and fees/amounts collected."
                    );
                }
            }
            t if t.ends_with("::pool::CollectEvent")
                || t.ends_with("::position_manager::CollectEvent") =>
            {
                let _ = write!(line, "Collect fees; ");
                if verbose {
                    let _ = writeln!(
                        details,
                        "• Collect: accrued fees or rewards withdrawn from the position."
                    );
                }
            }
            _ => {}
        }
    }

    if line.is_empty() {
        line = "Summary: (unrecognized events; see above log).".to_string();
    } else {
        line.insert_str(0, "Summary: ");
    }

    if verbose {
        format!("{line}\n\n{details}")
    } else {
        line
    }
}

use reqwest::Client;
/// Analyze a Shio auction result that you've already parsed into a Vec<Value>.
/// - Detects if bids were placed
/// - Picks the winner (highest bidAmount, tiebreaker earliest timestamp)
/// - Fetches the winner's tx from Sui RPC and prints a MEV-oriented summary.
pub async fn analyze_shio_auction_result(
    events: &[Value],
    sui_rpc: &str,
) -> Result<(), Box<dyn Error + Send + Sync>> {
    // 1) Collect accepted bids
    let mut bids: Vec<(u128, String, u64)> = Vec::new(); // (amount, bidDigest, ts)

    for e in events {
        let ety = e.get("eventType").and_then(|v| v.as_str()).unwrap_or("");
        if ety == "bid_accepted" {
            if let (Some(amt_u64), Some(digest)) = (
                e.get("bidAmount").and_then(|v| v.as_u64()),
                e.get("bidDigest").and_then(|v| v.as_str()),
            ) {
                let ts = e.get("timestampMs").and_then(|v| v.as_u64()).unwrap_or(0);
                bids.push((amt_u64 as u128, digest.to_string(), ts));
            }
        }
    }

    // 2) Winner?
    if bids.is_empty() {
        println!("Auction had no accepted bids (only start/end).");
        return Ok(());
    }

    // Highest amount wins; tie-break by earlier timestamp
    bids.sort_by(|a, b| {
        match a.0.cmp(&b.0) {
            Ordering::Less => Ordering::Greater,
            Ordering::Greater => Ordering::Less,
            Ordering::Equal => a.2.cmp(&b.2),
        }
    });

    let (win_amt, win_digest, win_ts) = bids[0].clone();
    println!("✅ Winner: {win_digest} with bid amount {win_amt} (ts={win_ts})");
    if bids.len() > 1 {
        println!("   Other accepted bids:");
        for (i, (amt, d, ts)) in bids.iter().enumerate().skip(1) {
            println!("   {i}. {d}  amount={amt} ts={ts}");
        }
    }

    // 3) Pull the winner’s tx from Sui RPC and explain
    let tx = get_tx_with_details(&Client::new(), sui_rpc, &win_digest).await?;
    explain_mev_from_tx(&tx);

    Ok(())
}

/// Fetch a transaction block with all useful details for analysis.
async fn get_tx_with_details(
    client: &Client,
    sui_rpc: &str,
    digest: &str,
) -> Result<Value, Box<dyn Error + Send + Sync>> {
    use serde_json::json;
    let req = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "sui_getTransactionBlock",
        "params": [
            digest,
            {
                "showInput": true,
                "showEffects": true,
                "showEvents": true,
                "showObjectChanges": true,
                "showBalanceChanges": true
            }
        ]
    });

    let resp = client.post(sui_rpc).json(&req).send().await?;
    let v: Value = resp.error_for_status()?.json().await?;
    let result = v.get("result").cloned().ok_or("missing result in Sui RPC response")?;
    Ok(result)
}

/// Heuristic explanation of MEV: identify swaps/flash-repay pattern and net deltas.
fn explain_mev_from_tx(tx: &Value) {
    println!("\n—— MEV Analysis (winner tx) ——");

    // 1) Swaps & repayments
    if let Some(events) = tx.get("events").and_then(|e| e.as_array()) {
        let mut saw_swap = false;
        let mut saw_repay = false;

        for (i, e) in events.iter().enumerate() {
            let ety = e.get("type").and_then(|v| v.as_str()).unwrap_or("");
            let parsed = e.get("parsedJson");

            if ety.ends_with("::trade::SwapEvent") {
                saw_swap = true;
                let pool = parsed.and_then(|p| p.get("pool_id")).and_then(|v| v.as_str()).unwrap_or("?");
                let x = parsed.and_then(|p| p.get("amount_x")).and_then(|v| v.as_str()).unwrap_or("-");
                let y = parsed.and_then(|p| p.get("amount_y")).and_then(|v| v.as_str()).unwrap_or("-");
                let dir = parsed.and_then(|p| p.get("x_for_y")).and_then(|v| v.as_bool()).unwrap_or(true);
                let before = parsed.and_then(|p| p.get("sqrt_price_before")).and_then(|v| v.as_str()).unwrap_or("-");
                let after  = parsed.and_then(|p| p.get("sqrt_price_after")).and_then(|v| v.as_str()).unwrap_or("-");
                println!(
                    "  • SwapEvent[{i}] pool={pool} amounts: X={x}, Y={y}, dir={}  √P(before→after)={before}→{after}",
                    if dir { "X→Y" } else { "Y→X" }
                );
            } else if ety.ends_with("::trade::RepayFlashSwapEvent") {
                saw_repay = true;
                let pool = parsed.and_then(|p| p.get("pool_id")).and_then(|v| v.as_str()).unwrap_or("?");
                let x_debt = parsed.and_then(|p| p.get("amount_x_debt")).and_then(|v| v.as_str()).unwrap_or("-");
                let y_debt = parsed.and_then(|p| p.get("amount_y_debt")).and_then(|v| v.as_str()).unwrap_or("-");
                println!("  • RepayFlashSwapEvent[{i}] pool={pool} repay: X_debt={x_debt} Y_debt={y_debt}");
            } else if ety.ends_with("::events::AssetSwap")
                || ety.ends_with("::bluefin::BluefinSwapEvent")
                || ety.ends_with("::settle::Swap")
            {
                println!("  • DEX/Settle event[{i}]: {ety} (see parsedJson for route-specific fields)");
            }
        }

        if saw_swap && saw_repay {
            println!("  › Pattern: flash-swap style routing (borrow, swap, then repay) — classic atomic MEV.");
        }
    } else {
        println!("  (No on-chain events present in tx.)");
    }

    // 2) Net PnL hints
    if let Some(bal) = tx.get("balanceChanges").and_then(|b| b.as_array()) {
        println!("\n  Balance changes:");
        for bc in bal {
            let owner = bc.get("owner").and_then(|o| o.get("AddressOwner")).and_then(|v| v.as_str()).unwrap_or("?");
            let ctype = bc.get("coinType").and_then(|v| v.as_str()).unwrap_or("?");
            let amt = bc.get("amount").and_then(|v| v.as_str()).unwrap_or("0");
            println!("    • {owner}  {ctype}  Δ={amt}");
        }
        println!("  (Positive Δ in a quote token after repaying debt suggests captured spread/arb.)");
    }

    // 3) Pools touched
    if let Some(ch) = tx.get("objectChanges").and_then(|c| c.as_array()) {
        let pools: Vec<_> = ch.iter()
            .filter_map(|c| c.get("objectType").and_then(|t| t.as_str()))
            .filter(|t| t.contains("::pool::Pool<"))
            .collect();
        if !pools.is_empty() {
            println!("\n  Pools touched:");
            for p in pools {
                println!("    • {p}");
            }
        }
    }

    println!("\n  Verdict: Winner likely routed swaps (possibly multi-hop), repaid flash debt, and kept the spread (MEV).");
    std::process::exit(0);
}

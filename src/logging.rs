use chrono::{DateTime, NaiveDateTime, Utc};

use crate::parser::{AuctionInfo, EventSummary, PoolSummary};
use rust_decimal::prelude::ToPrimitive;

fn short_symbol(typename: &str) -> &str {
    // e.g. "0x...::sui::SUI" -> "SUI"
    typename.rsplit("::").next().unwrap_or(typename)
}

fn readable_deadline_ms(ms: u64) -> String {
    // handle extreme sentinel (e.g., i64::MAX) from feed
    if ms >= i64::MAX as u64 {
        return "∞ (no effective deadline)".to_string();
    }
    let secs = (ms / 1000) as i64;
    let nsecs = ((ms % 1000) * 1_000_000) as u32;
    let ndt = NaiveDateTime::from_timestamp_opt(secs, nsecs).unwrap_or_else(|| NaiveDateTime::from_timestamp_opt(0, 0).unwrap());
    let dt: DateTime<Utc> = DateTime::<Utc>::from_naive_utc_and_offset(ndt, Utc);
    format!("{} ({} ms from epoch)", dt.to_rfc3339(), ms)
}

// Price math helpers ---------------------------------------------------------

const TWO64: f64 = 18446744073709551616.0;        // 2^64
const TWO96: f64 = 79228162514264337593543950336.0; // 2^96

fn parse_u128_str(s: &str) -> Option<f64> {
    // convert big int string safely to f64 (loses precision but ok for logging)
    let v = s.parse::<rust_decimal::Decimal>().ok()?;
    Some(v.to_f64().unwrap_or(0.0))
}

/// Approx price from sqrt_price assuming Q64.64: price ≈ (sqrtP / 2^64)^2
fn price_from_sqrt_q64_64(sqrt_str: &str) -> Option<f64> {
    let sp = parse_u128_str(sqrt_str)?;
    let r = sp / TWO64;
    Some(r * r)
}

/// Approx price from sqrt_price assuming Q64.96: price ≈ (sqrtP / 2^96)^2
fn price_from_sqrt_q64_96(sqrt_str: &str) -> Option<f64> {
    let sp = parse_u128_str(sqrt_str)?;
    let r = sp / TWO96;
    Some(r * r)
}

/// Spot (mid) price Y per X from reserves (no decimals scaling here)
fn spot_from_reserves(rx: &Option<String>, ry: &Option<String>) -> Option<f64> {
    let x = rx.as_deref()?.parse::<rust_decimal::Decimal>().ok()?.to_f64()?;
    let y = ry.as_deref()?.parse::<rust_decimal::Decimal>().ok()?.to_f64()?;
    if x > 0.0 { Some(y / x) } else { None }
}

/// Percent change helper
fn pct_change(a: f64, b: f64) -> f64 {
    if a == 0.0 { 0.0 } else { (b / a - 1.0) * 100.0 }
}

// Logging --------------------------------------------------------------------

pub fn log_auction_info(info: &AuctionInfo) {
    println!("\n================= 🚀 New Auction Started =================");
    println!("TxDigest   : {}", info.tx_digest);
    println!("Gas Price  : {} microunits", info.gas_price);
    println!("Deadline   : {}", readable_deadline_ms(info.deadline_ms));
    if let Some(gu) = info.gas_usage {
        println!("Gas Usage  : {}", gu);
    }

    println!("\n--- 📊 Mutated Pools ---");
    for (i, pool) in info.mutated_pools.iter().enumerate() {
        let sym_x = pool.token_x.as_deref().map(short_symbol).unwrap_or("?");
        let sym_y = pool.token_y.as_deref().map(short_symbol).unwrap_or("?");

        // spot price (Y per X) from reserves
        let spot = spot_from_reserves(&pool.reserve_x, &pool.reserve_y);

        // sqrt-based prices
        let (p64, p96) = match pool.sqrt_price.as_deref() {
            Some(s) => (price_from_sqrt_q64_64(s), price_from_sqrt_q64_96(s)),
            None => (None, None),
        };

        println!("Pool #{i}: {}", pool.id);
        println!("  Pair       : {} / {}", sym_x, sym_y);
        println!("  Type       : {}", pool.object_type);
        println!("  Reserves   : x={} | y={}",
            pool.reserve_x.as_deref().unwrap_or("?"),
            pool.reserve_y.as_deref().unwrap_or("?"));
        println!("  Sqrt Price : {}", pool.sqrt_price.as_deref().unwrap_or("?"));
        match (p64, p96) {
            (Some(a), Some(b)) => println!("  Price≈(Y/X): Q64.64={:.10} | Q64.96={:.10}  (approx)", a, b),
            (Some(a), None)    => println!("  Price≈(Y/X): Q64.64={:.10}  (approx)", a),
            (None, Some(b))    => println!("  Price≈(Y/X): Q64.96={:.10}  (approx)", b),
            _ => {}
        }
        if let Some(s) = spot {
            println!("  Spot(Y/X)  : {:.10} (from reserves, unscaled)", s);
        }
        println!("  Tick Index : {:?}", pool.tick_index);
        println!("  Fee (ppm?) : {:?}", pool.swap_fee_rate);
        println!("----------------------------------------------------------");
    }

    println!("\n--- 📜 Events ---");
    for (i, evt) in info.events.iter().enumerate() {
        log_event(i, evt);
    }
    println!("===========================================================\n");
}

fn log_event(i: usize, evt: &EventSummary) {
    println!("Event #{i}: {}", evt.event_type);
    if let Some(pid) = &evt.pool_id {
        println!("  Pool ID     : {pid}");
    }
    if let Some(sender) = &evt.sender {
        println!("  Sender      : {sender}");
    }
    // Show amounts (raw; you can scale by decimals later)
    println!("  Amount X    : {}", evt.amount_x.as_deref().unwrap_or("-"));
    println!("  Amount Y    : {}", evt.amount_y.as_deref().unwrap_or("-"));

    // Direction and price shift (if we have sqrt before/after)
    if let Some(xfy) = evt.x_for_y {
        println!("  Direction   : {}", if xfy { "X → Y" } else { "Y → X" });
    }
    let before_after = match (evt.sqrt_price_before.as_deref(), evt.sqrt_price_after.as_deref()) {
        (Some(b), Some(a)) => {
            let b64 = price_from_sqrt_q64_64(b);
            let a64 = price_from_sqrt_q64_64(a);
            let b96 = price_from_sqrt_q64_96(b);
            let a96 = price_from_sqrt_q64_96(a);

            if let (Some(pb), Some(pa)) = (b64, a64) {
                println!("  Price Q64.64: before={:.10} after={:.10} Δ={:+.4}%", pb, pa, pct_change(pb, pa));
            }
            if let (Some(pb), Some(pa)) = (b96, a96) {
                println!("  Price Q64.96: before={:.10} after={:.10} Δ={:+.4}%", pb, pa, pct_change(pb, pa));
            }
            true
        }
        _ => false,
    };

    if !before_after {
        // Fallback: show effective trade price if we have amounts (Y per X or X per Y)
        if let (Some(ax), Some(ay)) = (evt.amount_x.as_deref(), evt.amount_y.as_deref()) {
            if let (Ok(x), Ok(y)) = (ax.parse::<rust_decimal::Decimal>(), ay.parse::<rust_decimal::Decimal>()) {
                if let Some(xfy) = evt.x_for_y {
                    if xfy && x > rust_decimal::Decimal::ZERO {
                        println!("  Px (Y/X)    : ~{}", (y / x).normalize());
                    } else if !xfy && y > rust_decimal::Decimal::ZERO {
                        println!("  Px (X/Y)    : ~{}", (x / y).normalize());
                    }
                }
            }
        }
    }

    if let Some(fee) = &evt.fee_amount { println!("  Fee Amount  : {fee}"); }
    if let Some(pfee) = &evt.protocol_fee { println!("  ProtocolFee : {pfee}"); }
    println!("----------------------------------------------------------");
}

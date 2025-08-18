// src/opportunity.rs
use crate::parser::{AuctionInfo, PoolSummary, EventSummary};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct VenueState {
    pub reserve_x: f64,
    pub reserve_y: f64,
    pub fee_bps: f64, // e.g. 2000 => 0.20% if “2000” means 2,000 ppm; confirm per DEX
}

#[derive(Debug, Clone)]
pub struct Opportunity {
    pub digest: String,
    pub pair: String,
    pub direction: String, // "x_for_y" or "y_for_x"
    pub victim_price: f64,
    pub ref_price: f64,
    pub spread_pct: f64,
}

/// crude x*y=k quote (V2-ish). Good for first pass; replace with v3 later.
fn quote_out_constprod(x_reserve: f64, y_reserve: f64, dx: f64, fee_bps: f64) -> f64 {
    let fee = fee_bps / 10_000.0; // Shio fee fields look ppm; if it’s bps, change denominator to 10_000
    let dx_eff = dx * (1.0 - fee);
    // dy = y * dx_eff / (x + dx_eff)
    y_reserve * dx_eff / (x_reserve + dx_eff)
}

pub fn evaluate_simple_cross_venue(
    info: &AuctionInfo,
    // map "DEX-key for same pair" -> venue state (this is your other venues snapshot)
    reference_venues: &HashMap<String, VenueState>,
) -> Option<Opportunity> {
    // 1) find a SwapEvent we can use
    let evt = info.events.iter().find(|e| e.event_type.ends_with("::trade::SwapEvent"))?;
    let pool = info.mutated_pools.get(0)?; // simplistic: first pool summary
    let pair = format!("{} / {}", pool.token_x.as_deref().unwrap_or("?"), pool.token_y.as_deref().unwrap_or("?"));

    // 2) victim trade direction and size
    let x_for_y = evt.x_for_y.unwrap_or(false);
    let amount_x: f64 = evt.amount_x.as_deref().unwrap_or("0").parse::<f64>().ok()?;
    let amount_y: f64 = evt.amount_y.as_deref().unwrap_or("0").parse::<f64>().ok()?;

    // 3) victim pool state
    let rx = pool.reserve_x.as_deref().unwrap_or("0").parse::<f64>().ok()?;
    let ry = pool.reserve_y.as_deref().unwrap_or("0").parse::<f64>().ok()?;
    let fee_bps: f64 = pool.swap_fee_rate.as_deref().unwrap_or("0").parse().unwrap_or(0.0);

    // 4) compute victim effective price
    // If x_for_y: they sold X, received Y => victim_price = Y_out / X_in
    // else: they sold Y, received X => victim_price = X_out / Y_in
    let victim_price = if x_for_y {
        if amount_x > 0.0 { amount_y / amount_x } else { 0.0 }
    } else {
        if amount_y > 0.0 { amount_x / amount_y } else { 0.0 }
    };

    // 5) compare with a reference venue (you should pick the best one for this pair)
    // Simple mid-price from reserves
    let ref_state = reference_venues.get("ref").or_else(|| reference_venues.values().next())?;
    let ref_mid = ref_state.reserve_y / ref_state.reserve_x; // price of X in Y (adjust if your pair orientation differs)

    let spread = if ref_mid > 0.0 { (victim_price / ref_mid) - 1.0 } else { 0.0 };
    if spread.abs() < 0.001 { // <0.1%: ignore
        return None;
    }

    Some(Opportunity {
        digest: info.tx_digest.clone(),
        pair,
        direction: if x_for_y { "x_for_y".into() } else { "y_for_x".into() },
        victim_price,
        ref_price: ref_mid,
        spread_pct: spread * 100.0,
    })
}

// src/parser.rs
use serde_json::Value;
use std::error::Error;

#[derive(Debug, Clone)]
pub struct AuctionInfo {
    pub tx_digest: String,
    pub gas_price: u64,
    pub deadline_ms: u64,
    pub gas_usage: Option<u64>,
    pub mutated_pools: Vec<PoolSummary>,
    pub events: Vec<EventSummary>,
}

#[derive(Debug, Clone)]
pub struct PoolSummary {
    pub id: String,
    pub object_type: String,
    pub token_x: Option<String>,
    pub token_y: Option<String>,
    pub reserve_x: Option<String>,
    pub reserve_y: Option<String>,
    pub sqrt_price: Option<String>,
    pub tick_index: Option<i64>,
    pub swap_fee_rate: Option<String>,
}

#[derive(Debug, Clone)]
pub struct EventSummary {
    pub event_type: String,
    pub sender: Option<String>,
    pub pool_id: Option<String>,
    pub amount_x: Option<String>,
    pub amount_y: Option<String>,
    pub amount_x_debt: Option<String>,
    pub amount_y_debt: Option<String>,
    pub fee_amount: Option<String>,
    pub protocol_fee: Option<String>,
    pub x_for_y: Option<bool>,
    pub sqrt_price_before: Option<String>,
    pub sqrt_price_after: Option<String>,
    pub raw_parsed_json: Option<String>, // full parsedJson as JSON string (for debugging)
}

fn get_str(v: &Value, path: &[&str]) -> Option<String> {
    let mut cur = v;
    for key in path {
        cur = cur.get(*key)?;
    }
    match cur {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) => Some(n.to_string()),
        Value::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}

fn get_u64(v: &Value, path: &[&str]) -> Option<u64> {
    let mut cur = v;
    for key in path {
        cur = cur.get(*key)?;
    }
    match cur {
        Value::Number(n) => n.as_u64(),
        Value::String(s) => s.parse::<u64>().ok(),
        _ => None,
    }
}

fn get_i64(v: &Value, path: &[&str]) -> Option<i64> {
    let mut cur = v;
    for key in path {
        cur = cur.get(*key)?;
    }
    match cur {
        Value::Number(n) => n.as_i64(),
        Value::String(s) => s.parse::<i64>().ok(),
        _ => None,
    }
}

pub fn parse_auction_started(json_txt: &str) -> Result<AuctionInfo, Box<dyn Error + Send + Sync>> {
    let root: Value = serde_json::from_str(json_txt)?;

    let as_obj = root.get("auctionStarted")
        .ok_or("missing auctionStarted")?;

    let tx_digest = get_str(as_obj, &["txDigest"]).ok_or("missing txDigest")?;
    let gas_price = get_u64(as_obj, &["gasPrice"]).ok_or("missing gasPrice")?;
    let deadline_ms = get_u64(as_obj, &["deadlineTimestampMs"]).ok_or("missing deadlineTimestampMs")?;
    let gas_usage = get_u64(as_obj, &["gasUsage"]);

    // ---- mutated pools (we only summarize Pool objects) ----
    let mut mutated_pools = Vec::new();
    if let Some(side_effects) = as_obj.get("sideEffects") {
        if let Some(arr) = side_effects.get("mutatedObjects").and_then(|m| m.as_array()) {
            for obj in arr {
                let object_type = get_str(obj, &["objectType"]).unwrap_or_default();
                // Filter for pool objects (you can relax this if you want all mutated objects)
                let looks_like_pool = object_type.contains("::pool::Pool<");
                if !looks_like_pool { continue; }

                let id = get_str(obj, &["id", "id"]).unwrap_or_default();

                // These are deeply nested inside `content.fields`
                let token_x = get_str(obj, &["content","fields","type_x","fields","name"]);
                let token_y = get_str(obj, &["content","fields","type_y","fields","name"]);

                let reserve_x = get_str(obj, &["content","fields","reserve_x"]);
                let reserve_y = get_str(obj, &["content","fields","reserve_y"]);
                let sqrt_price = get_str(obj, &["content","fields","sqrt_price"]);
                let tick_index = get_i64(obj, &["content","fields","tick_index","fields","bits"]);
                let swap_fee_rate = get_str(obj, &["content","fields","swap_fee_rate"]);

                mutated_pools.push(PoolSummary {
                    id,
                    object_type,
                    token_x,
                    token_y,
                    reserve_x,
                    reserve_y,
                    sqrt_price,
                    tick_index,
                    swap_fee_rate,
                });
            }
        }
    }

    // ---- events (SwapEvent, RepayFlashSwapEvent, etc.) ----
    let mut events = Vec::new();
    if let Some(evts) = as_obj.get("sideEffects").and_then(|s| s.get("events")).and_then(|e| e.as_array()) {
        for e in evts {
            let event_type = get_str(e, &["type"]).unwrap_or_default();
            let parsed = e.get("parsedJson");
            let raw_parsed_json = parsed.map(|p| p.to_string());

            let sender = get_str(e, &["sender"]);
            let pool_id = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","pool_id"]));

            let amount_x = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","amount_x"]));
            let amount_y = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","amount_y"]));
            let amount_x_debt = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","amount_x_debt"]));
            let amount_y_debt = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","amount_y_debt"]));

            let fee_amount = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","fee_amount"]));
            let protocol_fee = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","protocol_fee"]));

            let x_for_y = parsed.as_ref().and_then(|p| p.get("x_for_y")).and_then(|v| v.as_bool());
            let sqrt_price_before = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","sqrt_price_before"]));
            let sqrt_price_after = parsed.as_ref().and_then(|_| get_str(e, &["parsedJson","sqrt_price_after"]));

            events.push(EventSummary {
                event_type,
                sender,
                pool_id,
                amount_x,
                amount_y,
                amount_x_debt,
                amount_y_debt,
                fee_amount,
                protocol_fee,
                x_for_y,
                sqrt_price_before,
                sqrt_price_after,
                raw_parsed_json,
            });
        }
    }

    Ok(AuctionInfo {
        tx_digest,
        gas_price,
        deadline_ms,
        gas_usage,
        mutated_pools,
        events,
    })
}

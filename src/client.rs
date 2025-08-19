use std::error::Error;

use std::time::Duration;
use futures_util::{SinkExt, StreamExt};
use serde::Deserialize;
use serde_json::{json, Value};
use tokio::sync::mpsc;
use tokio_tungstenite::{connect_async, tungstenite::Message};
use url::Url;

use crate::config::{SHIO_FEED_WS, SHIO_RPC_URL};

pub async fn run_pending_ws(
    tx: mpsc::UnboundedSender<String>,
    ws_url: &str
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let url = Url::parse(ws_url)?;
    let (ws_stream, _resp) = connect_async(url).await?;
    println!("Connected to Shio feed: {SHIO_FEED_WS}");

    let (mut write, mut read) = ws_stream.split();

    // No subscribe message required; Shio starts sending `auctionStarted` events immediately.

    while let Some(msg) = read.next().await {
        match msg {
            Ok(Message::Text(txt)) => {
                // Log raw to verify
                // println!("WS RAW: {txt}");

                // Forward only auctionStarted payloads (keeps your parser focused)
                if let Ok(v) = serde_json::from_str::<Value>(&txt) {
                    if v.get("auctionStarted").is_some() {
                        // optionally: pull digest for quick logs
                        if let Some(digest) = v["auctionStarted"]["txDigest"].as_str() {
                            println!("auctionStarted txDigest: {digest}");
                        }
                        let _ = tx.send(txt);
                    }
                }
            }
            Ok(Message::Ping(payload)) => {
                // IMPORTANT: reply to pings or you’ll be disconnected
                let _ = write.send(Message::Pong(payload)).await;
            }
            Ok(Message::Pong(_)) => { /* ignore */ }
            Ok(Message::Close(frame)) => {
                eprintln!("Shio WS closed: {frame:?}");
                break;
            }
            Ok(_) => {
                // Binary frames are possible; Shio may send large object `rawContent` separately in the future
                // println!("WS other: {other:?}");
            }
            Err(e) => return Err(Box::new(e)),
        }
    }

    Ok(())
}

#[derive(Debug, Deserialize)]
pub struct SuiCoinMetadata {
    pub decimals: u8,
    pub symbol: String,
    pub name: String,
    #[allow(dead_code, non_camel_case_types)]
    pub icon_url: Option<String>,
}

#[derive(Debug, Deserialize)]
struct RpcResp<T> { pub result: Option<T>, pub error: Option<serde_json::Value> }

pub async fn fetch_coin_metadata(
    rpc_url: &str,
    coin_type: &str,
) -> Result<SuiCoinMetadata, Box<dyn std::error::Error + Send + Sync>> {
    let body = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "suix_getCoinMetadata",
        "params": [ coin_type ],
    });
    let resp = reqwest::Client::new()
        .post(rpc_url)
        .json(&body)
        .send()
        .await?
        .error_for_status()?
        .json::<RpcResp<SuiCoinMetadata>>()
        .await?;

    if let Some(err) = resp.error {
        return Err(format!("suix_getCoinMetadata error: {err}").into());
    }
    resp.result.ok_or_else(|| "no metadata returned".into())
}


#[derive(Debug, Deserialize)]
struct RpcError {
    code: i64,
    message: String,
    #[serde(default)]
    data: Option<Value>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum RpcEnvelope {
    Ok { jsonrpc: String, id: Value, result: Value },
    Err { jsonrpc: String, id: Value, error: RpcError },
}


/// Fetch persisted auction events for a given opportunity tx digest.
///
/// `digest` is the *opportunity transaction digest* (the one you print as `TxDigest: ...`).
/// Returns the raw `result` as `Vec<Value>`; each item is a Shio event object.
/// Rejected bids are not included (per Shio’s docs).
pub async fn get_shio_auction_events(
    digest: &String,
) -> Result<Vec<Value>, Box<dyn Error + Send + Sync>> {
    // Basic input check
    if digest.trim().is_empty() {
        return Err("empty digest".into());
    }

    // JSON-RPC request body
    let body = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "shio_auctionEvents",
        "params": [ digest ],
    });

    // light retry/backoff for transient errors
    let mut last_err: Option<reqwest::Error> = None;
    for attempt in 0..3 {
        let resp = reqwest::Client::new()
            .post(SHIO_RPC_URL)
            .json(&body)
            .send()
            .await;

        match resp {
            Ok(r) => {
                let status_err = r.error_for_status();
                match status_err {
                    Ok(ok) => {
                        let env: RpcEnvelope = ok.json().await?;
                        match env {
                            RpcEnvelope::Ok { result, .. } => {
                                // Expect an array of event objects
                                let arr = result.as_array()
                                    .cloned()
                                    .ok_or("RPC result is not an array")?;
                                return Ok(arr);
                            }
                            RpcEnvelope::Err { error, .. } => {
                                // Permanent RPC error; don’t keep retrying unless it’s a  -32000-ish transient
                                return Err(format!(
                                    "RPC error {}: {}{}",
                                    error.code,
                                    error.message,
                                    error.data
                                        .as_ref()
                                        .map(|d| format!(" ({d})"))
                                        .unwrap_or_default()
                                ).into());
                            }
                        }
                    }
                    Err(e) => {
                        last_err = Some(e);
                    }
                }
            }
            Err(e) => {
                last_err = Some(e);
            }
        }

        // Backoff before next attempt (100ms, 300ms)
        if attempt < 2 {
            tokio::time::sleep(Duration::from_millis(100 + attempt * 200)).await;
        }
    }

    Err(format!("failed to fetch shio_auctionEvents after retries: {last_err:?}").into())
}

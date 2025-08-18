use futures_util::{SinkExt, StreamExt};
use serde::Deserialize;
use serde_json::{json, Value};
use tokio::sync::mpsc;
use tokio_tungstenite::{connect_async, tungstenite::Message};
use url::Url;

use crate::config::SHIO_FEED_WS;

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
    pub iconUrl: Option<String>,
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

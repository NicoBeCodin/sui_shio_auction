use futures_util::{SinkExt, StreamExt};
use serde::Deserialize;
use serde_json::{json, Value};
use tokio::sync::mpsc;
use tokio_tungstenite::{connect_async, tungstenite::Message};
use url::Url;

use crate::config::SHIO_FEED_WS;

pub async fn run_pending_ws(
    tx: mpsc::UnboundedSender<String>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let url = Url::parse(SHIO_FEED_WS)?;
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
            Ok(other) => {
                // Binary frames are possible; Shio may send large object `rawContent` separately in the future
                // println!("WS other: {other:?}");
            }
            Err(e) => return Err(Box::new(e)),
        }
    }

    Ok(())
}
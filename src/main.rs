
use tokio::sync::mpsc;

use crate::{coins_registry::CoinRegistry, config::SHIO_FEED_WS};
pub mod config;
pub mod client;
pub mod parser;
pub mod opportunity;
pub mod logging;
pub mod coins_registry;

pub async fn parse_message(msg: &str, registry: &mut CoinRegistry) {
    match parser::parse_auction_started(msg) {
        Ok(info) => {
          logging::log_auction_info_with_registry(&info, registry, "coins.json").await;
        }
        Err(err) => eprintln!("parse error: {err}"),
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, mut rx) = mpsc::unbounded_channel::<String>();

    println!("Attempt to connect to SHIO websocket...");
    // Spawn the WS task
    let ws_task = tokio::spawn({
        let tx = tx.clone();
        async move { client::run_pending_ws(tx, SHIO_FEED_WS).await }
    });
    println!("ws_task succesfully spawned...");

    let mut registry = coins_registry::load_coin_registry("coins.json").expect("Failed to load coin registry");
    // Main loop: for each incoming WS message, call the parser
    while let Some(msg) = rx.recv().await {
        parse_message(&msg, &mut registry).await;
    }

    // Propagate any WS task error
    let _  = ws_task.await?;
    Ok(())
}

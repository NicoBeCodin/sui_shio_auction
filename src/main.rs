use std::error::Error;
use tokio::sync::mpsc;
use client::get_shio_auction_events;
use crate::{coins_registry::CoinRegistry, config::{AUCTION_CHECK_DELAY, SHIO_FEED_WS, SUI_RPC_CHAINBASE}, logging::analyze_shio_auction_result};
pub mod config;
pub mod client;
pub mod parser;
pub mod logging;
pub mod coins_registry;

pub async fn parse_message(msg: &str, registry: &mut CoinRegistry) -> Result<std::string::String, Box<dyn Error+ std::marker::Send + Sync>>{
    match parser::parse_auction_started(msg) {
        Ok(info) => {
        logging::log_auction_info_with_registry(&info, registry, "coins.json").await
            
        }
        Err(err) =>{
            eprintln!("parse error: {err}");
            Err(err)
        } 
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
    let test_digest= "4dNVW6h2FCBnU7BGRK6nmsqdtG1SgFQbSbnjAXQzwqJK";
    let value = get_shio_auction_events(&test_digest.to_string()).await.expect("Failed getting digest for tx");
    println!("Test digest: {:?}", value);


    let mut registry = coins_registry::load_coin_registry("coins.json").expect("Failed to load coin registry");
    // Main loop: for each incoming WS message, call the parser
    while let Some(msg) = rx.recv().await {
        if let Ok(tx_digest) = parse_message(&msg, &mut registry).await{
            let digest_clone = tx_digest.clone();
            //This is for checking if somebody won the auction
            tokio::spawn(async move {
                tokio::time::sleep(std::time::Duration::from_secs(AUCTION_CHECK_DELAY)).await;
                match get_shio_auction_events(&digest_clone).await {
                    Ok(values)=>{
                        let res = analyze_shio_auction_result(values.as_slice(), SUI_RPC_CHAINBASE).await;
                        println!("Values for the tx digest : {}\n{:?}", digest_clone, values);
                        println!("---");
                    }
                    Err(err)=>eprintln!("Error fetching auction {err}"),

                }
            });
        }
    }
    // Propagate any WS task error
    let _  = ws_task.await?;
    Ok(())
}

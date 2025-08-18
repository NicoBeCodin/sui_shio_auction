
use tokio::sync::mpsc;
pub mod config;
pub mod ws_client;
pub mod parser;
pub mod opportunity;
pub mod logging;

fn parse_message(msg: &str) {
    match parser::parse_auction_started(msg) {
        Ok(info) => {
          logging::log_auction_info(&info);
        }
        Err(err) => eprintln!("parse error: {err}"),
    }
}




#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, mut rx) = mpsc::unbounded_channel::<String>();
    
    println!("Attempt to connect to SEI websocket...");
    // Spawn the WS task
    let ws_task = tokio::spawn({
        let tx = tx.clone();
        async move { ws_client::run_pending_ws(tx).await }
    });
    println!("ws_task succesfully spawned...");

    // Main loop: for each incoming WS message, call the parser
    while let Some(msg) = rx.recv().await {
        parse_message(&msg);
    }

    // Propagate any WS task error
    let _  = ws_task.await?;
    Ok(())
}

// src/coin_registry.rs  (ADD these to your existing file)
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fs, path::Path};
use crate::client::fetch_coin_metadata;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoinMeta {
    pub name: String,
    pub symbol: String,
    pub coin_type: String,
    pub decimals: u8,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub project_url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub coingecko_id: Option<String>,
    // keep other optional fields if you like…
}

pub type CoinRegistry = HashMap<String, CoinMeta>; // keyed by coin_type

pub fn load_coin_registry(path: &str) -> std::io::Result<CoinRegistry> {
    if !Path::new(path).exists() {
        return Ok(HashMap::new());
    }
    let txt = fs::read_to_string(path)?;
    let coins: Vec<CoinMeta> = serde_json::from_str(&txt).unwrap_or_default();
    Ok(coins.into_iter().map(|c| (c.coin_type.clone(), c)).collect())
}

pub fn save_coin_registry(path: &str, reg: &CoinRegistry) -> std::io::Result<()> {
    let mut v: Vec<&CoinMeta> = reg.values().collect();
    v.sort_by(|a, b| a.symbol.cmp(&b.symbol));
    let json = serde_json::to_string_pretty(&v).expect("serialize coins");
    fs::write(path, json)
}

pub fn normalize_struct_tag(s: &str) -> Option<String> {
    // Expect something like "[0x]addr::module::Type"
    let trimmed = s.trim();
    let mut parts = trimmed.split("::");
    let addr = parts.next()?;
    let module = parts.next()?;
    let ty = parts.next()?;
    if parts.next().is_some() {
        // more than 3 parts -> invalid
        return None;
    }

    // fix address: ensure 0x prefix and lower-case hex
    let addr_norm = if addr.starts_with("0x") {
        addr.to_lowercase()
    } else {
        // must be pure hex, length 64 ideally
        if addr.chars().all(|c| c.is_ascii_hexdigit()) {
            format!("0x{}", addr.to_lowercase())
        } else {
            return None;
        }
    };

    // basic sanity on address length (allow 0x followed by 1..64 hex, Sui uses 32-byte addrs)
    let hex = addr_norm.trim_start_matches("0x");
    if hex.is_empty() || hex.len() > 64 || !hex.chars().all(|c| c.is_ascii_hexdigit()) {
        return None;
    }

    Some(format!("{addr_norm}::{module}::{ty}"))
}

pub fn looks_like_struct_tag(s: &str) -> bool {
    s.contains("::")
}

pub async fn ensure_coin_in_registry(
    reg: &mut CoinRegistry,
    path: &str,
    rpc_url: &str,
    coin_type_in: &str,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // normalize first
    let Some(norm) = normalize_struct_tag(coin_type_in) else {
        eprintln!(
            "⚠️  Not a valid struct tag: '{coin_type_in}'. Expected 0x..::module::Type. Skipping."
        );
        return Ok(());
    };

    if reg.contains_key(&norm) {
        return Ok(());
    }

    match fetch_coin_metadata(rpc_url, &norm).await {
        Ok(meta) => {
            let cm = CoinMeta {
                name: meta.name.clone(),
                symbol: meta.symbol.clone(),
                coin_type: norm.clone(),
                decimals: meta.decimals,
                icon_url: None,
                project_url: None,
                coingecko_id: None,
            };
            reg.insert(norm.clone(), cm);
            save_coin_registry(path, reg)?;
            println!("✅ Added {norm} ({}) to coins.json", meta.symbol);
        }
        Err(e) => {
            eprintln!("⚠️  Could not fetch metadata for {norm}: {e}");
        }
    }
    Ok(())
}

pub fn find_decimals(reg: &CoinRegistry, coin_type_in: &str) -> Option<u8> {
    let norm = normalize_struct_tag(coin_type_in)?;
    reg.get(&norm).map(|m| m.decimals)
}
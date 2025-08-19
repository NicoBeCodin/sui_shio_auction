# Sui Shio Auction

For the moment, this program connects to the shio auction websocket and prints out the pending auctions.
It also maintains a `coins.json` file that keeps a record of the decimals of a coin, for accurate price calculation.
Warning: The estimated swap price is for AMMs with CPMM, so it's wrong for most AMMs. 
### TODO
 - Calculate the opportunities that arise and a fair bid price
 - Submit bids to win auctions

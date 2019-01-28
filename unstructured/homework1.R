# Using either iextrading (no key needed) or alphavantage (key needed),
# pull the appropriate data for the calls in the provided data file. 
# Pull a week before and a week after the call date.


library(httr)

test = GET("https://api.iextrading.com/1.0/stock/aapl/chart")

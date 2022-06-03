# avfintools
This is a complement to the Alpha Vantage API. It processes financial data and 
provides useful visuals that anyone can pick up and gain insights from. It has
a very user-friendly data extraction protocol where users can retrieve the data,
of course, after putting in the following:

Use your own token to access the data.


```{r}
#av_api_key("YOUR_AV_KEY_HERE")

```

Useful tools such as the instant candlestick charts

```{r }
candles(SPYdaily)
```

and more complex tools such as Volume Analysis

```{r }
volume_analysis(SPYdaily, "SPY")
```

Happy Investing/Trading!

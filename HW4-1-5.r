---
SID: 24061277
Class: Stat133
title: "Hw 4"
output: html_document
---
  
```{r plot1}
readData = function(fileName,dateFormat="%m/%d/%y"){
  stockData = read.csv(fileName, 
                       colClasses=c("character", "NULL","NULL", "NULL","NULL","NULL", "numeric"))
  stockData$Date = as.Date(stockData$Date, dateFormat)
  stockData = stockData[order(stockData$Date), ]
  return(stockData)
}


######################################################
# When you analyze your data, you have to restrict your attention
# to the common period of time for which we have prices for two stocks. Write a function that
# computes the days in common between the two data sets of stock prices. This function returns a
# data frame with rows for each day with the date, adjusted closing prices for the two stocks and the
# ratio of the two stock prices, i.e.,
# adjusted price for stockA/adjusted price for stockB
# These variables are to be called Date, Adj.Close.A , Adj.Close.B, and ratio. The inputs to this
# function are the two data frames from the calls to readData(). Call these arguments, stockA and
# stockB. They are required arguments
######################################################

stockA = readData("http://www.stat.berkeley.edu/users/nolan/data/stocks/f.csv")

stockB = readData("http://www.stat.berkeley.edu/users/nolan/data/stocks/gm.csv")

combine2stocks = function(stockA, stockB){
  stockA = stockA[stockA$Date %in% stockB$Date, ]
  Date = stockA$Date
  Adj.Close.A = stockA$Adj.Close
  Adj.Close.B = stockB$Adj.Close
  ratio = stockA$Adj.Close/stockB$Adj.Close
  return(data.frame(Date, Adj.Close.A, Adj.Close.B, ratio))
}
```
######################################################
# You now have the inputs for the pairs trading scheme, namely the prices
# and ratio. It will be helpful in debugging your code to create a visualization to see where the ratio
# 5goes outside of some range or limit that would make an opening a position. A visualization will
# also help you understand the pairs trading strategy. This visualization doesn't define or implement
# the calculations for our rule. It merely displays the cutoff points. The function plotRatio() makes
# a line plot of the time series for the ratio and also draws the upper and lower horizontal lines for
# the pair trading rule, for a given k. The parameters for this function are: ratio, which is required;
# k, which has a default value of 1; date, which has a default value of seq(along = ratio) but
# can also handle a Date object; and the special parameter ..., which is used to pass additional
# arguments to plot() such as a title, axis labels, etc. The plot should look like the one in Figure 4.
# Be sure to write this function in such a way that to allow the caller to pass a vector for k, not just
# a single value. In this case, the function draws the bounds/lines for multiple trading rules.
######################################################

stockA = stockA[stockA$Date %in% stockB$Date, ]
plotRatio = function(ratio, k = 1, date = seq(along = ratio),...){
  ratio = combine2stocks(stockA, stockB)$ratio
  plot(ratio ~ stockA$Date, xlab = "Date", ylab = "Ratio", type = 'l',...)
  abline(h = mean(ratio), col = 'green', lty = 3)
  abline(h = mean(ratio) + k * sd(ratio), col = 'red', lty = 3)
  abline(h = mean(ratio) - k * sd(ratio), col = "red", lty = 3)
}

#######################################################
# Function 4: showPosition() This function augments the plot created by plotRatio() to display
# the start and end positions of the ratio time series. We haven't figured out how to compute these
# positions yet, but we can create this function and test it with example positions. This function has
# four arguments: pos, ratios, col, and radius. The pos argument is required and will be a twoelement
# numeric vector of the open and close positions. Likewise, the ratios argument is required
# and will be a two-element numeric vector of the ratio values for the open and close positions. The
# col parameter takes an optional vector of two colors, one for the open and the other for the close;
# and radius is an optional argument to provide the size of the circle to place on the plot (use 100 for
# the default). Note that if we call plotRatio() with the Date values for the horizontal axis, we must
# pass the actual Date values for the open and close dates for our trading position to showPosition().
# Alternatively, if our call to plotRatio() uses the indices of the ratio vector (1, 2, . . . ), then we call
# showPosition() with the indices for the start and end of our position. The additions should appear
# as in Figure5
#######################################################
showPosition = function(pos, ratios, col = 2:3, radius = 100,...){
  points(ratios~pos, cex = radius, pch = 1, col = col,...)
  return(plot)
}
```

# Stock Prices and Portfolio Optimization

library(quantmod)
library(rmutil)
library(lpSolve)
library(lmtest) 
library(sandwich)
library(rmutil)
library(dplyr)

set.seed(32)

###### Assumtpions ######
# 1.  You are only allowed to invest in a total of 5 stocks.
# 2.  Each stock will yield a certain percentage of profit or loss.
# 3.  The only information you have available is the stock data.
# 4.  You will be optimizing your selection for the next 30 trading days into the future.
# 5.  You will sell the stocks at their predicted values.
# 6.  You  should  be  able  to  earn  an  expected  return  of  at  least  $2,000  while  minimizing  the overall variance of the portfolio.

###### Objectives ######
# 1.  Compute the stock returns for the downloaded stock prices.
# 2.  Complete a descriptive analysis of the data for each stock.
# 3.  Construct an empirical model that will be used to predict the stock’s price in the future.
# 4.  Leverage the models to construct a Linear Program.  The optimal solution to the program will be used to find the optimal allocation

#1 
# Pick 5 stocks.  Stocks that are on the Dow Jones will work best for this project, but are not
# necessary.  Ensure its not a "no-name" stock (that is, pick so called "Blue Chips").  For each
# stock that you selected, download then using the getSymbols command. The remainder of the problems require you to solve the problem description for each stock you chose.

  getSymbols("WMT")   #Walmart

  getSymbols("IBM")   #IBM
  
  getSymbols("JPM")   #JPM or JPMorgan Chase 
  
  getSymbols("AXP")   #American Express
  
  getSymbols("NKE")   #Nike

#2
# Extract each column of the data frame to a vector (use the as.vector() function for each column). Store them in the variables open, high, low, close, and volume, respectively
  #Walmart
  wmt.open   <- as.vector(WMT$WMT.Open)
  wmt.high   <- as.vector(WMT$WMT.High)
  wmt.low    <- as.vector(WMT$WMT.Low)
  wmt.close  <- as.vector(WMT$WMT.Close)
  wmt.volume <- as.vector(WMT$WMT.Volume)  
  
  #IBM
  ibm.open   <- as.vector(IBM$IBM.Open)
  ibm.high   <- as.vector(IBM$IBM.High)
  ibm.low    <- as.vector(IBM$IBM.Low)
  ibm.close  <- as.vector(IBM$IBM.Close)
  ibm.volume <- as.vector(IBM$IBM.Volume) 
  
  #JPMorgan Chase
  jpm.open   <- as.vector(JPM$JPM.Open)  
  jpm.high   <- as.vector(JPM$JPM.High)
  jpm.low    <- as.vector(JPM$JPM.Low)
  jpm.close  <- as.vector(JPM$JPM.Close)
  jpm.volume <- as.vector(JPM$JPM.Volume)
  
  #American Express
  axp.open   <- as.vector(AXP$AXP.Open)
  axp.high   <- as.vector(AXP$AXP.High)
  axp.low    <- as.vector(AXP$AXP.Low)
  axp.close  <- as.vector(AXP$AXP.Close)
  axp.volume <- as.vector(AXP$AXP.Volume)
  
  #Nike
  nke.open   <- as.vector(NKE$NKE.Open)
  nke.high   <- as.vector(NKE$NKE.High)
  nke.low    <- as.vector(NKE$NKE.Low)
  nke.close  <- as.vector(NKE$NKE.Close)
  nke.volume <- as.vector(NKE$NKE.Volume) 

#3
#For the open, high, low, and close, convert each data set to a data set of returns
#(the current day’s  price  -  the  previous  day’s  price).   Store  each  result  
#in  the  variable  names  open_r, high_r, low_r, close_r, respectively.

  #function to get returns current day - previous day
  get_returns <-  
    function(prices)
    {
      
      price_length <- length(prices)
      return_prices <- c()
      
      for(i in 1:price_length-1)
      {
        returns <- prices[i+1] - prices[i]
        return_prices <- c(return_prices,returns)  
      }
      return(return_prices)
    }
  
  #Walmart
  wmt.open_r   <- get_returns(wmt.open)
  wmt.high_r   <- get_returns(wmt.high)
  wmt.low_r    <- get_returns(wmt.low)
  wmt.close_r  <- get_returns(wmt.close)   
  wmt.volume_r <- get_returns(wmt.volume)   
  
  #IBM
  ibm.open_r   <- get_returns(ibm.open)
  ibm.high_r   <- get_returns(ibm.high)
  ibm.low_r    <- get_returns(ibm.low)
  ibm.close_r  <- get_returns(ibm.close)  
  ibm.volume_r <- get_returns(ibm.volume)  
  
  #JPMorgan Chase
  jpm.open_r   <- get_returns(jpm.open)
  jpm.high_r   <- get_returns(jpm.high)
  jpm.low_r    <- get_returns(jpm.low)
  jpm.close_r  <- get_returns(jpm.close) 
  jpm.volume_r <- get_returns(jpm.volume) 
  
  #American Express
  axp.open_r   <- get_returns(axp.open)
  axp.high_r   <- get_returns(axp.high)
  axp.low_r    <- get_returns(axp.low)
  axp.close_r  <- get_returns(axp.close) 
  axp.volume_r <- get_returns(axp.volume) 
  
  #Nike
  nke.open_r   <- get_returns(nke.open)
  nke.high_r   <- get_returns(nke.high)
  nke.low_r    <- get_returns(nke.low)
  nke.close_r  <- get_returns(nke.close)  
  nke.volume_r <- get_returns(nke.volume)   

#4
# For  each  vector  created  above,  create  two  new  vectors:  one  for  the  "before"  returns  and
# another for the "current" returns. Ensure they all have the same length. If they do not, then
# remove the data from the beginning of the vector, NOT THE END.

  # lagging variables
  #Walmart
  wmt.open_r_before  <- wmt.open_r[-length(wmt.open_r)]
  wmt.open_r_current <- wmt.open_r[-1]
  
  wmt.high_r_before  <- wmt.high_r[-length(wmt.high_r)]
  wmt.high_r_current <- wmt.high_r[-1]
  
  wmt.low_r_before  <- wmt.low_r[-length(wmt.low_r)]
  wmt.low_r_current <- wmt.low_r[-1] 
  
  wmt.close_r_before  <- wmt.close_r[-length(wmt.close_r)]
  wmt.close_r_current <- wmt.close_r[-1]
  
  wmt.volume_r_before  <- wmt.volume_r[-length(wmt.volume_r)]
  wmt.volume_r_current <- wmt.volume_r[-1] 
  
  #IBM
  ibm.open_r_before  <- ibm.open_r[-length(ibm.open_r)]
  ibm.open_r_current <- ibm.open_r[-1]
  
  ibm.high_r_before  <- ibm.high_r[-length(ibm.high_r)]
  ibm.high_r_current <- ibm.high_r[-1]
  
  ibm.low_r_before  <- ibm.low_r[-length(ibm.low_r)]
  ibm.low_r_current <- ibm.low_r[-1] 
  
  ibm.close_r_before  <- ibm.close_r[-length(ibm.close_r)]
  ibm.close_r_current <- ibm.close_r[-1]
  
  ibm.volume_r_before  <- ibm.volume_r[-length(ibm.volume_r)]
  ibm.volume_r_current <- ibm.volume_r[-1]
  
  #JPMorgan Chase
  jpm.open_r_before  <- jpm.open_r[-length(jpm.open_r)]
  jpm.open_r_current <- jpm.open_r[-1]
  
  jpm.high_r_before  <- jpm.high_r[-length(jpm.high_r)]
  jpm.high_r_current <- jpm.high_r[-1]
  
  jpm.low_r_before  <- jpm.low_r[-length(jpm.low_r)]
  jpm.low_r_current <- jpm.low_r[-1] 
  
  jpm.close_r_before  <- jpm.close_r[-length(jpm.close_r)]
  jpm.close_r_current <- jpm.close_r[-1]
  
  jpm.volume_r_before  <- jpm.volume_r[-length(jpm.volume_r)]
  jpm.volume_r_current <- jpm.volume_r[-1]
  
  #American Express
  axp.open_r_before  <- axp.open_r[-length(axp.open_r)]
  axp.open_r_current <- axp.open_r[-1]
  
  axp.high_r_before  <- axp.high_r[-length(axp.high_r)]
  axp.high_r_current <- axp.high_r[-1]
  
  axp.low_r_before  <- axp.low_r[-length(axp.low_r)]
  axp.low_r_current <- axp.low_r[-1] 
  
  axp.close_r_before  <- axp.close_r[-length(axp.close_r)]
  axp.close_r_current <- axp.close_r[-1]
  
  axp.volume_r_before  <- axp.volume_r[-length(axp.volume_r)]
  axp.volume_r_current <- axp.volume_r[-1]
  
  #Nike
  nke.open_r_before  <- nke.open_r[-length(nke.open_r)]
  nke.open_r_current <- nke.open_r[-1]
  
  nke.high_r_before  <- nke.high_r[-length(nke.high_r)]
  nke.high_r_current <- nke.high_r[-1]
  
  nke.low_r_before  <- nke.low_r[-length(nke.low_r)]
  nke.low_r_current <- nke.low_r[-1] 
  
  nke.close_r_before  <- nke.close_r[-length(nke.close_r)]
  nke.close_r_current <- nke.close_r[-1]
  
  nke.volume_r_before  <- nke.volume_r[-length(nke.volume_r)]
  nke.volume_r_current <- nke.volume_r[-1] 

#5
# Refine each vector so that only the last 400 days are in the data set. 
# (Hint:  Each vector is by default ordered axpsed on time. Time runs from 1 to the length of the vector. That is, the
# newest stock price is put at the end of the vector). 

  #function to returns the last number of rows of a dataframe
  getRows <- function(number_rows,price_vector)
  {
    #start with 2 to n-1 to avoid capturing NA values from the variable lagging in the code above
    return (tail(price_vector[2:length(price_vector)-1],number_rows))
  }
  
  row_size <- 400
  
  #Walmart
  wmt.open             <- getRows(row_size,wmt.open)
  wmt.high             <- getRows(row_size,wmt.high)
  wmt.low              <- getRows(row_size,wmt.low)
  wmt.close            <- getRows(row_size,wmt.close)
  wmt.volume           <- getRows(row_size,wmt.volume)
  wmt.open_r           <- getRows(row_size,wmt.open_r) 
  wmt.high_r           <- getRows(row_size,wmt.high_r) 
  wmt.low_r            <- getRows(row_size,wmt.low_r) 
  wmt.close_r          <- getRows(row_size,wmt.close_r) 
  wmt.volume_r         <- getRows(row_size,wmt.volume_r) 
  wmt.open_r_before    <- getRows(row_size,wmt.open_r_before) 
  wmt.open_r_current   <- getRows(row_size,wmt.open_r_current) 
  wmt.high_r_before    <- getRows(row_size,wmt.high_r_before) 
  wmt.high_r_current   <- getRows(row_size,wmt.high_r_current) 
  wmt.low_r_before     <- getRows(row_size,wmt.low_r_before) 
  wmt.low_r_current    <- getRows(row_size,wmt.low_r_current) 
  wmt.close_r_before   <- getRows(row_size,wmt.close_r_before) 
  wmt.close_r_current  <- getRows(row_size,wmt.close_r_current) 
  wmt.volume_r_current <- getRows(row_size,wmt.volume_r_current) 
  wmt.volume_r_before  <- getRows(row_size,wmt.volume_r_before) 
  
  wmt_table <- data.frame(
    wmt.open,
    wmt.high,
    wmt.low,
    wmt.close,
    wmt.volume,
    wmt.open_r,
    wmt.high_r,
    wmt.low_r,
    wmt.close_r,
    wmt.volume_r,
    wmt.open_r_before,
    wmt.open_r_current,
    wmt.high_r_before,
    wmt.high_r_current,
    wmt.low_r_before,
    wmt.low_r_current,
    wmt.close_r_before,
    wmt.close_r_current,
    wmt.volume_r_current,   
    wmt.volume_r_before  
  )
  
  #IBM
  ibm.open             <- getRows(row_size,ibm.open)
  ibm.high             <- getRows(row_size,ibm.high)
  ibm.low              <- getRows(row_size,ibm.low)
  ibm.close            <- getRows(row_size,ibm.close)
  ibm.volume           <- getRows(row_size,ibm.volume)
  ibm.open_r           <- getRows(row_size,ibm.open_r) 
  ibm.high_r           <- getRows(row_size,ibm.high_r) 
  ibm.low_r            <- getRows(row_size,ibm.low_r) 
  ibm.close_r          <- getRows(row_size,ibm.close_r) 
  ibm.volume_r         <- getRows(row_size,ibm.volume_r) 
  ibm.open_r_before    <- getRows(row_size,ibm.open_r_before) 
  ibm.open_r_current   <- getRows(row_size,ibm.open_r_current) 
  ibm.high_r_before    <- getRows(row_size,ibm.high_r_before) 
  ibm.high_r_current   <- getRows(row_size,ibm.high_r_current) 
  ibm.low_r_before     <- getRows(row_size,ibm.low_r_before) 
  ibm.low_r_current    <- getRows(row_size,ibm.low_r_current) 
  ibm.close_r_before   <- getRows(row_size,ibm.close_r_before) 
  ibm.close_r_current  <- getRows(row_size,ibm.close_r_current) 
  ibm.volume_r_current <- getRows(row_size,ibm.volume_r_current) 
  ibm.volume_r_before  <- getRows(row_size,ibm.volume_r_before) 
  
  ibm_table <- data.frame(
    ibm.open,
    ibm.high,
    ibm.low,
    ibm.close,
    ibm.volume,
    ibm.open_r,
    ibm.high_r,
    ibm.low_r,
    ibm.close_r,
    ibm.volume_r,
    ibm.open_r_before,
    ibm.open_r_current,
    ibm.high_r_before,
    ibm.high_r_current,
    ibm.low_r_before,
    ibm.low_r_current,
    ibm.close_r_before,
    ibm.close_r_current,
    ibm.volume_r_current,   
    ibm.volume_r_before  
  ) 
  
  #JPMorgan Chase
  jpm.open             <- getRows(row_size,jpm.open)
  jpm.high             <- getRows(row_size,jpm.high)
  jpm.low              <- getRows(row_size,jpm.low)
  jpm.close            <- getRows(row_size,jpm.close)
  jpm.volume           <- getRows(row_size,jpm.volume)
  jpm.open_r           <- getRows(row_size,jpm.open_r) 
  jpm.high_r           <- getRows(row_size,jpm.high_r) 
  jpm.low_r            <- getRows(row_size,jpm.low_r) 
  jpm.close_r          <- getRows(row_size,jpm.close_r) 
  jpm.volume_r         <- getRows(row_size,jpm.volume_r) 
  jpm.open_r_before    <- getRows(row_size,jpm.open_r_before) 
  jpm.open_r_current   <- getRows(row_size,jpm.open_r_current) 
  jpm.high_r_before    <- getRows(row_size,jpm.high_r_before) 
  jpm.high_r_current   <- getRows(row_size,jpm.high_r_current) 
  jpm.low_r_before     <- getRows(row_size,jpm.low_r_before) 
  jpm.low_r_current    <- getRows(row_size,jpm.low_r_current) 
  jpm.close_r_before   <- getRows(row_size,jpm.close_r_before) 
  jpm.close_r_current  <- getRows(row_size,jpm.close_r_current) 
  jpm.volume_r_current <- getRows(row_size,jpm.volume_r_current) 
  jpm.volume_r_before  <- getRows(row_size,jpm.volume_r_before) 
  
  jpm_table <- data.frame(
    jpm.open,
    jpm.high,
    jpm.low,
    jpm.close,
    jpm.volume,
    jpm.open_r,
    jpm.high_r,
    jpm.low_r,
    jpm.close_r,
    jpm.volume_r,
    jpm.open_r_before,
    jpm.open_r_current,
    jpm.high_r_before,
    jpm.high_r_current,
    jpm.low_r_before,
    jpm.low_r_current,
    jpm.close_r_before,
    jpm.close_r_current,
    jpm.volume_r_current,   
    jpm.volume_r_before  
  )
  
  #American Express
  axp.open             <- getRows(row_size,axp.open)
  axp.high             <- getRows(row_size,axp.high)
  axp.low              <- getRows(row_size,axp.low)
  axp.close            <- getRows(row_size,axp.close)
  axp.volume           <- getRows(row_size,axp.volume)
  axp.open_r           <- getRows(row_size,axp.open_r) 
  axp.high_r           <- getRows(row_size,axp.high_r) 
  axp.low_r            <- getRows(row_size,axp.low_r) 
  axp.close_r          <- getRows(row_size,axp.close_r) 
  axp.volume_r         <- getRows(row_size,axp.volume_r) 
  axp.open_r_before    <- getRows(row_size,axp.open_r_before) 
  axp.open_r_current   <- getRows(row_size,axp.open_r_current) 
  axp.high_r_before    <- getRows(row_size,axp.high_r_before) 
  axp.high_r_current   <- getRows(row_size,axp.high_r_current) 
  axp.low_r_before     <- getRows(row_size,axp.low_r_before) 
  axp.low_r_current    <- getRows(row_size,axp.low_r_current) 
  axp.close_r_before   <- getRows(row_size,axp.close_r_before) 
  axp.close_r_current  <- getRows(row_size,axp.close_r_current) 
  axp.volume_r_current <- getRows(row_size,axp.volume_r_current) 
  axp.volume_r_before  <- getRows(row_size,axp.volume_r_before) 
  
  axp_table <- data.frame(
    axp.open,
    axp.high,
    axp.low,
    axp.close,
    axp.volume,
    axp.open_r,
    axp.high_r,
    axp.low_r,
    axp.close_r,
    axp.volume_r,
    axp.open_r_before,
    axp.open_r_current,
    axp.high_r_before,
    axp.high_r_current,
    axp.low_r_before,
    axp.low_r_current,
    axp.close_r_before,
    axp.close_r_current,
    axp.volume_r_current,   
    axp.volume_r_before  
  )
  
  #Nike
  nke.open             <- getRows(row_size,nke.open)
  nke.high             <- getRows(row_size,nke.high)
  nke.low              <- getRows(row_size,nke.low)
  nke.close            <- getRows(row_size,nke.close)
  nke.volume           <- getRows(row_size,nke.volume)
  nke.open_r           <- getRows(row_size,nke.open_r) 
  nke.high_r           <- getRows(row_size,nke.high_r) 
  nke.low_r            <- getRows(row_size,nke.low_r) 
  nke.close_r          <- getRows(row_size,nke.close_r) 
  nke.volume_r         <- getRows(row_size,nke.volume_r) 
  nke.open_r_before    <- getRows(row_size,nke.open_r_before) 
  nke.open_r_current   <- getRows(row_size,nke.open_r_current) 
  nke.high_r_before    <- getRows(row_size,nke.high_r_before) 
  nke.high_r_current   <- getRows(row_size,nke.high_r_current) 
  nke.low_r_before     <- getRows(row_size,nke.low_r_before) 
  nke.low_r_current    <- getRows(row_size,nke.low_r_current) 
  nke.close_r_before   <- getRows(row_size,nke.close_r_before) 
  nke.close_r_current  <- getRows(row_size,nke.close_r_current) 
  nke.volume_r_current <- getRows(row_size,nke.volume_r_current) 
  nke.volume_r_before  <- getRows(row_size,nke.volume_r_before) 
  
  nke_table <- data.frame(
    nke.open,
    nke.high,
    nke.low,
    nke.close,
    nke.volume,
    nke.open_r,
    nke.high_r,
    nke.low_r,
    nke.close_r,
    nke.volume_r,
    nke.open_r_before,
    nke.open_r_current,
    nke.high_r_before,
    nke.high_r_current,
    nke.low_r_before,
    nke.low_r_current,
    nke.close_r_before,
    nke.close_r_current,
    nke.volume_r_current,   
    nke.volume_r_before  
  )

#6
# Construct an empirical model for the closing returns of the "current" days (vector) using
# the other vectors as the independent variable. Take a similar process that we took the other
# day in class to ensure that you have a "good" specification. 

  #Walmart
  wmt_model <- lm(wmt_table$wmt.close_r_current ~ wmt_table$wmt.high_r + wmt_table$wmt.low_r,data = wmt_table) 
  summary(wmt_model)
  
  #IBM
  ibm_model <- lm(ibm_table$ibm.close_r_current ~ ibm_table$ibm.high_r + ibm_table$ibm.low_r,data = ibm_table) 
  summary(ibm_model)
  
  #JPMorgan Chase
  jpm_model <- lm(jpm_table$jpm.close_r_current ~  jpm_table$jpm.open_r + jpm_table$jpm.high_r + jpm_table$jpm.low_r + jpm_table$jpm.close_r,data = jpm_table) 
  summary(jpm_model)
  
  #American Express
  axp_model <- lm(axp_table$axp.close_r_current ~ axp_table$axp.high_r + axp_table$axp.low_r + axp_table$axp.volume_r,data = axp_table) 
  summary(axp_model)
  
  #Nike
  nke_model <- lm(nke_table$nke.close_r_current ~ nke_table$nke.high_r + nke_table$nke.low_r,data = nke_table)
  summary(nke_model)

# best empirical models for each stock

#       walmart_close_r_current = B0 + B1*high_r + B2*low_r   + sigma
#       ibm_close_r_current = B0 + B1*high_r + B2*low_r   + sigma
#       jpmorganchase_close_r_current = B0 + B1*open_r + B2*high_r  + B3*low_r    + B4*close_r + sigma
#       americanexpress_close_r_current = B0 + B1*high_r + B2*low_r   + B3*volume_r + sigma
#       nike_close_r_current = B0 + B1*open_r + B2*high_r  + B3*low_r    + sigma

#7
# Test the model for heteroscdasticity. 
# If it has it, then fix it (refer to the Hint Guide to figure out how to do this).

  bptest(wmt_model)
  #Walmart: p-value is 0.22, fail to reject null hypothesis, model has homoscedasticity
  
  bptest(ibm_model)
  #IBM: p-value is 0.12, fail to reject null hypothesis, model has homoscedasticity
  
  bptest(jpm_model)
  #JPMorgan Chase:  p-value is 0.86, fail to reject null hypothesis, model has homoscedasticity
  
  bptest(axp_model)
  #American Express: p-value is 0.19, fail to reject null hypothesis, model has homoscedasticity
  
  bptest(nke_model)
  #Nike: p-value is 0.09, fail to reject null hypothesis, model has homoscedasticity  

#8
#  Which variables are significant?  Explain which variables have a positive or negative effect
#  on the dependent variable.  

  ### Walmart ###
  #   In our model for Walmart, the following variables are significant;
  #   VARIABLE      EFFECT  
  #   wmt.high_r   positive
  #   wmt.low_r    positive
  
  ### IBM ###
  #   In our model for IBM, the following variables are significant;
  #   VARIABLE      EFFECT  
  #   ibm.high_r    positive
  #   ibm.low_r     positive
  
  ### JPMorgan Chase ###
  #   In our model for JPMorgan Chase, the following variables are significant;
  #   VARIABLE      EFFECT  
  #   jpm.open_r    positive
  #   jpm.high_r    negative
  #   jpm.low_r     negative  
  #   jpm.close_r   positive  
  
  ### American Express ###
  #   In our model for American Express, the following variables are significant;
  #   VARIABLE      EFFECT  
  #   axp.high_r     positive
  #   axp.low_r      positive  
  #   axp.volume_r   positive , not significant 
  
  ### Nike ###
  #   In our model for Nike, the following variables are significant;
  #   VARIABLE      EFFECT  
  #   nke.high_r    positive
  #   nke.low_r     positive    

#9
#Write a function that will conduct a single iteration of the simulation. 

  #paramaters: 
    #lm_model: linear regression model
    #lm_model_coeff : vector containing coefficient names
    #data_table: stock data table
  initiate_simulation <- function(lm_model,lm_model_coeff,data_table)
  {
    #empty dataframe that will mimic the coefficients of the linear model coefficients order
    data_result_table <- data.frame(matrix(ncol = length(lm_model_coeff),nrow = 0))
    x <- c(lm_model_coeff)
    colnames(data_result_table) <- x
    
    #will hold the result calculations
    simulation_results <- c()
    for (i in 1:30) {
      for (iteration in lm_model_coeff) {
        #get all the column data
        column_sim   <- select(data_table,iteration)
        mean_sim     <- sapply(column_sim,mean)
        sd_sim       <- sapply(column_sim,sd)
        
        # test for normal distribution
        norm_dist    <- rnorm(400,mean = mean_sim,sd = sd_sim)
        norm_ks_test <- ks.test(column_sim,norm_dist)
        
        # test for laplace distribution        
        laplace_dist    <- rlaplace(400, m = mean_sim, s = sd_sim)
        laplace_ks_test <- ks.test(column_sim,laplace_dist)
        
        if(norm_ks_test$p.value >= laplace_ks_test$p.value)
        {
          sample_var <- sample(rnorm(400,mean = mean_sim,sd = sd_sim),1)
        }
        else{
          sample_var <- sample(rlaplace(400, m = mean_sim, s = sd_sim),1)
        }
        #if the variable is a before return, then use the last entry in dataset
        if(grepl("before",iteration))
        {
          sample_var <- tail(column_sim,1)
        }
        #calculate current close_r by using the variables we measured from the above distribution tests
        simulation_results <- c(simulation_results,lm_model$coefficients[iteration]*sample_var)
      }
      error_lm          <- rnorm(1,mean = 0, sd = sd(lm_model$residuals))
      predicted_result  <- sum(unlist(simulation_results)) + unname(lm_model$coefficients['intercept']) + error_lm
      data_result_table[nrow(data_result_table)+1,] <- unlist(simulation_results)
    }
    return(predicted_result)
  }
  
  #rename the linear regression coefficients so we can iterate through them more easily 
  names(wmt_model$coefficients)   <- c('intercept','wmt.high_r','wmt.low_r')
  names(ibm_model$coefficients)   <- c('intercept','ibm.high_r','ibm.low_r')
  names(jpm_model$coefficients)   <- c('intercept','jpm.open_r','jpm.high_r','jpm.low_r','jpm.close_r')
  names(axp_model$coefficients)   <- c('intercept','axp.high_r','axp.low_r','axp.volume_r')
  names(nke_model$coefficients)   <- c('intercept','nke.high_r','nke.low_r')

#10
# Now write the core of the simulation
  simulation_core <- function(input_model,input_coef,input_data){
  
    results <- c()
    for(simulation_index in 1:10)
    {
      simulation_output <- initiate_simulation(input_model,
                                               input_coef,
                                               input_data)
      results <- c(results,simulation_output)
    
    }
    return(results)
  }
  #run simulation for each stock
  wmt_simulation_results   <- simulation_core(wmt_model,c('wmt.high_r','wmt.low_r'),wmt_table)
   ibm_simulation_results  <- simulation_core(ibm_model,c('ibm.high_r','ibm.low_r'),ibm_table)
  jpm_simulation_results   <- simulation_core(jpm_model,c('jpm.open_r','jpm.high_r','jpm.low_r','jpm.close_r'),jpm_table)
    axp_simulation_results <- simulation_core(axp_model,c('axp.high_r','axp.low_r','axp.volume_r'),axp_table)
   nke_simulation_results  <- simulation_core(nke_model,c('nke.high_r','nke.low_r'),nke_table)

#11
# When the simulation is complete, find the average return and store it.   
   mean_wmt_simulation_results   <- mean(wmt_simulation_results) 
   mean_ibm_simulation_results   <- mean(ibm_simulation_results) 
   mean_jpm_simulation_results   <- mean(jpm_simulation_results) 
   mean_axp_simulation_results   <- mean(axp_simulation_results) 
   mean_nke_simulation_results   <- mean(nke_simulation_results)
   
  simulation_table <- data.frame(wmt_simulation_results,
                                 ibm_simulation_results,
                                 jpm_simulation_results,
                                 axp_simulation_results,
                                 nke_simulation_results
                                 )
#12   
#  When the simulation is complete for ALL stocks, take each vector, store it in a data frame.
#  Then, run the cov function to compute the variance-covariance matrix.  Store this matrix in a variable.

    var_cov <- cov(simulation_table)

#                        wmt_simulation_results ibm_simulation_results jpm_simulation_results axp_simulation_results nke_simulation_results
# wmt_simulation_results              26.831631             -20.839875              -4.339673             17.0800550             -8.7754252
# ibm_simulation_results             -20.839875              72.680456              -2.794826            -19.8464364              3.0457947
# jpm_simulation_results              -4.339673              -2.794826              66.620054             -5.5464475              1.7483537
# axp_simulation_results              17.080055             -19.846436              -5.546448             26.6007358              0.3921834
# nke_simulation_results              -8.775425               3.045795               1.748354              0.3921834              6.9899860    

#13
# Plug this model into R using the lpSolve
# package. Solve it using this package and find the
# optimal allocation of investments.

#objective function
  simulation_means <- c(mean_wmt_simulation_results,mean_ibm_simulation_results,mean_jpm_simulation_results,mean_axp_simulation_results,mean_nke_simulation_results)
  
  obj = c()
  for(i in 1:nrow(var_cov)){
    total = 0
    for(j in 1:ncol(var_cov)){
      total = total + (simulation_means[i] * simulation_means[j] * var_cov[i,j])
    }
    obj = c(obj, total)
  }


#direction
  con_dir <- c(">=", "<=")

#constraint
  matrix_constraint = matrix(
    c(mean_wmt_simulation_results,mean_ibm_simulation_results,mean_jpm_simulation_results,mean_axp_simulation_results,mean_nke_simulation_results,
      1, 1, 1, 1, 1),
    nrow = 2, byrow = TRUE
  )

  constraint <- c(2000,20000)

#right hand side of constrains 

  lp_results <- lp(
                  direction = "min",
                  objective.in = obj,
                  const.mat = matrix_constraint, 
                  const.dir = con_dir, 
                  const.rhs = constraint,
                  all.int = TRUE,
                  compute.sens = TRUE                
                )
  
  lp_results
  lp_results$solution
  
  # After optimization, the results recommend that the following amounts need to be invested per stock
  
  #> lp_results$solution
  #[1] 17682     0     0     0  2318
  
  # After optimization, invest 17682 in Walmart and 2318 in Nike. 
  # Optimization recommends that we do not invest in the following stocks; 
  # IBM, JPMorgan Chase, and American Express. 


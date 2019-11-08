library(neuralnet)
library(quantmod)
library(scales)
library(DMwR)
library(dplyr)
library(ggplot2)
library(Metrics)
options("scipen"=10, "digits"=4)

training <- function(stock, lagging, layers, threshold, seed){
  values <- getSymbols(stock, env=NULL, src="yahoo", from="2016-01-01", to="2018-01-01")
  head(values)

  lag <- c(1:lagging)
  OP <- scale(as.numeric(values[,1]))
  OP1 <- Lag(OP,k=lag)
  CP <- scale(as.numeric(values[,4]))
  CP1 <- Lag(CP,k=lag)
  LP <- scale(as.numeric(values[,3]))
  LP1 <- Lag(LP,k=lag)
  HP <- scale(as.numeric(values[,2]))
  HP1 <- Lag(HP,k=lag)
  Vol <- scale(as.numeric(values[,5]))
  Vol1 <- Lag(Vol,k=1)
  Rsi <- scale(as.numeric(RSI(CP,10)))
  Rsi1 <- Lag(Rsi,k=1)
  Ema <- scale(as.numeric(EMA(CP,10)))
  Ema1 <- Lag(Ema,k=1)

  plotData <- ts(cbind(OP, CP, LP, HP, Vol, Rsi, Ema))
  colnames(plotData) <- c("Opening Price", "Closing Price", "High Price", "Low Price", "Volume", "RSI", "EMA")
  plot(plotData)

  data <- cbind(LP,HP,OP1,CP1,HP1,LP1,Vol1,Rsi1,Ema1)
  colnames(data) <- c("LP", "HP", sprintf("input%d",seq(1:(ncol(data)-2))))
  data <- na.omit(data)
  scaledData <- ts(data)

  head(scaledData)

  index <- round(0.75*nrow(data))
  trainData <- scaledData[1:index,]
  testData <- scaledData[(index+1):nrow(data),]
  
  set.seed(seed)
  
  nn <- neuralnet(as.formula(LP + HP ~ .), trainData, 
                 hidden = layers, algorithm="rprop+", stepmax=1e+06, threshold = threshold)
  
  cat("The error rate for the training set is:",nn$result.matrix[1:1,],".\n")
  
  lastCP <- values[nrow(values),4]
  
  return (list(nn, testData, LP, HP, lastCP))
}

testing <- function(nn){
  neuralNet <- nn[[1]]
  testData <- nn[[2]]
  LP <- nn[[3]]
  HP <- nn[[4]]

  ##Calculate next day highs and lows based on the previous days values
  expected <- neuralnet::compute(neuralNet, testData[,3:(ncol(testData))])
  actualLow <- unscale(testData[,1],LP);
  actualHigh <- unscale(testData[,2],HP);

  predictedLow <- unscale(expected$net.result[,1],LP)
  predictedHigh <- unscale(expected$net.result[,2],HP)

  valueMatrix <- ts(cbind(predictedLow, predictedHigh, actualLow, actualHigh))
  colnames(valueMatrix) <- c("Predicted Low", "Predicted High", "Actual Low", "Actual High")
  valueMatrix

  cat("The RMSE for the Low Price values is:",rmse(actualLow, predictedLow), "\n")
  cat("The RMSE for the High Price values is:",rmse(actualHigh, predictedHigh), "\n")

  par(mfrow = c(2,1))
  ts.plot(ts(valueMatrix[,1]),ts(valueMatrix[,3]), gpars = list(col = c("red", "blue")),lty=1, main=("Testing Performance Low Price AAPL (Unscaled)"))
  legend("topleft", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty=1)

  ts.plot(ts(valueMatrix[,2]),ts(valueMatrix[,4]), gpars = list(col = c("red", "blue")),lty=1, main=("Testing Performance High Price AAPL (Unscaled)"))
  legend("topleft", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty=1)
  
  return (valueMatrix)
}

trading <- function(values, cap, variance, return, lastCP){
  ##Automated trading based on 
  initial = cap
  capital = cap
  numStocks = 0
  numHold = 0
  buyPrice = 0

  for(i in 1:nrow(values)){
    pLow <- values[i,1]
    pHigh <- values[i,2]
    aLow <- values[i,3]
    aHigh <- values[i,4]
    marginLow <- c((1-variance)*pLow, (1+variance)*pLow)
    marginHigh <- c((1-variance)*pHigh, (1+variance)*pHigh)
    
    if(between(aLow,marginLow[1],marginLow[2]) && (numStocks==0)){
      numStocks = capital/marginLow[2]
      buyPrice = marginLow[2]
      prevCapital <- capital
      capital=0
      cat("Day: ", i, "\n")
      if(numHold>0){
        cat("After holding for ", numHold, "days... \n")
        numHold <- 0
      }
      cat("Buy Stocks! \nNum stocks:", numStocks, "\nCapital:", capital, "\nPrice of buy is:", buyPrice,"\n-----------------------\n")
    } else if(between(aHigh,marginHigh[1],marginHigh[2]) && (capital==0) && (marginHigh[1]>(1+return)*buyPrice)){ ##Sell rules
      capital <- numStocks*marginHigh[1]
      fee <- 0.05*(capital-prevCapital)
      capital <- capital - fee
      cat("Day: ", i, "\n")
      if(numHold>0){
        cat("After holding for ", numHold, "days... \n")
        numHold <- 0
      }
      numStocks <- 0
      cat("Sell stocks! \nNum stocks:", numStocks, "\nCapital:", capital, "\nPrice of sell is:", marginHigh[1],"\n-----------------------\n")
    } else {
      numHold <- numHold + 1
    }
  }
  if(numStocks!=0){
    capital <- numStocks*lastCP
    cat("Day:",i,"\nSelling on the last day!\nThe price of sell is:",aLow,"\nThe initial capital was:",initial ,"\nThe final capital is:", capital, " \nThe profit made is: ",(capital-initial),"\n")
    cat("After:",nrow(values),"days the capital is increased by", ((capital-initial)/initial*100),"%.\n")
  } else {
    cat("Day:",i,"\nThe initial capital was:", initial, " \nThe final capital is:", capital, " \nThe profit made is: ", (capital-initial),"\n")
    cat("After:",nrow(values),"days the capital is increased by", ((capital-initial)/initial*100),"%.\n")
  }
  
}

##Mean
meanAT <- function(stock, cap, variance, return, meanDays){
  prices <- getSymbols(stock, env=NULL, src="yahoo", from="2016-01-01", to="2018-01-01")
  head(prices)
  
  HPActual <- ts(as.numeric(tail(prices[,2],123)))
  LPActual <- ts(as.numeric(tail(prices[,3],123)))
  
  HPPred <- ts(apply(na.omit(Lag(as.numeric(prices[,2]),k=seq(1:meanDays))),1,mean))
  LPPred <- ts(apply(na.omit(Lag(as.numeric(prices[,3]),k=seq(1:meanDays))),1,mean))

  HPPred <- tail(HPPred, 123)
  LPPred <- tail(LPPred, 123)
  
  lastCP <- prices[nrow(prices),4]
  
  cat("Using the mean of the last 7 days as an alternative to a neural network.\n")
  cat("The RMSE for the Low Price values is:",rmse(LPActual, LPPred), "\n")
  cat("The RMSE for the High Price values is:",rmse(HPActual, HPPred), "\n")
 
  par(mfrow = c(2,1))
  ts.plot(ts(LPPred),ts(LPActual), gpars = list(col = c("red", "blue")),lty=1, main=("Mean Value Performance Low Price AAPL (Unscaled)"))
  legend("topleft", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty=1)
  
  ts.plot(ts(HPPred),ts(HPActual), gpars = list(col = c("red", "blue")),lty=1, main=("Mean Value Performance High Price AAPL (Unscaled)"))
  legend("topleft", legend = c("Predicted", "Actual"), col = c("red", "blue"), lty=1)
  
  values <- cbind(LPPred, HPPred, LPActual, HPActual)
  colnames(values) <- c("Predicted Low", "Predicted High", "Actual Low", "Actual High")
  
  cat("---------------------------------------------\n")
  trading(values, cap, variance, return, lastCP)
}

##AAPL, VNET
stock <- "AAPL"
lagging <- 3
layers <- c(10,10,10)
threshold <- 0.01
capital <- 10000
variance <- 0.01
seed <- 2
return <- 0.06
meanDays <- 7

##Run the neural network on the training set with the specified parameters
nn <- training(stock, lagging, layers, threshold, seed)

##Predict the values for the test set and calculate the RMSE and plot the predictions against the actual data
val <- testing(nn)

##Perform short-term trading based on the predictions and other predetermined rules and print a summary of the actions
trading(val, capital, variance, return,as.numeric(nn[[5]]))

##Alternative method for short-term trading using the mean of the last n days as a prediction
meanAT(stock, capital, variance, return, meanDays)

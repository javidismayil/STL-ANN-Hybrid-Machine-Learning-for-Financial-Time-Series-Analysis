library(dplyr)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(ggplot2)
library(Metrics)
library(tictoc)
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2023-10-15")
getSymbols('AAPL', src = "yahoo", from = start_date, to = end_date)
df<-AAPL
chartSeries(AAPL)
quantmod::addMACD()
quantmod::addRSI()
#################
returnn<-CalculateReturns(df$AAPL.Adjusted)
returnn<-returnn[-c(1),]
chart_Series(returnn)
chart.Histogram(returnn, methods = c("add.density", "add.normal"), 
                colorset = c("blue", "red", "black"))
legend("topright", legend = c("return", "kernel", "normal dist"), fill = c("blue", "red", "black"))
annanalysisy<-function(data,lag, test, validation, hidden1, hidden2, rep, rate){
  library("xts")
  library("zoo")
  library("quantmod")
  library("neuralnet")
  library("Metrics")
  library("tictoc")
  ##########################fonksiyonlar
  normalization<-function(x){
    normalize<-(x-min(x))/(max(x)-min(x))
    return(normalize)
  }
  annormalization<-function(x,y){
    annorm<-(y*(max(x)-min(x))+min(x))
    return(annorm)
  }
  
  #data original data set
  #lag
  data<-data.frame(data)
  numrow<-nrow(data)
  dataframe13<-list()
  for (i in 1:lag){
    dataframe13[[i]]=na.omit(cbind(Lag(data, k=1:i), data))
  }
  for (i in 1:lag){
    n=ncol(dataframe13[[i]])
    colnames(dataframe13[[i]])=c(rep(paste("lag", 1:i, sep="")), "y")
  }
  size13<-list()
  for (i in 1:lag){
    size13[[i]]=c(((numrow-test-validation)-i), ((numrow-test)-i))
  }
  subdataframe13<-list()
  for (k in 1:lag){
    subdataframe13[[k]]=head(dataframe13[[k]], (size13[[k]][1]+test))
  } 
  for (k in 1:lag){
    apply(subdataframe13[[k]], 2, range)
  }
  maxvalue13=list()
  minvalue13=list()
  for (k in 1:lag){
    maxvalue13[[k]]=apply(subdataframe13[[k]], 2, max)
    minvalue13[[k]]=apply(subdataframe13[[k]], 2, min)
  }
  for (k in 1:lag){
    subdataframe13[[k]]=normalization(subdataframe13[[k]])
  }
  train13<-list()
  test13<-list()
  allvar13<-list()
  
  for (k in 1:lag){
    allvar13[[k]]=colnames(subdataframe13[[k]])
  }
  predicvar13<-list()
  
  for (k in 1:lag){
    predicvar13[[k]]=allvar13[[k]][!allvar13[[k]]%in%"y"]
  }
  
  for (k in 1:lag){
    predicvar13[[k]]<-paste(predicvar13[[k]], collapse = "+")
  }
  
  form13=list()
  for (k in 1:lag){
    form13[[k]]=as.formula(paste("y~", predicvar13[[k]], collapse = "+"))
  }
  for (k in 1:lag){
    #burada test degeri ne 
    train13[[k]]=head(subdataframe13[[k]], (nrow(subdataframe13[[k]])-test))
  }
  for (k in 1:lag){
    test13[[k]]=tail(subdataframe13[[k]], test)
  }    
  selecetvar13<-expand.grid(c(1:hidden1), c(1:hidden2))
  neural13<-replicate(lag, list(), simplify = FALSE)
  tic()
  for (i in 1:lag){
    for(k in 1:(hidden1*hidden2)){
      neural13[[i]][[k]]=neuralnet(formula = form13[[i]], 
                                   hidden = c(selecetvar13[k,1], selecetvar13[k,2]),
                                   linear.output = FALSE, data=train13[[i]], err.fct = "sse", 
                                   stepmax = 1e6, learningrate = rate, rep=rep)       
    }
  }
  toc()
  
  
  ######## 
  
  predictions<-replicate(lag, list(), simplify = FALSE)
  for (i in 1:lag){
    for(k in 1:(hidden1*hidden2)){
      predictions[[i]][[k]]<-neuralnet::compute(neural13[[i]][[k]], test13[[i]][,c(1:(i+1))])
    }
  }
  predictions13_<-replicate(lag, list(), simplify = FALSE)
  for (i in 1:lag){
    for (k in 1:(hidden1*hidden2)){
      predictions13_[[i]][[k]]<-annormalization(dataframe13[[i]][(nrow(subdataframe13[[i]])-validation):nrow(subdataframe13[[i]]), i+1],
                                                predictions[[i]][[k]]$net.result)
    }
  }
  
  mae1<-replicate(lag, list(), simplify = FALSE)
  for (i in 1:lag){
    for (k in 1:(hidden1*hidden2)){
      mae1[[i]][[k]]=rmse(predictions13_[[i]][[k]], dataframe13[[i]][(size13[[i]][1]+1):(size13[[i]][1]+validation), i+1])
    }
  }
  
  snc<-list()
  nkac<-list()
  for (i in 1:lag){
    nkac[[i]]<-which.min(mae1[[i]])
    snc[[i]]<-mae1[[i]][[nkac[[i]]]]
  }
  gecikme<-which.min(snc)
  hucre<-nkac[[gecikme]]
  selecetvar13[nkac[[gecikme]],2]
  
  #########################################
  
  subdataframe13_<-list()
  for (i in 1:lag){
    subdataframe13_[[i]]<-head(dataframe13[[i]], ((size13[[i]][2])+validation))
  }
  
  for (k in 1:lag){
    apply(subdataframe13_[[k]],2, range)
  }
  minvalue13_<-list()
  maxvalue13_<-list()
  for (k in 1:lag){
    maxvalue13_[[k]]<-apply(subdataframe13_[[k]],2, max)
  }
  for (k in 1:lag){
    minvalue13_[[k]]<-apply(subdataframe13_[[k]],2, min)
  }
  for (k in 1:lag){
    subdataframe13_[[k]]<-normalization(subdataframe13_[[k]])
  }
  train13_<-list()
  test13_<-list()
  for (k in 1:lag){
    train13_[[k]]<-head(subdataframe13_[[k]],(nrow(subdataframe13_[[k]]))-test)
  }
  for (k in 1:lag){
    test13_[[k]]<-tail(subdataframe13_[[k]],test)
  }
  
  
  neural13_<-neuralnet(formula = form13[[gecikme]], 
                       hidden = c(selecetvar13[nkac[[gecikme]],1],selecetvar13[nkac[[gecikme]],2]),
                       linear.output = FALSE, data=train13_[[gecikme]], err.fct = "sse", 
                       stepmax = 1e6, learningrate = rate, rep=rep)  
  
  
  predictions_<-neuralnet::compute(neural13_, test13_[[gecikme]][,c(1:gecikme)])
  predictions_1<-annormalization(dataframe13[[gecikme]][(nrow(subdataframe13_[[gecikme]])-test):nrow(subdataframe13_[[gecikme]]), gecikme+1],
                                 predictions_$net.result)
  my_list<-list(`formula`= form13, `trainsset`=train13, `validation`=test13, 
                `trainvalid`=subdataframe13, `neural`=neural13_, `predictions`=predictions_1)
  return(my_list)
}  

##### stl_ann
tic()
aaa<-df$AAPL.Close
bb<-ts(aaa, frequency = 7)
a<-decompose(bb)
seas<-as.data.frame(a$seasonal)
trend<-as.data.frame(a$trend)
remainderr<-as.data.frame(a$random)
seapredict<-annanalysisy(data=seas, lag=12, test =24, validation = 24,
                           hidden1=10, hidden2=10, rep=1)
trendpredict<-annanalysisy(data=trend, lag=12, test = 24, validation = 24,
                             hidden1=10, hidden2=10, rep=1)
remainderpredict<-annanalysisy(data=remainderr, lag=12, test = 24, validation = 24,
                                 hidden1=10, hidden2=10, rep=1)
allpredict<-seapredict$predictions+trendpredict$predictions+remainderpredict$predictions
mase(bb[437:448], allpredict)
mae(bb[437:448], allpredict)
rmse(bb[437:448], allpredict)
mape(bb[437:448], allpredict)
toc()
allpredict
mase(aa$`ETH-USD.Close`[189:200], allpredict)
cbind(bb[437:448], allpredict)
ts.plot(aa$`ETH-USD.Close`[1:200])
length(bb)
ts.plot(bb, allpredict)
aaa$AAPL.Close
# Create a data frame with common_x, bb[437:448], and allpredict
data <- data.frame(
  common_x = 1:length(allpredict),
  bb_values = bb[425:448],
  allpredict_values = allpredict
)

# Create the plot
ggplot(data) +
  geom_line(aes(x = common_x, y = bb_values), color = "blue", linetype = "solid") +
  geom_line(aes(x = common_x, y = allpredict_values), color = "red", linetype = "dashed") +
  labs(title = "Two Lines with Common X-Axis") +
  xlab("X-Axis Label") +
  ylab("Y-Axis Label")
plot(seapredict$neural)

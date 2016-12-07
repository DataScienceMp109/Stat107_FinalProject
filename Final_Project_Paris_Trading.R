library(moments)
library(boot)
library(logspline)
library(quantmod)
require(ggplot2)

# 
# Pairs Trading 
# (1)random walk
getSymbols('BBY',from="2012-01-02")

BBY_price<-Ad(BBY)
lag_BBY_price<-lag(apple_price,k=1)

fit<-lm(BBY_price~lag_BBY_price)
fit

resid(fit)

# (2) pair trading function

myvals=function(s1,s2,from) {
  ap1=Ad(getSymbols(s1,auto.assign=FALSE,from=from))
  ap2=Ad(getSymbols(s2,auto.assign=FALSE,from=from))
  n=length(ap2)
  nn=n-179
  vals=1:nn
  i=n
  p1 = ap1[(i-179):i]
  p2 = ap2[(i-179):i]
  fit=lm(p1~-1+p2)
  beta=coef(fit)[1]
  sprd=p1-beta*p2
  sprd=as.numeric(sprd)
  cat("Cointegration p=value = ",adf.test(sprd,alternative="stationary",k=0)$p.value,"\n")
  cat("Correlation = ",cor(p1,p2),"\n")
  
  coin<-adf.test(sprd,alternative="stationary",k=0)$p.value
  corre<-cor(p1,p2)
  out<-list(
    co_intigration=as.numeric(coin),
    correlation_coeff=as.numeric(corre)
  )
  
  return(out)
}



myvals('SINA','BABA',from='2015-01-01')

pairs.trade <- function(stock1, stock2,
                        from = "2010-01-01", to = Sys.Date(),
                        ma.days = 14,
                        method = "diff",
                        threshold = 2,
                        closeout = T){
  require(quantmod)
  
  x1 <- getSymbols(stock1, auto.assign = F, from = from, to = to)
  x1 <- as.numeric(Ad(x1))
  
  x2 <- getSymbols(stock2, auto.assign = F, from = from, to = to)
  x2 <- as.numeric(Ad(x2))
  
  if (method == "diff"){
    x1.norm <- (x1 - runMean(x1, n = ma.days))/runSD(x1, ma.days)
    x2.norm <- (x2 - runMean(x2, n = ma.days))/runSD(x2, ma.days)
    out.ts <- x1.norm - x2.norm
  } else if (method == "ratio"){
    ts <- x1/x2
    out.ts <- (ts - runMean(ts, n = ma.days))/runSD(ts, ma.days)
  } else if (method == "log ratio"){
    ma.days<-130
    ts<-log(x1/x2)
    out.ts <- (ts - runMean(ts, n = ma.days))/runSD(ts, ma.days)
  }
  
  numdays <- length(out.ts)
  
  # initialize quantities
  x1.traded = x2.traded = 0
  current = "neither"
  profit = 0
  maxprofit = minprofit = numtrades = winners = 0
  mytrade=c()
  
  for(i in ma.days:numdays){
    if(out.ts[i] < -threshold & current == "neither"){
      x1.traded = (10000/x1[i])
      x2.traded = (-10000/x2[i])
      current = "x2"
      numtrades = numtrades + 1
      ##print(paste("Short", stock2, "at", x2[i],
      ##"and Long", stock1, "at", x1[i]))
    }
    
    if(out.ts[i] > threshold & current == "neither"){
      x1.traded = (-10000/x1[i])
      x2.traded = (10000/x2[i])
      current = "x1"
      numtrades = numtrades + 1
      ##print(paste("Short", stock1, "at", x1[i],
      ##"and Long", stock2, "at", x2[i]))
    }
    
    if((out.ts[i] > 0 & current == "x2") | (out.ts[i] < 0 & current == "x1")){
      profit.temp = x1.traded*x1[i] + x2.traded*x2[i]
      profit = profit + profit.temp
      mytrade<-c(mytrade,profit.temp)
      winners = winners + (profit.temp > 0)
      maxprofit = max(maxprofit,profit.temp)
      minprofit = min(minprofit,profit.temp)
      x1.traded = 0
      x2.traded = 0
      current = "neither"
    }
  }
  
  # Note: this optional closing out is outside the for loop!
  if (x1.traded != 0 & closeout == T){
    profit.temp = x1.traded*x1[i] + x2.traded*x2[i]
    profit = profit + profit.temp
    mytrade<-c(mytrade,profit.temp)
    winners = winners + (profit.temp > 0)
    maxprofit = max(maxprofit,profit.temp)
    minprofit = min(minprofit,profit.temp)
    x1.traded = 0
    x2.traded = 0
    current = "neither"
  }
  
  # Counting issue
  if (x1.traded != 0 & closeout == F){
    numtrades = numtrades - 1
  }
  
  # tabulate results
  results = list(Winners = winners,
                 number.of.trades = numtrades,
                 winning.percentage = 100*winners/numtrades,
                 max.profit = maxprofit,
                 min.profit = minprofit,
                 # my.trade=mytrade,
                 avg.wining=mean(mytrade[mytrade>0]),
                 avg.losing=mean(mytrade[mytrade<0]),
                 profit = profit)
  return(results)
  
  
  
}

mycoin=function(s1,s2,from) {
  ap1=Ad(getSymbols(s1,auto.assign=FALSE,from=from))
  ap2=Ad(getSymbols(s2,auto.assign=FALSE,from=from))
  n=length(ap2)
  nn=n-179
  vals=1:nn
  for(i in 180:n) {
    p1 = ap1[(i-179):i]
    p2 = ap2[(i-179):i]
    fit=lm(p1~-1+p2)
    beta=coef(fit)[1]
    sprd=p1-beta*p2
    sprd=as.numeric(sprd)
    vals[i-179]=1-adf.test(sprd,alternative="stationary",k=0)$p.value
  }
  plot(EMA(vals),type="l",main = 'Cointegration',col='Blue')
}

mycor=function(s1,s2) {
  ap1=Ad(getSymbols(s1,auto.assign=FALSE,from="2012-01-02"))
  ap2=Ad(getSymbols(s2,auto.assign=FALSE,from="2012-01-02"))
  n=length(ap2)  ## this is how many days we want to work with
  ### some math...we want to start at day 180 and take the 
  ### correlation from day 1 to day 180
  ### we then contue and stop at day n-179
  ### store the results in vals
  nn=n-179
  vals=1:nn
  for(i in 180:n) {
    p1 = ap1[(i-179):i]
    p2 = ap2[(i-179):i]
    vals[i-179]=cor(p1,p2)
  }
  plot(EMA(vals),type="l",main = 'Correlation',col='Brown')
}

volume_price<-function(stock,stock1,from){
  s<-getSymbols(stock,auto.assign=FALSE,from=from)
  s1<-getSymbols(stock1,auto.assign=FALSE,from=from)
  stock<-c(stock,stock1)
  meanvolume<-c(mean(Vo(s)),mean(Vo(s1)))
  meanprice<-c(mean(Ad(s)),mean(Ad(s1)))
  return(list(stock_name=stock,mean_volume=meanvolume,mean_price=meanprice)) 
}


# stock_a<-'SINA'
# stock_b<-'BABA'
# 
# stock_a<-'TGT'
# stock_b<-'BBY'
# 
# stock_a<-'TXN'
# stock_b<-'QCOM'
# 
stock_a<-'AMZN'
stock_b<-'T'


from<-"2015-01-01"

par(mfrow=c(1,2))
mycor(s1=stock_a,s2=stock_b)
mycoin(s1=stock_a,s2=stock_b,from = from)
par(mfrow=c(1,1))

v_p<-volume_price(stock=stock_a,stock1=stock_b,from = from )
v_p<-data.frame(v_p)
v_p
coin_corr<-myvals(stock_a,stock_b,from=from)
coin_corr<-data.frame(coin_corr)
coin_corr
diff<-pairs.trade(stock_a,stock_b,from=from,method = 'diff')
ratio<-pairs.trade(stock_a,stock_b,from=from,method = 'ratio')
log_ratio<-pairs.trade(stock_a,stock_b,from=from,method = 'log ratio')





Item<-c('Winners',
        'number.of.trades',
        'winning.percentage' ,
        'max.profit',
        'min.profit' ,
        # my.trade=mytrade,
        'avg.wining',
        'avg.losing',
        'profit')

diff<-round(as.numeric(diff),3)
ratio<-round(as.numeric(ratio),3)
log_ratio<-round(as.numeric(log_ratio),3)
data_compare<-cbind(Item,diff,ratio,log_ratio)
data_compare<-data.frame(data_compare)

par(mfrow=c(1,1))

plot(diff,ratio,col='Brown',main="Diff vs. Ratio")
textxy(diff,ratio,Item) 
abline(a=0,b=1,col='Green')
abline(h=0,v=0)

plot(diff,ratio, ylim=c(-5000,5000),xlim=c(-5000,5000),,col='Brown',main="Diff vs. Ratio zoom")
textxy(diff,ratio,Item) 
abline(a=0,b=1,col='Green')
abline(h=0,v=0)

par(mfrow=c(1,1))




#RSI code
# install.packages(c("quantmod","TTR"))
library(quantmod)
library(TTR)
# Pull S&P500 index data from Yahoo! Finance
getSymbols('GSPC', from='2016-09-01', to='2016-12-03')

names(GSPC)

chart_Series(Ad(GSPC))
addRSI()

par(mfrow=c(2,1))
plot(Ad(GSPC))
plot(rsi)
abline(h=70,col='Brown')
abline(h=30,col='Brown')
c<-runMean(rsi,14)
# Calculate the RSI indicator
rsi <- RSI(Ad(GSPC),2) # Create the long (up) and short (dn) signals


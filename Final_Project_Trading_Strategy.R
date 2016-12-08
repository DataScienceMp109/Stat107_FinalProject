library(moments)
library(boot)
library(logspline)
library(quantmod)
require(ggplot2)
library(quantmod)
library(TTR)
library(PerformanceAnalytics)

# Single stock trading
# moving averag 200
#define a function to calculate the CAGR of VWMA and MA

ma_vwma200('AAPL','MA',from='2016-09-01', to='2016-12-03')

ma_vwma200=function(ticker,method,from,to) {
  startdate=from
  enddate = to
  malength=10
  ##ticker="AAPL"
  
  stockdata=getSymbols(ticker,from=startdate,to=enddate,auto.assign=FALSE)
  
  
  
  
  sp=as.numeric(Ad(stockdata))
  sv=as.numeric(Vo(stockdata))
  ndays=length(sp)
  
  # if(method=='VWMA'){
  #   ma=VWMA(sp,sv,malength)
  # }
  # 
  # if(method=='MA'){
  ma=SMA(sp,malength)
  # }
  
  signal="inStock"
  buyprice=sp[20]
  sellprice=0
  mawealth=1
  
  for(d in 20:ndays) {
    if((sp[d]>ma[d]) && (signal=="inCash")) {
      buyprice=sp[d]
      signal = "inStock"
      ###   print(paste("Buy Price = ",buyprice))
    }
    
    if(((sp[d]<ma[d]) || (d==ndays)) && (signal=="inStock")) {
      sellprice=sp[d]
      signal = "inCash"
      mawealth=mawealth*(sellprice/buyprice)
      ##print(paste("Sell Price = ",sellprice))
    }
  }
  
  
  bhwealth=sp[ndays]/sp[20]
  ndays=nrow(ostockdata)
  # if(method=='VWMA'){
  #   print(paste(ticker,"VWMA CAGR = ",round(mawealth^(1/((ndays-1)/12))-1,4)))
  #   print(paste(ticker,"BH CAGR = ",round(bhwealth^(1/((ndays-1)/12))-1,4)))
  # }
  # 
  # if(method=='MA'){
  #   print(paste(ticker,"MA CAGR = ",round(mawealth^(1/((ndays-1)/12))-1,4)))
  #   print(paste(ticker,"BH CAGR = ",round(bhwealth^(1/((ndays-1)/12))-1,4)))
  # }
  
  return<-list(Trading_Strategy='Moving_average',
               CAGR=round(mawealth^(1/((ndays-1)/252))-1,4))
  
  return(return) 
  
}





#VWAP

VWAP_SMA(2,3,'AAPL',from='2015-09-01', to='2016-12-03')


VWAP_SMA<-function(a,b,ticker,method,from,to){
  # startdate=" 2015-01-01"
  # enddate = "2016-08-30"
  # malength=10
  # ticker="AAPL"
  
  getSymbols(ticker, from=from,to=to)
  v <- adjustOHLC(v)
  
  plot(Vo(v)/1000000, type="h")
  lines(Cl(v),col="red")
  
  vma.aapl <- VWAP(Cl(v), Vo(v), n=100)
  sma.aapl <- SMA(Cl(v), n=5)  
  sp<-as.numeric(Cl(v))
  
  # length(vma.aapl)
  # length(sma.aapl)
  # length(sp)
  
  malength<-101
  ndays=length(sp)
  
  
  signal="inStock"
  buyprice=sp[101]
  sellprice=0
  mawealth=1
  
  plot(sma.aapl)
  lines(vma.aapl, col="red")  
  
  for(d in (malength+1):ndays) {
    if((vma.aapl[d]>sma.aapl[d]+a) && (signal=="inCash")) {
      buyprice=sp[d]
      signal = "inStock"
      
      ###   print(paste("Buy Price = ",buyprice))
    }
    
    if(((vma.aapl[d]<sma.aapl[d]+b) || (d==ndays)) && (signal=="inStock")) {
      sellprice=sp[d]
      signal = "inCash"
      mawealth=mawealth*(sellprice/buyprice)
      ##print(paste("Sell Price = ",sellprice))
    }
  }
  
  
  bhwealth=sp[ndays]/sp[20]
  
  
  # cat('Threshold A is', a, 'Threshold B is',b,'\n') 
  # print(paste(ticker,"VWAP-SMA CAGR = ",round(mawealth^(1/((ndays-1)/252))-1,4)))
  # print(paste(ticker,"BH CAGR = ",round(bhwealth^(1/((ndays-1)/252))-1,4)))
  
  return<-list(Trading_Strategy="VWAP-SMA CAGR",
               CAGR=round(mawealth^(1/((ndays-1)/252))-1,4))
  
  return(return) 
  
  
}




#RSI code
# install.packages(c("quantmod","TTR"))

# Pull S&P500 index data from Yahoo! Finance
getSymbols('AAPL', from='2016-09-01', to='2016-12-03')
rsi <- RSI(Ad(AAPL),2) # Create the long (up) and short (dn) signals

chart_Series(Ad(GSPC))
addRSI()

par(mfrow=c(2,1))
plot(as.numeric(Ad(GSPC)))
plot(rsi)
abline(h=70,col='Brown')
abline(h=30,col='Brown')
c<-runMean(rsi,14)
# Calculate the RSI indicator
par(mfrow=c(1,1))



RSI_calc(20,70,"AAPL",from="2013-01-01",to="2016-09-30")

RSI_calc<-function(a,b,ticker,  from, to){
  # startdate=" 2015-01-01"
  # enddate = "2016-08-30"
  # malength=10
  # ticker=ticker
  # from="2013-01-01"
  # to="2016-09-30"
  
  r<-getSymbols(ticker,from=from,to=to,auto.assign=FALSE)
  # r <- adjustOHLC(r)
  rsi <- RSI(Ad(r),2) 
  rsi<-as.numeric(rsi)
  
  plot(Vo(r)/1000000, type="h")
  lines(Cl(r),col="red")
  
  # vma.aapl <- VWAP(Cl(r), Vo(r), n=100)
  # sma.aapl <- SMA(Cl(r), n=5)  
  sp<-as.numeric(Ad(r))
  
  # length(vma.aapl)
  # length(sma.aapl)
  # length(sp)
  
  malength<-101
  ndays=length(sp)
  
  
  signal="inStock"
  buyprice=sp[20]
  sellprice=0
  mawealth=1
  
  # par(mfrow=c(2,1))
  # plot(sma.aapl)
  # lines(vma.aapl, col="red")  
  # plot(rsi)
  # par(mfrow=c(1,1))
  
  for(d in 20:ndays) {
    if((rsi[d]<a) && (signal=="inCash")) {
      buyprice=sp[d]
      signal = "inStock"
      
      ###   print(paste("Buy Price = ",buyprice))
    }
    
    if(((b<rsi[d]) || (d==ndays)) && (signal=="inStock")) {
      sellprice=sp[d]
      signal = "inCash"
      mawealth=mawealth*(sellprice/buyprice)
      ##print(paste("Sell Price = ",sellprice))
    }
  }
  
  
  bhwealth=sp[ndays]/sp[20]
  
  
  # cat('Threshold A is', a, 'Threshold B is',b,'\n') 
  # print(paste(ticker,"RSI CAGR = ",round(mawealth^(1/((ndays-1)/252))-1,4)))
  print(paste(ticker,"BH CAGR = ",round(bhwealth^(1/((ndays-1)/252))-1,4)))
  
  return<-list(Trading_Strategy="RSI CAGR",
               CAGR=round(mawealth^(1/((ndays-1)/252))-1,4))
  
  return(return) 
  
  
  
}






##BBand



bbd_calc("AAPL",from="2013-01-01",to="2016-09-30")

bbd_calc<-function(ticker,from,to){
  # startdate=" 2015-01-01"
  # enddate = "2016-08-30"
  # malength=10
  # ticker=ticker
  # from="2013-01-01"
  # to="2016-09-30"
  
  bd<-getSymbols(ticker,from=from,to=to,auto.assign=FALSE)
  # r <- adjustOHLC(r)
  BBands_dn<-BBands(Ad(bd))$dn
  BBands_up<-BBands(Ad(bd))$up
  # bbd <- BBands(Cl(bd))$dn
  # bbd<-as.numeric(bbd)
  
  plot(Vo(bd)/1000000, type="h")
  lines(Cl(bd),col="red")
  
  # vma.aapl <- VWAP(Cl(r), Vo(r), n=100)
  # sma.aapl <- SMA(Cl(r), n=5)  
  bbd_sp<-as.numeric(Ad(bd))
  
  
  # length(vma.aapl)
  # length(sma.aapl)
  # length(sp)
  
  # malength<-101
  ndays=length(bbd_sp)
  
  
  signal="inStock"
  buyprice=bbd_sp[20]
  sellprice=0
  mawealth=1
  
  # par(mfrow=c(2,1))
  # plot(sma.aapl)
  # lines(vma.aapl, col="red")  
  # plot(rsi)
  # par(mfrow=c(1,1))
  
  for(d in 20:ndays) {
    if((BBands_dn[d]>bbd_sp[d]) && (signal=="inCash")) {
      buyprice=bbd_sp[d]
      signal = "inStock"
      
      ###   print(paste("Buy Price = ",buyprice))
    }
    
    if(((bbd_sp[d]>BBands_up[d]) || (d==ndays)) && (signal=="inStock")) {
      sellprice=bbd_sp[d]
      signal = "inCash"
      mawealth=mawealth*(sellprice/buyprice)
      ##print(paste("Sell Price = ",sellprice))
    }
  }
  
  
  bhwealth=bbd_sp[ndays]/bbd_sp[20]
  
  
  # cat('Threshold A is', a, 'Threshold B is',b,'\n') 
  # print(paste(ticker,"Bollinger bands CAGR = ",round(mawealth^(1/((ndays-1)/252))-1,4)))
  print(paste(ticker,"BH CAGR = ",round(bhwealth^(1/((ndays-1)/252))-1,4)))
  
  return<-list(Trading_Strategy="Bollinger bands CAGR",
               CAGR=round(mawealth^(1/((ndays-1)/252))-1,4))
  
  return(return) 
  
}



stock_trad<-'AAPL'
start<-"2013-01-01"
end<-"2016-09-30"
trd_1<-ma_vwma200(stock_trad,'MA',from=start, to=end)
trd_2<-VWAP_SMA(2,3,stock_trad,from=start, to=end)
trd_3<-RSI_calc(20,70,stock_trad,from=start, to=end)
trd_4<-bbd_calc(stock_trad,from=start,to=end)

rbind(trd_1,trd_2,trd_3,trd_4)


stock_trad<-'AAPL'
start<-"2015-01-01"
end<-"2016-09-30"
trd_1<-ma_vwma200(stock_trad,'MA',from=start, to=end)
trd_2<-VWAP_SMA(2,3,stock_trad,from=start, to=end)
trd_3<-RSI_calc(20,70,stock_trad,from=start, to=end)
trd_4<-bbd_calc(stock_trad,from=start,to=end)

rbind(trd_1,trd_2,trd_3,trd_4)


stock_trad<-'M'
start<-"2015-01-01"
end<-"2016-09-30"
trd_1<-ma_vwma200(stock_trad,'MA',from=start, to=end)
trd_2<-VWAP_SMA(2,3,stock_trad,from=start, to=end)
trd_3<-RSI_calc(20,70,stock_trad,from=start, to=end)
trd_4<-bbd_calc(stock_trad,from=start,to=end)

rbind(trd_1,trd_2,trd_3,trd_4)


plot(Ad(bd))
stock_trad<-'BBY'
start<-"2015-01-01"
end<-"2016-09-30"
trd_1<-ma_vwma200(stock_trad,'MA',from=start, to=end)
trd_2<-VWAP_SMA(2,3,stock_trad,from=start, to=end)
trd_3<-RSI_calc(20,70,stock_trad,from=start, to=end)
trd_4<-bbd_calc(stock_trad,from=start,to=end)

rbind(trd_1,trd_2,trd_3,trd_4)

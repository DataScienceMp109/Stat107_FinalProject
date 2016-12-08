library(moments)
library(boot)
library(logspline)
library(quantmod)
library(calibrate)
library(tseries)



sample(as.character(samll_cap_com_lis_hist$Symbol),10)
stk_cmbn<-combn(sample(as.character(samll_cap_com_lis_hist$Symbol),10),3)
# portfolio_optimization()
# 
# portfolio_optimization<-function(){
stock_list<-1:3
dim(stk_cmbn)[2]
result<-list()
for(i in 1:dim(stk_cmbn)[2]){
  stock_list[1]<-stk_cmbn[1,i]
  stock_list[2]<-stk_cmbn[2,i]
  stock_list[3]<-stk_cmbn[3,i]
  from='2011-12-31'
  to='2014-12-31'
  
  s1<-getSymbols(stock_list[1],from=from,to=to,auto.assign=FALSE) 
  s2<-getSymbols(stock_list[2],from=from,to=to,auto.assign=FALSE) 
  s3<-getSymbols(stock_list[3],from=from,to=to,auto.assign=FALSE) 
  mk_ret<-getSymbols("SPY",from=from,to=to,auto.assign=FALSE)
  
  spyret=monthlyReturn(Ad(mk_ret)) 
  s1ret=monthlyReturn(Ad(s1)) 
  s2ret=monthlyReturn(Ad(s2)) 
  s3ret=monthlyReturn(Ad(s3)) 
  # s4ret=monthlyReturn(Ad(s4))
  
  s1beta=coef(lm(s1ret~spyret))[2] 
  s2beta=coef(lm(s2ret~spyret))[2] 
  s3beta=coef(lm(s3ret~spyret))[2]
  
  portf_beta<-mean(s1beta,s2beta,s3beta)
  
  er<-c(mean(s1ret),mean(s2ret),mean(s3ret))
  
  empcovmat=cov(cbind(s1ret,s2ret,s3ret)) 
  # rownames(empcovmat) = c(stock_list[1],stock_list[2],stock_list[3],stock_list[4])  
  # colnames(empcovmat) = c(stock_list[1],stock_list[2],stock_list[3],stock_list[4]) 
  # empcovmat=round(empcovmat,6)
  
  
  gmin.port <- globalMin.portfolio(er, empcovmat)
  
  
  ef=efficient.frontier(er,empcovmat)
  # plot(ef$sd,ef$er,type="l",xlab="risk",ylab="return")
  msharp=0
  
  rk.free=.0032
  
  
  if((gmin.port$er)<rk.free){
    next
  }
  
  if(length(which(empcovmat<0))==0){
    cov=1
  }else if(length(which(empcovmat<0))>0){
    cov=0
  }
  
  tan.port <- tangency.portfolio(er, empcovmat, rk.free)
  # print(tan.port)
  # names(summary(tan.port, risk.free=rk.free))
  # plot(tan.port) 
  
  Sharpe.Ratio<-(as.numeric(tan.port[2])-rk.free)/as.numeric(tan.port[3])
  
  Treynor.Ratio<-(mean(er)-rk.free)/portf_beta
  
  
  outc<-list(
    stock1=stock_list[1],
    stock2=stock_list[2],
    stock3=stock_list[3],
    w1=tan.port$weights[1],
    w2=tan.port$weights[2],
    w3=tan.port$weights[3],
    vol1=mean(as.numeric(Vo(s1))),
    vol2=mean(as.numeric(Vo(s2))),
    vol3=mean(as.numeric(Vo(s3))),
    cov_sign=cov,
    Sharpe_Ratio=Sharpe.Ratio,
    Treynor_Ratio=Treynor.Ratio,
    tang.ret=as.numeric(tan.port[2]),
    tang.sd=as.numeric(tan.port[3])
  )
  outc<-data.frame(outc)
  result<-rbind(result,outc)
}
# plot(ef$sd,ef$er,type="l",xlab="risk",ylab="return")
# abline(a=rk.free, b=Sharpe.Ratio)


portfo_slc<-subset(result,subset=cov_sign==0
                   &vol1>50000&vol2>50000&vol3>50000
                   &tang.ret>0.04
                   &Treynor_Ratio>0
                   &tang.sd<0.3
)




### BACK Test
portf_1<-portfo_slc[1,]
library(moments)
library(boot)
library(logspline)
library(quantmod)
library(calibrate)
library(tseries)



sample(as.character(samll_cap_com_lis_hist$Symbol),10)
stk_cmbn<-combn(sample(as.character(samll_cap_com_lis_hist$Symbol),10),3)
# portfolio_optimization()
# 
# portfolio_optimization<-function(){
stock_list<-1:3
dim(stk_cmbn)[2]
result<-list()
for(i in 1:dim(stk_cmbn)[2]){
  stock_list[1]<-stk_cmbn[1,i]
  stock_list[2]<-stk_cmbn[2,i]
  stock_list[3]<-stk_cmbn[3,i]
  from="2013-06-01"
  to="2015-06-01"
  
  s1<-getSymbols(stock_list[1],from=from,to=to,auto.assign=FALSE) 
  s2<-getSymbols(stock_list[2],from=from,to=to,auto.assign=FALSE) 
  s3<-getSymbols(stock_list[3],from=from,to=to,auto.assign=FALSE) 
  mk_ret<-getSymbols("SPY",from=from,to=to,auto.assign=FALSE)
  
  spyret=monthlyReturn(Ad(mk_ret)) 
  s1ret=monthlyReturn(Ad(s1)) 
  s2ret=monthlyReturn(Ad(s2)) 
  s3ret=monthlyReturn(Ad(s3)) 
  # s4ret=monthlyReturn(Ad(s4))
  
  s1beta=coef(lm(s1ret~spyret))[2] 
  s2beta=coef(lm(s2ret~spyret))[2] 
  s3beta=coef(lm(s3ret~spyret))[2]
  
  portf_beta<-mean(s1beta,s2beta,s3beta)
  
  er<-c(mean(s1ret),mean(s2ret),mean(s3ret))
  
  empcovmat=cov(cbind(s1ret,s2ret,s3ret)) 
  # rownames(empcovmat) = c(stock_list[1],stock_list[2],stock_list[3],stock_list[4])  
  # colnames(empcovmat) = c(stock_list[1],stock_list[2],stock_list[3],stock_list[4]) 
  # empcovmat=round(empcovmat,6)
  
  
  gmin.port <- globalMin.portfolio(er, empcovmat)
  
  
  ef=efficient.frontier(er,empcovmat)
  # plot(ef$sd,ef$er,type="l",xlab="risk",ylab="return")
  msharp=0
  
  rk.free=.0032
  
  
  if((gmin.port$er)<rk.free){
    next
  }
  
  if(length(which(empcovmat<0))==0){
    cov=1
  }else if(length(which(empcovmat<0))>0){
    cov=0
  }
  
  tan.port <- tangency.portfolio(er, empcovmat, rk.free)
  # print(tan.port)
  # names(summary(tan.port, risk.free=rk.free))
  # plot(tan.port) 
  
  Sharpe.Ratio<-(as.numeric(tan.port[2])-rk.free)/as.numeric(tan.port[3])
  
  Treynor.Ratio<-(mean(er)-rk.free)/portf_beta
  
  
  outc<-list(
    stock1=stock_list[1],
    stock2=stock_list[2],
    stock3=stock_list[3],
    w1=tan.port$weights[1],
    w2=tan.port$weights[2],
    w3=tan.port$weights[3],
    vol1=mean(as.numeric(Vo(s1))),
    vol2=mean(as.numeric(Vo(s2))),
    vol3=mean(as.numeric(Vo(s3))),
    cov_sign=cov,
    Sharpe_Ratio=Sharpe.Ratio,
    Treynor_Ratio=Treynor.Ratio,
    tang.ret=as.numeric(tan.port[2]),
    tang.sd=as.numeric(tan.port[3])
  )
  outc<-data.frame(outc)
  result<-rbind(result,outc)
}
# plot(ef$sd,ef$er,type="l",xlab="risk",ylab="return")
# abline(a=rk.free, b=Sharpe.Ratio)


portfo_slc<-subset(result,subset=cov_sign==0
                   &vol1>50000&vol2>50000&vol3>50000
                   &tang.ret>0.04
                   &Treynor_Ratio>0
                   &tang.sd<0.3
)




### BACK Test
portf_1<-portfo_slc[7,]
portf_1<-portfo_slc[8,]


portf_1<-portfo_slc[4,]
portf_1<-portfo_slc[5,]


# portf_2<-portfo_slc[2,]

p_s1<-as.character(portf_1$stock1)
p_s2<-as.character(portf_1$stock2)
p_s3<-as.character(portf_1$stock3)

# from="2013-06-01"
# to="2015-06-01"
from="2015-06-01"
to="2016-06-01"
# stock_list[1]<-stk_cmbn[1,i]
# stock_list[2]<-stk_cmbn[2,i]
# stock_list[3]<-stk_cmbn[3,i]



s1<-getSymbols(p_s1,from=from,to=to,auto.assign=FALSE) 
s2<-getSymbols(p_s2,from=from,to=to,auto.assign=FALSE) 
s3<-getSymbols(p_s3,from=from,to=to,auto.assign=FALSE) 
mk_ret<-getSymbols("SPY",from=from,to=to,auto.assign=FALSE)

spyret=monthlyReturn(Ad(mk_ret)) 
s1ret=monthlyReturn(Ad(s1)) 
s2ret=monthlyReturn(Ad(s2)) 
s3ret=monthlyReturn(Ad(s3)) 

w1<-portf_1$w1
w2<-portf_1$w2
w3<-portf_1$w3

portf_1_ret<-w1*s1ret + w2*s2ret + w3*s3ret

plot(portf_1_ret)

#Drawdown
# library(quantmod)
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
# getSymbols("AAPL",from="2010-01-01")
table.Drawdowns(portf_1_ret)
chart.Drawdown(portf_1_ret)
sortDrawdowns(findDrawdowns(portf_1_ret))


#SemiDeviation
SemiDeviation(portf_1_ret)


#Downsidedeviation
DownsideDeviation(portf_1_ret)


# The Omega Function
Omega(portf_1_ret)

validation<-list(omega=as.numeric(Omega(portf_1_ret)),
                 Downsidedeviation=as.numeric(DownsideDeviation(portf_1_ret)),
                 SemiDeviation=as.numeric(SemiDeviation(portf_1_ret)))
validation


mkt_ret<-getSymbols('SPY',from=from,to=to,auto.assign = FALSE)
plot(Ad(mkt_ret))

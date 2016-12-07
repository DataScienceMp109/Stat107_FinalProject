library(moments)
library(boot)
library(logspline)
library(quantmod)
library(calibrate)
library(tseries)

# Get a list of randomly selected stocks by different sector
Inds_sct<-unique(comls_nyse$Sector)
Inds_sct<-sample(Inds_sct,4)
stock_list<-c()
sec_name<-c()
j<-1
for( i in Inds_sct){
  stock_list[j]<-sample(comls_nyse$Symbol[comls_nyse$Sector==i],1)
  sec_name[j]<-i
  j<-j+1
}
random_stc_ls<-cbind(stock_list,sec_name)
random_stc_ls

from='2015-01-01'

# mean variance portfolio


# 1) Single factor model CAPM & Beta

# 2) pick up stocks

# Get coveriance by using method of Single factor model

s1<-getSymbols(stock_list[1],from=from,auto.assign=FALSE) 
s2<-getSymbols(stock_list[2],from=from,auto.assign=FALSE) 
s3<-getSymbols(stock_list[3],from=from,auto.assign=FALSE) 
s4<-getSymbols(stock_list[4],from=from,auto.assign=FALSE) 
mk_ret<-getSymbols("SPY",from=from,auto.assign=FALSE)

spyret=monthlyReturn(Ad(mk_ret)) 
s1ret=monthlyReturn(Ad(s1)) 
s2ret=monthlyReturn(Ad(s2)) 
s3ret=monthlyReturn(Ad(s3)) 
s4ret=monthlyReturn(Ad(s4))

# 3) Generate Variance-Coveriance matrix
s1beta=coef(lm(s1ret~spyret))[2] 
s2beta=coef(lm(s2ret~spyret))[2] 
s3beta=coef(lm(s3ret~spyret))[2] 
s4beta=coef(lm(s4ret~spyret))[2]

se_s1=summary(lm(s1ret~spyret))$sigma 
se_s2=summary(lm(s2ret~spyret))$sigma 
se_s3=summary(lm(s3ret~spyret))$sigma 
se_s4=summary(lm(s4ret~spyret))$sigma

sbetas=c(s1beta,s2beta,s3beta,s4beta)
regvars = c(se_s1^2,se_s2^2,se_s3^2,se_s4^2)
covmat=matrix(nrow=4,ncol=4)
rownames(covmat) = c(stock_list[1],stock_list[2],stock_list[3],stock_list[4]) 
colnames(covmat) = c(stock_list[1],stock_list[2],stock_list[3],stock_list[4]) 
varmarket = var(spyret)

for(i in 1:4) for(j in 1:4) 
   covmat[i,j]=sbetas[i]*sbetas[j]*varmarket 
   for(i in 1:4) 
   covmat[i,i] = covmat[i,i]+regvars[i] 
covmat=round(covmat,6)



# COV built in function
empcovmat=cov(cbind(s1ret,s2ret,s3ret,s4ret)) 
rownames(empcovmat) = c(stock_list[1],stock_list[2],stock_list[3],stock_list[4])  
colnames(empcovmat) = c(stock_list[1],stock_list[2],stock_list[3],stock_list[4]) 
empcovmat=round(empcovmat,6)

empcovmat
covmat

er<-c(mean(s1ret),mean(s2ret),mean(s3ret),mean(s4ret))

# 4) Make efficient frontier

# er=c(0.24, 0.15)
# covmat=covmat

# names(er)=c("Stock_fund","Bond_fund")
# colnames(covmat)=c("Stock_fund","Bond_fund")
# rownames(covmat)=c("Stock_fund","Bond_fund")

gmin.port <- globalMin.portfolio(er, covmat)


ef=efficient.frontier(er,covmat)
plot(ef$sd,ef$er,type="l",xlab="risk",ylab="return")
msharp=0

# 5) Tangent portfolio

rk.free=.00002
tan.port <- tangency.portfolio(er, covmat, rk.free)
print(tan.port)
names(summary(tan.port, risk.free=rk.free))
plot(tan.port) 

plot(ef$sd,ef$er,type="l",xlab="risk",ylab="return")
abline(a=rk.free, b=0.40505)



# 6) Portfolio analysis


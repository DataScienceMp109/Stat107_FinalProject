


x<-getSymbols('V',from="2010-01-01",to="2015-01-01",auto.assign=FALSE) 
spyret=dailyReturn(Ad(x)) 
today=spyret 
tomorrow=lag(today,-1) 
yesterday=lag(today) 
n=length(spyret) 

vol<-Vo(x)
today_vol<-vol
tomorrow_vol=lag(today_vol,-1) 
# yesterday=lag(today_vol) 
n=length(today_vol) 


#get rid of missing values 
today=as.numeric(today[-c(1,n)]) 
tomorrow=as.numeric(tomorrow[-c(1,n)]) 
# yesterday=as.numeric(yesterday[-c(1,n)]) 

today_vol=as.numeric(today_vol[-c(1,n)]) 
tomorrow_vol=as.numeric(tomorrow_vol[-c(1,n)]) 
# yesterday=as.numeric(yesterday[-c(1,n)]) 

ind=1.0*(tomorrow>0)## this is our Y (response) variable

print(summary(glm(ind~today+today_vol,family="binomial")))

names(summary(glm(ind~today,family="binomial")))






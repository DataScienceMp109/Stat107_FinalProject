
# data cleaning

companylist <- read.csv("C:/Users/mac/Desktop/Financial Statistics 107/Final_Project/companylist.csv")
comls_nyse<-subset(companylist,subset=Sector!='n/a'&MarketCap!='n/a'&Sector!='n/a'
                   &(LastSale!='n/a')&IPOyear!='n/a')

comls_nyse$LastSale<-as.numeric(comls_nyse$LastSale)

unit_mkcap<-sapply(strsplit(as.character(comls_nyse$MarketCap), ""), tail, 1)
comls_nyse<-cbind(comls_nyse,unit_mkcap)
comls_nyse$MarketCap<-gsub('\\$', '', comls_nyse$MarketCap)
comls_nyse$MarketCap<-gsub('\\M', '', comls_nyse$MarketCap)
comls_nyse$MarketCap<-sub('B$', '', comls_nyse$MarketCap)
comls_nyse$MarketCap<-as.numeric(comls_nyse$MarketCap)

comls_nyse$MarketCap[comls_nyse$unit_mkcap=='M']<-comls_nyse$MarketCap[comls_nyse$unit_mkcap=='M']*1
comls_nyse$MarketCap[comls_nyse$unit_mkcap=='B']<-comls_nyse$MarketCap[comls_nyse$unit_mkcap=='B']*1000

options(scipen=999)
hist(comls_nyse$MarketCap,probability = TRUE)
hist(comls_nyse$LastSale,probability = TRUE)

summary(comls_nyse$MarketCap)
summary(comls_nyse$LastSale)
aggregate(MarketCap ~ Sector, comls_nyse, mean)
aggregate(LastSale ~ Sector, comls_nyse, mean)

samll_cap<-(summary(comls_nyse$MarketCap)[6]-summary(comls_nyse$MarketCap)[1])/4
samll_cap<-as.numeric(samll_cap)
samll_cap_com_lis<-subset(comls_nyse,subset=(comls_nyse$MarketCap>0.25*samll_cap)
                          &(comls_nyse$MarketCap<2*samll_cap))


samll_cap_com_lis_hist<-subset(samll_cap_com_lis,subset= samll_cap_com_lis$IPOyear< 2008
                               &samll_cap_com_lis$LastSale> 2)






# Get a list of randomly selected stocks by different sector
Inds_sct<-unique(samll_cap_com_lis$Sector)
Inds_sct<-sample(Inds_sct,length(Inds_sct))
stock_list<-c()
sec_name<-c()
j<-1
for( i in Inds_sct){
  stock_list[j]<-sample(samll_cap_com_lis$Symbol[samll_cap_com_lis$Sector==i],1)
  sec_name[j]<-i
  j<-j+1
}
random_stc_ls<-cbind(stock_list,sec_name)
random_stc_ls




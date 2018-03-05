library(quantmod)

setwd("C:\Users\Alexander\Desktop\hospitaldata")
tick_g1=readRDS('tick_g1.rds')

getRatios=function(t,x)
{
  stockname=readRDS(t)
  tt=paste('P_',t,sep='')
  s=readRDS(tt)
  v=rep(0,14)
  v[1]=stockname$IS$Q["Net Income", 1]
  v[2]=stockname$CF$Q["Cash from Operating Activities", 1]
  v[3]=stockname$BS$Q["Total Assets", 1]
  v[is.na(v)]=0
  STA=(v[1]-v[2])/v[3]
  v[4]=stockname$BS$Q["Total Assets",1]
  v[5]=stockname$BS$Q["Cash & Equivalents", 1]
  v[7]=stockname$BS$Q["Total Current Liabilities",1]
  v[8]=stockname$BS$Q["Total Long Term Debt",1]
  v[9]=stockname$BS$Q["Minority Interest",1]
  v[10]=stockname$BS$Q["Redeemable Preferred Stock, Total",1]
  v[11]=stockname$BS$Q["Preferred Stock - Non Redeemable, Net",1]
  v[12]=stockname$BS$Q["Total Equity",1]
  v[13] = as.vector(tail(s[,6],1))
  v[14]=stockname$BS$Q["Total Common Shares Outstanding", 1]
  v[is.na(v)]=0
  OperatingAssets=v[4]-v[5]
  OperatingLiabilities=v[4]-v[7]-v[8]-v[9]-(v[10]+v[11])-v[12]
  SNOA=((OperatingAssets-OperatingLiabilities)/v[4])
  mcap=c(v[13]*v[14])
  
  
  v=rep(0,5)
  v[1]=stockname$BS$A["Total Receivables, Net",1]
  v[2]=stockname$BS$A["Total Receivables, Net",2]
  v[3]=stockname$IS$A["Total Revenue",1]
  v[is.na(v)]=0
  x1=(v[1]+v[2])/2
  y1=v[3]/x1
  ratio1=365/y1
  v[4]=stockname$BS$A["Total Receivables, Net",3]
  v[5]=stockname$IS$A["Total Revenue",2]
  v[is.na(v)]=0
  x2=(v[2]+v[4])/2
  y2=v[5]/x2
  ratio2=365/y2
  DSRI=ratio1/ratio2
  
  v[5]=stockname$IS$A["Gross Profit",1]
  v[6]=stockname$IS$A["Total Revenue",1]
  v[7]=stockname$IS$A["Gross Profit",2]
  v[8]=stockname$IS$A["Total Revenue",2]
  v[is.na(v)]=0
  gpm1=v[5]/v[6]
  gpm2=v[7]/v[8]
  GPMI=gpm2/gpm1
  
  
  v=rep(0,8)
  v[1]=stockname$BS$A["Total Assets",1]
  v[2]=stockname$BS$A["Total Current Assets",1]
  v[3]=stockname$BS$A["Property/Plant/Equipment, Total - Gross",1]
  v[4]=stockname$BS$A["Accumulated Depreciation, Total",1]
  v[5]=stockname$BS$A["Total Assets",2]
  v[6]=stockname$BS$A["Total Current Assets",2]
  v[7]=stockname$BS$A["Property/Plant/Equipment, Total - Gross",2]
  v[8]=stockname$BS$A["Accumulated Depreciation, Total",2]
  v[is.na(v)]=0
  aqi1=(v[1]-v[2]-(v[3]+v[4]))/v[1]
  aqi2=(v[5]-v[6]-(v[7]+v[8]))/v[5]
  AQI=aqi2/aqi1
  
  v=rep(0,3)
  v[1]=stockname$IS$A["Revenue",1]
  v[2]=stockname$IS$A["Revenue",2]
  v[is.na(v)]=0
  SGI=v[2]/v[1]
  
  v[1]=stockname$IS$Q["Depreciation/Amortization", 1]
  v[2]=stockname$IS$Q["Depreciation/Amortization", 2]
  DEPI=v[2]/v[1]
  
  v=rep(0,2)
  v[1]=stockname$IS$A["Selling/General/Admin. Expenses, Total",1]
  v[2]=stockname$IS$A["Selling/General/Admin. Expenses, Total",2]
  SGAI=v[1]/v[2]
  
  
  v=rep(0,4)
  v[1]=stockname$BS$A["Total Debt",1]
  v[2]=stockname$BS$A["Total Debt",2]
  v[3]=stockname$BS$A["Total Assets",1]
  v[4]=stockname$BS$A["Total Assets",2]
  v[is.na(v)]=0
  lvgi1=v[1]/v[3]
  lvgi2=v[2]/v[4]
  LVGI=lvgi1/lvgi2
  
  v=rep(0,6)
  v[1]=stockname$CF$A["Changes in Working Capital",1]
  v[2]=stockname$BS$Q["Cash & Equivalents",1]
  v[3]=stockname$BS$Q["Cash & Equivalents",2]
  v[4]=stockname$BS$A["Total Assets",1]
  v[5]=stockname$CF$A["Changes in Working Capital",2]
  v[6]=stockname$BS$A["Total Assets",2]
  v[is.na(v)]=0
  T1=(v[1]-v[2])/v[4]
  T2=(v[5]-v[3])/v[6]
  TATA=T2/T1
  
  v=matrix(0,13,4)
  v[1,]=stockname$IS$A["Net Income Before Extra. Items",]
  v[2,]=stockname$BS$A["Total Assets",]
  v[3,]=stockname$BS$A["Total Equity",]
  v[2,]=stockname$IS$A["Net Income",]
  v[3,]=stockname$IS$A["Income Before Tax",]
  v[4,]=stockname$BS$A["Accumulated Depreciation, Total",]
  v[5,]=stockname$BS$A["Total Current Assets",]
  v[6,]=stockname$BS$A["Total Current Liabilities",]
  v[7,]=stockname$IS$A["Gross Profit",]
  v[8,]=stockname$IS$A["Total Revenue",]
  v[9,]=stockname$IS$A["Operating Income",]
  v[10,]=stockname$BS$A["Property/Plant/Equipment, Total - Gross",]
  v[11,]=stockname$IS$Q["Depreciation/Amortization",]
  v[12,]=stockname$CF$A["Changes in Working Capital",]
  v[13,]=stockname$CF$A["Capital Expenditures",]
  
  v[is.na(v)]=0
  CFAO=sum((v[9,]+v[11,])-v[12,]-v[13,]/v[2,4])
  ROA=v[1,]/v[2,]
  ROC=v[9,]/((v[10,]+v[4,])+(v[5,]-v[6,]))
  GM=v[7,]/v[8,]
  ROC4=(prod(1+ROA))^(1/4)-1
  ROA4=(prod(1+ROA))^(1/4)-1
  MS=mean(GM)/sd(GM)
  MG=(prod(1+MG))^(1/3)-1
  
  
  total=c(STA,SNOA,DSRI,GPMI,AQI,SGI,DEPI,SGAI,LVGI,TATA,mcap,CFOA,ROC4,ROA4,MS,MG)
  return(total)
}
result=mapply(getRatios,tick_g1)

result=t(result)
resultsdf=data.frame(tick_g1,result)
colnames(resultsdf)=c("ticker","STA","SNOA","DSRI","GPMI","AQI","SGI","DEPI","SGAI","LVGI","TATA","MCAP","CFOA","ROC4","ROA4","MS","MG")
resultsdf$w = resultsdf$MCAP/sum(resultsdf$MCAP)
resultsdf$SNOAW = sum(resultsdf$SNOA*resultsdf$w)
dfp=subset(resultsdf,STA<0 & SNOA<=SNOAW,)
dfp[is.na(dfp)]=1.5

DSRIS = ifelse(dfp$DSRI<=1,0,1)
GPMIS = ifelse(dfp$GPMI<=1,0,1)
AQIS = ifelse(dfp$AQI<=1,0,1)
SGIS = ifelse(dfp$SGI<=1,0,1)
DEPIS = ifelse(dfp$DEPI<=1,0,1)
SGAIS = ifelse(dfp$SGAI<=1,0,1)
LVGIS = ifelse(dfp$LVGI<=1,0,1)
TATAS = ifelse(dfp$TATA<=1,0,1)

dfp$PScore = DSRIS+GPMIS+AQIS+SGIS+DEPIS+SGAIS+LVGIS+TATAS
dfp$PScore
df2 = subset(dfp,PScore<=3,)

tick_g2 = as.vector(df2$ticker)

rank=(50/100)*((length(resultsdf))-1)
MSRank=order(resultsdf$CFOA)>rank
MGRank=order(resultsdf$MG)>rank
ROC4Rank=order(resultsdf$ROC4)>rank 
ROA4Rank=order(resultsdf$ROA4)>rank
CFOARank=order(resultsdf$CFOA)>rank

TOTRank=MSRank+MGRank+ROC4Rank+ROA4Rank+CFOARank
resultsdf=subset(resultsdf,TOTRank>=3)
resultsdf 





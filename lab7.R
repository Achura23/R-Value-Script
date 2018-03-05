library(quantmod)
s=stockSymbols()
names(s)
df=data.frame(s$Symbol,s$Industry,s$Sector)
df
subset(df,s.Symbol=="AAPL",)
dfsub=subset(df,s.Industry=="Computer Manufacturing",)
dfsub
ticker=as.vector(dfsub$s.Symbol)

getRatios=function(t)
{
  stockname=getFinancials(t, auto.assign=F)
  s=getSymbols(t,auto.assign = F)
  price = as.vector(tail(s[,6],1))
  v=rep(0,12)
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
  mc=stockname$BS$Q["Total Common Shares Outstanding", 1]*price
  v[is.na(v)]=0
  OperatingAssets=v[4]-v[5]
  OperatingLiabilities=v[4]-v[7]-v[8]-v[9]-(v[10]+v[11])-v[12]
  SNOA=((OperatingAssets-OperatingLiabilities)/v[4])
  
  
  v=rep(0,6)
  v[1]=stockname$BS$A["Total Receivables, Net",1]
  v[2]=stockname$BS$A["Total Receivables, Net",2]
  v[3]=stockname$IS$A["Total Revenue",1]
  v[is.na(v)]=0
  x1=(v[1]+v[2])/2
  y1=v[3]/x1
  ratioofdays=365/y1
  v[4]=stockname$BS$A["Total Receivables, Net",2]
  v[5]=stockname$BS$A["Total Receivables, Net",3]
  v[6]=stockname$IS$A["Total Revenue",2]
  v[is.na(v)]=0
  x2=(v[4]/v[5])/2
  y2=v[6]/x1
  ratioofdaysMINUS1=365/y2
  
  v=rep(0,4)
  v[1]=stockname$IS$A["Gross Profit",1]
  v[2]=stockname$IS$A["Total Revenue",1]
  v[3]=stockname$IS$A["Gross Profit",2]
  v[4]=stockname$IS$A["Total Revenue",2]
  v[is.na(v)]=0
  gpm1=v[1]/v[2]
  gpm2=v[3]/v[4]
  
  DSRI=ratioofdays/ratioofdaysMINUS1
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
  
  v=rep(0,4)
  v[1]=stockname$BS$A["Accumulated Depreciation, Total",1]
  v[2]=stockname$BS$A["Accumulated Depreciation, Total",2]
  v[is.na(v)]=0
  DEPI=v[1]/v[2]
  
  v=rep(0,4)
  v[1]=stockname$IS$A["Selling/General/Admin. Expenses, Total",1]
  v[2]=stockname$IS$A["Revenue",1]
  v[3]=stockname$IS$A["Selling/General/Admin. Expenses, Total",2]
  v[4]=stockname$IS$A["Revenue",2]
  sgai1=v[1]/v[2]
  sgai2=v[2]/v[3]
  SGAI=sgai1/sgai2
  
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
  v[3]=stockname$BS$A["Cash & Equivalents",1]
  v[4]=stockname$BS$A["Cash & Equivalents",2]
  v[5]=stockname$CF$A["Changes in Working Capital",2]
  v[6]=stockname$BS$A["Total Assets",2]
  v[is.na(v)]=0
  tata1=v[1]-v[3]/v[6]
  tata2=v[5]-v[4]/v[6]
  TATA=tata2/tata1
  

  
  return(c(STA,SNOA,DSRI,GPMI,AQI,SGI,DEPI,SGAI,LVGI,TATA,mc))
}
mm=mapply(getRatios,ticker)
result=t(mm)
resultsdf=data.frame(ticker,result)
colnames(resultsdf)=c("ticker","STA","SNOA","DSRI","GPMI","AQI","SGI","DEPI","SGAI","LVGI","TATA","MCAP")
resultsdf$w = resultsdf$MCAP/sum(resultsdf$MCAP)
resultsdf$STAW = sum(resultsdf$STA*resultsdf$w)
resultsdf$SNOAW = sum(resultsdf$SNOA*resultsdf$w)
resultsdf

load('Gini-RData/FI-Hyy.Rda')

NEE=outData$data$NEE 
years=outData$data$years
precip=outData$data$precip
GPP=outData$data$GPP
TER=outData$data$TER
airTemp=outData$data$airTemp

plot(airTemp, NEE)
plot(precip, NEE)
plot(airTemp, GPP)
plot(airTemp, TER)

growingSeason<-function(years, NEE, yr){
  NEE_SUM=cumsum(NEE)
  NEE_year=NEE[years==yr]
  NEE_SUM_year=cumsum(NEE_year)
  
  MIN6=min(NEE_SUM_year)
  MAX6=max(NEE_SUM_year)
  
  xMAX=which(NEE_SUM_year==MAX6)
  xMIN=which(NEE_SUM_year==MIN6)
  NEEgs6_SUM=NEE_SUM_year[xMAX:xMIN]
  return(NEEgs6_SUM)
}
n=growingSeason(years,NEE,1998)
plot(n)
#1996-2007, years of data


#plot(NEE_SUM_year, main="Growing Season year", sub="Growing Season year", xlim=range(xMIN,xMAX), ylim=range(MIN6,MAX6))
#plot(gs6_SUM, main="Growing Season year", sub="Growing Season year", xlim=range(xMIN,xMAX), ylim=range(MIN6,MAX6))

precipitation<-function(years, precip, y){
  precip_SUM=cumsum(precip)
  #plot(precip_SUM)
  precip_year=precip[years==y]
  #plot(precip_year)
  
  
  precip_SUM_year=cumsum(precip_year)
  #plot(precip_SUM_year)
  MIN6=min(precip_SUM_year)
  MAX6=max(precip_SUM_year)
  
  #plot(precip_SUM_year,main="Growing Season year", sub="Growing Season year", ylim=range(MIN6,MAX6))
  #?plot
  xMAX=which(precip_SUM_year==MAX6)
  xMIN=which(precip_SUM_year==MIN6)
  Precipgs6_SUM=precip_SUM_year[xMAX:xMIN]
  return(Precipgs6_SUM)
}
p=precipitation(years,precip, 1998)
plot(p)

temperature<-function(years, airTemp, h){
  airTemp_SUM=cumsum(airTemp)
  #plot(airTemp_SUM)
  airTemp_year=airTemp[years==h]
  #plot(airTemp_year)
  
  airTemp_SUM_year=cumsum(airTemp_year)
  #plot(airTemp_SUM_year)
  MIN6=min(airTemp_SUM_year)
  MAX6=max(airTemp_SUM_year)
  
  #plot(airTemp_SUM_year,main="Growing Season year", sub="Growing Season year", ylim=range(MIN6,MAX6))
  #?plot
  xMAX=which(airTemp_SUM_year==MAX6)
  xMIN=which(airTemp_SUM_year==MIN6)
  Tempgs6_SUM=airTemp_SUM_year[xMAX:xMIN]
  return(Tempgs6_SUM)
}
t=temperature(years,airTemp, 1998)
plot(t)

GrossPrimaryProductivity<-function(years, GPP, g){
  GPP_SUM=cumsum(GPP)
  GPP_year=GPP[years==g]
  GPP_SUM_year=cumsum(GPP_year)

  MIN6=min(GPP_SUM_year)
  MAX6=max(GPP_SUM_year)
  
  xMAX=which(GPP_SUM_year==MAX6)
  xMIN=which(GPP_SUM_year==MIN6)
  
  Tempgs6_SUM=GPP_SUM_year[xMAX:xMIN]
  return(Tempgs6_SUM)
}
g=temperature(years,GPP, 1998)
plot(g)

TotalEcosystemRespiration<-function(years, TER, r){
  GPP_SUM=cumsum(GPP)
  GPP_year=GPP[years==r]
  GPP_SUM_year=cumsum(GPP_year)
  
  MIN6=min(GPP_SUM_year)
  MAX6=max(GPP_SUM_year)
  
  xMAX=which(GPP_SUM_year==MAX6)
  xMIN=which(GPP_SUM_year==MIN6)
  
  Tempgs6_SUM=GPP_SUM_year[xMAX:xMIN]
  return(Tempgs6_SUM)
}
r=temperature(years,GPP, 1998)
plot(r)


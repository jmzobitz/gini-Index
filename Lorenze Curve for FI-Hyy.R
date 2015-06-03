#Nazih Safi
#June 2, 2015
#Lorenz Curve
#source

load('Gini-RData/FI-Hyy.Rda')

NEE=outData$data$NEE 
years=outData$data$years
precip=outData$data$precip
GPP=outData$data$GPP
TER=outData$data$TER
airTemp=outData$data$airTemp

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
#plot(n)

temperature<-function(years, airTemp, h){
  airTemp_SUM=cumsum(airTemp)
  airTemp_year=airTemp[years==h]
  airTemp_SUM_year=cumsum(airTemp_year)

  MIN6=min(airTemp_SUM_year)
  MAX6=max(airTemp_SUM_year)
  

  xMAX=which(airTemp_SUM_year==MAX6)
  xMIN=which(airTemp_SUM_year==MIN6)

  Tempgs6_SUM=airTemp_SUM_year[xMAX:xMIN]
  return(Tempgs6_SUM)
}
t=temperature(years,airTemp, 1998)
#plot(t)

precipitation<-function(years, precip, y){
  precip_SUM=cumsum(precip)
  precip_year=precip[years==y]
  precip_SUM_year=cumsum(precip_year)

  MIN6=min(precip_SUM_year)
  MAX6=max(precip_SUM_year)
  
  xMAX=which(precip_SUM_year==MAX6)
  xMIN=which(precip_SUM_year==MIN6)
  
  Precipgs6_SUM=precip_SUM_year[xMAX:xMIN]
  return(Precipgs6_SUM)
}
p=precipitation(years,precip, 1998)
plot(p)

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

gini<- function(x){
y=cumsum(c(0,x))/sum(x)
z=seq (0,1,length=length(y))
#plot(y,z)
head(z,n=-1)
tail(z,n=length(x))
l=sum(head(y, n=-1)*diff(z))
r=sum(tail(y, n=length(x))*diff(z))
trap=(l+r)/2
gc=1-2*trap
return(gc) 
}

gini(n)
gini(t)
gini(g)  #do we need to exclude some temp. point values??
gini(r)  #do we need to exclude some temp. point values??

plot(t[1:181],n[1:181])# NEE versus Temperature Lorenz curve
plot(p[1:181],n[1:181])# NEE versus Precipitation Lorenz curve
plot(t[1:181],g[1:181])# GPP versus Temperature Lorenz curve // could do 1:203 
plot(t[1:181],r[1:181])# GPP versus Temperature Lorenz curve // could do 1:203 

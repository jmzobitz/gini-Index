siteNames = list.files('Gini-RData');   # List the files in the folder


for (siteLoop in 1 : length(siteNames)) {   # Loop through the vector of sites 
  currFile = paste0('Gini-RData/', siteNames[siteLoop])    # Make a string of the file we will upload
  print(currFile)  # Monitor what file we are analyzing
  siteData = get(load(currFile));   # load file into the workspace
  
} 

fog = data("AU-Fog.Rda")
plotPoints( temp ~ month, data=fog, xlab="Month (Jan=1, Dec=12)", ylab="Temperature (F)",  
            main="Ave. Monthly Temp.", xlim=range(0,12), ylim=range(0,80))

load('Gini-RData/Au-Fog.Rda')
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

NEE_SUM=cumsum(NEE)
plot(NEE_SUM)

NEE_2006=NEE[years==2006]
#plot(NEE_2006)

NEE_SUM_2006=cumsum(NEE_2006)
plot(NEE_SUM_2006)
MIN6=min(NEE_SUM_2006)
MAX6=max(NEE_SUM_2006)

plot(NEE_SUM_2006, main="AU-Fog.R", sub="Growing Season 2006", ylim=range(-335.28,0.35))
#?plot

gs6=NEE_2006[NEE_SUM_2006<=MAX6 & NEE_SUM_2006>MIN6]

gs6_SUM=cumsum(gs6)
plot(gs6_SUM)

#year 2007 growing season below 

NEE_2007=NEE[years==2007]
#plot(NEE_2007)

NEE_SUM_2007=cumsum(NEE_2007)
plot(NEE_SUM_2007)
MIN7=min(NEE_SUM_2007)
MAX7=max(NEE_SUM_2007)

plot(NEE_SUM_2007, main="AU-Fog.R", sub="Growing Season 2007", ylim=range(-.289.53,-.94))
#?plot

gs7=NEE_2007[NEE<=MAX7 & NEE>MIN7]

gs7_SUM=cumsum(gs)
plot(gs7_SUM)

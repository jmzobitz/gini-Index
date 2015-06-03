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

plot(NEE_SUM_2006,main="Growing Season 2006", sub="Growing Season 2006", ylim=range(MIN6,MAX6))
#?plot
which(NEE_SUM_2006==MAX6)
gs6_SUM=NEE_SUM_2006[NEE_SUM_2006<=MAX6 & NEE_SUM_2006>MIN6]

plot(NEE_SUM_2006, main="Growing Season 2006", sub="Growing Season 2006", xlim=range(11,250), ylim=range(MIN6,MAX6))
plot(NEE_SUM_2006, main="Growing Season 2006", sub="Growing Season 2006")

#year 20067growing season below 

NEE_2007=NEE[years==2007]
#plot(NEE_2007)

NEE_SUM_2007=cumsum(NEE_2007)
plot(NEE_SUM_2007)
MIN7=min(NEE_SUM_2007)
MAX7=max(NEE_SUM_2007)

plot(NEE_SUM_2007,main="Growing Season 2007", sub="Growing Season 2007", ylim=range(MIN7,MAX7))
#?plot

gs7=NEE_2007[NEE<=MAX7 & NEE>MIN7]

gs7_SUM=cumsum(gs7)
plot(gs7_SUM, main="Growing Season 2007", sub="Growing Season 2007", ylim=range(MIN7,MAX7))

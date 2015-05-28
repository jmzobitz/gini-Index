siteNames = list.files('Gini-RData');   # List the files in the folder


for (siteLoop in 1 : length(siteNames)) {   # Loop through the vector of sites 
  currFile = paste0('Gini-RData/', siteNames[siteLoop])    # Make a string of the file we will upload
  print(currFile)  # Monitor what file we are analyzing
  siteData = get(load(currFile));   # load file into the workspace
  
} 
install.packages('Gini-RData')

names('Gini-RData')
plot(x = outData\$data, y = outData\$flags)
plot('AU-Fog.Rda', xlim=range(0,25),)
dim('AU-Fog.Rda')

fog = data("AU-Fog.Rda")
plotPoints( temp ~ month, data=fog, xlab="Month (Jan=1, Dec=12)", ylab="Temperature (F)",  
            main="Ave. Monthly Temp.", xlim=range(0,12), ylim=range(0,80))
scan("AU-Fog.Rda")
scan('Gini-RData')

plot(NEE)
plot(GPP)
plot(TER)
plot(-GPP+TER)

NEE_SUM=cumsum(NEE)
plot(NEE_SUM)

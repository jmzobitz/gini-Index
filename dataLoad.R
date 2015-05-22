siteNames = list.files('Gini-RData');   # List the files in the folder


for (siteLoop in 1 : length(siteNames)) {   # Loop through the vector of sites 
  currFile = paste0('Gini-RData/', siteNames[siteLoop])    # Make a string of the file we will upload
  print(currFile)  # Monitor what file we are analyzing
  siteData = get(load(currFile));   # load file into the workspace
  
}
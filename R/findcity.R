# Name: Nikoula, Latifah & Nikos
# Date: 9 January 2015
# Assignment 6: Raster Vector integration in R
# Function to define the greenest city

findcity<-function(cal, month) {
# data is the dataframe 
# month is the month where you want to investigate which city has the highest ndvi value  
  new <- as.character(subset(cal,cal[,month]==max(cal[,month], na.rm=TRUE), select=c(NAME_2)))
  return(new)
}

# Name: Nikoula, Latifah & Nikos
# Date: 12 January 2015

# the function returns the name of the greenest (maximum NDVI) place (province or city) 
 
greenestPlace<-function(df, month, NAME) {
  # df is a dataframe 
  # month is the month where you want to investigate which city has the highest ndvi value  
  # NAME corresponds with the administrative level. NAME_1 and NAME_2 correspond with provinces and cities, respectively.
  new <- as.character(subset(df,df[,month]==max(df[,month], na.rm=TRUE), select=c(NAME)))
  return(new)
}


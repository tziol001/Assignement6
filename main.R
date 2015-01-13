# Name: Nikoula, Latifah & Nikos
# Date: 12 January 2015

rm(list=ls()) # clear the workspace

# Required packages and functions
lib <- c("raster", "rgdal")
sapply(lib, function(...) require(..., character.only = TRUE))

getwd()# make sure the data directory

# call the functions
source('R/unzipURL.R')
source('R/greenestPlace.R')

# Select the url links that you want to download 
URL <- c("https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip")

# download data
unzipURL(URL)
cities <- getData('GADM',country='NLD', path='data/',level=3)

# process modis data
MODISpath <- list.files('data/url',pattern = glob2rx('*.grd'), full.names = TRUE)
modis<-brick(MODISpath)
modis<-modis/10000 #ndvi scale from 0 to 1

# reprojection
cities_proj <- spTransform(cities, CRS(proj4string(modis)))

# extract the ndvi values for each city
ex_ndvi<- extract(modis, cities_proj, fun=mean, sp=TRUE, na.rm= TRUE)

# converted into data frame for faster computations
ndvi.df<-as.data.frame(ex_ndvi)

# make the subset of the annual ndvi
ndvi.df$annual <- rowMeans(ndvi.df[,15:26], na.rm = FALSE)

# find the greeness city for several months by using the greenestPlace function. 
gr_january<-greenestPlace(ndvi.df, "January", "NAME_2")
gr_august<-greenestPlace(ndvi.df, "August", "NAME_2")
gr_annual<-greenestPlace(ndvi.df, "annual", "NAME_2")

# aggregate the provinces for specific months
months<-c('January','August')
agr_ndvi<-aggregate(ex_ndvi, vars='NAME_1', sums=list(list(mean, months)))

# select greenest province for a specific month
agr_ndvi.df<-as.data.frame(agr_ndvi) #convert to data frame
agr_january<-greenestPlace(agr_ndvi.df, "January", "NAME_1") # select the greenest province for January. The selection of province level is done by NAME_1.

# display some results
# spplot results in one composite image.
p1=spplot(ex_ndvi, zcol = 'January',main=paste("The greenest city of January is", gr_january), col.regions = colorRampPalette(c('lightgreen', 'darkgreen'))(100))

p2=spplot(agr_ndvi, zcol = 'January',main=paste("The greenest province of January is", agr_january), col.regions = colorRampPalette(c('lightgreen', 'darkgreen'))(100))

print(p1, position = c(0,.5,.5,1),more=T)
print(p2, position = c(.5,.5,1,1),more = T)

# write down the name of the calculated greenest cities as one comment
print(paste("The cities that have the highest average value of NDVI during the January and August are", gr_january,"and", gr_august,",respectively. On average over the year", gr_annual, "can be characterized as the greenest city."))
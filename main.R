# Required packages and functions
lib <- c("raster", "rgdal")
sapply(lib, function(...) require(..., character.only = TRUE))

getwd()# make sure the data directory

# call the functions
source('R/unzipURL.R')

# Select the url links that you want to download 
URL <- c("https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip")

# download data
unzipURL(URL)
cities <- getData('GADM',country='NLD', path='data/',level=3)

#process modis data
MODISpath <- list.files('data/url',pattern = glob2rx('*.grd'), full.names = TRUE)
modis<-brick(MODISpath)
modis<-modis/10000 # convert the values in ndvi scale

# reprojection
cities_proj <- spTransform(cities, CRS(proj4string(modis)))

ex_ndvi<- extract(modis, cities_proj, fun=mean, sp=TRUE, na.rm= TRUE)

# plot
spplot(ex_ndvi, zcol = 'January', col.regions = colorRampPalette(c('lightgreen', 'darkgreen'))(50))

#calib <- cbind(cities$NAME_2, ex_ndvi)
cal<-as.data.frame(ex_ndvi)

#make the subset of the annual ndvi
cal$annual <- rowMeans(cal[,15:26], na.rm = FALSE)

#find the greeness city

gr_january<-findcity(cal, "January")
gr_august<-findcity(cal, "August")
gr_annual<-findcity(cal, "annual")

#spplot results in one composite image.
p1=spplot(ex_ndvi, zcol = 'January',main=paste("The greenest city of January is", gr_january), col.regions = colorRampPalette(c('lightgreen', 'darkgreen'))(50))

p2=spplot(ex_ndvi, zcol = 'August',main=paste("The greenest city of August is", gr_august), col.regions = colorRampPalette(c('lightgreen', 'darkgreen'))(50))

p3=spplot(ex_ndvi, zcol = 'annual',main=paste("The greenest city of complete year is", gr_annual), col.regions = colorRampPalette(c('lightgreen', 'darkgreen'))(50))

print(p1, position = c(0,.5,.5,1),more=T)
print(p2, position = c(.5,.5,1,1),more = T)
print(p3, position = c(0,0,1,.5))

# aggregate
provinces <-aggregate(cal,list(cal$NAME_1),FUN=mean, na.rm=TRUE)

pr<-aggregate(ex_ndvi, fact=2, fun=mean, expand=TRUE, na.rm=TRUE,)

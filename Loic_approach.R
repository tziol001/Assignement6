library(raster)
library(ggplot2)
library(rgdal)

# download data
dir.create('data', showWarnings = FALSE)
download.file(url = 'https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip', destfile = 'data/MODIS.zip')
unzip('data/MODIS.zip', exdir = 'data')

# Load objects
nlCity <- getData('GADM',country='NLD', level=3)
modis <- brick('data/MOD13A3.A2014001.h18v03.005.grd')

# Take a look at the modis object
modis

# Reproject vector layer to projection of the raster layer (MODIS default projection is called Sinusoidal)
nlCitySin <- spTransform(nlCity, CRS(proj4string(modis)))

# Extract the mean NDVI value of each month for each polygon of nlCitySin
# Ask the function to return an sp object, so that the return object is similar to the input spatialPolygonDataFrame, with extracted values appended to the @data slot of the object
# Setting na.rm to TRUE is necessary to avoid getting NA for entire municipality which main contain only one NA pixel
system.time(green <- extract(modis, nlCitySin, fun = mean, sp=TRUE, na.rm = TRUE))

# Let's look at the object's metadata
green
# And at the structure of the dataframe attached to it
str(green@data)

# Find out the greenest city in JAnuary
maxJanuarySPDF <- green[green$January == max(green$January),]
maxAugustSPDF <- green[green$August == max(green$August),]

# Or using subset() on the dataframe only
greenDf <- green@data
# The subset= argument selects rows; the select= argument selects columns
subset(greenDf, subset = January == max(greenDf$January), select = c(NAME_2, January))
subset(greenDf, subset = August == max(greenDf$August), select = c(NAME_2, August))

# Annual mean
# Compute new column
greenDf$Year <- rowMeans(greenDf[,15:26])
subset(greenDf, subset = Year == max(greenDf$Year), select = c(NAME_2, Year))

# Per province
greenProv <- aggregate(green, vars = 'NAME_1', sums=list(list(mean, 'January'), list(mean, 'August')))

# Visualize results
## ggplot2 only understand dataframe, so that our SPDF needs to be converted
## fortify can be used to coerce most R classes to dataframes usefull for ggplot2
gg0 <- fortify(green, region = 'NAME_2')
gg <-merge(gg0, green@data, by.x = 'id', by.y = 'NAME_2')
str(gg)

# January

ggJan <- fortify(maxJanuarySPDF, region = 'NAME_2')

ggplot(gg) +
  geom_polygon(aes(x=long, y=lat, group=group, fill = January)) +
  geom_polygon(aes(x=long, y=lat, group=group), colour = 'white', fill = NA) +
  geom_polygon(data = ggJan, aes(x=long, y=lat, group=group), colour = 'red', fill = NA) +
  scale_fill_distiller(type = 'seq', palette = "YlGn", na.value = "red") +
  coord_equal() +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

# August

ggAug <- fortify(maxAugustSPDF, region = 'NAME_2')

ggplot(gg) +
  geom_polygon(aes(x=long, y=lat, group=group, fill = August)) +
  geom_polygon(aes(x=long, y=lat, group=group), colour = 'white', fill = NA) +
  geom_polygon(data = ggAug, aes(x=long, y=lat, group=group), colour = 'red', fill = NA) +
  scale_fill_distiller(type = 'seq', palette = "YlGn", na.value = "red") +
  coord_equal() +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
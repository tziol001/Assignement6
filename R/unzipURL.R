# Name: Nikoula, Latifah & Nikos
# Date: 8 January 2015

# The Function download zipped shapefiles from url and unzip them to further analysis

unzipURL <- function(url){
  download.file(url=url, destfile='data/url.zip', method='auto')
  unzip('data/url.zip', exdir="data/url")
  unlink('data/url.zip', recursive = TRUE)
}



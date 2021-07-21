getwd()
HII <- raster("w001001x.adf")
?raster

require(raster)
library(raster)
r = raster("C:/Users/nisar/Desktop/R_wd/R/w001001x.adf")

#need to provide the whole folder for the import to work.
HII_adf <- raster("C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/hii_europe_geo_grid/hii_europe/w001001.adf") #on hannas comp
HII_adf <- raster("C:/Users/mbzhh/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/hii_europe_geo_grid/hii_europe/w001001.adf") #on hannas comp
plot(HII_adf)

read.adf("w001001.adf")
plot(w001001x.adf)
library(adfExplorer)
library(rrd)
install.packages("rrd")
install.packages("adfExplorer")

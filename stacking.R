# Extents ----------------------------------------------------
coordExt <- c(-12, 5, 49, 61) # UK extent
ext_uk <- c(-86644,	685295, -16137, 1247364) #picked up from the elev_27700 file 

# Bioclim ------------------------------------------------------
coordExt <- c(-12, 5, 49, 61) # UK extent
all_bioclimvars <- crop(bioclimVars, coordExt)
plot(all_bioclimvars)

all27700 <- projectRaster(all_bioclimvars, crs=27700)
plot(all27700)
str(all27700) 

all27700crop <- crop(all27700, ext_uk)
plot(all27700crop) #works! didn't work cropping to this extent straight
# away? no idea why?

# Elevation ----------------------------------------------------
elevation <- getData('alt', country='GBR')
elev_27700 <- projectRaster(elevation, crs=27700)
plot(elev_27700)
str(elev_27700)

elev_crop_uk<- crop(elev_27700, ext_uk)
plot(elev_27700)
plot(elev_crop_uk)

# Human Influence Index -----------------------------------------
HII = raster("C:/Users/nisar/Desktop/R_wd/R/hii-europe-geo-grid (1)/hii-europe-geo-grid/hii_europe_geo_grid/hii_europe/w001001x.adf")
HIIc <- crop(HII, coordExt)
plot(HIIc) #first crop

HII27700 <- projectRaster(HIIc, crs=27700) #setting crs

HII27700c <- crop(HII27700, ext_uk)
plot(HII27700c) #crop to elev extent! WORKS!

# Carbon ---------------------------------------------------------

carbon = raster("C:/Users/nisar/Desktop/R_wd/R/210621/totareaco2181.shp")
plot(carbon)

carbon27700 <- projectRaster(carbon, crs=27700)
carbon27700c <- crop(carbon27700, ext_uk)
plot(carbon27700c)

# Copper --------------------------------------------------------

copper = raster("C:/Users/nisar/Desktop/R_wd/R/210621/copper1.shp")
plot(copper)
plot(cu)

# Resample ------------------------------------------------------

elev_resamp <- resample(elev_crop_uk,all27700crop,method="bilinear")
HII_resamp <- resample(HII27700c, all27700crop, method="bilinear")
carbon_resamp <- resample(carbon27700c, all27700crop, method="bilinear")
cu_resamp <-resample(cu, all27700crop, method='bilinear')
# Let's stack! :) ------------------------------------------------

stack1 <- stack(all27700crop, elev_resamp, HII_resamp, carbon_resamp, cu_resamp)
plot(stack1)
stack1
nlayers(stack1)

# look for collinearity ---------------------------------------------------
#visually, easy to see which ones are problematic
pairs(sdmdata[,2:5], cex=0.1)
pairs(sdmdata[,6:9], cex=0.1)
pairs(sdmdata[,10:13], cex=0.1)
pairs(sdmdata[,14:17], cex=0.1)
pairs(sdmdata[,18:21], cex=0.1)
pairs(sdmdata[,21:23], cex=0.1)

#found a helpful package!
#install.packages("virtualspecies")
library(virtualspecies)

removeCollinearity(stack1, plot = TRUE, select.variables = TRUE)
keep <- removeCollinearity(stack1, plot = TRUE, select.variables = TRUE)
#default cut off is pearson correlation coefficient 0.7
#stick with 0.7 and choose most meaningful out of these? 

#to implement the selection
stack1_trimmed <- subset(stack1, keep, drop=TRUE) 
removeCollinearity(stack1_trimmed, plot = TRUE, select.variables = TRUE)
#no intercorrelation found now.

#rename for convenience, wont have to change stack name in code
stack1 <-stack1_trimmed


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


# Resample ------------------------------------------------------

elev_resamp <- resample(elev_crop_uk,all27700crop,resample="bilinear")
HII_resamp <- resample(HII27700c, all27700crop, resample="bilinear")
carbon_resamp <- resample(carbon27700c, all27700crop, resample="bilinear")

# Let's stack! :) ------------------------------------------------

stack1 <- stack(all27700crop, elev_resamp, HII_resamp, carbon_resamp)
plot(stack1)
stack1
nlayers(stack1)

?layers_correlation
layers_correlation(c("all27700crop", "elev_resamp", "HII_resamp", "carbon_resamp"))
layers_correlation (as.matrix(stack1))
layers_correlation(stack1)[1:22, 1:22]

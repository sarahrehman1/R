
# projecting and ensuring same extent -----------------------------
#as an example, project two bioclim layers to OSGB coord system

# Changing projection
# Use `projectRaster` function:

bio2_27700 <- projectRaster(bio2, crs=27700)
plot(bio2_27700)
str(bio2_27700) 

bio4_27700 <- projectRaster(bio4, crs=27700)
plot(bio4_27700)
str(bio4_27700)

elevation <- getData('alt', country='GBR')
elev_27700 <- projectRaster(elevation, crs=27700)
plot(elev_27700)
str(elev_27700)
#not bioclim data - extent smaller

#define to extents
ext_sarah <- extent(-12, 5, 49, 61)
ext_uk <- extent(-86644,	685295, -16137, 1247364) #picked up from the elev_27700 file 

#crop the bioclim and see difference
bio2_crop_sarah <- crop(bio2_27700, ext_sarah)
bio2_crop_uk <- crop(bio2_27700, ext_uk)

plot(bio2_crop_sarah)
plot(bio2_crop_uk)
#only UK extent works in this order - project then crop.
bio4_crop_uk <- crop(bio4_27700, ext_uk)

#this should not change as extent was taken from here
elev_crop_uk<- crop(elev_27700, ext_uk)
plot(elev_27700)
plot(elev_crop_uk) #yes works

# Stacking of rasters with presumed same extent ---------------------------

#stack the two bioclim ones - should work fine
stack1 <- stack(bio2_crop_uk,bio4_crop_uk)
#stack the uncropped and cropped, should not work
stack2 <- stack(bio2_crop_uk, bio2_27700)
#as expected :)

#now try the tricky case...
stack3 <- stack(bio2_crop_uk, elev_crop_uk)
str(bio2_crop_uk)
str(elev_crop_uk)
str(elev_27700)
#not expected - annoying! maybe due to raster resolution the extent set is almost the same but not quite
#this means we need to resample to make same resolution
#but which one to resample? test both!

bio2_crop_uk_resamp <- resample(bio2_crop_uk,elev_27700,resample="bilinear")
elev_resamp <- resample(elev_27700,bio2_crop_uk,resample="bilinear")

#stack them - this works now!
stack4 <- stack(bio2_crop_uk_resamp, elev_27700)
stack5 <- stack(bio2_crop_uk, bio4_crop_uk, elev_resamp)
#choosing the most common resolution might be the best route forward, which then would be the bioclim one. 
plot(stack4)
plot(stack5)


# Stacking rasters for real -----------------------------------------------

#bioclim set
bioclimVars <- getData(name="worldclim", #other options available 
                       res = 0.5, # resolution
                       var = "bio",
                       lon = -12,
                       lat = 64) # which variable(s)?
all <- projectRaster(bioclimVars, crs=27700)
all_bioclimvars <- crop(bioclimVars, coordExt)

all_pts_data <- raster::extract(all[[1:19]],coord,df=T)
all_pts_data

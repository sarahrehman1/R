# load packages
library(rgdal) # 'Geospatial' Data Abstraction Library ('GDAL')
library (sp)
library(raster) # for all things raster and more
library(dismo) # species distribution modeling and much more
library(maps) # quick plotting of countries, etc.
library(gtools) # various functions
library(rasterVis) # raster visualization methods
library(fields)# Curve / function fitting for spatial analyses
library(sf)

setwd("C:/Users/nisar/Desktop/R_wd")
getwd()

#load species coordinates
coord <- shapefile("newcoord")
plot(coord)
points(coord)

coordinates <- read.csv(file = 'Ines_coordinates.csv')
head(coordinates)
crs(coordinates)
plot(coordinates)

st_crs(27700)$proj4string
coord2 = st_transform(coord, 27700)
newcrs <- CRS("+proj=longlat +datum=OSGB36")
newcrs <- CRS("+init=epsg:27700")
crs(newcrs)
bng <- "+init=epsg:27700"
crs(bng)


#load UK shapefile
britain <- shapefile("infuse_ctry_2011.shp")
plot(britain)

#plot species data points on britain map
plot(coord, add=TRUE) 

#Downloading raster climate data from internet
?getData
bioclimVars <- getData(name="worldclim", #other options available 
                       res = 10, # resolution
                       var = "bio") # which variable(s)?

class(bioclimVars) # raster stack - same projection, spatial extent,
# and resolution
bioclimVars #name of entire raster stack
extent(bioclimVars)
plot(bioclimVars) # takes a few seconds...plots first 16/19 in stack
#I only need bio2, bio3, bio4, bio10, bio11 - variation in temperature.
#And bio18 + bio19 for precipitation predictors.

#Loading each single raster layer
filePath <- paste(getwd(), "/wc10/bio19.bil", sep="")
filePath
bio2 <- raster(paste(getwd(), "/wc10/bio2.bil", sep=""))
bio3 <- raster(paste(getwd(), "/wc10/bio3.bil", sep=""))
bio4 <- raster(paste(getwd(), "/wc10/bio4.bil", sep=""))
bio10 <- raster(paste(getwd(), "/wc10/bio10.bil", sep=""))
bio11 <- raster(paste(getwd(), "/wc10/bio11.bil", sep=""))
bio18 <- raster(paste(getwd(), "/wc10/bio18.bil", sep=""))
bio19 <- raster(paste(getwd(), "/wc10/bio19.bil", sep=""))
plot(bio2) #mean dirunal range
zoom(bio2) #click 2 locations on map

# Creating a raster stack 
# Let's collect several raster files from disk 
# and read them as a single raster stack:
file.remove(paste(getwd(), "/wc10/", "bio_10m_bil.zip", sep=""))
# sort the file names using ?mixedsort
files <- list.files(path=paste(getwd(), "/wc10/", sep=""), 
                    full.names=T, 
                    pattern=".bil") 
# all files with .bil pattern 
# we want to stack them in order by name (1-19), so we need to sort 
# the file paths
list.ras <- mixedsort(files)
list.ras 
# in order to stack rasters, they MUST ALL be identical in terms
# of extent, resolution, etc.
bioclimVars <- stack(list.ras)
bioclimVars


# Alternatively, provide coordinates for the limits of the region of interest:
coordExt <- c(-12,	5, 49, 61) # UK extent 
bio2crop <- crop(bio2, coordExt)
plot(bio2crop)
points(coord)
plot(coord)
plot(britain)
plot(britain, add=T)

# Plotting bryo data points on bioclim maps -------------------------------

##HH there appears to be some issues with the datum, it is discarded - and throws a warning. 
##the vignette below seems to suggest its because of an updates in the packahe.
##https://cran.r-project.org/web/packages/rgdal/vignettes/PROJ6_GDAL3.html#Status_22_April_2020
packageVersion("rgdal")
rgdal_extSoftVersion()

##it seems the +datum errors come even with other datums - therefore not specific to OSGB36!
(crs <- CRS("+proj=longlat +ellps=WGS84")) #simply define a crs as a demonstrating
## notice discarded datum warning. Document above says the error messages are over cautious

discarded_datum <- showSRID("EPSG:27700", "PROJ") #this is the OSGB36 code
discarded_datum # this is the outcome when trying to assign EPSG:27700
# the projection seems to be OK despite the datum not defined. 
str(coord) #notice the crs format here (str() just shows the object attributes)

#trying to next plot these on to a bioclim map

# taking bio2 and using Sarahs extent
coordExt <- c(-12,	5, 49, 61) # UK extent 
bio2crop <- crop(bio2, coordExt)
plot(bio2crop)
str(bio2crop)
#notice this is a raster layer with crs "+proj=longlat +datum=WGS84 +no_defs"
#reproject onto 27700 before laying the points on top

# Changing projection
# Use `projectRaster` function:
bio2crop_sw <- projectRaster(bio2crop, crs=27700)
plot(bio2crop_sw)
str(bio2crop_sw) #note crs has changed and looks like the one for the coord file...
#add the points
plot(coord, add=T)

#bonus - extract data from bio2crop_sw for coord points
pts_data <- raster::extract(clim[[1:4]],pts,df=T)
pts_data <- raster::extract(bio2crop_sw,coord,df=T)
pts_data

#and for all vars
all_bioclimvars <- crop(bioclimVars, coordExt)
all <- projectRaster(all_bioclimvars, crs=27700)
all_pts_data <- raster::extract(all[[1:7]],coord,df=T)
all_pts_data

#over to sarah for triage
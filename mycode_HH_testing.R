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

##HH added from email
newcrs <- CRS("+proj=longlat +datum=OSGB36")
##HH there appears to be some issues with this
## https://cran.r-project.org/web/packages/rgdal/vignettes/PROJ6_GDAL3.html#Status_22_April_2020
packageVersion("rgdal")
rgdal_extSoftVersion()
## we have the newest packages which are affected
(crs <- CRS("+proj=longlat +ellps=WGS84"))
##notice the errors come even with WGS84 - therefore not specific to OSGB36
##some code pinched from doc above to see what is in the crs string
##
(discarded_datum <- showSRID("EPSG:27700", "PROJ"))
(x <- list_coordOps(paste0(discarded_datum, " +type=crs"), "EPSG:4326"))
best_instantiable_coordOp(x)
list_coordOps(paste0(discarded_datum, " +datum=OSGB36 +type=crs"), "EPSG:4326")
wkt_source_datum <- showSRID("EPSG:27700", "WKT2")
wkt_target_datum <- showSRID("EPSG:4326", "WKT2")
(x <- list_coordOps(wkt_source_datum, wkt_target_datum))
y <-best_instantiable_coordOp(x)
x
y1 <- y[1]
y1

newcrs_HH <- CRS("+proj=pipeline +step +inv +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +step +proj=push +v_3 +step +proj=cart +ellps=airy +step +proj=helmert +x=446.448 +y=-125.157 +z=542.06 +rx=0.15 +ry=0.247 +rz=0.842 +s=-20.489 +convention=position_vector +step +inv +proj=cart +ellps=WGS84 +step +proj=pop +v_3 +step +proj=unitconvert +xy_in=rad +xy_out=deg")
newcrs_HH <- CRS(y)
(cat(strwrap(gsub(",", ", ", (comment(newcrs)))), sep="\n")) # note looking at your newcrs here
##HH it is defined here. Document above says the error messages are over cautious
newData <- projectRaster(bio2crop, proj4string(coord))

##this error is different
bio2GB <- spTransform(bio2crop, newcrs)

?projectRaster
# Changing projection
# Use `projectRaster` function:
bio19.swProj <- projectRaster(bio19.sw, crs="+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs")   # can also use a template raster, see ?projectRaster
bio19.swProj   # notice info at coord.ref.
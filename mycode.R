# load packages
library(rgdal) # 'Geospatial' Data Abstraction Library ('GDAL')
library(sdm)
library (sp)
library(raster) # for all things raster and more
library(dismo) # species distribution modeling and much more
library(maps) # quick plotting of countries, etc.
library(gtools) # various functions
library(rasterVis) # raster visualization methods
library(fields)# Curve / function fitting for spatial analyses
library(sf)
library(maptools)
library(ecospat)
library(colorRamps)
install.packages(sdm)

setwd("C:/Users/nisar/Desktop/R_wd/R")
getwd()

#load species coordinates
coord <- shapefile("newcoord")
plot(coord)
points(coord)
head(coord)

#load UK shapefile
britain <- shapefile("infuse_ctry_2011.shp")
plot(britain)

#plot species data points on britain map
plot(coord, add=TRUE) 

#Downloading raster climate data from internet
?getData
bioclimVars <- getData(name="worldclim", #other options available 
                       res = 2.5, # resolution
                       var = "bio") # which variable(s)?

class(bioclimVars) # raster stack - same projection, spatial extent,
# and resolution
bioclimVars #name of entire raster stack
extent(bioclimVars)
plot(bioclimVars) # takes a few seconds...plots first 16/19 in stack
#I only need bio2, bio3, bio4, bio10, bio11 - variation in temperature.
#And bio18 + bio19 for precipitation predictors.

#Loading each single raster layer
filePath <- paste(getwd(), "/wc2-5/bio19.bil", sep="")
filePath
bio2 <- raster(paste(getwd(), "/wc2-5/bio2.bil", sep=""))
bio3 <- raster(paste(getwd(), "/wc2-5/bio3.bil", sep=""))
bio4 <- raster(paste(getwd(), "/wc2-5/bio4.bil", sep=""))
bio10 <- raster(paste(getwd(), "/wc2-5/bio10.bil", sep=""))
bio11 <- raster(paste(getwd(), "/wc2-5/bio11.bil", sep=""))
bio18 <- raster(paste(getwd(), "/wc2-5/bio18.bil", sep=""))
bio19 <- raster(paste(getwd(), "/wc2-5/bio19.bil", sep=""))
plot(bio2) #mean dirunal range
zoom(bio2) #click 2 locations on map

nlayers(bioclimVars)


# Creating a raster stack 
# Let's collect several raster files from disk 
# and read them as a single raster stack:
file.remove(paste(getwd(), "/wc2-5/", "bio_2-5m_bil.zip", sep=""))
# sort the file names using ?mixedsort
files <- list.files(path=paste(getwd(), "/wc2-5/", sep=""), 
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
bioclimVars1 <- bioclimVars[[c(2, 3, 4, 10, 11, 18, 19)]] 
plot(bioclimVars1)


# Alternatively, provide coordinates for the limits of the region of interest:
coordExt <- c(-12,	5, 49, 61) # UK extent 
bio2crop <- crop(bio2, coordExt)
plot(bio19crop)
points(coord)
plot(coord)
plot(britain)
plot(britain, add=T)
(bioclim_stack_cropped <- crop(bioclimVars1, coordExt)) #HH cropping the stack into the UK extent)

  # Plotting bryo data points on bioclim maps -------------------------------

##HH there appears to be some issues with the datum, it is discarded - and throws a warning. 
##the vignette below seems to suggest its because of an updates in the package.
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

bio19crop <- crop(bio19, coordExt)
plot(bio19crop)
str(bio19crop)

bio19crop_sw <- projectRaster(bio19crop, crs=27700)
plot(bio19crop_sw)
str(bio19crop_sw) #note crs has changed and looks like the one for the coord file...
#add the points
plot(coord, add=T)

# Changing projection
# Use `projectRaster` function:
bio2crop_sw <- projectRaster(bio2crop, crs=27700)
plot(bio2crop_sw)
str(bio2crop_sw) #note crs has changed and looks like the one for the coord file...
#add the points
plot(coord, add=T)

#over to sarah for triage

# Elevations, slope, aspect, etc
# Download elevation data:
dev.off()
elevation <- getData('alt', country='GBR')

#cropping data 
elevationcrop <- crop(elevation, coordExt)
plot(elevationcrop)
str(elevationcrop)

#projecting OSGB36 crs. 
elevationcrop_sw <- projectRaster(elevationcrop, crs=27700)
plot(elevationcrop_sw)
plot(coord, add=T)


# Some quick maps:
slopAsp <- terrain(elevationcrop_sw, opt=c('slope', 'aspect'), unit='degrees')
par(mfrow=c(1,2))
plot(slopAsp)
dev.off()
slope <- terrain(elevationcrop_sw, opt='slope')
aspect <- terrain(elevationcrop_sw, opt='aspect')
hill <- hillShade(slope, aspect, 20, 280)
plot(hill, col=grey(0:100/100), legend=FALSE, main='UK')
plot(elevationcrop_sw, col=rainbow(25, alpha=0.35), add=TRUE)
plot(aspect)
plot(slope)
#now need to figure out how to include these in the raster stack

save(elevationcrop_sw, file="elevation.data")
load("elevationcrop_sw")
#load("elevation.data") HH added this - not sure where its used again...
##--------------------Plotting data------------------------------------
plot(britain)
plot(coord, add=TRUE) 
points(coord, cex=0.5, pch=20, col='blue')

##--------------extract environmental data----------------------------
#bonus - extract data from bio2crop_sw for coord points
#pts_data <- raster::extract(clim[[1:4]],pts,df=T) redundant code
pts_data <- raster::extract(bio2crop_sw,coord,df=T)
pts_data

#and for all vars
all_bioclimvars <- crop(bioclimVars, coordExt)
all <- projectRaster(all_bioclimvars, crs=27700)
all_pts_data <- raster::extract(all[[1:19]],coord,df=T)
all_pts_data


##----------creating background data----------------------------------
# setting a random seed to always create the same
# random set of points for this example
set.seed(0)
# create 500 random background points 
?randomPoints

#what are the predictors we are working with now? WE need a rasterstack of them
#this looks appropriate - RasterBrick and RasterStack are similar classes so either is fine
names(all) # generated above, projecting al bioclim to 27700
plot(all)
str(all)

#then generate the random points
backgr <- randomPoints(all, 500)
colnames(backgr)
# and then extract env data at the background points
absvals <- extract(all, backgr)

##what about the presence data?
#this is the bioclimvariable data extracted for the presence sites
presvals <- extract(all, coord) #coord is the file imported by Sarah that has the species records, can use directly as all already projected to 27700
head(presvals)
str(presvals)

# make a vector of 1's and 0's to match the
# presence records and the background data
# See ?rep
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))

# now we bind everything together into a data.frame
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))

# if you need to transform any into factors, use below
# biome is a factor, so define it that way
# sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
# or you could drop it completely using the below
# # make new stack without the categorical variable 'biome'
# as not all SDMs can use categorical data
# pred_nf <- dropLayer(predictors, 'biome')

# and have a look at the data
head(sdmdata)
tail(sdmdata)
summary(sdmdata)
#plot(sdmdata) takes a while

## ---- Create training / test dataset ----------------------------------------
set.seed(0)
?kfold

group_coord <- kfold(coord, k=5) # 5 groups = 80/20 split for training vs testing 
(pres_train <- coord[group_coord != 1, ])
(pres_test <- coord[group_coord == 1, ])
#for plotting below, these should only have 
# let's set an extent to crop the env data to make predictions faster
#ext <- extent(-90, -32, -33, 23)

#And for the random points
#
#coordgroup_backgr <- kfold(backgr, 5)
backg_train <- backgr[group_backgr != 1, ]
backg_test <- backgr[group_backgr == 1, ]

## ----  Background data for train/test set ------------------------------------
set.seed(10)
#backgr <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25) not needed, we cropped our stack before
#pred_nf is the raster stack of predictors with categorical factors dropped off. not a problem for the bioclim data
#we already did this generation of random points, using backgr

colnames(backgr) = c('lon', 'lat') # not sure why, for convenience?

r <- raster(all, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
#plot(ext, add=TRUE, col='black', lwd=2) we already set out extent
points(backg_train, pch=20, cex=0.5, col='yellow')
points(backg_test, pch=20, cex=0.5, col='black')
points(pres_train, pch= '+', col='red') 
points(pres_test, pch='+', col='blue')

## ---- Fit MaxEnt -------------------------------------------------------------
?maxent #needs predictor variables in rasterstack. needs occurrence
# data, either matrix or spatialpoints object (2 colums x y)
# can give background points, or it will automatically
# tell it if any of the data is categorical using factors argument

maxent()

filePath <- "C:/Users/nisar/Desktop/R_wd/MaxentOut"
getwd()
filePath <- "C:/Users/mbzhh/Documents/GitHub/R" # on HH machine
mx <- maxent(predictors, # env data as a raster stack
             pres_train, # presence data
             factors='biome', # biome is categorical
             path=filePath) # where to save all the output
plot(mx) # shows you each variable's contribution
mx #website summarising all of the output

# evaluate the model using the test data
e <- evaluate(pres_test, backg_test, mx, predictors) 
# evaluate model using test presences and background data that 
# was previously prepared.
e # when evaluating model on data the model hasn't seen, it always 
# performs a little worse.

# predict back to geography, make mapped prediction giving it the
# model and the predictors.
mxPred <- predict(mx, predictors)
plot(mxPred, col=rgb.tables(1000)) # give it the no. of colours u want
# Predict in 'raw' format and save raster of prediction
# change some values using args function
mxPred <- predict(mx, predictors, args=c("outputformat=raw"),
                  filename=paste0(filePath, '/maxent_predictionRAW.tif'))

# let's check model quality using the Boyce Index
predRast <- raster(paste0(filePath, '/maxent_predictionRAW.tif'))
ecospat.boyce(predRast, pres_test)

# there are lots of options to change (see the provided MaxEntHELP.pdf)
# we use the 'args' argument to set the options we want by providing the 
# appropriate "flag". Doing so changes the the default value of the flag. 
# For example, here I am asking for 'responsecurves' to be generated. 
# The default is to not report response curves (responsecurves=FALSE). 
# By flagging 'responsecurves', we change it from the default (TRUE).
# Let's run the model:
mx <- maxent(predictors, 
             pres_train, 
             factors='biome',
             path=filePath,
             args=c("responsecurves"))
mx

# perform multiple replicates (5 in this example - will take
# a few minutes to run)
mx <- maxent(predictors, 
             pres_train, 
             factors='biome',
             path=filePath,
             args=c("responsecurves", "replicates=5"))
mx # shows uncertainty envelopes around different measures, AUC and
# around response curves. 

# Let's now measure variable importance using jackknife and also produce 
# response curves. Running this model will take a minute or two.
mx <- maxent(predictors, 
             pres_train, 
             factors='biome',
             path=filePath,
             args=c("jackknife", "responsecurves"))
# jackknife fits model with all variables, without one of the variables,#
# and then again with just that variable.

# To fit a model with only linear and product features, we have to turn off all 
# other feature types
mx <- maxent(predictors, 
             pres_train, 
             factors='biome',
             path=filePath,
             args=c("jackknife", "responsecurves",
                    "-h", # turn off hinge features
                    "-q", # turn off quadratic features
                    "nothreshold")) # turn off threshold))
#simplifies model, making more linear, simple graphs if you remove
# threshold, hinge, and quadratic features. 

# Change the 'beta mulitplier' which controls model complexity (smaller
# values = greater complexity). Should run a range of values. 
mx <- maxent(predictors, 
             pres_train, 
             factors='biome',
             path=filePath,
             args=c("betamultiplier=0.25","responsecurves"))

mx # let's try projecting to future climate. For simplicity, we will create
# some fake future climate layers
future <- predictors
future$bio5 <- future$bio5+40 # increase max temp by 4C (recall temp is 10x)
future$bio6 <- future$bio6+80 # increase min temp by 8C (recall temp is 10x)
future$bio7 <- future$bio5-future$bio6 # recalculate temp annual range (bio7)
future$bio12 <- future$bio12*0.67 # decrease precipitaiton by 33%

# save each layer to disk
projNames <- paste0("C:/Users/nisar/Desktop/R_wd/MaxentOut",
                    names(future), ".asc")
for(i in 1:length(projNames)){
  writeRaster(future[[i]], projNames[i], overwrite=T)
}

# run the model and project to the new layers by providing the directory
# where the new layers are saved. Much more output will be written to disk
# automatically. We will step through some of this output below.
mx <- maxent(predictors, 
             pres_train, 
             factors='biome',
             path=filePath,
             args=c("projectionlayers=C:/Users/nisar/Desktop/R_wd/MaxentOut"))

mx
# Where the model extrapolating beyond the data?
clamping <- raster("/Users/mfitzpatrick/code/PRStats_SDMs/maxentOut/species_projectionLayers_clamping.asc")
# areas in red are problematic
plot(clamping, col=rgb.tables(1000))

# multivariate environmental similarity surface (MESS)
# a measure of how novel the new climate is relative to the conditions under
# which the model was fit.
mess <- raster("/Users/mfitzpatrick/code/PRStats_SDMs/maxentOut/species_projectionLayers_novel.asc")
# areas in red have one or more environmental variables outside the range 
# present in the training data
plot(mess, col=rgb.tables(1000)[1000:1])

# which variable is 
novelLimit <- raster("/Users/mfitzpatrick/code/PRStats_SDMs/maxentOut/species_projectionLayers_novel_limiting.asc")
plot(novelLimit)



################################################################################
# script to model ant distributions in New England using maxent
# 
#
# Results described in:
# Fitzpatrick et al. (2013) MAXENT vs. MAXLIKE: Empirical Comparisons with 
# Ant Species Distributions. Ecosphere
#
# DESCRIPTION & NOTES
# The code needs the following to run:
# (1) climate & elevation rasters (provided as neClim)
# (2) ant distribution data (provided as antData_use4R_snappedToClim.csv). Note
# that as the file name suggests these data have been snapped to cell centroids.
#
################################################################################

# Our goal here is to fit and compare several different maxent models. 
# All will be fit to the same data, but we will see how changing the complexity
# of the features and correcting for sampling bias (or not) alter outcomes. We
# will compare them in terms of predictions and using model selection 
# approaches (AIC).

################################################################################
# CHUNK 1: Read in & prep data
################################################################################
library(dismo)
library(raster)
library(sp)
library(sm)
library(colorRamps)
library(ENMeval)

setwd("/Users/mfitzpatrick/code/PRStats_SDMs/data") # setwd to where data files are stored

# ant ocurrence data for New England, USA
# FOr about 120 species of ants
antsGeoXY <- read.csv("antData.csv")

# read in climate & elev data
neClim <- stack("neClim.grd") # three variables
antsSp <- antsGeoXY # make a new object for use later
# convert to sp object
coordinates(antsSp) <- c("x", "y")
# assign projection
projection(antsSp) <- projection(neClim)

# let's look at the data
plot(neClim$bio_1, col=rgb.tables(1000))
points(antsSp, pch=20, cex=0.5, col=rgb(0,0,0,0.25))

# make a mask raster to use below
mask <- neClim[[1]]>-1000


# Sampling bias ----------------------------------------------------------------
# To deal with spatial sampling bias, we will create
# a raster that reflects the sampling density of ants
# in the study region using kernel density estimation (KDE)

# use the x-y coords to get the cell numbers
bias <- cellFromXY(mask, antsSp) # cells with records
cells <- unique(sort(bias))
# xy-locations of all samples
kernelXY <- xyFromCell(mask, cells)
samps <- as.numeric(table(bias)) 
# number of samples in each grid cell
head(samps)
max(samps) #one grid cell has 255 records in it

# code to make KDE (kernel density estimate) raster
# Our goal is to extrapolate sampling density across the study region
# and to make a raster that matches the env predictors exactly. To do that,
# we need to set some parameters to produce a grid that is close in size
# and resolution to the env rasters. The 'sm.density' function will perform
# 2D density estimation.
KDEsur <- sm.density(kernelXY, 
                     weights=samps, 
                     display="none", # do not plot 
                     ngrid=812, # number of grid cells along each dimension,
                     # have a look at the clim data = 812 columns
                     ylim=c(40,48), # approximate latitude range 
                     xlim=c(-75,-65), # approximate longitude range
                     nbins=0) # see help, can ignore

# let's look at the structure
str(KDEsur)
head(KDEsur$eval.points) # point locations where denisty was estimated
KDEsur$estimate[1:5, 1:5] # the density estimates

# now we need to turn this output into a raster
# expand grid will make all possible combinations of two vectors
# in this case we want to get all possible combinates of the x-y coordinates
# which will produce a full grid
pointGrid <- expand.grid(x=KDEsur$eval.points[,1], y=KDEsur$eval.points[,2])
KDErast <- SpatialPoints(pointGrid) # convert to sp object
# And now a step closer to a grid
KDErast <- SpatialPixelsDataFrame(KDErast, # points
                                  data.frame(kde = array(KDEsur$estimate, # data
                                                         length(KDEsur$estimate))))
# convert to a raster and plot
KDErast <- raster(KDErast)
plot(KDErast)

# now crop, etc so that it matches the climate data
KDErast <- resample(KDErast, mask)
KDErast <- KDErast*mask
plot(KDErast)
stack(neClim, KDErast) # no errors!

# convert to points for use as background locations 
KDEpts <- rasterToPoints(KDErast)
bg <- randomPoints(KDErast, 500, prob=T) # alternate method
################################################################################


################################################################################
# CHUNK 2: Run models
################################################################################
# extract data for ant spp
antSpp <- c("camher", "camnov", "forint", "monema", "phepil", "preimp")
a <- 6 # let's work with only preimp in this example
# loop to run models for each species
#for(a in 1:length(antSpp)){
antGeoXY <- antsGeoXY[antsGeoXY$spcode==antSpp[a],-1]
dim(antGeoXY)
plot(neClim[[1]])
points(antGeoXY$x, antGeoXY$y, pch=20)

########## make sets of training/test (t/t) data
ttSplit = 0.25 # ttSplit = test/train split percentage
# function to partition data into t/t
fold <- function(ttSplit){ 
  k = round(1/ttSplit, 0)
  fold <- kfold(antGeoXY, k=k)
}

# make sets of t/t data
sets = 5 # number of t/t sets to make. We used 50 in the paper.
folds <- replicate(sets, fold(ttSplit)) # replicate five different random folds
head(folds)

# now loop through to build lists of t/t data
antTrain <- list()
antTest <- list()
for(h in 1:sets){
  antTrain[[h]] <- antGeoXY[folds[,h]!=1,]
  antTest[[h]] <- antGeoXY[folds[,h]==1,]
}
str(antTrain)

plot(neClim[[1]])
points(antTrain[[1]], pch=20)
points(antTrain[[2]], pch=20, col="red")



# now we will work through four different maxent models:
# (1) linear features only
# (2) Default features (set by number of occurrence records)
# (3) linear features only, bias corrected background
# (4) default features, bias corrected background



# MODEL 1 -----------------------------------------------------------------
#### MAXENT - LINEAR FEATURES
antmaxMods_LF <- list()
# loop through each fold (5 total), fit a model, and place fitted model
# in a list
for(f in 1:sets){
  print(f)
  #### MAXENT - LINEAR FEATURES
  antmaxMods_LF[[f]] <- maxent(neClim, antTrain[[f]], 
                               args=c(c("-h", # turn off hinge features
                                        "-q", # turn off quadratic features
                                        "-p", # turn off product features
                                        "-P", # return response curves
                                        "nothreshold"), # turn off threshold
                                      c("-m", 10000))) # max iterations = 10K
}
# predict to geography
antmaxStack_LF <- predict(antmaxMods_LF[[1]], neClim) #cloglog
antmaxStackRAW_LF <- predict(antmaxMods_LF[[1]], neClim, args='outputformat=raw')

# loop through the rest of the models and predict
for(j in 2:sets){
  print(j)
  mod_LF <- predict(antmaxMods_LF[[j]], neClim)
  modRAW_LF <- predict(antmaxMods_LF[[j]], neClim, args='outputformat=raw')
  antmaxStack_LF <- stack(antmaxStack_LF, mod_LF)
  antmaxStackRAW_LF <- stack(antmaxStackRAW_LF, modRAW_LF)
}

plot(antmaxStack_LF, col=rgb.tables(1000))
plot(mean(antmaxStack_LF), col=rgb.tables(1000)) # mean of the five models
plot(calc(antmaxStack_LF, sd), col=rgb.tables(1000)) #std dev of the five models

# MODEL 2 -----------------------------------------------------------------
#### MAXENT - DEFAULT FEATURES
# Same as above, but now using default features
antmaxMods_allF <- list()
for(f in 1:sets){
  print(f)
  antmaxMods_allF[[f]] <- maxent(neClim, antTrain[[f]], 
                                 args=c("-P",c("-m", 10000)))
}
# Predict to geography and stack
antmaxStack_allF <- predict(antmaxMods_allF[[1]], neClim)
antmaxStackRAW_allF <- predict(antmaxMods_allF[[1]], neClim, args='outputformat=raw')
for(j in 2:sets){
  print(j)
  mod_allF <- predict(antmaxMods_allF[[j]], neClim)
  modRAW_allF <- predict(antmaxMods_allF[[j]], neClim, args='outputformat=raw')
  antmaxStack_allF <- stack(antmaxStack_allF, mod_allF)
  antmaxStackRAW_allF <- stack(antmaxStackRAW_allF, modRAW_allF)
}

# MODEL 3 -----------------------------------------------------------------
#### MAXENT - LINEAR FEATURES + SAMPLING BIAS
# Account for smapling bias, linear features only
# 10,000 locations selected using probabilistic target-group sampling
# from KDE bias surface created above

# example of selecting background points
# to reflect bias in the presence data
# BG points are more likely to be selected where
# presence-only sampling is most dense
bg <- KDEpts[sample(seq(1:nrow(KDEpts)), 
                    size=1000, 
                    replace=T, 
                    prob=KDEpts[,"layer"]),1:2]
plot(neClim[[1]])
points(bg, pch=20)

antmaxMods_LF_bias <- list()
for(f in 1:sets){
  print(f)
  antmaxMods_LF_bias[[f]] <- maxent(neClim, 
                                    antTrain[[f]],
                                    # background points selected from KDEpts
                                    a=KDEpts[sample(seq(1:nrow(KDEpts)), 
                                                    size=10000, 
                                                    replace=T, 
                                                    prob=KDEpts[,"layer"]),1:2], 
                                    args=c(c("-h", "-q", "-p", "-P", "nothreshold"),c("-m", 10000)))
}

# Predict to geography and stack 
antmaxStack_LF_bias <- predict(antmaxMods_LF_bias[[1]], neClim)
antmaxStackRAW_LF_bias <- predict(antmaxMods_LF_bias[[1]], neClim, args='outputformat=raw')

for(j in 2:sets){
  print(j)
  mod_LF_bias <- predict(antmaxMods_LF_bias[[j]], neClim)
  modRAW_LF_bias <- predict(antmaxMods_LF_bias[[j]], neClim, args='outputformat=raw')
  antmaxStack_LF_bias <- stack(antmaxStack_LF_bias, mod_LF_bias)
  antmaxStackRAW_LF_bias <- stack(antmaxStackRAW_LF_bias, modRAW_LF_bias)
}

# MODEL 4 -----------------------------------------------------------------
#### MAXENT - DEFAULT FEATURES - BIAS CORRECTED BACKGROUND
antmaxMods_allF_bias <- list()
for(f in 1:sets){
  print(f)
  antmaxMods_allF_bias[[f]] <- maxent(neClim, 
                                      antTrain[[f]], 
                                      a=KDEpts[sample(seq(1:nrow(KDEpts)), 
                                                      size=10000, 
                                                      replace=T, 
                                                      prob=KDEpts[,"layer"]),1:2], 
                                      args=c("-P",c("-m", 10000)))
}

# Predict to geography and stack
antmaxStack_allF_bias <- predict(antmaxMods_allF_bias[[1]], neClim)
antmaxStackRAW_allF_bias <- predict(antmaxMods_allF_bias[[1]], neClim, 
                                    args='outputformat=raw')
for(j in 2:sets){
  print(j)
  mod_allF_bias <- predict(antmaxMods_allF_bias[[j]], neClim)
  modRAW_allF_bias <- predict(antmaxMods_allF_bias[[j]], neClim, 
                              args='outputformat=raw')
  antmaxStack_allF_bias <- stack(antmaxStack_allF_bias, mod_allF_bias)
  antmaxStackRAW_allF_bias <- stack(antmaxStackRAW_allF_bias, modRAW_allF_bias)
}
save.image(file=paste(antSpp[a], ".RData", sep=""))
#}
################################################################################


# Next, we will calculate mean and standard deviation (sd) rasters for all the 
# models fitted above and save the prediction rasters to disk
################################################################################
# CHUNK 3: Write mean and sd rasters of predictions for evaluation, mapping, etc
################################################################################

#antFile <- "/Users/mfitzpatrick/code/PRStats_SDMs/data/preimp.RData"
#load(antFile)

fileName <- "preimp"

#maxent - LINEAR FEATURES
writeRaster(sum(antmaxStack_LF)/dim(antmaxStack_LF)[3], 
            paste0(fileName, "_maxent_prob_LF.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(antmaxStack_LF, sd), 
            paste0(fileName, "_maxent_sd_LF.tiff",sep=""), 
            type="GTiff", overwrite=T)  

#maxent - ALL FEATURES
writeRaster(sum(antmaxStack_allF)/dim(antmaxStack_allF)[3], 
            paste0(fileName, "_maxent_prob_allF.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(antmaxStack_allF, sd), 
            paste0(fileName, "_maxent_sd_allF.tiff",sep=""), 
            type="GTiff", overwrite=T)

#maxent - LINEAR FEATURES - ***bias corrected***
writeRaster(sum(antmaxStack_LF_bias)/dim(antmaxStack_LF_bias)[3], 
            paste0(fileName,"_maxent_prob_LF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(antmaxStack_LF_bias, sd), 
            paste0(fileName,"_maxent_sd_LF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)  

#maxent - ALL FEATURES - ***bias corrected***
writeRaster(sum(antmaxStack_allF_bias)/dim(antmaxStack_allF_bias)[3], 
            paste0(fileName, "_maxent_prob_allF_biasCorrected.tiff"), 
            type="GTiff", overwrite=T)
writeRaster(calc(antmaxStack_allF_bias, sd), 
            paste0(fileName,"_maxent_sd_allF_biasCorrected.tiff"), 
            type="GTiff", 
            overwrite=T)
################################################################################


################################################################################
# CHUNK 4: Calculate AICc for maxent models
################################################################################
library(PresenceAbsence)
library(raster)
library(dismo)
library(zoo)
library(ENMeval)

#antFile <- "/Users/mfitzpatrick/code/PRStats_SDMs/data/preimp.RData"
#load(antFile)

aicc.LF <- aicc.allF <- aicc.LF_bias <- aicc.allF_bias <- NULL

for(ii in 1:5){
  aicc.LF[ii] <- calc.aicc(nparam = get.params(antmaxMods_LF[[ii]]), 
                           occ=antGeoXY, 
                           antmaxStack_LF[[ii]])$AICc
  
  aicc.allF[ii] <- calc.aicc(nparam = get.params(antmaxMods_allF[[ii]]), 
                             occ=antGeoXY, 
                             antmaxStack_allF[[ii]])$AICc
  
  aicc.LF_bias[ii] <- calc.aicc(nparam = get.params(antmaxMods_LF_bias[[ii]]), 
                                occ=antGeoXY, 
                                antmaxStack_LF_bias[[ii]])$AICc
  
  aicc.allF_bias[ii] <- calc.aicc(nparam = get.params(antmaxMods_allF_bias[[ii]]), 
                                  occ=antGeoXY, 
                                  antmaxStack_allF_bias[[ii]])$AICc
}    

mean(aicc.LF)
mean(aicc.allF)
mean(aicc.LF_bias)
mean(aicc.allF_bias)
################################################################################


################################################################################
# CHUNK 5: Evaluate models
################################################################################
library(PresenceAbsence)
library(raster)
library(dismo)
library(zoo)
library(ecospat)


######### maxent LF ############# 
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(antmaxStack_LF, randomPoints(neClim, 10000))

for(ii in 1:dim(antmaxStack_LF)[3]){    
  probTest <- as.numeric(na.omit(extract(antmaxStack_LF[[ii]], antTest[[ii]])))
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(antmaxStack_LF[[ii]], antTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_LF <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent allF #############  
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(antmaxStack_allF, randomPoints(neClim, 10000))

for(ii in 1:dim(antmaxStack_allF)[3]){    
  
  probTest <- as.numeric(na.omit(extract(antmaxStack_allF[[ii]], antTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(antmaxStack_allF[[ii]], antTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_allF <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent LF bias corrected ############# 
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(antmaxStack_LF_bias, randomPoints(neClim, 10000))

for(ii in 1:dim(antmaxStack_LF_bias)[3]){    
  
  probTest <- as.numeric(na.omit(extract(antmaxStack_LF_bias[[ii]], antTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(antmaxStack_LF_bias[[ii]], antTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_LF_bias <- rbind(Boyce, AUCmod, meanProb, meanBG)

######### maxent allF bias corrected #############  
Boyce <- AUCmod <- meanProb <- meanBG <- NULL

# predicted probability at random background points
probBG <- extract(antmaxStack_allF_bias, randomPoints(neClim, 10000))

for(ii in 1:dim(antmaxStack_allF_bias)[3]){    
  
  probTest <- as.numeric(na.omit(extract(antmaxStack_allF_bias[[ii]], antTest[[ii]])))
  
  # predicted probability at test points
  Boyce[[ii]] <- ecospat.boyce(antmaxStack_allF_bias[[ii]], antTest[[ii]],
                               PEplot=FALSE)$Spearman.cor
  evalDismo <- evaluate(p=probTest, a=probBG[,ii])
  AUCmod[[ii]] <- evalDismo@auc
  meanProb[[ii]] <- mean(probTest)
  meanBG[[ii]] <- mean(probBG[,ii])
}

maxentEval_allF_bias <- rbind(Boyce, AUCmod, meanProb, meanBG)

rowMeans(maxentEval_LF)
rowMeans(maxentEval_allF)
rowMeans(maxentEval_LF_bias)
rowMeans(maxentEval_allF_bias)
################################################################################


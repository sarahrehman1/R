
# 1. load and prep libraries ----------------------------------------------

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
library(XML)
library(sdmpredictors)
library(ggplot2)
install.packages(sdm)
library(sdm)

getwd()
"C:/Users/mbzhh/Documents/GitHub/R"
#setwd("C:/Users/nisar/Desktop/R_wd/R")

# 2. load occurrence of species -------------------------------------------

#load species coordinates - from sarah
coord <- shapefile("C:/Users/mbzhh/Documents/GitHub/R/occ_data/newcoord")
plot(coord)

#below takes a while, not necessary unless testing
#load UK shapefile
#britain <- shapefile("C:/Users/mbzhh/Documents/GitHub/R/map_files/infuse_ctry_2011")
#plot(britain)
#plot species data points on britain map
#plot(coord, add=TRUE) 
#good sanity check

# 3. download predictor data ----------------------------------------------
#Downloading raster climate data from internet

#bioclim set
bioclimVars <- getData(name="worldclim", #other options available 
                       res = 2.5, # resolution
                       var = "bio") # which variable(s)?
#could also go to 0.5 res
#put into data_predictos folder, called wc2-5

#if you already have it, use stack all the files in the folder
rastlist <- list.files(path = "C:/Users/mbzhh/Documents/GitHub/R/data_predictors/wc2-5/", all.files=TRUE, pattern='.bil', full.names=TRUE)
rastlist
bioclimVars <- stack(rastlist)
class(bioclimVars) # raster stack - same projection, spatial extent,
# and resolution
bioclimVars #name of entire raster stack
plot(bioclimVars)

# Download elevation data:
dev.off()
elevation <- getData('alt', country='GBR')

#others = human influence index
HII = raster(paste(getwd(), "/data_predictors/hii_europe_geo_grid/hii_europe/w001001.adf", sep=""))
#others = carbon emissions
carbon = raster(paste(getwd(), "/data_predictors/carbon/totareaco2181.shp", sep=""))
#others = nitrogen, copper, moraines, do not work!
copper = raster(paste(getwd(), "/data_predictors/copper/wd001001.adf", sep=""))
#moraines = raster(paste(getwd(), "/data_predictors/Moraines__BRITICE_glacial_landforms_-shp/Moraines__BRITICE_glacial_landforms_.shp", sep=""))

# 3A: more detailed manipulation - good for learning ----------------------
#extra - could also load each single raster layer - below is a different way of achieving above
filePath <- paste(getwd(), "/wc2-5/bio19.bil", sep="")
filePath
bio2 <- raster(paste(getwd(), "/wc2-5/bio2.bil", sep=""))
bio3 <- raster(paste(getwd(), "/wc2-5/bio3.bil", sep=""))
bio4 <- raster(paste(getwd(), "/wc2-5/bio4.bil", sep=""))
bio10 <- raster(paste(getwd(), "/wc2-5/bio10.bil", sep=""))
bio11 <- raster(paste(getwd(), "/wc2-5/bio11.bil", sep=""))
bio18 <- raster(paste(getwd(), "/wc2-5/bio18.bil", sep=""))
bio19 <- raster(paste(getwd(), "/wc2-5/bio19.bil", sep=""))#Loading each single raster layer

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

bioclimVars <- stack(list.ras)
bioclimVars1 <- bioclimVars[[c(1, 2, 3, 4, 8, 9, 10, 11, 12, 18)]] 
plot(bioclimVars1)
bioclimVars1


# 4. Cropping and stacking environmental variables ----------------------
#implement rough extent to make files smaller

#extents used
coordExt <- c(-12, 5, 49, 61) # UK extent
ext_uk <- c(-86644,	685295, -16137, 1247364) #picked up from the elev_27700 file 

#first project to UK map system
bc_uk <- crop(bioclimVars, coordExt)
bc_uk <- projectRaster(bc_uk, crs=27700)
bc_uk <- crop(bc_uk, ext_uk)
elevation_uk <- crop(elevation, coordExt)
elevation_uk <- projectRaster(elevation_uk, crs=27700)
HII_uk <- crop(HII, coordExt)
HII_uk <- projectRaster(HII_uk, crs=27700) #setting crs
HII_uk <- crop(HII27700, ext_uk)
carbon_uk <- projectRaster(carbon, crs=27700)
carbon_uk <- crop(carbon_uk, ext_uk)
plot(carbon_uk)
plot(bc_uk)
plot(HII_uk)
plot(elevation_uk)
plot(coord, add=T)

# Some more data on the elevation layers...
slope <- terrain(elev_uk, opt="slope", unit='degrees')
slope_uk <- projectRaster(slope, crs=27700) #setting crs
slope_uk <- crop(slope_uk, ext_uk)
aspect <- terrain(elev_uk, opt="aspect", unit="degrees")
aspect_uk <- projectRaster(aspect, crs=27700) #setting crs
aspect_uk <- crop(aspect_uk, ext_uk)
hill <- hillShade(slope, aspect, 20, 280)
hill_uk <- projectRaster(hill, crs = 27700)
hill_uk <- crop(hill_uk, ext_uk)

plot(hill, col=grey(0:100/100), legend=FALSE, main='UK')
plot(elev_uk, col=rainbow(25, alpha=0.35), add=TRUE)

#resample to same resolution as the bioclim data
HII_uk <- resample(HII_uk, bc_uk, method="bilinear")
carbon_uk <- resample(carbon_uk, bc_uk, method="bilinear")
elev_uk <- resample(elevation_uk, bc_uk, method="bilinear")
hill_uk <- resample(hill_uk, bc_uk, method="bilinear")
slope_uk <- resample(slope_uk, bc_uk, method="bilinear")
aspect_uk <- resample(aspect_uk, bc_uk, method="bilinear")

#stacking the rasters
stack1 <- stack(bc_uk, elev_uk, HII_uk, carbon_uk, hill_uk, aspect_uk, slope_uk)
plot(stack1)
nlayers(stack1)


# 5. Extract predictor data for points, training and background sets ----------------------------------

#bonus - extract data from bio2crop_sw for coord points
pts_data <- raster::extract(stack1,coord,df=T)
pts_data

#stack_cropped with specic variables we want, not entire list
all_pts_data <- raster::extract(stack1[[2:19]],coord,df=T)
all_pts_data #but not essential

#Creating random background samples
# random set of points for this example
set.seed(1)
# create 500 random background points 

#what are the predictors we are working with now? WE need a rasterstack of them
#this looks appropriate - RasterBrick and RasterStack are similar classes so either is fine
names(stack1) # generated above, projecting al bioclim to 27700
plot(stack1)
str(stack1)

#then generate the random points
backgrstack <- randomPoints(stack1, 500)
colnames(backgrstack)
# and then extract env data at the background points
absvals <- extract(stack1, backgrstack)

##what about the presence data?
#this is the bioclimvariable data extracted for the presence sites
presvals <- extract(stack1, coord) #coord is the file imported by Sarah that has the species records, can use directly as all already projected to 27700
head(presvals)
str(presvals)

# make a vector of 1's and 0's to match the
# presence records and the background data
# See ?rep
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))

# now we bind everything together into a data.frame
sdm_data <- data.frame(cbind(pb, rbind(presvals, absvals)))

# if you need to transform any into factors, use below
# biome is a factor, so define it that way
# sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
# or you could drop it completely using the below
# # make new stack without the categorical variable 'biome'
# as not all SDMs can use categorical data
# pred_nf <- dropLayer(predictors, 'biome')

# and have a look at the data
head(sdm_data)
tail(sdm_data)
summary(sdm_data)
#plot(sdmdata) takes a while

#Create training / test dataset
set.seed(1)

group_coord <- kfold(coord, k=5) # 5 groups = 80/20 split for training vs testing 
(pres_train <- coord[group_coord != 1, ])
(pres_test <- coord[group_coord == 1, ])
#for plotting below, these should only have 
# let's set an extent to crop the env data to make predictions faster
#ext <- extent(-90, -32, -33, 23)

#And for the random points
#
#coordgroup_backgr <- kfold(backgr, 5)
backg_train <- backgrstack[group_coord != 1, ]
backg_test <- backgrstack[group_coord == 1, ]

#Background data for train/test set 
set.seed(1)
#backgr <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25) not needed, we cropped our stack before
#pred_nf is the raster stack of predictors with categorical factors dropped off. not a problem for the bioclim data
#we already did this generation of random points, using backgr

colnames(backgrstack) = c('lon', 'lat') # not sure why, for convenience?

r1 <- raster(stack1, 1)
plot(!is.na(r1), col=c('white', 'light grey'), legend=FALSE)
#plot(ext, add=TRUE, col='black', lwd=2) we already set out extent
points(backg_train, pch=20, cex=0.5, col='yellow')
points(backg_test, pch=20, cex=0.5, col='black')
points(pres_train, pch= '+', col='red') 
points(pres_test, pch='+', col='blue')



# 6. Mahalanobis Distance -------------------------------------------------

mm <- mahal(stack1, # raster stack
            pres_train) #presence-only data

# evaluate the model using test data (presences + background)
?evaluate
e <- evaluate(pres_test, # presences
              backg_test, # background / absences
              mm, # model
              stack1) # raster stack
e

# predict the distribution
pm <- predict(stack1, # raster stack
              mm, # model
              ext=ext_uk) # ext
plot(pm) # predictions are 1-distance

# let's convert to a p-value
# Mahal distances (D^2) are Chi-square distributed
probMap <- (1-pm)
dists <- as.numeric(na.omit(getValues(probMap)))
p.value <- 1-as.numeric(pchisq(dists, df=nlayers(stack1)))
probMap[!is.na(probMap[])] <- p.value

par(mfrow=c(1,2))
plot(probMap, main='Mahalanobis distance (p-value)')
plot(!is.na(r1), add=TRUE, border='dark grey')
tr <- threshold(e, # model eval object
                'spec_sens')
plot(probMap > tr, main='presence/absence')
plot(!is.na(r1), add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(pres_test, pch='x', col="red")

# let's check model quality using the Boyce Index
ecospat.boyce(1-pm, # prediction
              pres_train) # not good...

# what we might expect for a good model
cheat <- xyFromCell(probMap, which(probMap[]>0.5))
ecospat.boyce(probMap, cheat)

# 6A. select non correlated variables -------------------------------------


# 7. fit maxent -----------------------------------------------------------

?maxent #needs predictor variables in rasterstack. needs occurrence
# data, either matrix or spatialpoints object (2 colums x y)
# can give background points, or it will automatically
# tell it if any of the data is categorical using factors argument

maxent()
library(rJava)

filePath <- "C:/Users/nisar/Desktop/R_wd/R/MaxentOut"
getwd()
mx <- maxent(stack1, # env data as a raster stack
             pres_train, # presence data
             path=filePath) # where to save all the output
plot(mx) # shows you each variable's contribution
mx #website summarising all of the output
 
# evaluate the model using the test data
e <- evaluate(pres_test, backg_test, mx, stack1) 
# evaluate model using test presences and background data that 
# was previously prepared.
e # when evaluating model on data the model hasn't seen, it always 
# performs a little worse.

# predict back to geography, make mapped prediction giving it the
# model and the predictors.
mxPred <- predict(mx, stack1)
plot(mxPred, col=rgb.tables(1000)) # give it the no. of colours u want
# Predict in 'raw' format and save raster of prediction
# change some values using args function
mxPred <- predict(mx, stack1, args=c("outputformat=raw"),
                  filename=paste0(filePath, '/maxent_predictionRAW5.tif', overwrite=T))
plot(mxPred)

# let's check model quality using the Boyce Index
predRast <- raster(paste0(filePath, '/maxent_predictionRAW5.tif'))
ecospat.boyce(predRast, pres_test)

# there are lots of options to change (see the provided MaxEntHELP.pdf)
# we use the 'args' argument to set the options we want by providing the 
# appropriate "flag". Doing so changes the the default value of the flag. 
# For example, here I am asking for 'responsecurves' to be generated. 
# The default is to not report response curves (responsecurves=FALSE). 
# By flagging 'responsecurves', we change it from the default (TRUE).
# Let's run the model:
mx <- maxent(stack1, 
             pres_train, 
             path=filePath,
             args=c("responsecurves"))
mx

# perform multiple replicates (5 in this example - will take
# a few minutes to run)
mx <- maxent(stack1, 
             pres_train, 
             path=filePath,
             args=c("responsecurves", "replicates=5"))
mx # shows uncertainty envelopes around different measures, AUC and
# around response curves. 

# Let's now measure variable importance using jackknife and also produce 
# response curves. Running this model will take a minute or two.
mx <- maxent(stack1, 
             pres_train, 
             path=filePath,
             args=c("jackknife", "responsecurves"))
mx
# jackknife fits model with all variables, without one of the variables,#
# and then again with just that variable.

# To fit a model with only linear and product features, we have to turn off all 
# other feature types
mx <- maxent(stack1, 
             pres_train, 
             path=filePath,
             args=c("jackknife", "responsecurves",
                    "-h", # turn off hinge features
                    "-q", # turn off quadratic features
                    "nothreshold")) # turn off threshold))
#simplifies model, making more linear, simple graphs if you remove
# threshold, hinge, and quadratic features. 
mx

# Change the 'beta mulitplier' which controls model complexity (smaller
# values = greater complexity). Should run a range of values. 
mx <- maxent(stack1, 
             pres_train, 
             path=filePath,
             args=c("betamultiplier=0.25","responsecurves"))

mx 

# 8. test diff maxent models ----------------------------------------------


#see script fitzpatrick_test_maxent_models.R
# 9. future predictions ---------------------------------------------------

# let's try projecting to future climate. For simplicity, we will create
# some fake future climate layers
future <- stack1
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
mask <- stack1[[1]]>-1000
plot(mask)

# Sampling bias ----------------------------------------------------------------
# To deal with spatial sampling bias, we will create
# a raster that reflects the sampling density of ants
# in the study region using kernel density estimation (KDE)

# use the x-y coords to get the cell numbers
bias <- cellFromXY(mask, coord) # cells with records
cells <- unique(sort(bias))
cells
# xy-locations of all samples
kernelXY <- xyFromCell(mask, cells)
points(kernelXY, pch=".")
samps <- as.numeric(table(bias)) 
# number of samples in each grid cell
head(samps)
samps[1:10]
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
                     ngrid=96, # number of grid cells along each dimension,
                     # have a look at the clim data = 812 columns
                     ylim=c(-16137, 1247364), # approximate latitude range 
                     xlim=c(-86644,	685295), # approximate longitude range
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
#my edit for convenience

antGeoXY <- coord
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
#convenience renaming
neClim <-stack1
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

#from model1
plot(antmaxStack_LF, col=rgb.tables(1000))
plot(mean(antmaxStack_LF), col=rgb.tables(1000)) # mean of the five models
plot(calc(antmaxStack_LF, sd), col=rgb.tables(1000)) #std dev of the five models
#from model3 (here)
plot(antmaxStack_LF_bias, col=rgb.tables(1000))
plot(mean(antmaxStack_LF_bias), col=rgb.tables(1000)) # mean of the five models
plot(calc(antmaxStack_LF_bias, sd), col=rgb.tables(1000)) #std dev of the five models


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
#
##from model2
plot(antmaxStack_allF, col=rgb.tables(1000))
plot(mean(antmaxStack_allF), col=rgb.tables(1000)) # mean of the five models
plot(calc(antmaxStack_allF, sd), col=rgb.tables(1000)) #std dev of the five models
#from model4 (here)
plot(antmaxStack_allF_bias, col=rgb.tables(1000))
plot(mean(antmaxStack_allF_bias), col=rgb.tables(1000)) # mean of the five models
plot(calc(antmaxStack_allF_bias, sd), col=rgb.tables(1000)) #std dev of the five models
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

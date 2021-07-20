# 8. test diff maxent models ----------------------------------------------


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

# 9. future predictions ---------------------------------------------------

# let's try projecting to future climate. For simplicity, we will create
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



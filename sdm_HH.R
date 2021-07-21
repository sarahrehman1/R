
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
#install.packages(sdm)
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
getwd()
rastlist <- list.files(path = paste(getwd(),"/data_predictors/wc2-5/", sep = ""), all.files=TRUE, pattern='.bil', full.names=TRUE)
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

#whats the problem with moraines? the format sarah imported them in most likely...
copper <- raster(choose.files()) #so this does not work for any of the digimaps spatialpolygonsdataframes...
#lets at least bring in the shapefile
shp_copper <- shapefile(choose.files()) #click on the file you want in the pop up window
#this is for convenience here, for reproducibility its much better to name the file in the code,
#but when debugging, trying different files is quicker this way

#inspect the file
str(shp_copper)
names(shp_copper)
dim(shp_copper)

#plot the maps to see what is going on....
library(ggplot2)
shapedf <- fortify(shp_copper) #takes a while...
head(shapedf)
ggplot(data=shapedf, aes(x=long, y=lat, group=group))+
  geom_path()+
  coord_quickmap()
#it seems fine - why does raster not import it?
#no crs assigned - do so
proj4string(shp_copper) <-CRS("+init=epsg:27700")
proj4string(shp_copper)
plot(shp_copper) #takes a while...
#this seems to look about right...

#how to make spatialpolygonsdataframe into a raster?
#using rasterize from "raster"

#needs a dummy raster to sample to
#use the elevation raster?
dummy <- elev_crop_uk
str(elev_crop_uk)

#replace NAs with 0 in the dummy data, otherwise next steps wont work
summary(as.data.frame(dummy, na.omit=T)) 
dummy[is.na(dummy)] <- 0
#https://pialentini.com/2018/01/25/getting-spatial-data-into-shape-for-species-distribution-modelling-sdms/#loading-projecting-and-clipping-rasters-creating-and-buffering-polygons-1
#http://www.sdmtoolbox.org/technical-info
#found the NA solution in the first link, but this has some cool code for later work too, eg measure distance to roads

#try to make the raster...
cu <- rasterize(shp_copper, dummy) #this runs and takes long time, about 10 mins
plot(cu)
str(cu)
#over to Sarah for checking stacking 
#other sources
#https://andrewmaclachlan.github.io/CASA0005repo/rasters-descriptive-statistics-and-interpolation.html
#https://www.rpubs.com/AndrewMacLachlan/504624

shp_moraines <- shapefile(choose.files())
str(shp_moraines)
plot(shp_moraines)
proj4string(shp_moraines) <-CRS("+init=epsg:27700")
moraines <- spTransform(shp_moraines, CRSobj = "+init=epsg:27700")
proj4string(moraines)
plot(moraines)

mor <- rasterize(shp_moraines,dummy)
mora<- projectRaster(mor,crs=27700)
morai<- crop(mora, ext_uk)
plot(mor)#???

shapedf <- fortify(shp_moraines)
head(shapedf)
ggplot(data=shapedf, aes(x=long, y=lat, group=group))+
  geom_path()+
  coord_quickmap()

################################anyway
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


#note on the error that is thrown when converting the crs
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
############################ anyway...

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
slope <- terrain(elevation_uk, opt="slope", unit='degrees')
slope_uk <- projectRaster(slope, crs=27700) #setting crs
slope_uk <- crop(slope_uk, ext_uk)
aspect <- terrain(elevation_uk, opt="aspect", unit="degrees")
aspect_uk <- projectRaster(aspect, crs=27700) #setting crs
aspect_uk <- crop(aspect_uk, ext_uk)
hill <- hillShade(slope, aspect, 20, 280)
hill_uk <- projectRaster(hill, crs = 27700)
hill_uk <- crop(hill_uk, ext_uk)

plot(hill, col=grey(0:100/100), legend=FALSE, main='UK')
plot(elevation_uk, col=rainbow(25, alpha=0.35), add=TRUE)

#resample to same resolution as the bioclim data
HII_uk <- resample(HII_uk, bc_uk, method="bilinear")
carbon_uk <- resample(carbon_uk, bc_uk, method="bilinear")
elev_uk <- resample(elevation_uk, bc_uk, method="bilinear")
hill_uk <- resample(hill_uk, bc_uk, method="bilinear")
slope_uk <- resample(slope_uk, bc_uk, method="bilinear")
aspect_uk <- resample(aspect_uk, bc_uk, method="bilinear")
mora_uk <- resample(mora, bc_uk, method="bilinear")
#stacking the rasters
stack1 <- stack(bc_uk, elev_uk, HII_uk, carbon_uk, hill_uk, aspect_uk, slope_uk)
plot(stack1)
nlayers(stack1)
#including mora_uk causes a fail here...
# 5. remove highly correlated variables in the data -----------------------

#found a helpful package!
#install.packages("virtualspecies")
library(virtualspecies)
keep <- removeCollinearity(stack1, plot = TRUE, select.variables = TRUE)
keep
#default cut off is pearson correlation coefficient 0.7
#at 0.5...
removeCollinearity(stack1, plot = TRUE, select.variables = TRUE, multicollinearity.cutoff = 0.5)
#stick with 0.7 and choose most meaningful out of these? 
#to implement the selection
stack1_trimmed <- subset(stack1, keep, drop=TRUE) 
#rename for convenience, wont have to change stack name in code
stack1 <-stack1_trimmed

# 6. Extract predictor data for points, training and background sets ----------------------------------

#check for correlation visually, easy to see which ones are problematic
pairs(sdm_data[,2:5], cex=0.1)
pairs(sdm_data[,6:9], cex=0.1)
pairs(sdm_data[,10:13], cex=0.1)
pairs(sdm_data[,14:17], cex=0.1)
pairs(sdm_data[,18:21], cex=0.1)
pairs(sdm_data[,21:23], cex=0.1)

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
group_coord
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
backgrstack
r1 <- raster(stack1, 1)
plot(!is.na(r1), col=c('white', 'light grey'), legend=FALSE)
#plot(ext, add=TRUE, col='black', lwd=2) we already set out extent
points(backg_train, pch=20, cex=0.5, col='yellow')
points(backg_test, pch=20, cex=0.5, col='black')
points(pres_train, pch= '+', col='red') 
points(pres_test, pch='+', col='blue')



# 7. Mahalanobis Distance -------------------------------------------------

mm <- mahal(stack1, # raster stack
            pres_train) #presence-only data

# evaluate the model using test data (presences + background)
#?evaluate
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
library(ecospat)
ecospat.boyce(1-pm, # prediction
              pres_train) # not good...

# what we might expect for a good model
cheat <- xyFromCell(probMap, which(probMap[]>0.5))
ecospat.boyce(probMap, cheat)


# 7B. create bias layer ---------------------------------------------------

# 8. fit maxent -----------------------------------------------------------

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

# 8A. test diff maxent models ----------------------------------------------


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




#from albert
Similar as you run the maxent.bat you can rung project.bat to project to your new environment  (future rasters). 
scenario <- your_scenario
path<- the_path_to_your_new_environment
outputdir <- "C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/mx_out/outputdir"
maxprojargs<-paste(paste0('"', , '\\', Species, '_', nr, '.lambdas"'), 
                   paste0('"', path, '"'), 
                   paste0('"', outputdir, '\\', Species, '_', nr, scenario,'"'), collapse="")
system2("E:/Ros/maxent/project.bat",maxprojargs) # run the model  


You then have to average the models and cut it based on the cut offs in the maxentResults.csv table... for example:
px<-stack1
tr<-temptable<-(read.csv(paste0("C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/mx_out/outputdir/","\\maxentResults.csv")))
tr1<-tr$X10.percentile.training.presence.Cloglog.threshold
tr2<-tr$Equate.entropy.of.thresholded.and.original.distributions.Cloglog.threshold 

temp1<-(mean(px)>mean(tr1)*maskpres) 
temp2<-(mean(px)>mean(tr2)*maskpres) 
temp<-temp1+temp2 

  
  for (i in (0:19)) {
    px<-stack(px,raster(paste0(outputdir,'\\',Species,'_',i,scene1)))
  }

#from sdm course
# let's try projecting to future climate. For simplicity, we will create
# some fake future climate layers
future <- sdm_data

future <- stack1
str(stack1)
names(stack1)

future$bio8 <- future$bio8+40 # increase max temp by 4C (recall temp is 10x)
future$bio9 <- future$bio9+80 # increase min temp by 8C (recall temp is 10x)
#future$bio7 <- future$bio5-future$bio6 # recalculate temp annual range (bio7)
future$slope <- future$slope*0.33 # decrease precipitaiton by 33%
names(future)
# save each layer to disk
path <- paste(getwd(),"/projection_files/",sep="")
projNames <- paste0(path,
                    names(future))
projNames
warnings()
for(i in 1:length(projNames)){
  writeRaster(future[[i]], projNames[i], overwrite=T)
}

# run the model and project to the new layers by providing the directory
# where the new layers are saved. Much more output will be written to disk
# automatically. We will step through some of this output below.
#make the paths explicit

output <-paste(getwd(),"/mx_out/",sep="")
future_clim<- paste(getwd(),"/projection_layers/",sep="")
output #copy paste this in 
future_clim #copy paste this in 

mx <- maxent(x=stack1, 
             p=pres_train, 
             #factors='biome',
             path="C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/mx_out",
             args=("projectionlayers=C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/projection_files"))
mx
# Where the model extrapolating beyond the data?
clamping <- raster("C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/mx_out/species_projection_files_clamping.asc")
# areas in red are problematic
library(colorRamps)
plot(clamping, col=rgb.tables(1000))

# multivariate environmental similarity surface (MESS)
# a measure of how novel the new climate is relative to the conditions under
# which the model was fit.
#mess <- raster("/Users/mfitzpatrick/code/PRStats_SDMs/maxentOut/species_projectionLayers_novel.asc")
mess <- raster("C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/mx_out//species_projection_files_novel.asc")

# areas in red have one or more environmental variables outside the range 
# present in the training data
plot(mess, col=rgb.tables(1000)[1000:1])

# which variable is limiting
novelLimit <- raster("C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/mx_out//species_projection_files_novel_limiting.asc")
plot(novelLimit)

#https://damariszurell.github.io/SDM-Intro/#2_SDM_step_by_step
#ok this now runs with mock data -what about real data?
?getData
CMIP5 <- getData("CMIP5", var="bio", res="2.5", rcp="85", model="AC", year="70")
plot(CMIP5)

#prep the data
future_uk <- crop(CMIP5, coordExt)
future_uk <- projectRaster(future_uk, crs=27700)
future_uk <- crop(future_uk, ext_uk)
plot(future_uk) #ok

names(stack1)
names(future_uk)
names(future_uk) <- c('bio1','bio2','bio3', 'bio4', 'bio5','bio6','bio7','bio8','bio9','bio10','bio11','bio12','bio13','bio14','bio15','bio16','bio17','bio18','bio19')
future_uk <- future_uk[[c(2, 3, 7, 8, 9, 10, 19)]] 
names(future_uk)

# save each layer to disk
path <- paste(getwd(),"/projection_files/",sep="")
projNames <- paste0(path,
                    names(future_uk))
projNames
warnings()
for(i in 1:length(projNames)){
  writeRaster(future[[i]], projNames[i], overwrite=T)
}


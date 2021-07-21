#other way to limit the data to a certain area
# this creates a 4-decimal-degree buffer around the occurrence data
occ_buff <- buffer(coord, 80000)

# plot the first element ([[1]]) in the raster stack
plot(stack1[[2]])

plot(coord, add = T, col = "red")  # adds occurrence data to the plot
plot(occ_buff, add = T, col = "blue")  # adds buffer polygon to the plot

#With a defined study area and the environmental layers stacked, 
#we then clip the layers to the extent of our study area. 
#However, for ease of processing, we do this in two steps rather than one. 
#First, we create a coarse rectangular shaped study area around the study area 
#to reduce the size of environmental data 
#and then extract by mask using the buffer 
#we create to more accurately clip environmental layers. 
#This approach could be faster than directly masking. 
#We save the cropped environmental layers as .asc (ascii files) as inputs for Maxent.

# crop study area to a manageable extent (rectangle shaped)
studyArea <- crop(stack1,extent(occ_buff))
#studyArea <- extend(stack1, res=30)
plot(studyArea)
# the 'study area' created by extracting the buffer area from the raster stack
studyArea <- mask(studyArea,occ_buff)
# output will still be a raster stack, just of the study area
# save the new study area rasters as ascii
writeRaster(studyArea,
            # a series of names for output files
            filename=paste0("C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/data/studyarea/",names(studyArea),".asc"), 
            format="GTiff", ## the output format
            bylayer=TRUE, ## this will save a series of layers
            overwrite=T)
writeFormats()
#the error here seems to be to do with the pixel shape = not square... changed the output format from ascii to GeoTiff to allow rectanguler pixels

# select background points from this buffered area; when the number provided 
# to set.seed() function, the same random sample will be selected in the next line			
# use this code before the sampleRandom function every time, if you want to get
# the same "random samples"
set.seed(1) 
bg <- sampleRandom(x=studyArea,
                   size=1000,
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 

plot(studyArea[[1]])
# add the background points to the plotted raster
plot(bg,add=T) 
# add the occurrence data to the plotted raster
occ_final <- coord
plot(occ_final,add=T,col="red")

# get the same random sample for training and testing
set.seed(1)

# randomly select 50% for training
selected <- sample(1:nrow(occ_final), nrow(occ_final) * 0.5)

occ_train <- occ_final[selected, ]  # this is the selection to be used for model training
occ_test <- occ_final[-selected, ]  # this is the opposite of the selection which will be used for model testing

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
clim <- stack1
p <- extract(clim, occ_train)
# env conditions for testing occ
p_test <- extract(clim, occ_test)
# extracting env conditions for background
a <- extract(clim, bg)
#https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md

# repeat the number 1 as many numbers as the number of rows
# in p, and repeat 0 as the rows of background points
pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))

# (rep(1,nrow(p)) creating the number of rows as the p data
# set to have the number '1' as the indicator for presence;
# rep(0,nrow(a)) creating the number of rows as the a data
# set to have the number '0' as the indicator for absence;
# the c combines these ones and zeros into a new vector that
# can be added to the Maxent table data frame with the
# environmental attributes of the presence and absence
# locations
pder <- as.data.frame(rbind(p, a))

# train Maxent with spatial data
# mod <- maxent(x=clim,p=occ_train)

# train Maxent with tabular data
mod <- maxent(x=pder, ## env conditions
              p=pa,   ## 1:presence or 0:absence
              
              path=paste0("../output/maxent_outputs"), ## folder for maxent output; 
              # if we do not specify a folder R will put the results in a temp file, 
              # and it gets messy to read those. . .
              args=c("responsecurves") ## parameter specification
)
# the maxent functions runs a model in the default settings. To change these parameters,
# you have to tell it what you want...i.e. response curves or the type of features

# view the maxent model in a html brower
mod
mod@results

# example 1, project to study area [raster]
ped1 <- predict(mod, studyArea)  # studyArea is the clipped rasters 
plot(ped1)  # plot the continuous prediction

# example 2, project to the world 
ped2 <- predict(mod,clim)
plot(ped2)

# example 3, project with training occurrences [dataframes]
ped3 <- predict(mod, p)
head(ped3)
hist(ped3)

# using 'training data' to evaluate p & a are dataframe/s
# (the p and a are the training presence and background
# points)
mod_eval_train <- dismo::evaluate(p = p, a = a, model = mod)
print(mod_eval_train)

mod_eval_test <- dismo::evaluate(p = p_test, a = a, model = mod)
print(mod_eval_test)  # training AUC may be higher than testing AUC

#To threshold our continuous predictions of suitability into binary predictions we use the threshold function of the "dismo" package. To plot the binary prediction, we plot the predictions that are larger than the threshold.
# calculate thresholds of models
thd1 <- threshold(mod_eval_train, "no_omission")  # 0% omission rate 
thd2 <- threshold(mod_eval_train, "spec_sens")  # highest TSS

# plotting points that are above the previously calculated
# thresholded value
plot(ped1 >= thd1)

# load the function that prepares parameters for maxent
source("prepPara.R")

mod1_autofeature <- maxent(x=pder, 
                           ## env conditions
                           p=pa,
                           ## 1:presence or 0:absence
                           path="../output",
                           ## this is the folder you will find manxent output
                           args=prepPara(userfeatures=NULL) ) 
## default is autofeature
mod1_autofeature
# or select Linear& Quadratic features
mod1_lq <- maxent(x=pder,
                  p=pa,
                  path=paste0("../output/maxent_outputs1_lq"),
                  args=prepPara(userfeatures="LQ") ) 
## default is autofeature, here LQ represents Linear& Quadratic
## (L-linear, Q-Quadratic, H-Hinge, P-Product, T-Threshold)

#change betamultiplier for all features
mod2 <- maxent(x=pder, 
               p=pa, 
               path=paste0("../output/maxent_outputs2_0.5"), 
               args=prepPara(userfeatures="LQ",
                             betamultiplier=0.5) ) 

mod2 <- maxent(x=pder, 
               p=pa, 
               path=paste0("../output/maxent_outputs2_complex"), 
               args=prepPara(userfeatures="LQPH",
                             ## include L, Q, H features
                             beta_lqp=1.5, 
                             ## use different betamultiplier for different features
                             beta_hinge=0.5 ) ) 


# note: (1) the projection layers must exist in the hard disk (as relative to computer RAM); 
# (2) the names of the layers (excluding the name extension) must match the names 
# of the predictor variables; 
mod3 <- maxent(x=pder, 
               p=pa, 
               path=paste0("C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/output"), 
               args=prepPara(userfeatures="LQ",
                             betamultiplier=1,
                             projectionlayers="C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/data/studyarea") ) 

# load the projected map
ped <- raster(paste0("C:/Users/hanna/switchdrive/04_RESEARCH_PROJECTS/02_RESEARCH_PROJECTS/R/output/species_studyarea.asc"))
plot(ped)
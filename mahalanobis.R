
## ---- Mahalanobis Distance ---------------------------------------------------
?mahal
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
library(ecospat)
# let's check model quality using the Boyce Index
ecospat.boyce(1-pm, # prediction
              pres_train) # not good...

# what we might expect for a good model
cheat <- xyFromCell(probMap, which(probMap[]>0.5))
ecospat.boyce(probMap, cheat)


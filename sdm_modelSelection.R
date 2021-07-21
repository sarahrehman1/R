#code to mask a study area from a wider area, and test maxent model parameters in 
#cropped and uncropped data

# this creates a 4-decimal-degree buffer around the occurrence data
occ_buff <- buffer(coord, 60000)
# plot the first element ([[1]]) in the raster stack
plot(stack1[[2]])
plot(coord, add = T, col = "red")  # adds occurrence data to the plot
plot(occ_buff, add = T, col = "blue")  # adds buffer polygon to the plot
predictors_crop <- mask(stack1,occ_buff) #mask the stack to the polygon

# Prepare presence and background locations
#(p_coords <- virtualSp$presence)
#(bg_coords <- virtualSp$background)
(p_coords <- as.data.frame(coord))
(p_coords <- p_coords[,6:7])
(bg_coords_uk <- randomPoints(predictors,500))
(bg_coords_buff <- randomPoints(predictors_crop,500))

# Create SWD object
data_uk <- prepareSWD(species = "Fredericella", p = p_coords, a = bg_coords_uk,
                      env = predictors)#, categorical = "biome")
data_buff <- prepareSWD(species = "Fredericella", p = p_coords, a = bg_coords_buff,
                        env = predictors_crop)#, categorical = "biome")

#first non cropped
# Split presence locations in training (80%) and testing (20%) datasets
datasets <- trainValTest(data_uk, test = 0.2, only_presence = TRUE, seed = 25)
train <- datasets[[1]]
test <- datasets[[2]]
# Train a Maxnet model
model <- train(method = "Maxnet", data = train, addsamplestobackground=T)
# Define the hyperparameters to test
h <- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
# Test all the possible combinations with gridSearch
gs <- gridSearch(model, hypers = h, metric = "auc", test = test)
head(gs@results[order(-gs@results$test_AUC), ])  # Best combinations

# Use the genetic algorithm instead with optimizeModel
om <- optimizeModel(model, hypers = h, metric = "auc", test = test, seed = 4)

head(om@results)  # Best combinations

#first the cropped to buffer around sampling areas

# Split presence locations in training (80%) and testing (20%) datasets

datasets_buff <- trainValTest(data_buff, test = 0.2, only_presence = TRUE, seed = 25)
train_buff <- datasets_buff[[1]]
test_buff <- datasets_buff[[2]]
# Train a Maxnet model
model_buff <- train(method = "Maxnet", data = train_buff, addsamplestobackground=T)
# Define the hyperparameters to test
h <- list(reg = seq(0.1, 3, 0.1), fc = c("lq", "lh", "lqp", "lqph", "lqpht"))
# Test all the possible combinations with gridSearch
gs_buff <- gridSearch(model_buff, hypers = h, metric = "auc", test = test_buff)

head(gs_buff@results[order(-gs_buff@results$test_AUC), ])  # Best combinations
# Use the genetic algorithm instead with optimizeModel
om_buff <- optimizeModel(model_buff, hypers = h, metric = "auc", test = test_buff, seed = 4)

head(om_buff@results)  # Best combinations

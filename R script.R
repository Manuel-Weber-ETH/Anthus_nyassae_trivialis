################################################################################################
### Ecological niche modelling of Anthus trivialis and Anthus nyassae ###
################################################################################################
### Author: Manuel Weber ###
################################################################################################
### Date: 22/06/2023 ###
################################################################################################
### Countries covered: South Africa, Botswana, Zimbabwe, Namibia, Angola, Mozambique, Malawi ###
################################################################################################

### =========================================================
### Data acquisition and cleaning ###
### =========================================================

### 1. GBIF ###


library(rgbif)


anthus_trivialis <- occ_search(scientificName = "Anthus trivialis",
                     continent = "africa",
                     hasCoordinate = TRUE, # Only observations with coordinates
                     eventDate = "1900,2023", # Observations between 1980 and 2023
                     hasGeospatialIssue = FALSE, # No geospatial issues
                     limit = 100000) # Maxiumum number of records

write.csv(gbi_vg$data, "Occurence data/gbi_vg.csv")

anthus_trivialis <- anthus_trivialis$data
write.csv(anthus_trivialis, "Anthus_trivialis_gbif.csv")



anthus_nyassae <- occ_search(scientificName = "Anthus nyassae",
                               continent = "africa",
                               hasCoordinate = TRUE, # Only observations with coordinates
                               eventDate = "1900,2023", # Observations between 1980 and 2023
                               hasGeospatialIssue = FALSE, # No geospatial issues
                               limit = 100000) # Maxiumum number of records


anthus_nyassae <- anthus_nyassae$data
write.csv(anthus_nyassae, "Anthus_nyassae_gbif.csv")

library(CoordinateCleaner)
coordinate_flags_trivialis <- clean_coordinates(x=anthus_trivialis, lon="decimalLongitude", lat="decimalLatitude", countries="countryCode", species="scientificName", tests= c("centroids","gbif","institutions","duplicates","seas","zeros"), verbose=T)
coordinate_flags_nyassae <- clean_coordinates(x=anthus_nyassae, lon="decimalLongitude", lat="decimalLatitude", countries="countryCode", species="scientificName", tests= c("centroids","gbif","institutions","duplicates","seas","zeros"), verbose=T)
anthus_trivialis$flags <- as.factor(coordinate_flags_trivialis$.summary)
anthus_nyassae$flags <- as.factor(coordinate_flags_nyassae$.summary)
anthus_trivialis <- subset(anthus_trivialis, anthus_trivialis$flags=="TRUE")
anthus_nyassae <- subset(anthus_nyassae, anthus_nyassae$flags=="TRUE")
anthus_trivialis <- anthus_trivialis[,c("decimalLongitude", "decimalLatitude", "dateIdentified",
                        "coordinateUncertaintyInMeters", "occurrenceStatus")]
anthus_nyassae <- anthus_nyassae[,c("decimalLongitude", "decimalLatitude", "dateIdentified",
                                        "coordinateUncertaintyInMeters", "occurrenceStatus")]

anthus_trivialis <- anthus_trivialis[,2:4]
anthus_trivialis$Source <- "GBIF"
names(anthus_trivialis) <- c("Longitude", "Latitude", "Date", "Source")

anthus_nyassae <- anthus_nyassae[,2:4]
anthus_nyassae$Source <- "GBIF"
names(anthus_nyassae) <- c("Longitude", "Latitude", "Date", "Source")

write.csv(anthus_trivialis, "Anthus_trivialis_gbif_cleaned.csv")
write.csv(anthus_nyassae, "Anthus_nyassae_gbif_cleaned.csv")

### 2. eBird ###

anthus_trivialis <- read.csv("ebd_trepip_relMay-2023.csv")
anthus_nyassae <- read.csv("ebd_woopip1_relMay-2023.csv")

unique(anthus_trivialis$COUNTRY)

library(dplyr)
anthus_trivialis <- anthus_trivialis %>%
  filter(COUNTRY %in% c("South Africa", "Namibia", "Botswana", "Zimbabwe", "Zambia", "Angola", "Mozambique", "Malawi"))

str(anthus_nyassae)

library(CoordinateCleaner)
coordinate_flags_trivialis <- clean_coordinates(x=anthus_trivialis, lon="LONGITUDE", lat="LATITUDE", countries="COUNTRY", species="SCIENTIFIC.NAME", tests= c("centroids","gbif","institutions","duplicates","seas","zeros"), verbose=T)
coordinate_flags_nyassae <- clean_coordinates(x=anthus_nyassae, lon="LONGITUDE", lat="LATITUDE", countries="COUNTRY", species="SCIENTIFIC.NAME", tests= c("centroids","gbif","institutions","duplicates","seas","zeros"), verbose=T)
anthus_trivialis$flags <- as.factor(coordinate_flags_trivialis$.summary)
anthus_nyassae$flags <- as.factor(coordinate_flags_nyassae$.summary)
anthus_trivialis <- subset(anthus_trivialis, anthus_trivialis$flags=="TRUE")
anthus_nyassae <- subset(anthus_nyassae, anthus_nyassae$flags=="TRUE")
anthus_trivialis <- anthus_trivialis[,c("LONGITUDE", "LATITUDE", "OBSERVATION.DATE")]
anthus_nyassae <- anthus_nyassae[,c("LONGITUDE", "LATITUDE", "OBSERVATION.DATE")]

anthus_nyassae$source <- "eBird"
anthus_trivialis$source <- "eBird"

names(anthus_nyassae) <- c("Longitude", "Latitude", "Date", "Source")
names(anthus_trivialis) <- c("Longitude", "Latitude", "Date", "Source")

write.csv(anthus_trivialis, "Anthus_trivialis_eBird_cleaned.csv")
write.csv(anthus_nyassae, "Anthus_nyassae_eBird_cleaned.csv")



### 3. SABAP ###
anthus_nyassae <- read.csv("Anthus nyassae SABAP.csv")
anthus_trivialis <- read.csv("Anthus trivialis SABAP.csv")

anthus_trivialis$Source <- "SABAP"
anthus_trivialis$Species <- "Anthus trivialis"
anthus_nyassae$Source <- "SABAP"
anthus_nyassae$Species <- "Anthus nyassae"

library(CoordinateCleaner)
coordinate_flags_trivialis <- clean_coordinates(x=anthus_trivialis, lon="Longitude", lat="Latitude", species = "Species", tests= c("duplicates"), verbose=T)
coordinate_flags_nyassae <- clean_coordinates(x=anthus_nyassae, lon="Longitude", lat="Latitude", species = "Species", tests= c("duplicates"), verbose=T)
anthus_trivialis$flags <- as.factor(coordinate_flags_trivialis$.summary)
anthus_nyassae$flags <- as.factor(coordinate_flags_nyassae$.summary)
anthus_trivialis <- subset(anthus_trivialis, anthus_trivialis$flags=="TRUE")
anthus_nyassae <- subset(anthus_nyassae, anthus_nyassae$flags=="TRUE")


write.csv(anthus_trivialis[,1:4], "Anthus_trivialis_SABAP_cleaned.csv")
write.csv(anthus_nyassae[,1:4], "Anthus_nyassae_SABAP_cleaned.csv")


### 4. iNaturalist ###
anthus_nyassae <- read.csv("Anthus nyassae iNaturalist.csv")
anthus_trivialis <- read.csv("Anthus trivialis iNaturalist.csv")

anthus_nyassae <- data.frame(Longitude = anthus_nyassae$longitude,
                             Latitude = anthus_nyassae$latitude,
                             Date = anthus_nyassae$observed_on)
anthus_nyassae$Source <- "iNaturalist"

anthus_trivialis <- data.frame(Longitude = anthus_trivialis$longitude,
                             Latitude = anthus_trivialis$latitude,
                             Date = anthus_trivialis$observed_on)
anthus_trivialis$Source <- "iNaturalist"
anthus_trivialis$Species <- "Anthus trivialis"
anthus_nyassae$Source <- "iNaturalist"
anthus_nyassae$Species <- "Anthus nyassae"

library(CoordinateCleaner)
coordinate_flags_trivialis <- clean_coordinates(x=anthus_trivialis, lon="Longitude", lat="Latitude", species = "Species", tests= c("duplicates"), verbose=T)
coordinate_flags_nyassae <- clean_coordinates(x=anthus_nyassae, lon="Longitude", lat="Latitude", species = "Species", tests= c("duplicates"), verbose=T)
anthus_trivialis$flags <- as.factor(coordinate_flags_trivialis$.summary)
anthus_nyassae$flags <- as.factor(coordinate_flags_nyassae$.summary)
anthus_trivialis <- subset(anthus_trivialis, anthus_trivialis$flags=="TRUE")
anthus_nyassae <- subset(anthus_nyassae, anthus_nyassae$flags=="TRUE")


write.csv(anthus_trivialis[,1:4], "Anthus_trivialis_iNaturalist_cleaned.csv")
write.csv(anthus_nyassae[,1:4], "Anthus_nyassae_iNaturalist_cleaned.csv")


### 5. Data assembling ###

ebird <- read.csv("Occurrences/Anthus_nyassae_eBird_cleaned.csv")
sabap <- read.csv("Occurrences/Anthus_nyassae_SABAP_cleaned.csv")
gbif <- read.csv("Occurrences/Anthus_nyassae_gbif_cleaned.csv")
inaturalist <- read.csv("Occurrences/Anthus_nyassae_iNaturalist_cleaned.csv")

anthus_nyassae <- rbind(ebird, sabap, gbif, inaturalist)
anthus_nyassae$Species <- "Anthus nyassae"

ebird <- read.csv("Occurrences/Anthus_trivialis_eBird_cleaned.csv")
sabap <- read.csv("Occurrences/Anthus_trivialis_SABAP_cleaned.csv")
sarbn <- read.csv("Occurrences/Anthus_trivialis_SARBN_cleaned.csv")
X <- 1:6
sarbn <- cbind(X,sarbn)
gbif <- read.csv("Occurrences/Anthus_trivialis_gbif_cleaned.csv")
inaturalist <- read.csv("Occurrences/Anthus_trivialis_iNaturalist_cleaned.csv")

anthus_trivialis <- rbind(ebird, sarbn, sabap, gbif, inaturalist)
anthus_trivialis$Species <- "Anthus trivialis"


library(CoordinateCleaner)
coordinate_flags_trivialis <- clean_coordinates(x=anthus_trivialis, lon="Longitude", lat="Latitude", species = "Species", tests= c("duplicates"), verbose=T)
coordinate_flags_nyassae <- clean_coordinates(x=anthus_nyassae, lon="Longitude", lat="Latitude", species = "Species", tests= c("duplicates"), verbose=T)
anthus_trivialis$flags <- as.factor(coordinate_flags_trivialis$.summary)
anthus_nyassae$flags <- as.factor(coordinate_flags_nyassae$.summary)
anthus_trivialis <- subset(anthus_trivialis, anthus_trivialis$flags=="TRUE")
anthus_nyassae <- subset(anthus_nyassae, anthus_nyassae$flags=="TRUE")

write.csv(anthus_trivialis[,2:5], "Occurrences/Anthus_trivialis_full.csv")
write.csv(anthus_nyassae[,2:5], "Occurrences/Anthus_nyassae_full.csv")


### =========================================================
### Pseudo-absences and data preprocessing ###
### =========================================================

setwd("C:/0_Documents/2_Projects/Pipits")
anthus_nyassae <- read.csv("Occurrences/Anthus_nyassae_study_area.csv")
anthus_trivialis <- read.csv("Occurrences/Anthus_trivialis_study_area.csv")

library(terra)
predictors <- rast("Raster/predictors.tif")
names(predictors)

anthus_nyassae_vect = vect(anthus_nyassae,geom=c("Longitude","Latitude"),
             crs=crs(predictors))
anthus_trivialis_vect = vect(anthus_trivialis,geom=c("Longitude","Latitude"),
                           crs=crs(predictors))

# Finally we thin the data, so that only one presence is kept per raster cell of 1x1 km (the DEM serves as template)

anthus_nyassae_raster <- rasterize(anthus_nyassae_vect, predictors, fun = "length")
anthus_trivialis_raster <- rasterize(anthus_trivialis_vect, predictors, fun = "length")

anthus_nyassae_pres <- crds(anthus_nyassae_raster)
anthus_trivialis_pres <- crds(anthus_trivialis_raster)

anthus_nyassae_pres <- as.data.frame(anthus_nyassae_pres)
anthus_nyassae_pres$Presence <- rep(1, nrow(anthus_nyassae_pres))
anthus_trivialis_pres <- as.data.frame(anthus_trivialis_pres)
anthus_trivialis_pres$Presence <- rep(1, nrow(anthus_trivialis_pres))

## PAs: Sampling from Drongo occurrences (proxy for SABAP coverage)

drongo <- read.csv("Occurrences/drongo.csv")
str(drongo)
set.seed(123)

pa <- drongo[sample(nrow(drongo), size = 2500, replace = FALSE, prob = drongo$full.protocol.cards), ]
pa_vect <- vect(pa,geom=c("Longitude","Latitude"),
                             crs=crs(predictors))
pa_raster <- rasterize(pa_vect, predictors, fun = "length")
pa_pres <- crds(pa_raster)
pa_pres <- as.data.frame(pa_pres)
pa_pres$Presence <- rep(0, nrow(pa_pres))

trivialis_modelmat <- rbind(anthus_trivialis_pres, pa_pres)
nyassae_modelmat <- rbind(anthus_nyassae_pres, pa_pres)

# Remove duplicates
library(misty)
duplicates <- as.numeric(rownames(df.unique(df.duplicated(trivialis_modelmat, x, y))))
trivialis_modelmat <- trivialis_modelmat[-duplicates,]

duplicates <- as.numeric(rownames(df.unique(df.duplicated(nyassae_modelmat, x, y))))
nyassae_modelmat <- nyassae_modelmat[-duplicates,]

# Extracting values of predictors
trivialis_modelmat_full <- cbind(trivialis_modelmat, extract(predictors, trivialis_modelmat[, c("x", "y")], ID = F))
nyassae_modelmat_full <- cbind(nyassae_modelmat, extract(predictors, nyassae_modelmat[, c("x", "y")], ID = F))

write.csv(trivialis_modelmat_full, "Occurrences/Anthus trivialis_Model matrix.csv")
write.csv(nyassae_modelmat_full, "Occurrences/Anthus nyassae_Model matrix.csv")

### =========================================================
### Models ###
### =========================================================

rm(list = ls())
setwd("C:/0_Documents/2_Projects/Pipits")
trivialis <- read.csv("Occurrences/Anthus trivialis_Model matrix.csv")
nyassae <- read.csv("Occurrences/Anthus nyassae_Model matrix.csv")

### 1. Anthus trivialis ###

trivialis <- na.omit(trivialis)
trivialis_scale <- scale(trivialis[,c(5,6,7,8,9,11,12)])
corma <- cor(trivialis_scale)

# Prepare the plot
par(mfrow = c(1,1), oma = c(0,7.5,7,0), mar = c(0,0,0,0), ps = 8, cex = 1, xpd = NA)

plot(1, 1, xlim = c(0, ncol(corma)-.5), ylim = c(0.5, ncol(corma)), 
     xaxs = "i", yaxs = "i", type = "n", xaxt = "n", yaxt = "n", bty = "n", 
     ylab = "", xlab = "")

pred_names <- c("Annual Mean Temperature",
                "Annual Precipitation",
                "Human Development",
                "Landscape Intactness",
                "Tree Density",
                "Elevation",
                "Leaf Area Index")

# Loop over the upper left half of the correlation matrix and plot the values
for(i in 1:(ncol(corma)-1)){
  
  if(i<ncol(corma)){
    text(i-.5, ncol(corma)+.3, pred_names[i], pos=2,offset=0,srt=-90) #x-axis labels
  }
  
  for(j in (i+1):ncol(corma)){
    # Define color code: green = OK, orange = problematic, red = big problem
    cl <- ifelse(abs(corma[i,j]) < .7, "green", 
                 ifelse(abs(corma[i,j]) < .9, "orange", "red"))
    points(i-.5, j-.5, cex = 5, pch = 16, col = cl)
    # Add Pearson correlation coefficients
    text(i-.5, j-.5, round(corma[i,j], digits = 2), cex = .9) 
    
    if(i==1){
      text(i-.5, j-.5, pred_names[j], pos = 2, offset = 2) # y-axis labels
    }
  }
}

# RF, GBM and GLM
glm_trivialis <- glm(Presence ~ CHELSA_BIO_Annual_Mean_Temperature + 
                       I(CHELSA_BIO_Annual_Mean_Temperature^2) + 
                       CHELSA_BIO_Annual_Precipitation + 
                       I(CHELSA_BIO_Annual_Precipitation^2) + 
                       ConsensusLandCover_Human_Development_Percentage + 
                       I(ConsensusLandCover_Human_Development_Percentage^2) + 
                       CrowtherLab_IntactLandscapes + 
                       I(CrowtherLab_IntactLandscapes^2) + 
                       CrowtherLab_Tree_Density + 
                       I(CrowtherLab_Tree_Density^2) +
                       #EarthEnvTexture_Shannon_Index + 
                       #I(EarthEnvTexture_Shannon_Index^2) + 
                       EarthEnvTopoMed_Elevation + 
                       I(EarthEnvTopoMed_Elevation^2) + 
                       MODIS_LAI + 
                       I(MODIS_LAI^2)
                       #NASA_ForestCanopyHeight + 
                       #I(NASA_ForestCanopyHeight^2) + 
                       #(Pixel_Lat + Pixel_Long),
                     ,data = trivialis, family = "binomial")
glm_trivialis <- step(glm_trivialis, directions = "both", trace = 0)

# For the decision trees, we'll use fewer pseudo-absences located further north (500 each)
absences <- subset(trivialis, Presence == 0)
absences_new <- absences[sample(nrow(absences), size = 500, replace = FALSE, prob = absences$Latitude), ]
trivialis_new <- rbind(subset(trivialis, Presence == 1), absences_new)

library(ranger)
rf_trivialis <- ranger(Presence ~ CHELSA_BIO_Annual_Mean_Temperature + 
                         CHELSA_BIO_Annual_Precipitation + 
                         ConsensusLandCover_Human_Development_Percentage + 
                         CrowtherLab_IntactLandscapes + 
                         CrowtherLab_Tree_Density + 
                         #EarthEnvTexture_Shannon_Index + 
                         EarthEnvTopoMed_Elevation + 
                         MODIS_LAI 
                         #NASA_ForestCanopyHeight + 
                         #Pixel_Lat + Pixel_Long # no need to specify the interaction in a tree-based model
                       ,data = trivialis_new, num.trees = 500, probability = TRUE,
                       min.node.size = 10) ## node size can be decreased for a more complex model)

library(gbm)
trivialis_new$Presence <- as.numeric(trivialis_new$Presence)
gbm_trivialis <- gbm(Presence ~ CHELSA_BIO_Annual_Mean_Temperature + 
                       CHELSA_BIO_Annual_Precipitation + 
                       ConsensusLandCover_Human_Development_Percentage + 
                       CrowtherLab_IntactLandscapes + 
                       CrowtherLab_Tree_Density + 
                       #EarthEnvTexture_Shannon_Index + 
                       EarthEnvTopoMed_Elevation + 
                       MODIS_LAI 
                       #NASA_ForestCanopyHeight + 
                       #(Pixel_Lat * Pixel_Long) # Interaction between longitude and latitude
            ,data = trivialis_new, distribution = "bernoulli", cv.folds = 10, shrinkage = 0.1, n.minobsinnode = 10,
            n.trees = 500)

### 2. Anthus nyassae ###

nyassae <- na.omit(nyassae)
nyassae_scale <- scale(nyassae[,c(5,6,7,8,9,11,12)])
corma <- cor(nyassae_scale)

# Prepare the plot
par(mfrow = c(1,1), oma = c(0,7.5,7,0), mar = c(0,0,0,0), ps = 8, cex = 1, xpd = NA)

plot(1, 1, xlim = c(0, ncol(corma)-.5), ylim = c(0.5, ncol(corma)), 
     xaxs = "i", yaxs = "i", type = "n", xaxt = "n", yaxt = "n", bty = "n", 
     ylab = "", xlab = "")

pred_names <- c("Annual Mean Temperature",
                "Annual Precipitation",
                "Human Development",
                "Landscape Intactness",
                "Tree Density",
                "Elevation",
                "Leaf Area Index")

# Loop over the upper left half of the correlation matrix and plot the values
for(i in 1:(ncol(corma)-1)){
  
  if(i<ncol(corma)){
    text(i-.5, ncol(corma)+.3, pred_names[i], pos=2,offset=0,srt=-90) #x-axis labels
  }
  
  for(j in (i+1):ncol(corma)){
    # Define color code: green = OK, orange = problematic, red = big problem
    cl <- ifelse(abs(corma[i,j]) < .7, "green", 
                 ifelse(abs(corma[i,j]) < .9, "orange", "red"))
    points(i-.5, j-.5, cex = 5, pch = 16, col = cl)
    # Add Pearson correlation coefficients
    text(i-.5, j-.5, round(corma[i,j], digits = 2), cex = .9) 
    
    if(i==1){
      text(i-.5, j-.5, pred_names[j], pos = 2, offset = 2) # y-axis labels
    }
  }
}

# RF, GBM and GLM
glm_nyassae <- glm(Presence ~ CHELSA_BIO_Annual_Mean_Temperature + 
                       I(CHELSA_BIO_Annual_Mean_Temperature^2) + 
                       CHELSA_BIO_Annual_Precipitation + 
                       I(CHELSA_BIO_Annual_Precipitation^2) + 
                       ConsensusLandCover_Human_Development_Percentage + 
                       I(ConsensusLandCover_Human_Development_Percentage^2) + 
                       CrowtherLab_IntactLandscapes + 
                       I(CrowtherLab_IntactLandscapes^2) + 
                       CrowtherLab_Tree_Density + 
                       I(CrowtherLab_Tree_Density^2) +
                       #EarthEnvTexture_Shannon_Index + 
                       #I(EarthEnvTexture_Shannon_Index^2) + 
                       EarthEnvTopoMed_Elevation + 
                       I(EarthEnvTopoMed_Elevation^2) + 
                       MODIS_LAI + 
                       I(MODIS_LAI^2)
                     #NASA_ForestCanopyHeight + 
                     #I(NASA_ForestCanopyHeight^2) + 
                     #(Pixel_Lat + Pixel_Long),
                     ,data = nyassae, family = "binomial")
glm_nyassae <- step(glm_nyassae, directions = "both", trace = 0)

# For the decision trees, we'll use fewer pseudo-absences located further north (500 each)
absences <- subset(nyassae, Presence == 0)
absences_new <- absences[sample(nrow(absences), size = 500, replace = FALSE, prob = absences$Latitude), ]
nyassae_new <- rbind(subset(nyassae, Presence == 1), absences_new)

library(ranger)
rf_nyassae <- ranger(Presence ~ CHELSA_BIO_Annual_Mean_Temperature + 
                         CHELSA_BIO_Annual_Precipitation + 
                         ConsensusLandCover_Human_Development_Percentage + 
                         CrowtherLab_IntactLandscapes + 
                         CrowtherLab_Tree_Density + 
                         #EarthEnvTexture_Shannon_Index + 
                         EarthEnvTopoMed_Elevation + 
                         MODIS_LAI 
                       #NASA_ForestCanopyHeight + 
                       #Pixel_Lat + Pixel_Long # no need to specify the interaction in a tree-based model
                       ,data = nyassae_new, num.trees = 500, probability = TRUE,
                       min.node.size = 10) ## node size can be decreased for a more complex model)

library(gbm)
nyassae_new$Presence <- as.numeric(nyassae_new$Presence)
gbm_nyassae <- gbm(Presence ~ CHELSA_BIO_Annual_Mean_Temperature + 
                       CHELSA_BIO_Annual_Precipitation + 
                       ConsensusLandCover_Human_Development_Percentage + 
                       CrowtherLab_IntactLandscapes + 
                       CrowtherLab_Tree_Density + 
                       #EarthEnvTexture_Shannon_Index + 
                       EarthEnvTopoMed_Elevation + 
                       MODIS_LAI 
                     #NASA_ForestCanopyHeight + 
                     #(Pixel_Lat * Pixel_Long) # Interaction between longitude and latitude
                     ,data = nyassae_new, distribution = "bernoulli", cv.folds = 10, shrinkage = 0.1, n.minobsinnode = 10,
                     n.trees = 500)

### =========================================================
### Model Validation ###
### =========================================================

### 1. Trivialis ###

## Write a custom function to refit models and generate cross-validation predictions 
cv.model <- function(model, K, dat = model$data){
  if("ranger" %in% class(model)){
    dat <- trivialis_new
  }
  if("gbm" %in% class(model)){
    dat <- trivialis_new
  }
  # Randomly define indices for cross-validation folds 
  k0 <- rep(1:K, each = ceiling(nrow(dat)/K)) 
  ks <- sample(k0, size = nrow(dat))
  
  # Prepare empty data.frame 
  cvpreds <- rep(NA, nrow(dat))
  
  # Loop over the cross-validation folds 
  for(i in 1:K){
    train <- dat[ks != i,] 
    test <- dat[ks == i,]
    # Convert response to factor for random forest 
    if("ranger" %in% class(model)){
      train$Presence <- as.factor(train$Presence) 
      # Update model on trainig subset 
      modtmp <- update(model, data = train) 
      # Predict to the test subset
      prd <- predict(modtmp, data = test)$predictions[,2] 
    } else {
      form.glm <- model$formula 
      # Update model on trainig subset 
      modtmp <- update(model, data = train) 
      # Predict to the test subset
      prd <- predict(modtmp, newdata = test, type = "response") 
    } 
    cvpreds[which(ks == i)] <- prd 
  } 
  return(cvpreds) 
}

## Create cv predictions and store in table 
### Trivialis
cv_trivialis_glm <- data.frame(ID = 1:nrow(glm_trivialis$data),
                               Presence = glm_trivialis$data$Presence,
                               GLM = cv.model(glm_trivialis, K = 5))


cv_trivialis_gbmrf <- data.frame(ID = 1:701,
                               Presence = trivialis_new$Presence,
                               RF = cv.model(rf_trivialis, K = 5),
                               GBM = cv.model(gbm_trivialis, K = 5))



# Calculate ROC curves

# Load the necessary library
library(pROC)

# par(mfrow = c(1,1), oma = c(3,3,3,3), mar = c(0,0,0,0), ps = 8, cex = 1, xpd = NA)
# Create an empty plot
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "Anthus trivialis")

# Iterate over each data frame
data_frames <- list(glm = cv_trivialis_glm[,2:3],
                    rf = cv_trivialis_gbmrf[,2:3],
                    gbm = cv_trivialis_gbmrf[,c(2,4)])
colors <- c("darkgreen", "green", "khaki")  # Choose colors for each curve

for (i in 1:length(data_frames)) {
  # Extract observation and prediction values from the data frame
  observations <- data_frames[[i]][[1]]
  predictions <- data_frames[[i]][[2]]
  
  # Generate ROC curve
  roc_obj <- roc(observations, predictions)
  
  # Plot the ROC curve
  lines(roc_obj, col = colors[i])
  
  # Calculate and print the area under the ROC curve (AUC)
  auc_val <- auc(roc_obj)
  auc_label <- paste("AUC =", round(auc_val, 3))
  text(0.6, 0.35 - i * 0.1, auc_label, col = colors[i])
}

# Add a legend
legend("bottomleft", legend = c("GLM", "RF", "GBM"), col = colors, lty = 1)

### 2. Nyassae ###

## Write a custom function to refit models and generate cross-validation predictions 
cv.model <- function(model, K, dat = model$data){
  if("ranger" %in% class(model)){
    dat <- nyassae_new
  }
  if("gbm" %in% class(model)){
    dat <- nyassae_new
  }
  # Randomly define indices for cross-validation folds 
  k0 <- rep(1:K, each = ceiling(nrow(dat)/K)) 
  ks <- sample(k0, size = nrow(dat))
  
  # Prepare empty data.frame 
  cvpreds <- rep(NA, nrow(dat))
  
  # Loop over the cross-validation folds 
  for(i in 1:K){
    train <- dat[ks != i,] 
    test <- dat[ks == i,]
    # Convert response to factor for random forest 
    if("ranger" %in% class(model)){
      train$Presence <- as.factor(train$Presence) 
      # Update model on trainig subset 
      modtmp <- update(model, data = train) 
      # Predict to the test subset
      prd <- predict(modtmp, data = test)$predictions[,2] 
    } else {
      form.glm <- model$formula 
      # Update model on trainig subset 
      modtmp <- update(model, data = train) 
      # Predict to the test subset
      prd <- predict(modtmp, newdata = test, type = "response") 
    } 
    cvpreds[which(ks == i)] <- prd 
  } 
  return(cvpreds) 
}

## Create cv predictions and store in table 
### nyassae
cv_nyassae_glm <- data.frame(ID = 1:nrow(glm_nyassae$data),
                               Presence = glm_nyassae$data$Presence,
                               GLM = cv.model(glm_nyassae, K = 5))


cv_nyassae_gbmrf <- data.frame(ID = 1:904,
                                 Presence = nyassae_new$Presence,
                                 RF = cv.model(rf_nyassae, K = 5),
                                 GBM = cv.model(gbm_nyassae, K = 5))



# Calculate ROC curves

# Load the necessary library
library(pROC)

# par(mfrow = c(1,1), oma = c(3,3,3,3), mar = c(0,0,0,0), ps = 8, cex = 1, xpd = NA)
# Create an empty plot
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "Anthus nyassae")

# Iterate over each data frame
data_frames <- list(glm = cv_nyassae_glm[,2:3],
                    rf = cv_nyassae_gbmrf[,2:3],
                    gbm = cv_nyassae_gbmrf[,c(2,4)])
colors <- c("darkgreen", "green", "khaki")  # Choose colors for each curve

for (i in 1:length(data_frames)) {
  # Extract observation and prediction values from the data frame
  observations <- data_frames[[i]][[1]]
  predictions <- data_frames[[i]][[2]]
  
  # Generate ROC curve
  roc_obj <- roc(observations, predictions)
  
  # Plot the ROC curve
  lines(roc_obj, col = colors[i])
  
  # Calculate and print the area under the ROC curve (AUC)
  auc_val <- auc(roc_obj)
  auc_label <- paste("AUC =", round(auc_val, 3))
  text(0.6, 0.35 - i * 0.1, auc_label, col = colors[i])
}

# Add a legend
legend("bottomleft", legend = c("GLM", "RF", "GBM"), col = colors, lty = 1)

### =========================================================
### Projections ###
### =========================================================

### 1. Trivialis ###

# GBM
library(terra)
predictors <- rast("Raster/predictors.tif")
pred_gbm_trivialis <- predict(predictors, gbm_trivialis, type = "response")
plot(pred_gbm_trivialis)

# GLM
pred_glm_trivialis <- predict(predictors, glm_trivialis, type = "response")
plot(pred_glm_trivialis)

# RF
predictors_df <- as.data.frame(predictors, xy = T, na.rm = T)
#predictors_df <- predictors_df[,c(1,2,4,5,6)]
library(randomForest)
pred_rf_trivialis <- predict(rf_trivialis, data = predictors_df)
rf_df <- cbind(predictors_df[,1:2], pred_rf_trivialis$predictions[,1])
pred_rf_trivialis <- rast(rf_df)

writeRaster(pred_gbm_trivialis, "Raster/gbm_trivialis.tif", overwrite = T)
writeRaster(pred_glm_trivialis, "Raster/glm_trivialis.tif", overwrite = T)
writeRaster(pred_rf_trivialis, "Raster/rf_trivialis.tif", overwrite = T)

### 2. Nyassae ###

# GBM
pred_gbm_nyassae <- predict(predictors, gbm_nyassae, type = "response")

# GLM
pred_glm_nyassae <- predict(predictors, glm_nyassae, type = "response")

# RF
predictors_df <- as.data.frame(predictors, xy = T, na.rm = T)
pred_rf_nyassae <- predict(rf_nyassae, data = predictors_df)
rf_df <- cbind(predictors_df[,1:2], pred_rf_nyassae$predictions[,1])
pred_rf_nyassae <- rast(rf_df)

writeRaster(pred_gbm_nyassae, "Raster/gbm_nyassae.tif", overwrite = T)
writeRaster(pred_glm_nyassae, "Raster/glm_nyassae.tif", overwrite = T)
writeRaster(pred_rf_nyassae, "Raster/rf_nyassae.tif", overwrite = T)

#Import all neccesary libraries. library(tidyverse) library(leaflet)
library(rgdal)
library(dismo) 
library(gstat) 
library(sp) 
library(gstat) 
library(tidyverse) 
library(spdep) 
library(raster) 
library(spdep)

#============Visualize the locations sensors===================
#switch to the Texas 2013 dataset from spaces from the column names.
#of the pollutant
#Grab epa pollution dataset for 2013 fiveChosenStates <- read_csv('a3Data.csv')
#Grab all the states that we need Oklahoma, texas, Louisiana, New Mexico and Arkansas
fiveChosenStates <- fiveChosenStates %>%
  filter(`StateCode` == '48' | `StateCode` == '22' |`StateCode` == '5' | `StateCode` == '40' | `StateCode` == '35' )
#Grabs all pollutants of interest. fiveChosenStatesPollutants <- fiveChosenStates %>%
filter(`ParameterName` == 'Ozone'| `ParameterName` == 'PM2.5 - Local Conditions' |`ParameterName` == 'Nitrogen dioxide (NO2)' )
#Removes duplicated locations to aid with krieging later. fiveChosenStatesPollutants <- fiveChosenStatesPollutants[! duplicated(fiveChosenStatesPollutants$SiteNum),]
#Assigns colors for the pollutants to display on the map (leaflet). colorsForTex <- colorFactor(palette = c("red","green","blue"),domain = c(fiveChosenStatesPollutants$ParameterName))
#Makes leaflet graph containg all sensors. leaflet(fiveChosenStatesPollutants)%>%
addTiles()%>%
  addCircleMarkers(color = colorsForTex(fiveChosenStatesPollutants$ParameterName))%>%
  addLegend("topright", pal = colorsForTex, values = ~ParameterName, title = "Pollutatant Observation Stations",
  )

#==================================MODEL 1 Ozone================================
#Grab epa pollution dataset for 2013 fiveChosenStates <- read_csv('a3Data.csv')
#Grab all the states that we need Oklahoma, Texas, Louisiana, New Mexico and Arkansas
fiveChosenStates <- fiveChosenStates %>%
  filter(`StateCode` == '48' | `StateCode` == '22' |`StateCode` == '5' | `StateCode` == '40' | `StateCode` == '35' )#filter for fiveChosenStates.
#Grabs all pollutant of interest. modelfiveChosenStates <- fiveChosenStates %>%
filter(`ParameterName` == 'Ozone' )
#Function made to find Root mean squared error. RMSEKrigeResidual <- function(x){
return(sqrt(mean(x^2))) }
#Removes duplicated locations to aid with krieging later. modelfiveChosenStates <- modelfiveChosenStates[! duplicated(modelfiveChosenStates$SiteNum),]
#Done so the x is longitude and y is latitude. (Used for graphing later) coordinates(modelfiveChosenStates) = c('Longitude','Latitude')
#===============================IDW=============================================
#Assemble into a spatialpointdataset for IDW. coordinates(modelfiveChosenStates) <- ~ Latitude + Longitude
#Sets boundaries used later bbox(modelfiveChosenStates)
#Grabs outlier points and prepares data for polygons to be made. grabOutlierPoints <- modelfiveChosenStates@coords %>%
chull()
grabOutlierPoints <- modelfiveChosenStates@coords[ grabOutlierPoints, ]%>%
  as.data.frame()
# Makes/grabs a matrix from the points we feed it. coordinatesMade <- coordinates(grabOutlierPoints)
# Make a polygon that represents the shape of our shapes for interpolation. poly <- sp::Polygon(coordinatesMade)
# Prepare/make the data ready to be made into a spatial polygon that will be used for IDW.
polygonWithBoundary <- Polygons(list(poly), ID="Minimum Boundary")

spatialPolygonWithBoundary <- SpatialPolygons(list(polygonWithBoundary))
#Assemble a grid that will be used as locations for IDW interpolation. grid <- makegrid(spatialPolygonWithBoundary, cellsize = 0.12) coordinates(grid) <- ~ x1+x2
#Overlap the polygon made previously and the grid to form a grid in the shape of the polygon.
gridAsPolygon <- sp::over(grid , spatialPolygonWithBoundary) predictionPolygon <- grid[!is.na(gridAsPolygon),]
#Finding best exponent for IDW Interpolation.
#Perform Cross validation on the models. (simulate various k values) #k=1
LOOCVMODEL <- krige.cv(ArithmeticMean~1,
                       modelfiveChosenStates,
                       nfold = nrow(modelfiveChosenStates), set = list(idp = 1)
)
#k=2
LOOCVMODEL2 <- krige.cv(ArithmeticMean~1,
                        modelfiveChosenStates,
                        nfold = nrow(modelfiveChosenStates), set = list(idp = 2)
)
#k=3
LOOCVMODEL3 <- krige.cv(ArithmeticMean~1,
                        modelfiveChosenStates,
                        nfold = nrow(modelfiveChosenStates), set = list(idp = 3)
)
#Perform Root Mean Squared Error to find/measure the possible error in the models we made based on simulated k/exponent values).
#k=1 RMSEKrigeResidual(LOOCVMODEL@data$residual)
#k=2 RMSEKrigeResidual(LOOCVMODEL2@data$residual)
#k=3 RMSEKrigeResidual(LOOCVMODEL3@data$residual)
#2 seems to be the lowest hence k will = 2

#Run IDW interpolation based using the exponent/k value we determined best fits.
IDWModel <- idw(ArithmeticMean ~ 1,
                modelfiveChosenStates,
                predictionPolygon, #Input and prediction locations same. idp = 2
)
#Plot the IDW intepolation using a SPPLOT.
spplot(IDWModel, "var1.pred",xlab = "Longitude", scales = list(draw = TRUE), ylab = "Latitude",
       main = list(label = "IDW Prediction For Ozone"), key.space=list(x=1.033,y=.95,corner=c(0,1),label = "title"),auto.key = list(title = "Prediction in PPM", cex = 0.9))
#Cross Validation for IDW Interpolation k = 2 (Double Check). LOOCVMODEL <- krige.cv(ArithmeticMean~1,
modelfiveChosenStates,
nfold = nrow(modelfiveChosenStates), set = list(idp = 2)
)
#=========================Kriging===============================================
#Assemble into a spatialpointdataset for Kriging. coordinates(modelfiveChosenStates) <- ~ Latitude + Longitude
#Sets boundaries used later bbox(modelfiveChosenStates)
#Test Data to make sure it is normally distributed (Check Assumptions). hist(modelfiveChosenStates@data$ArithmeticMean)
#Log the Data to try and normally distribute it. modelfiveChosenStates@data$LogArithmeticMean <- log(modelfiveChosenStates@data$ArithmeticMean)
# Check the data to see if it is normally distributed after logging it. hist(modelfiveChosenStates@data$LogArithmeticMean)
#Check Stationarity using mean and SSPLOT.
modelfiveChosenStates$mean <- over(modelfiveChosenStates,modelfiveChosenStates,fn = mean,na.rm = T)[,1] spplot(modelfiveChosenStates, "mean", main = list(label = "Mean"))
#Build a variogram to start the Kriging process. ArithmeticMean.vgm <- variogram(LogArithmeticMean~1, data=modelfiveChosenStates)

plot(ArithmeticMean.vgm) show.vgms()
#Wav variogram was found to be best fit.
ArithmeticMean.fit <- fit.variogram(ArithmeticMean.vgm, model = vgm("Wav")) plot(ArithmeticMean.vgm, ArithmeticMean.fit)
ArithmeticMean.fit
#Grabs outlier points and prepares data for polygons to be made. grabOutlierPoints <- modelfiveChosenStates@coords %>%
chull()
grabOutlierPoints <- modelfiveChosenStates@coords[ grabOutlierPoints, ]%>%
  as.data.frame()
# Makes/grabs a matrix from the points we feed it. coordinatesMade <- coordinates(grabOutlierPoints)
# Make a polygon that represents the shape of our shapes for Kriging. poly <- sp::Polygon(coordinatesMade)
# Prepare/make the data ready to be made into a spatial polygon that will be used for Kriging.
polygonWithBoundary <- Polygons(list(poly), ID="Minimum Boundary") list(polygonWithBoundary)
spatialPolygonWithBoundary <- SpatialPolygons(list(polygonWithBoundary))
#Assemble a grid that will be used as locations for Kriging. grid <- makegrid(spatialPolygonWithBoundary, cellsize = 0.12) coordinates(grid) <- ~ x1+x2
#Overlap the polygon made previously and the grid to form a grid in the shape of the polygon.
gridAsPolygon <- sp::over(grid , spatialPolygonWithBoundary) predictionPolygon <- grid[!is.na(gridAsPolygon),]
#Make the Kriging interpolation for the 5 chosen states. KrigeModel <- krige(LogArithmeticMean~1, modelfiveChosenStates, predictionPolygon, model=ArithmeticMean.fit)
#Plot the Kriging intepolation using a SPPLOT.
spplot(KrigeModel, "var1.pred",xlab = "Longitude", scales = list(draw = TRUE), ylab = "Latitude",
       main = list(label = "Kriging Prediction For Ozone (Logged)"), key.space=list(x=1.033,y=.95,corner=c(0,1),label = "title"),auto.key = list(title = "Prediction in PPM", cex = 0.9))
#Cross Validation for Kriging model.
LOOCVKrige <-krige.cv(LogArithmeticMean~1, modelfiveChosenStates, model=ArithmeticMean.fit )

#Perform Root Mean Squared Error to find/measure the possible error. RMSEKrigeResidual(LOOCVKrige@data$residual)
#============================Spatial Clustering=================================
#Grab epa pollution dataset for 2013 fiveChosenStates <- read_csv('a3Data.csv')
#Grab all the states that we need Oklahoma, Texas, Louisiana, New Mexico and Arkansas
fiveChosenStates <- fiveChosenStates %>%
  filter(`StateCode` == '48' | `StateCode` == '22' |`StateCode` == '5' | `StateCode` == '40' | `StateCode` == '35')
#Grabs all pollutant of interest. modelfiveChosenStates <- fiveChosenStates %>%
filter(`ParameterName` == 'Ozone' )
#Code above removes duplicate locations krieging does not like duplicated!!! modelfiveChosenStates <- modelfiveChosenStates[! duplicated(modelfiveChosenStates$SiteNum),]
#Done so the x is longitude and y is latitude. coordinates(modelfiveChosenStates) = c('Longitude','Latitude')
#Create voronoi polygons to define individual areas around points so contiguity based skater can be done.
voronoi <- voronoi(modelfiveChosenStates)
#Setup the data to perform the skater algorithm using the original point locations and the variable ArithmeticMean.
voronoi.nb <- poly2nb(voronoi)
lcosts <- nbcosts(voronoi.nb,modelfiveChosenStates$ArithmeticMean)
nb.w <- nb2listw(voronoi.nb, lcosts, style="B") mst.voronoi <- mstree(nb.w,5)
#Run the skater algorithm to find the clusters. skaterClusterFinder <- skater(mst.voronoi[,1:2], modelfiveChosenStates$ArithmeticMean, 2)
#Plot the results of skater algorithm to show the clusters. plot(skaterClusterFinder, coordinates(voronoi), cex.circles=0.125, cex.lab=0.001, col=c("red","blue","green"))
legend("bottomleft", title="Individual Clusters", legend = c("Cluster 1 (Red)", "Cluster 2 (Green)", "Cluster 3 (Blue)"),col = c(rgb(0.25,0.4,0.1,0.7) ))
title("Ozone Spatial Clustering",line = -2) axis(1)
axis(2)

#===============================MODEL 2 PM2.5===================================
#Grab epa pollution dataset for 2013 fiveChosenStates <- read_csv('a3Data.csv')
#Grab all the states that we need Oklahoma, Texas, Louisiana, New Mexico and Arkansas
fiveChosenStates <- fiveChosenStates %>%
  filter(`StateCode` == '48' | `StateCode` == '22' |`StateCode` == '5' | `StateCode` == '40' | `StateCode` == '35' )#filter for fiveChosenStates.
#Grabs all pollutant of interest. modelfiveChosenStates <- fiveChosenStates %>%
filter(`ParameterName` == 'PM2.5 - Local Conditions' )
#Function made to find Root mean squared error. RMSEKrigeResidual <- function(x){
return(sqrt(mean(x^2))) }
#Removes duplicated locations to aid with krieging later. modelfiveChosenStates <- modelfiveChosenStates[! duplicated(modelfiveChosenStates$SiteNum),]
#Done so the x is longitude and y is latitude. (Used for graphing later) coordinates(modelfiveChosenStates) = c('Longitude','Latitude')
#===============================IDW=============================================
#Assemble into a spatialpointdataset for IDW. coordinates(modelfiveChosenStates) <- ~ Latitude + Longitude
#Sets boundaries used later bbox(modelfiveChosenStates)
#Grabs outlier points and prepares data for polygons to be made. grabOutlierPoints <- modelfiveChosenStates@coords %>%
chull()
grabOutlierPoints <- modelfiveChosenStates@coords[ grabOutlierPoints, ]%>%
  as.data.frame()
# Makes/grabs a matrix from the points we feed it. coordinatesMade <- coordinates(grabOutlierPoints)
# Make a polygon that represents the shape of our shapes for interpolation. poly <- sp::Polygon(coordinatesMade)

# Prepare/make the data ready to be made into a spatial polygon that will be used for IDW.
polygonWithBoundary <- Polygons(list(poly), ID="Minimum Boundary") spatialPolygonWithBoundary <- SpatialPolygons(list(polygonWithBoundary))
#Assemble a grid that will be used as locations for IDW interpolation. grid <- makegrid(spatialPolygonWithBoundary, cellsize = 0.12) coordinates(grid) <- ~ x1+x2
#Overlap the polygon made previously and the grid to form a grid in the shape of the polygon.
gridAsPolygon <- sp::over(grid , spatialPolygonWithBoundary) predictionPolygon <- grid[!is.na(gridAsPolygon),]
#Finding best exponent for IDW Interpolation.
#Perform Cross validation on the models. (simulate various k values) #k=1
LOOCVMODEL <- krige.cv(ArithmeticMean~1,
                       modelfiveChosenStates,
                       nfold = nrow(modelfiveChosenStates), set = list(idp = 1)
)
#k=2
LOOCVMODEL2 <- krige.cv(ArithmeticMean~1,
                        modelfiveChosenStates,
                        nfold = nrow(modelfiveChosenStates), set = list(idp = 2)
)
#k=3
LOOCVMODEL3 <- krige.cv(ArithmeticMean~1,
                        modelfiveChosenStates,
                        nfold = nrow(modelfiveChosenStates), set = list(idp = 3)
)
#Perform Root Mean Squared Error to find/measure the possible error in the models we made based on simulated k/exponent values).
#k=1 RMSEKrigeResidual(LOOCVMODEL@data$residual)
#k=2 RMSEKrigeResidual(LOOCVMODEL2@data$residual)
#k=3 RMSEKrigeResidual(LOOCVMODEL3@data$residual)
#3 seems to be the lowest hence k will = 3

#Run IDW interpolation based using the exponent/k value we determined best fits.
IDWModel <- idw(ArithmeticMean ~ 1,
                modelfiveChosenStates,
                predictionPolygon, #Input and prediction locations same. idp = 3
)
#Plot the IDW intepolation using a SPPLOT.
spplot(IDWModel, "var1.pred",xlab = "Longitude", scales = list(draw = TRUE), ylab = "Latitude",
       main = list(label = "IDW Prediction For PM2.5 - Local Conditions"), key.space=list(x=1.063,y=.95,corner=c(0,1),label = "title"),auto.key = list(title = "Prediction in Micrograms", cex = 0.8))
#Cross Validation for IDW Interpolation k = 3 (Double Check). LOOCVMODEL <- krige.cv(ArithmeticMean~1,
modelfiveChosenStates,
nfold = nrow(modelfiveChosenStates), set = list(idp = 3)
)
#=========================Kriging===============================================
#Assemble into a spatialpointdataset for Kriging. coordinates(modelfiveChosenStates) <- ~ Latitude + Longitude
#Sets boundaries used later bbox(modelfiveChosenStates)
#Test Data to make sure it is normally distributed (Check Assumptions). hist(modelfiveChosenStates@data$ArithmeticMean)
#Log the Data to try and normally distribute it. modelfiveChosenStates@data$LogArithmeticMean <- log(modelfiveChosenStates@data$ArithmeticMean)
# Check the data to see if it is normally distributed after logging it. hist(modelfiveChosenStates@data$LogArithmeticMean)
#Check Stationarity using mean and SSPLOT.
modelfiveChosenStates$mean <- over(modelfiveChosenStates,modelfiveChosenStates,fn = mean,na.rm = T)[,1] spplot(modelfiveChosenStates, "mean", main = list(label = "Mean"))
#Build a variogram to start the Kriging process.

ArithmeticMean.vgm <- variogram(LogArithmeticMean~1, data=modelfiveChosenStates)
plot(ArithmeticMean.vgm)
show.vgms()
#Wav variogram was found to be best fit.
ArithmeticMean.fit <- fit.variogram(ArithmeticMean.vgm, model = vgm("Wav")) plot(ArithmeticMean.vgm, ArithmeticMean.fit)
ArithmeticMean.fit
#Grabs outlier points and prepares data for polygons to be made. grabOutlierPoints <- modelfiveChosenStates@coords %>%
chull()
grabOutlierPoints <- modelfiveChosenStates@coords[ grabOutlierPoints, ]%>%
  as.data.frame()
# Makes/grabs a matrix from the points we feed it. coordinatesMade <- coordinates(grabOutlierPoints)
# Make a polygon that represents the shape of our shapes for Kriging. poly <- sp::Polygon(coordinatesMade)
# Prepare/make the data ready to be made into a spatial polygon that will be used for Kriging.
polygonWithBoundary <- Polygons(list(poly), ID="Minimum Boundary") list(polygonWithBoundary)
spatialPolygonWithBoundary <- SpatialPolygons(list(polygonWithBoundary))
#Assemble a grid that will be used as locations for Kriging. grid <- makegrid(spatialPolygonWithBoundary, cellsize = 0.12) coordinates(grid) <- ~ x1+x2
#Overlap the polygon made previously and the grid to form a grid in the shape of the polygon.
gridAsPolygon <- sp::over(grid , spatialPolygonWithBoundary) predictionPolygon <- grid[!is.na(gridAsPolygon),]
#Make the Kriging interpolation for the 5 chosen states. KrigeModel <- krige(LogArithmeticMean~1, modelfiveChosenStates, predictionPolygon, model=ArithmeticMean.fit)
#Plot the Kriging intepolation using a SPPLOT.
spplot(KrigeModel, "var1.pred",xlab = "Longitude", scales = list(draw = TRUE), ylab = "Latitude",
       main = list(label = "Kriging Prediction For PM2.5 - Local Conditions (Logged)"), key.space=list(x=1.063,y=.95,corner=c(0,1),label = "title"),auto.key = list(title = "Prediction in Micrograms", cex = 0.8))
#Cross Validation for Kriging model.

LOOCVKrige <-krige.cv(LogArithmeticMean~1, modelfiveChosenStates, model=ArithmeticMean.fit )
#Perform Root Mean Squared Error to find/measure the possible error. RMSEKrigeResidual(LOOCVKrige@data$residual)
#============================Spatial Clustering=================================
#Grab epa pollution dataset for 2013 fiveChosenStates <- read_csv('a3Data.csv')
#Grab all the states that we need Oklahoma, Texas, Louisiana, New Mexico and Arkansas
fiveChosenStates <- fiveChosenStates %>%
  filter(`StateCode` == '48' | `StateCode` == '22' |`StateCode` == '5' | `StateCode` == '40' | `StateCode` == '35')
#Grabs all pollutant of interest. modelfiveChosenStates <- fiveChosenStates %>%
filter(`ParameterName` == 'PM2.5 - Local Conditions' )
#Code above removes duplicate locations krieging does not like duplicated!!! modelfiveChosenStates <- modelfiveChosenStates[! duplicated(modelfiveChosenStates$SiteNum),]
#Done so the x is longitude and y is latitude. coordinates(modelfiveChosenStates) = c('Longitude','Latitude')
#Create voronoi polygons to define individual areas around points so contiguity based skater can be done.
voronoi <- voronoi(modelfiveChosenStates)
#Setup the data to perform the skater algorithm using the original point locations and the variable ArithmeticMean.
voronoi.nb <- poly2nb(voronoi)
lcosts <- nbcosts(voronoi.nb,modelfiveChosenStates$ArithmeticMean)
nb.w <- nb2listw(voronoi.nb, lcosts, style="B") mst.voronoi <- mstree(nb.w,5)
#Run the skater algorithm to find the clusters. skaterClusterFinder <- skater(mst.voronoi[,1:2], modelfiveChosenStates$ArithmeticMean, 2)
#Plot the results of skater algorithm to show the clusters. plot(skaterClusterFinder, coordinates(voronoi), cex.circles=0.125, cex.lab=0.001, col=c("red","blue","green"))
legend("bottomleft", title="Individual Clusters", legend = c("Cluster 1 (Red)", "Cluster 2 (Green)", "Cluster 3 (Blue)"),col = c(rgb(0.25,0.4,0.1,0.7) ))
title("PM 2.5 Spatial Clustering",line = -2) axis(1)

axis(2)
#===========================MODEL 3 Nitrogen Dioxide============================
#Grab epa pollution dataset for 2013 fiveChosenStates <- read_csv('a3Data.csv')
#Grab all the states that we need Oklahoma, Texas, Louisiana, New Mexico and Arkansas
fiveChosenStates <- fiveChosenStates %>%
  filter(`StateCode` == '48' | `StateCode` == '22' |`StateCode` == '5' | `StateCode` == '40' | `StateCode` == '35' )#filter for fiveChosenStates.
#Grabs all pollutant of interest. modelfiveChosenStates <- fiveChosenStates %>%
filter(`ParameterName` == 'Nitrogen dioxide (NO2)' )
#Function made to find Root mean squared error. RMSEKrigeResidual <- function(x){
return(sqrt(mean(x^2))) }
#Removes duplicated locations to aid with krieging later. modelfiveChosenStates <- modelfiveChosenStates[! duplicated(modelfiveChosenStates$SiteNum),]
#NO2 has NA values hence, this code is needed to remove them. modelfiveChosenStates <- modelfiveChosenStates[! is.na(modelfiveChosenStates$ArithmeticMean),]
#Done so the x is longitude and y is latitude. (Used for graphing later) coordinates(modelfiveChosenStates) = c('Longitude','Latitude')
#===============================IDW=============================================
#Assemble into a spatialpointdataset for IDW. coordinates(modelfiveChosenStates) <- ~ Latitude + Longitude
#Sets boundaries used later bbox(modelfiveChosenStates)
#Grabs outlier points and prepares data for polygons to be made. grabOutlierPoints <- modelfiveChosenStates@coords %>%
chull()
grabOutlierPoints <- modelfiveChosenStates@coords[ grabOutlierPoints, ]%>%
  as.data.frame()

# Makes/grabs a matrix from the points we feed it. coordinatesMade <- coordinates(grabOutlierPoints)
# Make a polygon that represents the shape of our shapes for interpolation. poly <- sp::Polygon(coordinatesMade)
# Prepare/make the data ready to be made into a spatial polygon that will be used for IDW.
polygonWithBoundary <- Polygons(list(poly), ID="Minimum Boundary") spatialPolygonWithBoundary <- SpatialPolygons(list(polygonWithBoundary))
#Assemble a grid that will be used as locations for IDW interpolation. grid <- makegrid(spatialPolygonWithBoundary, cellsize = 0.12) coordinates(grid) <- ~ x1+x2
#Overlap the polygon made previously and the grid to form a grid in the shape of the polygon.
gridAsPolygon <- sp::over(grid , spatialPolygonWithBoundary) predictionPolygon <- grid[!is.na(gridAsPolygon),]
#Finding best exponent for IDW Interpolation.
#Perform Cross validation on the models. (simulate various k values) #k=1
LOOCVMODEL <- krige.cv(ArithmeticMean~1,
                       modelfiveChosenStates,
                       nfold = nrow(modelfiveChosenStates), set = list(idp = 1)
)
#k=2
LOOCVMODEL2 <- krige.cv(ArithmeticMean~1,
                        modelfiveChosenStates,
                        nfold = nrow(modelfiveChosenStates), set = list(idp = 2)
)
#k=3
LOOCVMODEL3 <- krige.cv(ArithmeticMean~1,
                        modelfiveChosenStates,
                        nfold = nrow(modelfiveChosenStates), set = list(idp = 3)
)
#Perform Root Mean Squared Error to find/measure the possible error in the models we made based on simulated k/exponent values).
#k=1 RMSEKrigeResidual(LOOCVMODEL@data$residual)
#k=2 RMSEKrigeResidual(LOOCVMODEL2@data$residual)

#k=3 RMSEKrigeResidual(LOOCVMODEL3@data$residual)
#1 seems to be the lowest hence k will = 1
#Run IDW interpolation based using the exponent/k value we determined best fits.
IDWModel <- idw(ArithmeticMean ~ 1,
                modelfiveChosenStates,
                predictionPolygon, #Input and prediction locations same. idp = 1
)
#Plot the IDW intepolation using a SPPLOT.
spplot(IDWModel, "var1.pred",xlab = "Longitude", scales = list(draw = TRUE), ylab = "Latitude",
       main = list(label = "IDW Prediction For Nitrogen dioxide/NO2"), key.space=list(x=1.033,y=.95,corner=c(0,1),label = "title"),auto.key = list(title = "Prediction in PPB", cex = 0.9))
#Cross Validation for IDW Interpolation k = 1 (Double Check). LOOCVMODEL <- krige.cv(ArithmeticMean~1,
modelfiveChosenStates,
nfold = nrow(modelfiveChosenStates), set = list(idp = 1)
)
#=========================Kriging===============================================
#Assemble into a spatialpointdataset for Kriging. coordinates(modelfiveChosenStates) <- ~ Latitude + Longitude
#Sets boundaries used later bbox(modelfiveChosenStates)
#Test Data to make sure it is normally distributed (Check Assumptions). hist(modelfiveChosenStates@data$ArithmeticMean)
#Log the Data to try and normally distribute it. modelfiveChosenStates@data$LogArithmeticMean <- log(modelfiveChosenStates@data$ArithmeticMean)
# Check the data to see if it is normally distributed after logging it. hist(modelfiveChosenStates@data$LogArithmeticMean)
#Check Stationarity using mean and SSPLOT.

modelfiveChosenStates$mean <- over(modelfiveChosenStates,modelfiveChosenStates,fn = mean,na.rm = T)[,1] spplot(modelfiveChosenStates, "mean", main = list(label = "Mean"))
#Build a variogram to start the Kriging process. ArithmeticMean.vgm <- variogram(LogArithmeticMean~1, data=modelfiveChosenStates)
plot(ArithmeticMean.vgm)
show.vgms()
#The variogram fit for this is Lin
ArithmeticMean.fit <- fit.variogram(ArithmeticMean.vgm, model = vgm("Lin")) plot(ArithmeticMean.vgm, ArithmeticMean.fit)
ArithmeticMean.fit
#Grabs outlier points and prepares data for polygons to be made. grabOutlierPoints <- modelfiveChosenStates@coords %>%
chull()
grabOutlierPoints <- modelfiveChosenStates@coords[ grabOutlierPoints, ]%>%
  as.data.frame()
# Makes/grabs a matrix from the points we feed it. coordinatesMade <- coordinates(grabOutlierPoints)
# Make a polygon that represents the shape of our shapes for Kriging. poly <- sp::Polygon(coordinatesMade)
# Prepare/make the data ready to be made into a spatial polygon that will be used for Kriging.
polygonWithBoundary <- Polygons(list(poly), ID="Minimum Boundary") list(polygonWithBoundary)
spatialPolygonWithBoundary <- SpatialPolygons(list(polygonWithBoundary))
#Assemble a grid that will be used as locations for Kriging. grid <- makegrid(spatialPolygonWithBoundary, cellsize = 0.12) coordinates(grid) <- ~ x1+x2
#Overlap the polygon made previously and the grid to form a grid in the shape of the polygon.
gridAsPolygon <- sp::over(grid , spatialPolygonWithBoundary) predictionPolygon <- grid[!is.na(gridAsPolygon),]
#Make the Kriging interpolation for the 5 chosen states. KrigeModel <- krige(LogArithmeticMean~1, modelfiveChosenStates, predictionPolygon, model=ArithmeticMean.fit)
#Plot the Kriging intepolation using a SPPLOT.

spplot(KrigeModel, "var1.pred",xlab = "Longitude", scales = list(draw = TRUE), ylab = "Latitude",
       main = list(label = "Kriging Prediction For Nitrogen dioxide/NO2 (Logged)"), key.space=list(x=1.033,y=.95,corner=c(0,1),label = "title"),auto.key = list(title = "Prediction in PPB"))
#Cross Validation for Kriging model.
LOOCVKrige <-krige.cv(LogArithmeticMean~1, modelfiveChosenStates, model=ArithmeticMean.fit )
#Perform Root Mean Squared Error to find/measure the possible error. RMSEKrigeResidual(LOOCVKrige@data$residual)
#============================Spatial Clustering=================================
#Grab epa pollution dataset for 2013 fiveChosenStates <- read_csv('a3Data.csv')
#Grab all the states that we need Oklahoma, Texas, Louisiana, New Mexico and Arkansas
fiveChosenStates <- fiveChosenStates %>%
  filter(`StateCode` == '48' | `StateCode` == '22' |`StateCode` == '5' | `StateCode` == '40' | `StateCode` == '35')
#Grabs all pollutant of interest. modelfiveChosenStates <- fiveChosenStates %>%
filter(`ParameterName` == 'Nitrogen dioxide (NO2)' )
#Code above removes duplicate locations krieging does not like duplicated!!! modelfiveChosenStates <- modelfiveChosenStates[! duplicated(modelfiveChosenStates$SiteNum),]
#Done so the x is longitude and y is latitude. coordinates(modelfiveChosenStates) = c('Longitude','Latitude')
#Create voronoi polygons to define individual areas around points so contiguity based skater can be done.
voronoi <- voronoi(modelfiveChosenStates)
#Setup the data to perform the skater algorithm using the original point locations and the variable ArithmeticMean.
voronoi.nb <- poly2nb(voronoi)
lcosts <- nbcosts(voronoi.nb,modelfiveChosenStates$ArithmeticMean)
nb.w <- nb2listw(voronoi.nb, lcosts, style="B") mst.voronoi <- mstree(nb.w,5)
#Run the skater algorithm to find the clusters. skaterClusterFinder <- skater(mst.voronoi[,1:2], modelfiveChosenStates$ArithmeticMean, 2)
#Plot the results of skater algorithm to show the clusters.

plot(skaterClusterFinder, coordinates(voronoi), cex.circles=0.125, cex.lab=0.001, col=c("red","blue","green"))
legend("bottomleft", title="Individual Clusters", legend = c("Cluster 1 (Red)", "Cluster 2 (Green)", "Cluster 3 (Blue)"),col = c(rgb(0.25,0.4,0.1,0.7) ))
title("Nitrogen dioxide (NO2) Spatial Clustering",line = -2) axis(1)
axis(2)

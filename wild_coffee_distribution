library(readxl)
library(dismo)
library(raster)
library(maptools)
data(wrld_simpl)
library(ggplot2)
library(rgdal)

# Download the occurrences data from gbif
arabica <- gbif("Coffea","arabica", geo=FALSE) #download occurrences
dups2 <- duplicated(arabica[, c('lon', 'lat')]) 
arabica <- arabica[!dups2, ] #remove duplicate occurrences
arabica <- subset(arabica, !is.na(lon) & !is.na(lat))
arabica <- arabica[arabica$lon > -20  & arabica$lat > -20, ]
ar <- arabica[arabica$lon < 40  & arabica$lat < 40, ]
dim(ar)

# Map arabica
plot(wrld_simpl)
box()
# add the points
points(ar$lon, ar$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(ar$lon, ar$lat, col='red', cex=0.75)

View(ar)
colnames(ar)
arabica_not_clean <- ar[c("acceptedScientificName","lat","lon","basisOfRecord","year","country","cloc","collectionCode","datasetName")]

# Write csv and maually check the coordinates, remove something in the sea and cultivated
write.csv(arabica_not_clean,"arabica_not_clean.csv")

# import clean arabica dataset
wild_arabica <- read_excel("Desktop/coffee coordinates.xlsx", sheet = "Arabica_clean")

# plot clean Arabic 
plot(wrld_simpl, xlim=c(-20,40), ylim=c(-40,40), axes=TRUE, col="light yellow") 
box()
points(wild_arabica$lon, wild_arabica$lat, col='red', cex=0.15)

  
# Current and future climate
  
current_climate <- getData('worldclim', var='bio', res=2.5)
future_climate <- getData('CMIP5', var='bio', res=2.5, rcp=85, model='AC', year=70) # How to chose the best model?
names(future_climate) <- names(current_climate)
ext <- extent(-15, 56, -35, 36)

# Split data into training and testing with 5 fold cross validation

coordinates(wild_arabica) <- ~lon+lat
r <- raster(extent(wild_arabica)+1, res=0.25)
wild_arabica <- gridSample(wild_arabica, r, n=1)
groupf <- kfold(wild_arabica, 5)
pres_train_arabica <- wild_arabica[groupf != 1, ]
pres_test_arabica <- wild_arabica[groupf == 1, ]

# Set background points

set.seed(10)
backgf <- randomPoints(current_climate, n=5000, ext=ext, extf = 1.25)
colnames(backgf) = c('lon', 'lat')

# Fit current & future climate with points

maxent_arabica <- maxent(current_climate, pres_train_arabica, backgf) #error
max_arabica_current <- predict(current_climate, maxent_arabica, ext=ext, progress='')
max_arabica_future <- predict(future_climate, maxent_arabica, ext=ext, progress='')

# Canephora

canephora <- gbif("Coffea","canephora", geo=FALSE) #download occurrences
dups2 <- duplicated(canephora[, c('lon', 'lat')]) 
canephora <- canephora[!dups2, ] #remove duplicate occurrences
canephora <- subset(canephora, !is.na(lon) & !is.na(lat))
canephora <- canephora[canephora$lon > -20  & canephora$lat > -20, ]
ca <- canephora[canephora$lon < 40  & canephora$lat < 40, ]
dim(ca)

# Map canephora
plot(wrld_simpl, xlim=c(-20,40), ylim=c(-40,40), axes=TRUE, col="light yellow") 
box()
# add the points
points(ca$lon, ca$lat, col='orange', pch=20, cex=0.75)
points(ca$lon, ca$lat, col='red', cex=0.75)

canephora_not_clean <- ca[c("acceptedScientificName","lat","lon","basisOfRecord","year","country","cloc","collectionCode","datasetName")]
View(canephora_not_clean)

# Write csv and maually check the coordinates, remove something in the sea and cultivated
write.csv(canephora_not_clean,"canephora_not_clean.csv")

wild_canephora <- read_excel("Desktop/coffee coordinates.xlsx", sheet = "Canephora")
dim(wild_canephora)

# plot clean Canephora 
plot(wrld_simpl, xlim=c(-20,40), ylim=c(-40,40), axes=TRUE, col="light yellow") 
box()
points(wild_canephora$lon, wild_canephora$lat, col='red', cex=0.15)

# Split data into training and testing with 5 fold cross validation

coordinates(wild_canephora) <- ~lon+lat
r1 <- raster(extent(wild_canephora)+1, res=0.25)
wild_canephora <- gridSample(wild_canephora, r, n=1)
groupc <- kfold(wild_canephora, 5)  #error
pres_train_canephora <- wild_canephora[groupf != 1, ]
pres_test_canephora <- wild_canephora[groupf == 1, ]

maxent_canephora <- maxent(current_climate, pres_train_canephora, backgf)
max_canephora_current <- predict(current_climate, maxent_canephora, ext=ext, progress='')
max_canephora_future <- predict(future_climate, maxent_canephora, ext=ext, progress='')


# eugenioides 

eugenioides <- gbif("Coffea","eugenioides", geo=FALSE) #download occurrences
dups2 <- duplicated(eugenioides[, c('lon', 'lat')]) 
eugenioides <- eugenioides[!dups2, ] #remove duplicate occurrences
eugenioides <- subset(eugenioides, !is.na(lon) & !is.na(lat))
eugenioides <- eugenioides[eugenioides$lon > -20  & eugenioides$lat > -20, ]
eu <- eugenioides[eugenioides$lon < 40  & eugenioides$lat < 40, ]
dim(eu)

# Map 
plot(wrld_simpl, xlim=c(-20,40), ylim=c(-40,40), axes=TRUE, col="light yellow") 
box()
# add the points
points(eu$lon, eu$lat, col='orange', pch=20, cex=0.75)
points(eu$lon, eu$lat, col='red', cex=0.75)

eugenioides_not_clean <- eu[c("acceptedScientificName","lat","lon","basisOfRecord","year","country","cloc","collectionCode","datasetName")]
View(eugenioides_not_clean)

# Write csv and maually check the coordinates, remove something in the sea and cultivated
write.csv(eugenioides_not_clean,"eugenioides_not_clean.csv")

wild_eugenioides <- read_excel("Desktop/coffee coordinates.xlsx", sheet = "Eugenioides")


# Map 
plot(wrld_simpl, xlim=c(-20,40), ylim=c(-40,40), axes=TRUE, col="light yellow") 
box()
# add the points
points(wild_eugenioides$lon, wild_eugenioides$lat, col='orange', pch=20, cex=0.75)
points(wild_eugenioides$lon, wild_eugenioides$lat, col='red', cex=0.75)

# Split data into training and testing with 5 fold cross validation

coordinates(wild_eugenioides) <- ~lon+lat
r2 <- raster(extent(wild_eugenioides)+1, res=0.25)
wild_eugenioides <- gridSample(wild_eugenioides, r, n=1)
groupe <- kfold(wild_eugenioides, 5)  #error
pres_train_eugenioides <- wild_eugenioides[groupf != 1, ]
pres_test_eugenioides <- wild_eugenioides[groupf == 1, ]

maxent_eugenioides <- maxent(current_climate, pres_train_eugenioides, backgf)
max_eugenioides_current <- predict(current_climate, maxent_eugenioides, ext=ext, progress='')
max_eugenioides_future <- predict(future_climate, maxent_eugenioides, ext=ext, progress='')


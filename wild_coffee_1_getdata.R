
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	setwd("c:/github/sdmcoffee")
} else {
	setwd("")
}

dir.create("data", FALSE, FALSE)

library(geodata)
fgbif <- "data/gbif_arabica.rds"
if (!file.exists(fgbif)) {
	arabica <- sp_occurrence("Coffea","arabica", geo=FALSE)
	saveRDS(arabica, fgbif)
} else {
	arabica <- readRDS(fgbif)
}

dups2 <- duplicated(arabica[, c('lon', 'lat')]) 
arabica <- arabica[!dups2, ] #remove duplicate occurrences
arabica <- subset(arabica, !is.na(lon) & !is.na(lat))
arabica <- arabica[arabica$lon > -20  & arabica$lat > -20, ]
ar <- arabica[arabica$lon < 40  & arabica$lat < 40, ]
dim(ar)

w <- geodata::world(path="data")
plot(w)

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


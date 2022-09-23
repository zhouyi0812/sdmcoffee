
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

# points that are in the ocean
v <- vect(ar, c("lon", "lat"))
e <- extract(w, v)
i <- which(is.na(e$GID_0))
fix <- values(v[i, ])

fix[,"cloc"]
#[1] "São Tomé and Príncipe"                                              
#[2] "Faz. Curralinho, margem esquerda do rio Corumbá., Brazil"           
#[3] "Barra do rio Santo Antônio, próximo á futura balsa., Brazil"        
#[4] "Fazenda Sucupira. Mata de galeria (córrego do Açudinho II)., Brazil"
#[5] "Aburi, Ghana"                                                       
#[6] "Legon, Ghana"                                                       
#[7] "Gran Canaria, Bañaderos, Spain"                                     
 

## better to fix than to remove... 
##v <- v[-i,]
##ar <- ar[-i, ]

#arabica_not_clean <- ar[c("acceptedScientificName","lat","lon","basisOfRecord","year","country","cloc","collectionCode","datasetName")]

# Write csv and maually check the coordinates, remove something in the sea and cultivated
#write.csv(arabica_not_clean,"arabica_not_clean.csv")

# import clean arabica dataset
#wild_arabica <- read_excel("Desktop/coffee coordinates.xlsx", sheet = "Arabica_clean")


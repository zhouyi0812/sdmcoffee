
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	setwd("c:/github/sdmcoffee")
} else {
	setwd("")
}

dir.create("data", FALSE, FALSE)
library(geodata)

spp <- c("arabica", "canephora", "eugenioides")
for (sp in spp) {
	print(sp)
	fname <- paste0("data/gbif_", sp, "_1.rds")
	if (!file.exists(fname)) {
		d <- sp_occurrence("Coffea", sp, geo=FALSE)
		saveRDS(d, fname)
	} else {
		d <- readRDS(fname)
	}
	fname2 <- paste0("data/gbif_", sp, "_2.rds")
	if (!file.exists(fname2)) {
		i <- duplicated(d[, c('lon', 'lat')]) 
		d <- d[!i, ]
		d <- subset(d, !is.na(lon) & !is.na(lat))
		d <- d[d$lon > -20 & d$lat > -20, ]
		d <- d[d$lon < 40 & d$lat < 40, ]
		saveRDS(d, fname2)
	}
}


w <- geodata::world(path="data")
plot(w)
points(d$lon, d$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(d$lon, d$lat, col='red', cex=0.75)

# points that are in the ocean
v <- vect(d, c("lon", "lat"))
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



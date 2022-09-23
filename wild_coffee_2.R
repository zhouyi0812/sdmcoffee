this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	setwd("c:/github/sdmcoffee")
} else {
	setwd("")
}

library(dismo)

current_climate <- getData('worldclim', var='bio', res=2.5)
future_climate <- getData('CMIP5', var='bio', res=2.5, rcp=85, model='AC', year=70) # How to chose the best model?

names(future_climate) <- names(current_climate)
ext <- extent(-15, 56, -35, 36)


spp <- c("arabica", "canephora", "eugenioides")
for (sp in spp) {

	f <- paste0("data/gbif_", sp, "_2.rds")
	d <- readRDS(f)

	coordinates(d) <- ~lon+lat
	r <- raster(extent(d)+1, res=0.25)
	d <- gridSample(d, r, n=1)
	groupf <- kfold(d, 5)
	pres_train <- d[groupf != 1, ]
	pres_test <- d[groupf == 1, ]

 
# Current and future climate
  
	# Split data into training and testing with 5 fold cross validation
	# Set background points

	set.seed(10)
	backgf <- randomPoints(current_climate, n=5000, ext=ext, extf = 1.25)
	colnames(backgf) = c('lon', 'lat')

	# Fit current & future climate with points

	maxent_model <- maxent(current_climate, pres_train, backgf) #error
	fcur <- paste0("output/", sp, "_current.tif")
	ffut <- paste0("output/", sp, "_future.tif")
	
	max_current <- predict(current_climate, maxent_model, ext=ext, filename=fcur)
	max_future <- predict(future_climate, maxent_model, ext=ext, filename=ffut)

}


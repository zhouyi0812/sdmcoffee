this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	setwd("c:/github/sdmcoffee")
} else {
	setwd("")
}

dir.create("climate", FALSE, FALSE)
dir.create("output", FALSE, FALSE)

library(dismo)


current_climate <- getData('worldclim', var='bio', res=2.5, path="climate")
future_climate <- getData('CMIP5', var='bio', res=2.5, rcp=85, model='AC', year=70, path="climate") # How to chose the best model?

names(future_climate) <- names(current_climate)
ext <- extent(-15, 56, -35, 36)

validate <- function(d) {
	set.seed(10)
	groupf <- kfold(d, 5)


}

spp <- c("arabica", "canephora", "eugenioides")
for (sp in spp) {

	cat(sp); flush.console()
	
	f <- paste0("data/gbif_", sp, "_2.rds")
	d <- readRDS(f)

	coordinates(d) <- ~lon+lat

	backgf <- randomPoints(current_climate, n=5000, ext=ext, extf = 1.25)
	colnames(backgf) = c('lon', 'lat')


	r <- raster(res=0.25)
	d <- gridSample(d, r, n=1)
	#v <- validate(d, backgf)

	maxent_model <- maxent(current_climate, pres_train, backgf)
	
	fcur <- paste0("output/", sp, "_current.tif")
	ffut <- paste0("output/", sp, "_future.tif")

	max_current <- predict(current_climate, maxent_model, ext=ext, filename=fcur)
	max_future <- predict(future_climate, maxent_model, ext=ext, filename=ffut)

}


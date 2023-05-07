library(dismo)
library(raster)
library(dplyr)

setwd("/Users/yizhou/sdmcoffee")

current_climate <- getData('worldclim', var='bio', res=2.5, path="climate")
ext <- extent(-15, 56, -35, 36)

models <- c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
#rcps <- c(26, 45, 60, 85) #keep 45/85
head(models)
length(models)

f <- paste0("data/gbif_arabica_2.rds") # You can replace 'arabica' with other species if needed
d <- readRDS(f)
head(d)

coordinates(d) <- ~lon+lat

backgf <- randomPoints(current_climate, n=5000, ext=ext, extf = 1.25)
colnames(backgf) = c('lon', 'lat')

auc_matrix <- matrix(NA, nrow=length(models), ncol=1, dimnames=list(models, "AUC"))

for (model in models) {
  #for (rcp in rcps) {
  current_climate <- tryCatch(
    getData('worldclim', var='bio', res=2.5, rcp=45, model=models, year=70, path="climate"),
    error = function(e) NULL
  )
  
  maxent_model <- maxent(current_climate, d, backgf)
  pred_probs <- predict(current_climate, maxent_model, newdata=d)
  auc <- dismo::auc(p=d, x=pred_probs)
  
  auc_matrix[model, "45"] <- auc
}
}


# Find the best model and RCP based on the highest AUC
best_combination <- which(auc_matrix == max(auc_matrix, na.rm = TRUE), arr.ind = TRUE)
best_model <- rownames(best_combination)
best_rcp <- colnames(best_combination)



library(pROC)

ext <- extent(-15, 56, -35, 36)
models <- c("AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
rcps <- c()
auc_values <- numeric(length(models))
#current_climate <- getData('worldclim', var='bio', res=2.5, path="climate")

for (i in 1:length(models)) {
  current_climate <- getData('worldclim', var='bio', res=2.5, rcp=45, path="climate", model = models[i])
  # calculate AUC between current_climate and your data
  predictions <- predict(model, current_climate)
}
# select the model with the highest AUC
best_model <- models[which.max(auc_values)]




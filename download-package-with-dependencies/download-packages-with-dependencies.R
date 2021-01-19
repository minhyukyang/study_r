# - ref. : https://ahyangs.com/12


download_dir <- "d:/temp"

pkgs_list <- c("robustfa","ggplot2","robustfa","ggplot2","GPArotation","gplots","RColorBrewer","semPlot","stats","MASS","xgboost","caret","glmnet","rpart","nnet","zoo","ica")

getDependencies <- function(packs){
  
  dependencyNames <- unlist(
    tools::package_dependencies(packages = packs, db = available.packages(),
                                which = c("Depends", "Imports"),
                                recursive = TRUE))
  packageNames <- union(packs, dependencyNames)
  print(packageNames)
}

# Get dependencies
packages <- getDependencies(pkgs_list)

pkgs_list <- "testthat"

for(pkgs_name in pkgs_list){
  
  pkgs_len <- length(getDependencies(pkgs_name))
  
  for(pkgs_len)
  
  cat("- pkgs_name :", pkgs_name, "\t", pkgs_len, "\n")
}


# Download package files
setwd(download_dir)
download.packages(pkgs = packages, destdir = getwd(), type = "source")

download.packages(pkgs = 'testthat', destdir = getwd(), type = "source")


getDependencies <- function(packs){
  
  dependencyNames <- unlist(
    tools::package_dependencies(packages = packs, db = available.packages(),
                                which = c("Depends", "Imports"),
                                recursive = TRUE))
  packageNames <- union(packs, dependencyNames)
  print(packageNames)
}

packs <- "testthat"

download.packages(pkgs = getDependencies("testthat"), destdir = getwd(), type = "source")


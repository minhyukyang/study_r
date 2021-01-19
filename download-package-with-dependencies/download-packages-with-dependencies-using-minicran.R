# - https://stackoverflow.com/questions/51769231/r-how-do-i-install-packages-and-dependencies-offline

# Download packages
library(miniCRAN)

download_dir <- "d:/local_cran"

pkgs <- c("robustfa","ggplot2","GPArotation","gplots","RColorBrewer","semPlot","MASS","xgboost","caret","glmnet","rpart","nnet","zoo","ica")

pkgList <- pkgDep(pkgs, type = "source", suggests = FALSE)
pkgList

makeRepo(pkgList, path=download_dir, type = "source")

# Upload packages

# Execute on RStudio Server (CentOS7)

minicran_path <- "/dsteam/miniCRAN"

install.packages("robustfa", 
                 repos=paste0("file://", minicran_path), 
                 type="source")

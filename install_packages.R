# install_packages.R
packages <- c(
  "tidyverse", "dplyr", "ggplot2", "reshape2", "caret", "broom",
  "GGally", "ggpubr", "glmnet", "cluster", "factoextra", "dbscan",
  "randomForest", "xgboost", "fmsb"
)

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

lapply(packages, install_if_missing)

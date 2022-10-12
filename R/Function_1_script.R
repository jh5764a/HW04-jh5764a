library(tidyverse)

library(ggplot2)

Function_1 <- function(dataset){
  column_names <- c(colnames(dataset))
  class_data <- c(sapply(dataset,class))
  unique_data <- c(sapply(dataset, function(z) NROW(unique(z))))
  na_data <- c(sapply(dataset, function(y) sum(length(which(is.na(y))))))
  data.frame(column_names,class_data,unique_data,na_data)
}

Function_1(iris)



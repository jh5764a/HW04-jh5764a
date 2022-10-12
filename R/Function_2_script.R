


library(tidyr)
library(ggplot2)
library(dplyr)



Function_2 <- function(dataset, class_name){
  class_data <- c(sapply(dataset,class))

  if(!(class_name %in% class_data))
    stop("the class name you've chosen does not exist in this dataset")

  if (class_name == "numeric"){
    dataset1 <- select(dataset, where(is.numeric))

    if (length(dataset1 > 0)) {
      dataset1 %>%
        pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot(dataset1, aes(x = values))+
        geom_histogram()+
        ggtitle(paste0("Variables in dataset of class ", class_name))+
        facet_wrap(var ~.)->Graph1

    }

    else {
      print("there are no class numerics in this dataset.")
    }
  }

  else if (class_name == "double"){
    dataset1 <- select(dataset, where(is.double))

    if (length(dataset1 > 0)) {
      dataset1 %>%
        pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot(dataset1, aes(x = values))+
        geom_histogram()+
        ggtitle(paste0("Variables in dataset of class ", class_name))+
        facet_wrap(var ~.)->Graph1

    }

    else {
      print("there are no class double in this dataset.")
    }
  }
  else if (class_name == "integer"){
    dataset1 <- select(dataset, where(is.integer))

    if (length(dataset1 > 0)) {
      dataset1 %>%
        pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot(dataset1, aes(x = values))+
        geom_histogram()+
       ggtitle(paste0("Variables in dataset of class ", class_name))+
       facet_wrap(var ~.)->Graph1

    }

    else {
      print("there are no class integer in this dataset.")
    }
  }
  else if (class_name == "character"){
    dataset1 <- select(dataset, where(is.character))

    if (length(dataset1 > 0)) {
      dataset1 %>%
        pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot(dataset1, aes(x = values))+
        geom_bar()+
        ggtitle(paste0("Variables in dataset of class ", class_name))+
        facet_wrap(var ~.)->Graph1

    }
    else if (length(dataset1 > 7)) {
      dataset1 %>%
        pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot(dataset1, aes(x = values))+
        geom_bar()+
        ggtitle(paste0("Variables in dataset of class ", class_name))+
        facet_wrap(var ~.)+
        coord_flip()->Graph1

    }
    else {
      print("there are no class character in this dataset.")
    }
  }
  else if (class_name == "factor"){
    dataset1 <- select(dataset, where(is.factor))

    if (length(dataset1 <= 7) && length(dataset1 > 0)) {
      dataset1 %>%
        tidyr::pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

     ggplot(dataset1, aes(x = values))+
        geom_bar()+
       ggtitle(paste0("Variables in dataset of class ", class_name))+
        facet_wrap(var ~.)->Graph1

    }
    else if (length(dataset1 > 7)) {
      dataset1 %>%
        pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot(dataset1, aes(x = values))+
        geom_bar()+
        ggtitle(paste0("Variables in dataset of class ", class_name))+
        facet_wrap(var ~.)+
        coord_flip()->Graph1

    }
    else {
      print("there are no class factor in this dataset.")
    }
  }
  else if (class_name == "logical"){
    dataset1 <- select(dataset, where(is.logical))

    if (length(dataset1 <= 7) && length(dataset1 > 0)) {
      dataset1 %>%
        pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot(dataset1, aes(x = values))+
        geom_bar()+
        ggtitle(paste0("Variables in dataset of class ", class_name))+
        facet_wrap(var ~.)->Graph1

    }
    else if (length(dataset1 > 7)) {
      dataset1 %>%
        pivot_longer(cols = c(1:length(dataset1)),names_to = "var", values_to = "values") -> dataset1

      ggplot(dataset1, aes(x = values))+
        geom_bar()+
        ggtitle(paste0("Variables in dataset of class ", class_name))+
        facet_wrap(var ~.)+
        coord_flip()->Graph1

    }
    else {
      print("there are no class logical in this dataset.")
    }

  }
  Graph1
}
Function_2(iris, "numeric")

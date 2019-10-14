# Read data
library(foreign)
library(tibble)
library(tidyverse)
#setwd("C:/Users/abrenes/Downloads")
data<- read.spss("./data/data.sav", to.data.frame=TRUE, use.value.labels = TRUE)
attr(data, "variable.labels")
summary(data)
names(data)

# Change to tible table
data<-as_tibble(data)
data %>% summary()

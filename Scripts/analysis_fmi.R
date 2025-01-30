rm(list = ls())

#libs================
library(tidyverse)
library(ggplot2)

#load & filter data===========
load(file = "Data/FMI/data_fmi.RData") 
load(file = "Data/FMI/indicadores_fmi.RData")
load(file = "Data/paises.RData")

data_lac <- data_fmi |> 
  left_join(select(paises, ISO, lac), by = "ISO") |> 
  filter(lac == T)
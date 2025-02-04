rm(list = ls())

#libs======================
library(readxl)
library(tidyverse)

#custom functions==========
wide_to_long <- function(data, wef) {
  data |>
    pivot_longer(-c("ISO", "WEO Country Code", "WEO Subject Code", "Country", 
                    "Subject Descriptor", "Subject Notes", "Units", "Scale", 
                    "Estimates Start After", 
                    "Country/Series-specific Notes"),
                 names_to = "Fecha", 
                 values_to = "Valor") |> 
    mutate('Fecha Reporte' = as.Date(wef), 
           Fecha = as.numeric(Fecha))
}
#Load data=================
load("Data/FMI/data_fmi.rdata")

#Abrir archivos y guardar como excel workbook

data_apr24 <- read_xlsx(path = "Data/FMI/WEOApr2024all.xlsx", 
                        na = c("n/a", "--"))
data_oct24 <- read_xlsx(path = "Data/FMI/WEOOct2024all.xlsx", 
                        na =  c("n/a", "--"))

#convertir a formato long======

data_apr24_long <- wide_to_long(data_apr24, "2024-04-01")
data_oct24_long <- wide_to_long(data_oct24, "2024-10-01")

#Unir data ===========
data_fmi <- data_apr24_long |> 
  bind_rows(data_oct24_long) |> 
  na.omit()

save(data_fmi, file = "Data/FMI/data_fmi.Rdata")

#Lista de paises e indicadores=============

lac <- c("ARG", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", 
         "COL", "CRI", "DMA", "DOM", "ECU", "SLV", "GTM", 
         "GUY", "HTI", "HND", "JAM", "MEX", "NIC", "PAN", 
         "PRY", "PER", "PRI", "SUR", "URY", "VEN", "TTO")

latam <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM",
           "ECU", "SLV", "GTM", "HND", "MEX", "NIC", "PAN", 
           "PRY", "PER", "PRI", "URY", "VEN")

hfhi <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM",
          "SLV", "GTM", "HND", "MEX", "NIC","PRY", 
          "PER", "TTO")

length(hfhi)

paises <- data_fmi |> 
  select(`WEO Country Code`, ISO, Country) |> 
  distinct() |> 
  na.omit() |> 
  mutate(lac = ifelse(ISO %in% lac, T, F), 
         latam = ifelse(ISO %in% latam, T, F), 
         hfhi = ifelse(ISO %in% hfhi, T, F))

save(paises, file = "Data/paises.Rdata")

indicadores_fmi <- data_fmi |> 
  select(`WEO Subject Code`, `Subject Descriptor`, 
         `Subject Notes`, Units, Scale) |> 
  distinct() |> 
  na.omit()

save(indicadores_fmi, file = "Data/FMI/indicadores_fmi.Rdata")
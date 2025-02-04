#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#libs================
library(shiny)
library(tidyverse)
library(ggplot2)
library(gt)
library(rsconnect)
rsconnect::writeManifest()


#load & filter data===========
load(file = "data_fmi.RData") 
load(file = "indicadores_fmi.RData")
load(file = "paises.RData")

data_lac <- data_fmi |> 
  left_join(select(paises, ISO, hfhi), by = "ISO") |> 
  filter(hfhi == T)

#color palette================
primary <- "#1134A6"
secondary <- "#1e8ad3"
light <- "#d3f4fb"
highlight <- "#10bc10"
white <- "#ffffff"
dark <- "#1d3537"
info <- "#1aa53f"
success <- "#009758"
warning <- "#ddc800"
danger <- "#ea1d00"

#Funciones====================

tabla_indicador <- function(indicador_id, weo_report, min_year, max_year, percent = F) {
  
  divisor <- 1
  if(percent == T) {
    divisor <- 100
  }
  
  indicador <- indicadores_fmi |> 
    filter(`WEO Subject Code` == indicador_id) |> 
    select(`Subject Descriptor`)
  
  indicador_unidad <- indicadores_fmi |> 
    filter(`WEO Subject Code` == indicador_id) |> 
    select(Units)
  indicador_notas <- indicadores_fmi |> 
    filter(`WEO Subject Code` == indicador_id) |> 
    select(`Subject Notes`)
  
  tab <- data_lac |> 
    filter(`Fecha Reporte` == weo_report &
             `WEO Subject Code` == indicador_id &
             Fecha > min_year & Fecha <= max_year)  |> 
    mutate(Valor = Valor / divisor) |> 
    select(Country, Fecha, Valor, `Estimates Start After`) |>
    pivot_wider(id_cols = c(Country, `Estimates Start After`) , 
                names_from = Fecha, 
                values_from = Valor) |>
    arrange(desc(`2025`)) |> 
    gt(rowname_col = "Country") |> 
    tab_header(title = indicador, 
               subtitle = indicador_unidad) |>
    tab_footnote(indicador_notas) |> 
    tab_style(style =
                list(cell_fill(color = light), 
                     cell_text(color = primary, 
                               weight = "bold")), 
              locations = cells_title()
    ) |> 
    tab_style(style =
                list(cell_text(
                  style = "italic",
                  size = "small")
                ), 
              locations = cells_footnotes())
  
  if(percent == T) {
    tab <- tab |> 
      fmt_percent(-2,
                  decimals = 1)
  } else {    
    
    tab <- tab |>
      fmt_number(-2,
                 decimals = 1)
  }
  
  return(tab)  
}


# APP==============

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

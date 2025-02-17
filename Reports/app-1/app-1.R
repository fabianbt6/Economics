
#libs================
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(gt)
library(rsconnect)

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

#Funciones ----

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


# APP ----

# Define UI for application that draws a histogram ----

ui <- page_sidebar(
  # App title ----
  title = "Hello Shiny!",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
  # Output: Histogram ----
  plotOutput(outputId = "distPlot")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}
  
shinyApp(ui = ui, server = server)


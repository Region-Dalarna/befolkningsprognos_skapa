
library(shiny)
library(bslib)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(tidyr)
library(purrr)
library(dbplyr)
library(scales)
library(openxlsx)
library(shinyjs)

source("R/kontext.R")

source("R/domain/prognos_kor.R")
source("R/domain/data_provider.R")
source("R/domain/risk_kor.R")
source("R/domain/prognos_enskild.R")
source("R/domain/prognos_regional.R")


source("R/konfiguration/ui_risk.R")
source("R/konfiguration/ui_revidera_komponenter.R")
source("R/konfiguration/ui_config.R")
source("R/konfiguration/server_config.R")

source("R/resultat/diagram_funktioner.R")
source("R/resultat/ui_resultat.R")
source("R/resultat/server_resultat.R")

source("R/domain/geografi_data.R")

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

geo_db <- tbl(
  shiny_uppkoppling_las("oppna_data"),
  dbplyr::in_schema("scb", "totfolkmangd")
) %>%
  distinct(regionkod, region) %>%
  filter(regionkod != "00") %>%
  collect()

geografier_i_data <- geo_db %>%
  dplyr::pull(region) %>%
  sort()

lan_i_data <- geo_db %>%
  filter(nchar(regionkod) == 2) %>%
  dplyr::pull(region) %>%
  sort()

ui <- page_navbar(
  title = "Befolkningsprognos",
  id    = "huvud_nav",


  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    )
  ),

  nav_panel(
    "1. Konfiguration",
    ui_config(app_kontext, lan_i_data, geografier_i_data)
  ),

  nav_panel(
    "2. Resultat",
    conditionalPanel(
      condition = "output.fas == 'resultat'",
      ui_resultat(app_kontext)
    ),
    conditionalPanel(
      condition = "output.fas != 'resultat'",
      div(
        style = "padding: 2rem; color: #666;",
        h4("Resultat är inte tillgängliga än"),
        p("Genomför först en prognoskörning.")
      )
    )
  )
)

server <- function(input, output, session) {

  ## 2.1 – exponera fas till UI
  output$fas <- reactive({
    app_kontext$fas
  })
  outputOptions(output, "fas", suspendWhenHidden = FALSE)

  ## koppla in respektive server-del
  server_config(input, output, session, app_kontext)
  server_resultat(input, output, session, app_kontext)
}

shinyApp(ui, server)

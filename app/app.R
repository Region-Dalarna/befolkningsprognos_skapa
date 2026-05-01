
library(shiny)
library(bslib)

source("R/kontext.R")

source("R/konfiguration/ui_risk.R")
source("R/konfiguration/ui_revidera_komponenter.R")
source("R/konfiguration/ui_config.R")
source("R/konfiguration/server_config.R")


source("R/resultat/ui_resultat.R")
source("R/resultat/server_resultat.R")

source("R/domain/geografi_data.R")

ui <- page_navbar(
  title = "Befolkningsprognos",


  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),

  nav_panel(
    "1. Konfiguration",
    ui_config(app_kontext)
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

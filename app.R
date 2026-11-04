
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

source("R/domain/prognos_kor.R")
source("R/domain/data_provider.R")
source("R/domain/risk_kor.R")
source("R/domain/prognos_enskild.R")
source("R/domain/prognos_regional.R")


source("R/konfiguration/ui_risk.R")
source("R/konfiguration/ui_revidera_komponenter.R")
source("R/konfiguration/installningar_io.R")
source("R/konfiguration/ui_config.R")
source("R/konfiguration/server_config.R")

source("R/resultat/diagram_funktioner.R")
source("R/resultat/ui_resultat.R")
source("R/resultat/server_resultat.R")

source("R/domain/geografi_data.R")

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

# ---- Telemetri (användningsstatistik, se Adminportalen) --------------------
telemetry <- skapa_telemetry("befolkningsprognos_skapa")

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

# ---- Region Dalarnas grafiska profil: petrolblå + Poppins (självhostat,
# samma typsnittsfiler som brott/utbildning/branscher/befolkning) --------
rd_font <- font_collection(
  font_face(family = "Poppins", weight = 300, src = "url('fonts/poppins-v24-latin-300.woff2') format('woff2')"),
  font_face(family = "Poppins", weight = 400, src = "url('fonts/poppins-v24-latin-regular.woff2') format('woff2')"),
  font_face(family = "Poppins", weight = 500, src = "url('fonts/poppins-v24-latin-500.woff2') format('woff2')"),
  font_face(family = "Poppins", weight = 600, src = "url('fonts/poppins-v24-latin-600.woff2') format('woff2')"),
  font_face(family = "Poppins", weight = 700, src = "url('fonts/poppins-v24-latin-700.woff2') format('woff2')"),
  "Arial", "Helvetica", "sans-serif"
)

APP_TITEL <- "Skapa befolkningsprognoser"
APP_BESKRIVNING <- "Verktyg för att konfigurera och ta fram egna befolkningsprognoser för valfri geografi."

ui <- page_navbar(
  title = APP_TITEL,
  id    = "huvud_nav",

  theme = bs_theme(
    primary      = "#158daf",
    base_font    = rd_font,
    heading_font = rd_font
  ),
  navbar_options = navbar_options(bg = "#158daf", theme = "dark"),

  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),

      # använd telemetry-paketet om det finns konfigurerat på maskinen (kommer att fungera på servrarna men ignoreras om man kör appen lokalt)
      telemetri_ui(telemetry)
    )
  ),

  nav_panel(
    "1. Konfiguration",
    ui_config(lan_i_data, geografier_i_data)
  ),

  nav_panel(
    "2. Resultat",
    conditionalPanel(
      condition = "output.fas == 'resultat'",
      ui_resultat()
    ),
    conditionalPanel(
      condition = "output.fas != 'resultat'",
      div(
        style = "padding: 2rem; color: #666;",
        h4("Resultat är inte tillgängliga än"),
        p("Genomför först en prognoskörning.")
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://www.regiondalarna.se",
      target = "_blank",
      tags$img(src = "logo_liggande_fri_vit.png", alt = "Region Dalarna", style = "height: 28px;")
    )
  )
)

server <- function(input, output, session) {

  telemetri_server(telemetry, navigation_id = "huvud_nav", forsta_flik = "1. Konfiguration")

  ## Per-session-tillstånd: skapas en gång per ansluten användare
  ## så att olika webbläsare/sessioner inte delar samma reactiveValues.
  app_kontext <- reactiveValues(
    konfiguration = NULL,              # konfiguration (ersätter konfig-R-fil)
    resultat      = NULL,              # prognosresultat i minnet
    fas           = "konfiguration"    # "konfiguration" | "korning" | "resultat"
  )

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

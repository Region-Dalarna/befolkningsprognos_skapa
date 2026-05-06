ui_config <- function(app_kontext, lan_i_data, geografier_i_data) {

  geo <- hamta_geografi_val()
  lan_val <- lan_i_data
  enskild_val <- geografier_i_data

  tagList(

    div(
      class = "config-scroll",

      accordion(
        open = 1,

        accordion_panel(
          title = "🌍 Geografi & grundläggande val",
          fluidRow(
            column(5,
                   card(
                     card_header("Geografi"),
                     conditionalPanel(
                       "input.prognostyp == 'regional'",
                       selectizeInput(
                         "geografi_regional",
                         "Välj län",
                         choices = lan_val,
                         selected = "Dalarnas län"
                       )
                     ),
                     conditionalPanel(
                       "input.prognostyp == 'enskild'",
                       selectizeInput(
                         "geografi_enskild",
                         "Välj län eller kommun",
                         choices = enskild_val,
                         selected = "Dalarnas län"
                       )
                     ),
                     selectInput(
                       "prognostyp", "Prognostyp",
                       choices = c(
                         "Regional (hela länet)" = "regional",
                         "Enskild (ett område)"  = "enskild"
                       )
                     ),
                     numericInput(
                       "prognos_slut", "Prognosslutår",
                       2050, min = 2026
                     )
                   )
            ),
            column(7,
                   tags$details(
                     tags$summary(
                       "⚙️ Avancerade inställningar",
                       style = "cursor: pointer; font-weight: 600; padding: 0.75rem 1rem; background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; user-select: none;"
                     ),
                     div(
                       style = "padding: 1rem; border: 1px solid #dee2e6; border-top: none; border-radius: 0 0 4px 4px;",
                       checkboxInput("ckm", "Använd CKM-data", TRUE),
                       selectInput(
                         "namnare_dodsrisker",
                         "Nämnare för dödsrisker",
                         choices = c(
                           "Medelfolkmängd" = "medelfolkmangd",
                           "Totfolkmängd (31 dec)" = "totfolkmangd"
                         )
                       ),
                       checkboxInput(
                         "dodsfall_fore_aldring",
                         "Dödsfall före åldring",
                         TRUE
                       )
                     )
                   )
            )
          )
        ),

        accordion_panel(
          title = "⚙️ Antaganden – viktningsparametrar",
          card(
            card_header("Viktningsparametrar per demografisk komponent"),

            ## ---- Förklaring ----
            tags$div(
              style = "padding: 0.75rem 1rem; border: 1px solid #cfe0f3; border-radius: 4px; background: #fafcff; font-size: 0.88em; margin-bottom: 0.75rem;",
              tags$p(
                tags$b("ℹ️ Förklaring av viktningsmetoder och Alpha")
              ),
                tags$p(
                  tags$b("Antal år"),
                  " avgör hur många historiska år som används för att beräkna risktalen.
                  Fler år ger stabilare värden men reagerar långsammare på trendbrott."
                ),
                tags$p(tags$b("Viktningsmetoder:")),
                tags$ul(
                  tags$li(
                    tags$b("1 – Jämn: "),
                    "alla år väger lika mycket (1/N). Bra när historien är stabil och inga
                    tydliga trender finns."
                  ),
                  tags$li(
                    tags$b("2 – Linjär: "),
                    "vikten ökar linjärt med året — det senaste året väger mest, det äldsta
                    minst. Ger en mjuk övergång mot nyare data."
                  ),
                  tags$li(
                    tags$b("3 – EWMA "),
                    "(Exponentially Weighted Moving Average): vikten avtar exponentiellt
                    bakåt i tiden. Reagerar snabbast på trendbrott."
                  )
                ),
                tags$p(
                  tags$b("Alpha (α)"),
                  " styr hur snabbt EWMA-vikterna avtar bakåt i tiden, mellan 0,1 och 0,9:"
                ),
                tags$ul(
                  tags$li(
                    tags$b("Lågt alpha (t.ex. 0,1–0,2): "),
                    "långsam avtagning — många år påverkar resultatet ungefär lika mycket.
                    Stabilt men reagerar långsamt."
                  ),
                  tags$li(
                    tags$b("Högt alpha (t.ex. 0,5–0,9): "),
                    "snabb avtagning — de senaste åren dominerar. Reagerar snabbt på
                    förändringar men blir känsligt för enskilda år."
                  )
                )
            ),

            ## ---- Header-rad ----
            fluidRow(
              class = "risk-header-row",
              column(3, span("Komponent", class = "risk-header")),
              column(2, span("Antal år", class = "risk-header")),
              column(3, span("Viktningsmetod", class = "risk-header")),
              column(3, span("Alpha (EWMA, 0.1–0.9)", class = "risk-header"))
            ),

            risk_ui("fodelserisker", "Födelserisker"),
            risk_ui("dodsrisker", "Dödsrisker", default_vtyp = 1),
            risk_ui("inflyttningsrisker", "Inflyttningsrisker"),
            risk_ui("utflyttningsrisker", "Utflyttningsrisker"),
            risk_ui("invandringsrisker", "Invandringsrisker"),
            risk_ui("utvandringsrisker", "Utvandringsrisker")
          )
        ),

        accordion_panel(
          title = "📊 Revidera komponenter",

          # checkbox högst upp
          tags$div(
            style = "display:flex; align-items:center; margin-bottom:6px;",
            checkboxInput("alt_aktivera", label = "Aktivera revidering av komponenter", value = FALSE),
            tags$span(
              "Multiplikatorer på beräknade risktal: ",
              tags$b("1.00"), " = ingen förändring ",
              tags$b("1.10"), " = +10% ",
              tags$b("0.90"), " = −10%.",
              style = "font-size:0.82em; color:#555; margin-left:10px;"
            )
          ),

          tags$div(
            id = "alt_wrap",
            style = "position: relative;",

            # overlay som blockerar klick när alt_aktivera = FALSE (som i gamla)
            conditionalPanel(
              condition = "!input.alt_aktivera",
              tags$div(class = "alt-overlay")
            ),

            # cards per komponent
            tags$div(
              id = "alt_container",
              class = "scroll-cards",
              lapply(
                list(
                  list(id="fodelserisker",  label="Födelserisker"),
                  list(id="dodsrisker",     label="Dödsrisker"),
                  list(id="inflyttningsrisker", label="Inflyttningsrisker"),
                  list(id="utflyttningsrisker", label="Utflyttningsrisker"),
                  list(id="invandringsrisker", label="Invandringsrisker"),
                  list(id="utvandringsrisker", label="Utvandringsrisker")
                ),
                function(k) {
                  card(
                    card_header(k$label),
                    uiOutput(paste0("alt_ui_", k$id))
                  )
                }
              )
            )
          )
        )
      )
    ),

    br(), br(),

    div(
      class = "config-footer",
      uiOutput("korstatus"),
      actionButton(
        "kor",
        "🚀 Kör prognos",
        class = "btn-primary btn-lg"
      )
    )
  )
}

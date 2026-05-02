ui_resultat <- function(app_kontext) {
  navset_tab(

    # ----------------------------------------------------------
    # FLIK 1 – DEMOGRAFISKA KOMPONENTER (tidsserie)
    # ----------------------------------------------------------
    nav_panel(
      title = "Demografiska komponenter",

      layout_sidebar(
        sidebar = sidebar(
          title = "Information",
          p("Visar demografiska komponenter för vald geografi med både historiska ",
            "data (svart) och prognosdata (blå)."),
          p("Den streckade linjen markerar övergången mellan historisk data och prognos.")
        ),

        div(
          # Rad: Total befolkning + förändring
          fluidRow(
            class = "mb-3",
            column(6, card(
              card_header("Total befolkning"),
              card_body(girafeOutput("plot_total_befolkning", height = "300px"))
            )),
            column(6, card(
              card_header("Total befolkningsförändring"),
              card_body(girafeOutput("plot_total_forandring", height = "300px"))
            ))
          ),

          # Rad: Födelsekomponenter
          fluidRow(
            class = "mb-3",
            column(4, card(
              card_header("Födda"),
              card_body(girafeOutput("plot_fodda"))
            )),
            column(4, card(
              card_header("Döda"),
              card_body(girafeOutput("plot_doda"))
            )),
            column(4, card(
              card_header("Födelsenetto"),
              card_body(girafeOutput("plot_fodelsenetto"))
            ))
          ),

          # Rad: Inrikes flyttningar
          fluidRow(
            class = "mb-3",
            column(4, card(
              card_header("Inrikes inflyttade"),
              card_body(girafeOutput("plot_inrikes_inflyttade"))
            )),
            column(4, card(
              card_header("Inrikes utflyttade"),
              card_body(girafeOutput("plot_inrikes_utflyttade"))
            )),
            column(4, card(
              card_header("Inrikes flyttnetto"),
              card_body(girafeOutput("plot_inrikes_netto"))
            ))
          ),

          # Rad: Utrikes flyttningar
          fluidRow(
            class = "mb-3",
            column(4, card(
              card_header("Invandrade"),
              card_body(girafeOutput("plot_invandrade", height = "300px"))
            )),
            column(4, card(
              card_header("Utvandrade"),
              card_body(girafeOutput("plot_utvandrade", height = "300px"))
            )),
            column(4, card(
              card_header("Utrikes flyttnetto"),
              card_body(girafeOutput("plot_utrikes_netto", height = "300px"))
            ))
          )
        )
      )
    ),

    # ----------------------------------------------------------
    # FLIK 2 – 1-ÅRSKLASSER
    # ----------------------------------------------------------
    nav_panel(
      title = "1-årsklasser",

      layout_sidebar(
        sidebar = sidebar(
          title = "Inställningar",
          checkboxGroupInput(
            "ar_ettar",
            "Välj år att visa:",
            choices  = NULL,
            selected = NULL
          ),
          hr(),
          h6("Information"),
          p("Visar demografiska komponenter fördelade på 1-årsklasser (summerat över könen)."),
          p("'Födda efter moderns ålder' visar antal födda fördelat på moderns ålder (15-49 år)."),
          p("Välj ett eller flera år för att jämföra historiska data och prognosdata.")
        ),

        div(
          # Rad 1: Födelsekomponenter
          fluidRow(
            class = "mb-3",
            column(4, card(
              card_header("Födda efter moderns ålder"),
              card_body(plotOutput("plot_fodda_ettar", height = "300px"))
            )),
            column(4, card(
              card_header("Döda per åldersklass"),
              card_body(plotOutput("plot_doda_ettar", height = "300px"))
            )),
            column(4, card(
              card_header("Total befolkning per åldersklass"),
              card_body(plotOutput("plot_total_befolkning_ettar", height = "300px"))
            ))
          ),

          # Rad 2: Inrikes
          fluidRow(
            class = "mb-3",
            column(4, card(
              card_header("Inrikes inflyttade per åldersklass"),
              card_body(plotOutput("plot_inrikes_inflyttade_ettar", height = "300px"))
            )),
            column(4, card(
              card_header("Inrikes utflyttade per åldersklass"),
              card_body(plotOutput("plot_inrikes_utflyttade_ettar", height = "300px"))
            )),
            column(4, card(
              card_header("Inrikes flyttnetto per åldersklass"),
              card_body(plotOutput("plot_inrikes_netto_ettar", height = "300px"))
            ))
          ),

          # Rad 3: Utrikes
          fluidRow(
            class = "mb-3",
            column(4, card(
              card_header("Invandrade per åldersklass"),
              card_body(plotOutput("plot_invandrade_ettar", height = "300px"))
            )),
            column(4, card(
              card_header("Utvandrade per åldersklass"),
              card_body(plotOutput("plot_utvandrade_ettar", height = "300px"))
            )),
            column(4, card(
              card_header("Utrikes flyttnetto per åldersklass"),
              card_body(plotOutput("plot_utrikes_netto_ettar", height = "300px"))
            ))
          )
        )
      )
    ),

    # ----------------------------------------------------------
    # FLIK 3 – RISKTAL
    # ----------------------------------------------------------
    nav_panel(
      title = "Risktal",

      layout_sidebar(
        sidebar = sidebar(
          title = "Inställningar",
          checkboxGroupInput(
            "ar_risk_multi",
            "Välj år att visa:",
            choices  = NULL,
            selected = NULL
          ),
          selectInput(
            "kon_risk",
            "Välj kön:",
            choices  = c("Båda" = "Båda", "Kvinnor" = "kvinnor", "Män" = "män"),
            selected = "kvinnor"
          ),
          hr(),
          h6("Information"),
          p(strong("Historiska år"), " visas med tunnare linjer och är något genomskinliga."),
          p(strong("Prognosår"), " visas med tjockare linjer."),
          p("Födelserisker visas enbart för kvinnor 15-49 år."),
          p("För övriga risktal, välj ett kön för tydligare visualisering.")
        ),

        div(
          # Rad 1: Födelserisker + Dödsrisker
          fluidRow(
            class = "mb-3",
            column(6, card(
              card_header("Födelserisker"),
              card_body(plotOutput("plot_fodelserisker", height = "300px"))
            )),
            column(6, card(
              card_header("Dödsrisker"),
              card_body(plotOutput("plot_dodsrisker", height = "300px"))
            ))
          ),

          # Rad 2: Inrikes
          fluidRow(
            class = "mb-3",
            column(6, card(
              card_header("Inflyttningsrisker"),
              card_body(plotOutput("plot_inflyttningsrisker", height = "300px"))
            )),
            column(6, card(
              card_header("Utflyttningsrisker"),
              card_body(plotOutput("plot_utflyttningsrisker", height = "300px"))
            ))
          ),

          # Rad 3: Utrikes
          fluidRow(
            class = "mb-3",
            column(6, card(
              card_header("Invandringsrisker"),
              card_body(plotOutput("plot_invandringsrisker", height = "300px"))
            )),
            column(6, card(
              card_header("Utvandringsrisker"),
              card_body(plotOutput("plot_utvandringsrisker", height = "300px"))
            ))
          )
        )
      )
    ),

    # ----------------------------------------------------------
    # FLIK 4 – METOD OCH INSTÄLLNINGAR
    # ----------------------------------------------------------
    nav_panel(
      title = "Metod och inställningar",

      layout_sidebar(
        sidebar = sidebar(
          title = "Information",
          uiOutput("metod_scenario_info"),
          hr(),
          p(em("Klicka på rubrikerna nedan för att läsa mer om respektive komponent."))
        ),

        div(
          # Övergripande metodbeskrivning
          card(
            card_header(class = "bg-primary text-white",
                        h4("Om kohort-komponent-metoden")),
            card_body(
              HTML("
              <p><strong>Kohort-komponent-metoden</strong> är den vanligaste metoden för
              befolkningsprognoser och används av de flesta statistikmyndigheter världen över,
              inklusive SCB. Metoden följer befolkningen uppdelad i kohorter (födelseårsgrupper)
              och beräknar hur varje kohort förändras över tiden genom demografiska komponenter.</p>

              <h5>Prognosens huvudsteg:</h5>
              <ol>
                <li><strong>Startbefolkning:</strong> Utgår från den senaste kända befolkningen
                uppdelad efter kön och 1-årsklasser (0-100+ år).</li>
                <li><strong>Åldring:</strong> För varje prognosår åldras befolkningen ett år.</li>
                <li><strong>Demografiska händelser:</strong> Antal födda, döda, in- och utflyttade
                samt in- och utvandrade beräknas med åldersspecifika risktal.</li>
                <li><strong>Ny befolkning:</strong> Gammal befolkning + Födda − Döda +
                Inflyttade − Utflyttade + Invandrade − Utvandrade.</li>
                <li><strong>Upprepning:</strong> Processen upprepas för varje prognosår.</li>
              </ol>

              <h5>Risktal och antaganden:</h5>
              <p>Risktalen baseras på historiska mönster viktade med vald viktningsmetod och
              utjämnas sedan med splines. Lokala risktal sätts i relation till riksprognosen
              för att fånga nationella trender.</p>
              ")
            )
          ),

          # Komponent-accordion
          accordion(
            open = FALSE,

            accordion_panel(
              title = "Födelserisker",
              uiOutput("fodda_info")
            ),
            accordion_panel(
              title = "Dödsrisker",
              uiOutput("doda_info")
            ),
            accordion_panel(
              title = "Inflyttningsrisker",
              uiOutput("inflyttning_info")
            ),
            accordion_panel(
              title = "Utflyttningsrisker",
              uiOutput("utflyttning_info")
            ),
            accordion_panel(
              title = "Invandringsrisker",
              uiOutput("invandring_info")
            ),
            accordion_panel(
              title = "Utvandringsrisker",
              uiOutput("utvandring_info")
            )
          )
        )
      )
    )

  ) # navset_tab
}

server_resultat <- function(input, output, session, app_kontext) {

  # ----------------------------------------------------------
  # Reaktiv accessor – hela resultatobjektet
  # ----------------------------------------------------------
  res <- reactive({
    req(app_kontext$resultat)
    app_kontext$resultat
  })

  # ----------------------------------------------------------
  # Initiera UI-kontroller när nytt resultat finns
  # ----------------------------------------------------------
  observe({
    req(res())

    prognos_ar <- sort(as.numeric(
      unique(res()$prognos$totalbefolkning$ar)
    ))
    forsta_prognos_ar <- min(prognos_ar)

    # Historiska år ur underlag
    historiska_ar <- integer(0)
    kl <- res()$underlag$kommun_lista
    if (!is.null(kl) && "totfolkmangd" %in% names(kl)) {
      historiska_ar <- sort(as.numeric(unique(kl$totfolkmangd$ar)))
      historiska_ar <- historiska_ar[historiska_ar < forsta_prognos_ar]
    }

    alla_ar <- sort(c(historiska_ar, prognos_ar))

    # 1-årsklasser: default = första prognosåret
    updateCheckboxGroupInput(
      session, "ar_ettar",
      choices  = as.character(alla_ar),
      selected = as.character(forsta_prognos_ar)
    )

    # Risktal: default = senaste 3 historiska + första prognosåret
    senaste_hist <- if (length(historiska_ar) > 0)
      tail(historiska_ar, 3) else integer(0)
    risktal_default <- as.character(c(senaste_hist, forsta_prognos_ar))

    updateCheckboxGroupInput(
      session, "ar_risk_multi",
      choices  = as.character(alla_ar),
      selected = risktal_default
    )
  })

  # ----------------------------------------------------------
  # METOD – scenario-info
  # ----------------------------------------------------------
  output$metod_scenario_info <- renderUI({
    req(res())
    scenario <- res()$metadata$scenario %||% "standard"
    geografi <- res()$metadata$geografi %||% "\u2013"

    tagList(
      h6("Geografi"),
      p(strong(geografi)),
      h6("Scenariotyp"),
      if (scenario == "alternativ") {
        tagList(
          tags$span(class = "badge bg-warning text-dark", "Alternativscenario"),
          br(), br(),
          p("Prognosen använder justerade risktal enligt alternativscenariot.")
        )
      } else {
        tagList(
          tags$span(class = "badge bg-success", "Standardscenario"),
          br(), br(),
          p("Prognosen använder standardrisktal utan justeringar.")
        )
      }
    )
  })

  # ----------------------------------------------------------
  # METOD – komponent-informationsrutor
  # ----------------------------------------------------------

  .render_komponent_info <- function(komp_id, komp_label,
                                     default_antal_ar, default_vtyp,
                                     default_alpha = 0.5,
                                     extra_html = "") {
    renderUI({
      req(res())
      params <- res()$konfiguration$riskparametrar[[komp_id]] %||%
        list(antal_ar = default_antal_ar, viktningstyp = default_vtyp,
             alpha = default_alpha)
      vikt_info <- formatera_vikter(
        params$antal_ar, params$viktningstyp,
        if (params$viktningstyp == 3 && !is.null(params$alpha))
          params$alpha else default_alpha
      )
      scenario <- res()$metadata$scenario %||% "standard"
      alt_j    <- res()$konfiguration$alternativ_justeringar
      juster   <- if (scenario == "alternativ" && !is.null(alt_j))
        formatera_justeringar(alt_j[[komp_id]])
      else
        "Inga justeringar (standardscenario)"

      HTML(paste0(
        "<h5>Inställningar</h5><ul>",
        "<li><strong>Antal historiska år:</strong> ", params$antal_ar, " år</li>",
        "<li><strong>Viktningsmetod:</strong> ", vikt_info$metod, "</li>",
        "<li><strong>Vikter per år:</strong> ", vikt_info$vikter, "</li>",
        "<li><strong>Scenariojusteringar:</strong> ", juster, "</li>",
        "</ul>",
        extra_html
      ))
    })
  }

  output$fodda_info <- .render_komponent_info(
    "fodelserisker", "Födelserisker", 7, 2,
    extra_html = "<p>Födelserisker beräknas som åldersspecifika fruktsamhetskvoter,
    utjämnas med splines och sätts i relation till riksprognosen.</p>"
  )
  output$doda_info <- .render_komponent_info(
    "dodsrisker", "Dödsrisker", 7, 1,
    extra_html = "<p>Dödsrisker beräknas via åldersgrupperade dödstal, viktade jämnt
    för att motverka kortsiktiga variationer. Rikets dödstrender används för framskrivning.</p>"
  )
  output$inflyttning_info <- .render_komponent_info(
    "inflyttningsrisker", "Inflyttningsrisker", 7, 2,
    extra_html = "<p>Inflyttningsrisk = Inflyttade / Riksbefolkning. Appliceras på
    SCB:s riksbefolkningsprognos per prognosår.</p>"
  )
  output$utflyttning_info <- .render_komponent_info(
    "utflyttningsrisker", "Utflyttningsrisker", 7, 3,
    extra_html = "<p>Utflyttningsrisk = Utflyttade / Lokal befolkning.
    Appliceras på prognosbefolkningen varje år.</p>"
  )
  output$invandring_info <- .render_komponent_info(
    "invandringsrisker", "Invandringsrisker", 7, 3, 0.3,
    extra_html = "<p>Invandringsrisk = Kommunens andel av riksinvandringen.
    Andelarna appliceras på SCB:s riksprognos för invandring.</p>"
  )
  output$utvandring_info <- .render_komponent_info(
    "utvandringsrisker", "Utvandringsrisker", 7, 3, 0.3,
    extra_html = "<p>Utvandringsrisk = Utvandrade / Lokal befolkning.
    Appliceras på prognosbefolkningen varje år.</p>"
  )

  # ----------------------------------------------------------
  # DEMOGRAFISKA KOMPONENTER – reaktiva data
  # ----------------------------------------------------------
  .get_kl <- function() res()$underlag$kommun_lista

  data_total_befolkning <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Total befolkning", .get_kl())
  })
  data_total_forandring <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Total befolkningsförändring", .get_kl())
  })
  data_fodda <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Födda", .get_kl())
  })
  data_doda <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Döda", .get_kl())
  })
  data_fodelsenetto <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Födelsenetto", .get_kl())
  })
  data_inrikes_inflyttade <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Inrikes inflyttade", .get_kl())
  })
  data_inrikes_utflyttade <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Inrikes utflyttade", .get_kl())
  })
  data_inrikes_netto <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Inrikes flyttnetto", .get_kl())
  })
  data_invandrade <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Invandrade", .get_kl())
  })
  data_utvandrade <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Utvandrade", .get_kl())
  })
  data_utrikes_netto <- reactive({
    req(res())
    skapa_komponent_data(res()$prognos, "Utrikes flyttnetto", .get_kl())
  })

  # ----------------------------------------------------------
  # DEMOGRAFISKA KOMPONENTER – plottar
  # ----------------------------------------------------------
  output$plot_total_befolkning <- renderGirafe({
    skapa_komponent_plot(data_total_befolkning(), "Total befolkning")
  })
  output$plot_total_forandring <- renderGirafe({
    skapa_komponent_plot(data_total_forandring(), "Total befolkningsförändring")
  })
  output$plot_fodda <- renderGirafe({
    skapa_komponent_plot(data_fodda(), "Födda")
  })
  output$plot_doda <- renderGirafe({
    skapa_komponent_plot(data_doda(), "Döda")
  })
  output$plot_fodelsenetto <- renderGirafe({
    skapa_komponent_plot(data_fodelsenetto(), "Födelsenetto")
  })
  output$plot_inrikes_inflyttade <- renderGirafe({
    skapa_komponent_plot(data_inrikes_inflyttade(), "Inrikes inflyttade")
  })
  output$plot_inrikes_utflyttade <- renderGirafe({
    skapa_komponent_plot(data_inrikes_utflyttade(), "Inrikes utflyttade")
  })
  output$plot_inrikes_netto <- renderGirafe({
    skapa_komponent_plot(data_inrikes_netto(), "Inrikes flyttnetto")
  })
  output$plot_invandrade <- renderGirafe({
    skapa_komponent_plot(data_invandrade(), "Invandrade")
  })
  output$plot_utvandrade <- renderGirafe({
    skapa_komponent_plot(data_utvandrade(), "Utvandrade")
  })
  output$plot_utrikes_netto <- renderGirafe({
    skapa_komponent_plot(data_utrikes_netto(), "Utrikes flyttnetto")
  })

  # ----------------------------------------------------------
  # 1-ÅRSKLASSER – reaktiva data
  # ----------------------------------------------------------
  .ar_ettar <- reactive({
    req(input$ar_ettar)
    input$ar_ettar
  })

  data_fodda_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Födda efter moderns ålder",
                           .ar_ettar(), .get_kl(),
                           fodelserisker = res()$risktal$fodelserisker)
  })
  data_doda_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Döda", .ar_ettar(), .get_kl())
  })
  data_total_befolkning_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Total befolkning", .ar_ettar(), .get_kl())
  })
  data_inrikes_inflyttade_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Inrikes inflyttade", .ar_ettar(), .get_kl())
  })
  data_inrikes_utflyttade_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Inrikes utflyttade", .ar_ettar(), .get_kl())
  })
  data_inrikes_netto_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Inrikes flyttnetto", .ar_ettar(), .get_kl())
  })
  data_invandrade_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Invandrade", .ar_ettar(), .get_kl())
  })
  data_utvandrade_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Utvandrade", .ar_ettar(), .get_kl())
  })
  data_utrikes_netto_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(res()$prognos, "Utrikes flyttnetto", .ar_ettar(), .get_kl())
  })

  # ----------------------------------------------------------
  # 1-ÅRSKLASSER – plottar
  # ----------------------------------------------------------
  output$plot_fodda_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_fodda_ettar(), "Födda efter moderns ålder")
  })
  output$plot_doda_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_doda_ettar(), "Döda per åldersklass")
  })
  output$plot_total_befolkning_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_total_befolkning_ettar(), "Total befolkning per åldersklass")
  })
  output$plot_inrikes_inflyttade_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_inrikes_inflyttade_ettar(), "Inrikes inflyttade per åldersklass")
  })
  output$plot_inrikes_utflyttade_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_inrikes_utflyttade_ettar(), "Inrikes utflyttade per åldersklass")
  })
  output$plot_inrikes_netto_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_inrikes_netto_ettar(), "Inrikes flyttnetto per åldersklass")
  })
  output$plot_invandrade_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_invandrade_ettar(), "Invandrade per åldersklass")
  })
  output$plot_utvandrade_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_utvandrade_ettar(), "Utvandrade per åldersklass")
  })
  output$plot_utrikes_netto_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_utrikes_netto_ettar(), "Utrikes flyttnetto per åldersklass")
  })

  # ----------------------------------------------------------
  # RISKTAL – reaktiva data
  # ----------------------------------------------------------
  .ar_risk <- reactive({
    req(input$ar_risk_multi)
    input$ar_risk_multi
  })
  .kon_risk <- reactive({
    req(input$kon_risk)
    input$kon_risk
  })

  data_fodelserisker <- reactive({
    req(res(), .ar_risk())
    skapa_risk_data_multi(res()$risktal$fodelserisker,
                          res()$prognos$geografi, .ar_risk(),
                          .get_kl(), "Födelserisker")
  })
  data_dodsrisker <- reactive({
    req(res(), .ar_risk())
    skapa_risk_data_multi(res()$risktal$dodsrisker,
                          res()$prognos$geografi, .ar_risk(),
                          .get_kl(), "Dödsrisker")
  })
  data_inflyttningsrisker <- reactive({
    req(res(), .ar_risk())
    skapa_risk_data_multi(res()$risktal$inflyttningsrisker,
                          res()$prognos$geografi, .ar_risk(),
                          .get_kl(), "Inflyttningsrisker")
  })
  data_utflyttningsrisker <- reactive({
    req(res(), .ar_risk())
    skapa_risk_data_multi(res()$risktal$utflyttningsrisker,
                          res()$prognos$geografi, .ar_risk(),
                          .get_kl(), "Utflyttningsrisker")
  })
  data_invandringsrisker <- reactive({
    req(res(), .ar_risk())
    skapa_risk_data_multi(res()$risktal$invandringsrisker,
                          res()$prognos$geografi, .ar_risk(),
                          .get_kl(), "Invandringsrisker")
  })
  data_utvandringsrisker <- reactive({
    req(res(), .ar_risk())
    skapa_risk_data_multi(res()$risktal$utvandringsrisker,
                          res()$prognos$geografi, .ar_risk(),
                          .get_kl(), "Utvandringsrisker")
  })

  # ----------------------------------------------------------
  # RISKTAL – plottar
  # ----------------------------------------------------------
  output$plot_fodelserisker <- renderPlot({
    skapa_risk_plot_multi(data_fodelserisker(), "Födelserisker",
                          "Fruktsamhetskvot", .kon_risk())
  })
  output$plot_dodsrisker <- renderPlot({
    skapa_risk_plot_multi(data_dodsrisker(), "Dödsrisker",
                          "Dödsrisk", .kon_risk())
  })
  output$plot_inflyttningsrisker <- renderPlot({
    skapa_risk_plot_multi(data_inflyttningsrisker(), "Inflyttningsrisker",
                          "Inflyttningsrisk", .kon_risk())
  })
  output$plot_utflyttningsrisker <- renderPlot({
    skapa_risk_plot_multi(data_utflyttningsrisker(), "Utflyttningsrisker",
                          "Utflyttningsrisk", .kon_risk())
  })
  output$plot_invandringsrisker <- renderPlot({
    skapa_risk_plot_multi(data_invandringsrisker(), "Invandringsrisker",
                          "Invandringsrisk", .kon_risk())
  })
  output$plot_utvandringsrisker <- renderPlot({
    skapa_risk_plot_multi(data_utvandringsrisker(), "Utvandringsrisker",
                          "Utvandringsrisk", .kon_risk())
  })

}

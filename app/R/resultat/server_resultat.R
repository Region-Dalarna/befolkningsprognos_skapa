server_resultat <- function(input, output, session, app_kontext) {

  # ----------------------------------------------------------
  # Reaktiv accessor – hela resultatobjektet
  # ----------------------------------------------------------
  res <- reactive({
    req(app_kontext$resultat)
    app_kontext$resultat
  })

  # ----------------------------------------------------------
  # Geografi-väljare + nedladdningsknapp (regional prognos)
  # ----------------------------------------------------------
  output$vald_geografi_ui <- renderUI({
    req(res())
    er_regional <- isTRUE(res()$konfiguration$prognostyp == "regional")

    lan_namn <- res()$metadata$geografi

    val <- if (er_regional) {
      kommuner <- names(res()$prognos$kommun_resultat %||% list())
      # Länet finns redan i kommun_resultat – avduplicera
      unique(c(lan_namn, sort(setdiff(kommuner, lan_namn))))
    } else {
      lan_namn
    }

    div(
      style = "padding: 0.5rem 1rem; background: #f8f9fa; border-bottom: 1px solid #dee2e6;",
      fluidRow(
        column(4,
               selectizeInput(
                 "vald_geografi",
                 "Visa geografi:",
                 choices  = val,
                 selected = lan_namn,
                 width    = "100%",
                 options  = list(placeholder = "Skriv för att söka …")
               )
        ),
        column(4, offset = 4,
               div(style = "padding-top: 28px; text-align: right;",
                   actionButton(
                     "spara_prognos_btn",
                     "💾 Spara prognos (Excel)",
                     class = "btn-primary",
                     icon = NULL
                   )
               )
        )
      )
    )
  })

  # Aktiv prognos: län-nivå eller vald kommun
  vald_prognos <- reactive({
    req(res())
    if (isTRUE(res()$konfiguration$prognostyp == "regional") &&
        !is.null(input$vald_geografi) &&
        input$vald_geografi != res()$metadata$geografi) {
      kr <- res()$prognos$kommun_resultat[[input$vald_geografi]]
      if (!is.null(kr)) return(kr)
    }
    res()$prognos
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
    geografi <- if (isTRUE(res()$konfiguration$prognostyp == "regional") &&
                    !is.null(input$vald_geografi)) {
      input$vald_geografi
    } else {
      res()$metadata$geografi %||% "\u2013"
    }

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
    skapa_komponent_data(vald_prognos(), "Total befolkning", .get_kl())
  })
  data_total_forandring <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Total befolkningsförändring", .get_kl())
  })
  data_fodda <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Födda", .get_kl())
  })
  data_doda <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Döda", .get_kl())
  })
  data_fodelsenetto <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Födelsenetto", .get_kl())
  })
  data_inrikes_inflyttade <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Inrikes inflyttade", .get_kl())
  })
  data_inrikes_utflyttade <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Inrikes utflyttade", .get_kl())
  })
  data_inrikes_netto <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Inrikes flyttnetto", .get_kl())
  })
  data_invandrade <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Invandrade", .get_kl())
  })
  data_utvandrade <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Utvandrade", .get_kl())
  })
  data_utrikes_netto <- reactive({
    req(res())
    skapa_komponent_data(vald_prognos(), "Utrikes flyttnetto", .get_kl())
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
    skapa_ettarsklass_data(vald_prognos(), "Födda efter moderns ålder",
                           .ar_ettar(), .get_kl(),
                           fodelserisker = res()$risktal$fodelserisker)
  })
  data_doda_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(vald_prognos(), "Döda", .ar_ettar(), .get_kl())
  })
  data_total_befolkning_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(vald_prognos(), "Total befolkning", .ar_ettar(), .get_kl())
  })
  data_inrikes_inflyttade_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(vald_prognos(), "Inrikes inflyttade", .ar_ettar(), .get_kl())
  })
  data_inrikes_utflyttade_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(vald_prognos(), "Inrikes utflyttade", .ar_ettar(), .get_kl())
  })
  data_inrikes_netto_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(vald_prognos(), "Inrikes flyttnetto", .ar_ettar(), .get_kl())
  })
  data_invandrade_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(vald_prognos(), "Invandrade", .ar_ettar(), .get_kl())
  })
  data_utvandrade_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(vald_prognos(), "Utvandrade", .ar_ettar(), .get_kl())
  })
  data_utrikes_netto_ettar <- reactive({
    req(res(), .ar_ettar())
    skapa_ettarsklass_data(vald_prognos(), "Utrikes flyttnetto", .ar_ettar(), .get_kl())
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

  # ----------------------------------------------------------
  # EXCEL-NEDLADDNING
  # ----------------------------------------------------------

  .bygg_excel_data <- function(prognosobjekt, regionkod_lookup) {
    if (is.null(prognosobjekt) || is.null(prognosobjekt$totalbefolkning)) {
      return(NULL)
    }

    totbef <- prognosobjekt$totalbefolkning %>%
      dplyr::select(region, kon, alder, ar, total_folkmangd = varde) %>%
      dplyr::mutate(ar = as.integer(ar), alder = as.integer(alder))

    plattning <- function(komp_namn) {
      arsnycklar <- names(prognosobjekt$komponenter)
      purrr::map_dfr(arsnycklar, function(arstr) {
        d <- prognosobjekt$komponenter[[arstr]][[komp_namn]]
        if (is.null(d) || nrow(d) == 0) return(NULL)
        d %>%
          dplyr::select(region, kon, alder, ar, varde) %>%
          dplyr::mutate(ar = as.integer(ar), alder = as.integer(alder))
      })
    }

    komp_kolumner <- c("fodda", "doda",
                       "inrikes_inflyttning", "inrikes_utflyttning",
                       "invandring", "utvandring")

    resultat <- totbef
    for (kn in komp_kolumner) {
      d <- plattning(kn)
      if (!is.null(d) && nrow(d) > 0) {
        d <- d %>% dplyr::rename(!!kn := varde)
        resultat <- dplyr::left_join(resultat, d,
                                     by = c("region", "kon", "alder", "ar"))
      } else {
        resultat[[kn]] <- NA_real_
      }
    }

    resultat$regionkod <- unname(regionkod_lookup[resultat$region])

    resultat %>%
      dplyr::select(regionkod, region, kon, alder, ar,
                    total_folkmangd, fodda, doda,
                    inrikes_inflyttning, inrikes_utflyttning,
                    invandring, utvandring) %>%
      dplyr::arrange(region, ar, kon, alder)
  }

  .bygg_antaganden <- function(konfig) {
    komp_def <- list(
      list(id = "fodelserisker",      label = "Födelserisker",      def_alpha = 0.5),
      list(id = "dodsrisker",         label = "Dödsrisker",         def_alpha = 0.5),
      list(id = "inflyttningsrisker", label = "Inflyttningsrisker", def_alpha = 0.5),
      list(id = "utflyttningsrisker", label = "Utflyttningsrisker", def_alpha = 0.5),
      list(id = "invandringsrisker",  label = "Invandringsrisker",  def_alpha = 0.3),
      list(id = "utvandringsrisker",  label = "Utvandringsrisker",  def_alpha = 0.3)
    )

    scenario <- konfig$scenario %||% "standard"
    geografi <- if (isTRUE(konfig$prognostyp == "regional"))
      konfig$regional_installningar$lan
    else
      konfig$enskild_geografi$namn

    generellt <- data.frame(
      Inställning = c("Prognostyp", "Scenario", "Slutår", "Geografi", "Avrundning"),
      Värde = c(
        konfig$prognostyp %||% "",
        scenario,
        as.character(konfig$prognos_slut %||% ""),
        geografi %||% "",
        konfig$avrundning %||% ""
      ),
      stringsAsFactors = FALSE
    )

    forklaring_rader <- data.frame(
      Begrepp = c(
        "Antal år",
        "1 – Jämn",
        "2 – Linjär",
        "3 – EWMA",
        "Alpha (α)",
        "  Lågt alpha (0,1–0,3) – långsam anpassning",
        "  Mellanalpha (0,4–0,6) – balanserad anpassning",
        "  Högt alpha (0,7–0,9) – snabb anpassning"
      ),
      Förklaring = c(
        "Antal historiska år som används för att beräkna risktalen. Fler år ger stabilare värden men reagerar långsammare på trendbrott.",
        "Alla år väger lika mycket (1/N). Bra när historien är stabil och inga tydliga trender finns.",
        "Vikten ökar linjärt med året – det senaste året väger mest, det äldsta minst. Ger en mjuk övergång mot nyare data.",
        "Exponentially Weighted Moving Average. Vikten avtar exponentiellt bakåt i tiden. Reagerar snabbast på trendbrott.",
        "Styr hur snabbt EWMA-vikterna avtar bakåt i tiden (mellan 0,1 och 0,9). Använd komma som decimaltecken.",
        "Många år påverkar resultatet ungefär lika mycket. Stabilt men reagerar långsamt på trendbrott.",
        "Rimlig balans mellan stabilitet och lyhördhet för senare års mönster.",
        "De senaste åren dominerar. Reagerar snabbt men blir känsligt för enskilda år."
      ),
      stringsAsFactors = FALSE
    )

    risktal_rader <- purrr::map_dfr(komp_def, function(k) {
      params <- konfig$riskparametrar[[k$id]]
      if (is.null(params)) return(NULL)
      vtyp <- params$viktningstyp
      alpha_v <- if (!is.null(params$alpha)) params$alpha else k$def_alpha
      vikt_info <- formatera_vikter(params$antal_ar, vtyp, alpha_v)
      data.frame(
        Komponent      = k$label,
        Antal_år       = params$antal_ar,
        Viktningsmetod = vikt_info$metod,
        Vikter_per_år  = vikt_info$vikter,
        stringsAsFactors = FALSE
      )
    })

    just_rader <- if (scenario == "alternativ" &&
                      !is.null(konfig$alternativ_justeringar)) {
      purrr::map_dfr(komp_def, function(k) {
        j <- konfig$alternativ_justeringar[[k$id]]
        if (is.null(j) || length(j$perioder) == 0) return(NULL)
        purrr::map_dfr(j$perioder, function(p) {
          data.frame(
            Komponent     = k$label,
            Från_år       = p$från_år,
            Till_år       = p$till_år,
            Multiplikator = p$multiplikator,
            Förändring    = sprintf("%+.0f%%", (p$multiplikator - 1) * 100),
            stringsAsFactors = FALSE
          )
        })
      })
    } else {
      data.frame(
        Komponent     = character(0),
        Från_år       = integer(0),
        Till_år       = integer(0),
        Multiplikator = numeric(0),
        Förändring    = character(0),
        stringsAsFactors = FALSE
      )
    }

    list(generellt = generellt, forklaring = forklaring_rader,
         risktal = risktal_rader, justeringar = just_rader)
  }

  # Bygg arbetsboken och spara till temp-fil. Returnerar sökvägen.
  .bygg_prognos_excel <- function(r) {
    konfig <- r$konfiguration

    geo_data <- tryCatch(hamta_geografi_val(), error = function(e) NULL)
    regionkod_lookup <- c()
    if (!is.null(geo_data)) {
      alla <- c(geo_data$lan_val, geo_data$enskild_val$Kommun)
      regionkod_lookup <- setNames(unname(alla), names(alla))
    }

    data_df <- if (isTRUE(konfig$prognostyp == "regional") &&
                   !is.null(r$prognos$kommun_resultat)) {
      purrr::map_dfr(r$prognos$kommun_resultat, function(kr) {
        .bygg_excel_data(kr, regionkod_lookup)
      })
    } else {
      .bygg_excel_data(r$prognos, regionkod_lookup)
    }

    antaganden <- .bygg_antaganden(konfig)

    wb <- openxlsx::createWorkbook()

    header_style <- openxlsx::createStyle(
      textDecoration = "bold", fgFill = "#305496",
      fontColour = "white", border = "bottom", halign = "left"
    )
    rubrik_style <- openxlsx::createStyle(
      textDecoration = "bold", fontSize = 12
    )

    openxlsx::addWorksheet(wb, "data")
    openxlsx::writeData(wb, "data", data_df, headerStyle = header_style)
    openxlsx::freezePane(wb, "data", firstRow = TRUE)
    openxlsx::setColWidths(wb, "data", cols = 1:ncol(data_df), widths = "auto")

    openxlsx::addWorksheet(wb, "antaganden")

    rad <- 1
    openxlsx::writeData(wb, "antaganden", "Generella inställningar",
                        startRow = rad, startCol = 1)
    openxlsx::addStyle(wb, "antaganden", rubrik_style, rows = rad, cols = 1)
    rad <- rad + 1
    openxlsx::writeData(wb, "antaganden", antaganden$generellt,
                        startRow = rad, startCol = 1,
                        headerStyle = header_style)
    rad <- rad + nrow(antaganden$generellt) + 3

    openxlsx::writeData(wb, "antaganden", "Förklaring – viktningsmetoder och Alpha",
                        startRow = rad, startCol = 1)
    openxlsx::addStyle(wb, "antaganden", rubrik_style, rows = rad, cols = 1)
    rad <- rad + 1
    openxlsx::writeData(wb, "antaganden", antaganden$forklaring,
                        startRow = rad, startCol = 1,
                        headerStyle = header_style)
    rad <- rad + nrow(antaganden$forklaring) + 3

    openxlsx::writeData(wb, "antaganden", "Risktal – inställningar per komponent",
                        startRow = rad, startCol = 1)
    openxlsx::addStyle(wb, "antaganden", rubrik_style, rows = rad, cols = 1)
    rad <- rad + 1
    openxlsx::writeData(wb, "antaganden", antaganden$risktal,
                        startRow = rad, startCol = 1,
                        headerStyle = header_style)
    rad <- rad + nrow(antaganden$risktal) + 3

    openxlsx::writeData(wb, "antaganden", "Scenariojusteringar (alternativscenario)",
                        startRow = rad, startCol = 1)
    openxlsx::addStyle(wb, "antaganden", rubrik_style, rows = rad, cols = 1)
    rad <- rad + 1
    if (nrow(antaganden$justeringar) > 0) {
      openxlsx::writeData(wb, "antaganden", antaganden$justeringar,
                          startRow = rad, startCol = 1,
                          headerStyle = header_style)
    } else {
      openxlsx::writeData(wb, "antaganden",
                          "Inga justeringar (standardscenario)",
                          startRow = rad, startCol = 1)
    }

    openxlsx::setColWidths(wb, "antaganden", cols = 1:6, widths = "auto")

    tmp <- tempfile(fileext = ".xlsx")
    openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
    tmp
  }

  # Förslag på filnamn — samma format som tidigare.
  .prognos_filnamn <- function(r) {
    geo <- if (isTRUE(r$konfiguration$prognostyp == "regional"))
      r$konfiguration$regional_installningar$lan
    else
      r$konfiguration$enskild_geografi$namn
    # Tillåt alla unicode-bokstäver (åäö m.fl.) och siffror i filnamnet.
    geo_safe <- gsub("[^\\p{L}\\p{N}_-]+", "_", geo %||% "prognos", perl = TRUE)
    paste0("Befolkningsprognos_", geo_safe, "_",
           format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
  }

  # Bygger Excel, base64-kodar och skickar till klienten. JS-handlern
  # försöker showSaveFilePicker; faller tillbaka till vanlig nedladdning
  # om webbläsaren saknar API eller om anropet sker utan användargest.
  .skicka_prognos_excel <- function(r) {
    tmp <- .bygg_prognos_excel(r)
    on.exit(unlink(tmp), add = TRUE)
    raw_bytes <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
    b64       <- jsonlite::base64_enc(raw_bytes)
    session$sendCustomMessage("spara_prognos_filer", list(
      filnamn      = .prognos_filnamn(r),
      innehall_b64 = b64
    ))
  }

  observeEvent(input$spara_prognos_btn, {
    .skicka_prognos_excel(res())
  }, ignoreInit = TRUE)

  observeEvent(input$spara_prognos_status, {
    st <- input$spara_prognos_status
    if (isTRUE(st$ok)) {
      showNotification("✅ Prognos sparad", type = "message", duration = 4)
    } else if (!is.null(st$fel)) {
      showNotification(
        paste("❌ Kunde inte spara filen:", st$fel),
        type = "error", duration = 10
      )
    }
  }, ignoreInit = TRUE)

}

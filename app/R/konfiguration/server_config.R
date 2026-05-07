
server_config <- function(input, output, session, app_kontext) {

  # 1. Definiera listan med komponenter
  komp_lista <- list(
    list(id="fodelserisker",      label="Födelserisker",      def_fran=2030, def_mult=1.00),
    list(id="dodsrisker",         label="Dödsrisker",         def_fran=2027, def_mult=1.00),
    list(id="inflyttningsrisker", label="Inflyttningsrisker", def_fran=2026, def_mult=1.00),
    list(id="utflyttningsrisker", label="Utflyttningsrisker", def_fran=2026, def_mult=1.00),
    list(id="invandringsrisker",  label="Invandringsrisker",  def_fran=2026, def_mult=1.00),
    list(id="utvandringsrisker",  label="Utvandringsrisker",  def_fran=2026, def_mult=1.00)
  )

  # 2. Skapa reactiveVal per komponent (alt_perioder)
  alt_perioder <- lapply(komp_lista, function(k) {
    reactiveVal(
      data.frame(
        fran = k$def_fran,
        till = NA_integer_,       # sätts vid render (prognos_slut)
        mult = k$def_mult,
        stringsAsFactors = FALSE
      )
    )
  })
  names(alt_perioder) <- vapply(komp_lista, `[[`, "", "id")

  # 3. renderUI för output$alt_ui_<id
  for (k in komp_lista) {
    local({
      kid <- k$id
      def_mult <- k$def_mult
      def_fran <- k$def_fran

      output[[paste0("alt_ui_", kid)]] <- renderUI({
        df <- alt_perioder[[kid]]()

        # Sätt default till = prognos_slut för rader som saknar till (första rendern)
        if (any(is.na(df$till))) {
          df$till[is.na(df$till)] <- as.integer(input$prognos_slut)
          alt_perioder[[kid]](df)
        }

        tagList(
          lapply(seq_len(nrow(df)), function(i) {
            fluidRow(
              column(4,
                     numericInput(
                       paste0("alt_fran_", kid, "_", i),
                       "Från år",
                       value = df$fran[i],
                       min = 2020, step = 1
                     )
              ),
              column(4,
                     numericInput(
                       paste0("alt_till_", kid, "_", i),
                       "Till år",
                       value = df$till[i],
                       min = 2020, step = 1
                     )
              ),
              column(4,
                     textInput(
                       paste0("alt_mult_", kid, "_", i),
                       "Multiplikator",
                       value = format_decimal(df$mult[i], decimals = 2),
                       placeholder = "1,00"
                     )
              )
            )
          }),

          div(style="display:flex; gap:8px; margin-top:6px;",
              actionButton(paste0("alt_add_", kid), "➕ Lägg till period"),
              actionButton(paste0("alt_del_", kid), "➖ Ta bort sista", class="btn-outline-secondary")
          )
        )
      })

      # Lägg till ny period: default till = prognos_slut
      observeEvent(input[[paste0("alt_add_", kid)]], {
        df <- alt_perioder[[kid]]()
        df <- rbind(
          df,
          data.frame(
            fran = def_fran,
            till = as.integer(input$prognos_slut),
            mult = def_mult,
            stringsAsFactors = FALSE
          )
        )
        alt_perioder[[kid]](df)
      }, ignoreInit = TRUE)

      # Ta bort sista period (minst 1 rad kvar)
      observeEvent(input[[paste0("alt_del_", kid)]], {
        df <- alt_perioder[[kid]]()
        if (nrow(df) > 1) {
          df <- df[-nrow(df), , drop = FALSE]
          alt_perioder[[kid]](df)
        }
      }, ignoreInit = TRUE)
    })
  }

  # ------------------------------------------------------------------
  # Samlar konfiguration från Shiny-inputs + alt_perioder-reaktiver.
  # Definierad inuti server_config så att den stänger in över alt_perioder.
  # ------------------------------------------------------------------
  samla_konfiguration_fran_input <- function(input) {

    komp_ids <- c("fodelserisker", "dodsrisker", "inflyttningsrisker",
                  "utflyttningsrisker", "invandringsrisker", "utvandringsrisker")

    # --- Riskparametrar ---
    riskparametrar <- lapply(
      setNames(komp_ids, komp_ids),
      function(id) {
        vtyp   <- as.integer(input[[paste0("risk_vtyp_", id)]])
        params <- list(
          antal_ar     = as.integer(input[[paste0("risk_ar_", id)]]),
          viktningstyp = vtyp
        )
        if (!is.na(vtyp) && vtyp == 3) {
          params$alpha <- parse_decimal(input[[paste0("risk_alpha_", id)]], default = 0.33)
        }
        params
      }
    )

    # --- Alternativ-justeringar (endast om aktiverade) ---
    alt_aktivera <- isTRUE(input$alt_aktivera)
    alternativ_justeringar <- if (alt_aktivera) {
      lapply(
        setNames(komp_ids, komp_ids),
        function(id) {
          df <- alt_perioder[[id]]()
          perioder <- lapply(seq_len(nrow(df)), function(i) {
            # Läs aktuella värden från input — användarens inmatning
            # speglas inte tillbaka till df innan körning, så vi måste
            # läsa varje fält direkt.
            fr_in  <- input[[paste0("alt_fran_", id, "_", i)]]
            til_in <- input[[paste0("alt_till_", id, "_", i)]]
            mu_in  <- input[[paste0("alt_mult_", id, "_", i)]]

            fr  <- suppressWarnings(as.integer(if (!is.null(fr_in))  fr_in  else df$fran[i]))
            til <- suppressWarnings(as.integer(if (!is.null(til_in)) til_in else df$till[i]))
            mu  <- parse_decimal(if (!is.null(mu_in)) mu_in else df$mult[i],
                                 default = as.numeric(df$mult[i]))
            list(
              från_år       = fr,
              till_år       = til,
              multiplikator = mu
            )
          })
          list(perioder = perioder)
        }
      )
    } else {
      NULL
    }

    # --- Geografi (namn via cachat hamta_geografi_val()) ---
    geo_data         <- hamta_geografi_val()
    enskild_val_flat <- unlist(geo_data$enskild_val)
    lan_val          <- geo_data$lan_val

    geo_enskild_kod  <- input$geografi_enskild %||% ""
    geo_enskild_namn <- names(enskild_val_flat)[enskild_val_flat == geo_enskild_kod]
    if (length(geo_enskild_namn) == 0) geo_enskild_namn <- geo_enskild_kod

    geo_regional_kod  <- input$geografi_regional %||% ""
    geo_regional_namn <- names(lan_val)[lan_val == geo_regional_kod]
    if (length(geo_regional_namn) == 0) geo_regional_namn <- geo_regional_kod

    # --- Konfigurationslista (snake_case) ---
    list(
      prognostyp = input$prognostyp %||% "enskild",
      scenario   = if (alt_aktivera) "alternativ" else "standard",

      prognos_start = NULL,   # härleds från data i kor_prognos_enskild_in_memory
      prognos_slut  = as.integer(input$prognos_slut),

      enskild_geografi = list(
        namn = geo_enskild_namn,
        kod  = geo_enskild_kod
      ),

      regional_installningar = list(
        lan     = geo_regional_namn,
        lan_kod = geo_regional_kod
      ),

      riskparametrar         = riskparametrar,
      alternativ_justeringar = alternativ_justeringar,

      avrundning                                     = input$avrundning %||% "ingen",
      metod_avstamning_regional                      = input$metod_avstamning_regional %||% "minsta_kvadrat",
      bevara_summa_auto_spline_utvandring            = FALSE,
      bevara_summa_auto_spline_inflyttning_lansgrans = FALSE,
      bevara_summa_auto_spline_inflyttning           = TRUE,
      bevara_niva_per_ar_inflyttning                 = FALSE,

      # Övriga UI-val (sparas för referens)
      ckm                   = isTRUE(input$ckm),
      namnare_dodsrisker    = input$namnare_dodsrisker %||% "medelfolkmangd",
      dodsfall_fore_aldring = isTRUE(input$dodsfall_fore_aldring)
    )
  }

  # ------------------------------------------------------------------
  # Spara/ladda inställningar (antaganden + reviderade komponenter)
  # ------------------------------------------------------------------

  # Spara: skickar JSON till klienten som öppnar webbläsarens
  # Save As-dialog via showSaveFilePicker (Chrome/Edge) — eller faller
  # tillbaka på vanlig nedladdning (Firefox/Safari).
  observeEvent(input$spara_installningar_btn, {
    obj <- bygg_installningar_objekt(input, alt_perioder)
    json_text <- jsonlite::toJSON(
      obj,
      pretty     = TRUE,
      auto_unbox = TRUE,
      null       = "null",
      na         = "null"
    )
    default_namn <- paste0(
      "befolkningsprognos_installningar_",
      format(Sys.time(), "%Y%m%d_%H%M"),
      ".json"
    )
    session$sendCustomMessage("spara_installningar_filer", list(
      filnamn  = default_namn,
      innehall = as.character(json_text)
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$spara_status, {
    st <- input$spara_status
    if (isTRUE(st$ok)) {
      showNotification("✅ Inställningar sparade", type = "message", duration = 4)
    } else if (!is.null(st$fel)) {
      showNotification(
        paste("❌ Kunde inte spara filen:", st$fel),
        type = "error", duration = 10
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ladda_installningar_fil, {
    fil <- input$ladda_installningar_fil
    req(fil)

    obj <- tryCatch(
      jsonlite::fromJSON(fil$datapath, simplifyVector = FALSE),
      error = function(e) {
        showNotification(
          paste("❌ Kunde inte läsa filen som JSON:", conditionMessage(e)),
          type = "error", duration = 10
        )
        NULL
      }
    )
    if (is.null(obj)) return()

    validering <- validera_installningar(obj)
    if (!isTRUE(validering$ok)) {
      fel_html <- paste0(
        "<b>❌ Filen kunde inte läsas in</b>",
        "<ul style='margin-top:6px; padding-left:20px;'>",
        paste0("<li>", validering$fel, "</li>", collapse = ""),
        "</ul>"
      )
      showNotification(HTML(fel_html), type = "error", duration = 15)
      return()
    }

    applicera_installningar(obj, session, alt_perioder)
    har_revidering <- !is.null(obj$alternativ_justeringar)
    msg <- if (har_revidering) {
      "✅ Antaganden och reviderade komponenter har laddats"
    } else {
      "✅ Antaganden har laddats"
    }
    showNotification(msg, type = "message", duration = 5)
  }, ignoreInit = TRUE)


  observeEvent(input$kor, {
    app_kontext$konfiguration <- samla_konfiguration_fran_input(input)
    app_kontext$fas <- "korning"
  })

  observeEvent(app_kontext$fas, {

    if (app_kontext$fas != "korning") return()
    shinyjs::disable("kor")
    on.exit(shinyjs::enable("kor"), add = TRUE)

    # --- Live-logg som visas i en sticky-notifikation ------------------
    logg_id    <- "prognos_logg"
    logg_rader <- character(0)

    putsa_text <- function(txt) {
      txt <- sub("\\s+$", "", txt)
      txt <- gsub("\\s*\\(in-memory\\)", "", txt, ignore.case = TRUE)
      txt <- gsub("_in_memory", "", txt, ignore.case = TRUE)
      txt <- gsub("\\s+", " ", txt)
      trimws(txt)
    }

    uppdatera_logg <- function(ny_rad) {
      ny_rad <- putsa_text(ny_rad)
      if (!nzchar(ny_rad)) return(invisible())
      logg_rader <<- c(logg_rader, ny_rad)
      showNotification(
        HTML(paste(
          "<div style='max-height:220px; overflow:auto; font-family:monospace; font-size:0.85em;'>",
          paste(utils::tail(logg_rader, 12), collapse = "<br>"),
          "</div>"
        )),
        id          = logg_id,
        duration    = NULL,
        closeButton = FALSE,
        type        = "default"
      )
    }

    uppdatera_logg("⏳ Startar prognoskörning ...")

    resultat <- tryCatch(
      withCallingHandlers({

        uppdatera_logg("Hämtar underlag från databasen ...")
        hamtat_underlag <- normalisera_underlag(
          hamta_underlag_db(app_kontext$konfiguration)
        )

        berakna_risktal <- kor_riskberakningar_in_memory(
          underlag      = hamtat_underlag,
          konfiguration = app_kontext$konfiguration
        )

        prognos <- if (isTRUE(app_kontext$konfiguration$prognostyp == "regional")) {
          kor_prognos_regional_in_memory(
            underlag      = hamtat_underlag,
            risktal       = berakna_risktal,
            konfiguration = app_kontext$konfiguration
          )
        } else {
          kor_prognos_enskild_in_memory(
            underlag      = hamtat_underlag,
            risktal       = berakna_risktal,
            konfiguration = app_kontext$konfiguration
          )
        }

        list(
          prognos       = prognos,
          risktal       = berakna_risktal,
          underlag      = hamtat_underlag,
          konfiguration = app_kontext$konfiguration,
          metadata      = list(
            scenario = app_kontext$konfiguration$scenario,
            geografi = if (isTRUE(app_kontext$konfiguration$prognostyp == "regional"))
              app_kontext$konfiguration$regional_installningar$lan
            else
              app_kontext$konfiguration$enskild_geografi$namn
          )
        )
      },
      message = function(m) {
        uppdatera_logg(conditionMessage(m))
        invokeRestart("muffleMessage")
      }),
      error = function(e) {
        removeNotification(logg_id)
        showNotification(
          paste("Fel vid prognoskörning:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        NULL
      }
    )

    if (!is.null(resultat)) {
      app_kontext$resultat <- resultat
      app_kontext$fas <- "resultat"
      removeNotification(logg_id)
      showNotification("✅ Prognos klar", type = "message", duration = 5)
      bslib::nav_select("huvud_nav", "2. Resultat", session = session)
    } else {
      app_kontext$fas <- "konfiguration"
    }

  }, ignoreInit = TRUE)
}


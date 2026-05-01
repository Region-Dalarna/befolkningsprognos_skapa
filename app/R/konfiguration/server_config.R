
server_config <- function(input, output, session, app_kontext) {

  # 1. Definiera listan med komponenter
  komp_lista <- list(
    list(id="fodelserisker",      label="Födelserisker",      def_fran=2030, def_mult=1.05),
    list(id="dodsrisker",         label="Dödsrisker",         def_fran=2027, def_mult=0.98),
    list(id="inflyttningsrisker", label="Inflyttningsrisker", def_fran=2026, def_mult=1.10),
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
                     numericInput(
                       paste0("alt_mult_", kid, "_", i),
                       "Multiplikator",
                       value = df$mult[i],
                       min = 0.5, max = 1.5, step = 0.01
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
          alpha_val    <- input[[paste0("risk_alpha_", id)]]
          params$alpha <- if (!is.null(alpha_val)) as.numeric(alpha_val) else 0.33
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
            list(
              från_år       = as.integer(df$fran[i]),
              till_år       = as.integer(df$till[i]),
              multiplikator = as.numeric(df$mult[i])
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

      avrundning                                     = "heltal",
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

  observeEvent(input$kor, {
    app_kontext$konfiguration <- samla_konfiguration_fran_input(input)
    app_kontext$fas <- "korning"
  })

  observeEvent(app_kontext$fas, {

    if (app_kontext$fas != "korning") return()

    showNotification(
      "Prognosen körs …",
      type     = "message",
      duration = NULL
    )

    resultat <- tryCatch(
      kor_prognos(
        konfiguration = app_kontext$konfiguration,
        data_provider = hamta_underlag_stub
      ),
      error = function(e) {
        showNotification(
          paste("Fel vid prognoskörning:", conditionMessage(e)),
          type     = "error",
          duration = 10
        )
        NULL
      }
    )

    if (!is.null(resultat)) {
      app_kontext$resultat <- resultat
      app_kontext$fas      <- "resultat"
    } else {
      app_kontext$fas <- "konfiguration"
    }

  }, ignoreInit = TRUE)
}

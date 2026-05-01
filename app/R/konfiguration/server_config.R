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

  observeEvent(input$kor, {

    app_kontext$konfiguration <- samla_konfiguration_fran_input(input)
    app_kontext$fas <- "korning"
  })

  observeEvent(app_kontext$fas, {

    if (app_kontext$fas != "korning") return()

    showNotification(
      "Prognosen körs …",
      type = "message",
      duration = NULL
    )

    resultat <- kor_prognos_stub(app_kontext$konfiguration)

    app_kontext$resultat <- resultat
    app_kontext$fas <- "resultat"

  }, ignoreInit = TRUE)
}



kor_prognos_stub <- function(konfiguration) {
  Sys.sleep(2)  # simulera beräkning

  list(
    metadata = list(
      skapad = Sys.time(),
      kommentar = "Detta är ett testresultat"
    ),
    data = NULL
  )

  output$korstatus <- renderUI({
    if (app_kontext$fas == "korning") {
      tags$span(
        "⏳ Prognosen körs …",
        style = "font-weight: 600; color: #c77c02;"
      )
    } else if (app_kontext$fas == "resultat") {
      tags$span(
        "✅ Prognosen är klar",
        style = "font-weight: 600; color: #2c7d2c;"
      )
    }
  })

}



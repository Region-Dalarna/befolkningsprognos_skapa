server_resultat <- function(input, output, session, app_kontext) {

  # Metadata-panel
  output$resultat_metadata <- renderUI({
    req(app_kontext$resultat)
    meta <- app_kontext$resultat$metadata
    tagList(
      tags$dl(
        tags$dt("Skapad"),
        tags$dd(format(meta$skapad, "%Y-%m-%d %H:%M:%S")),
        tags$dt("Prognostyp"),
        tags$dd(meta$prognostyp %||% "\u2013"),
        tags$dt("Scenario"),
        tags$dd(meta$scenario %||% "\u2013"),
        tags$dt("Geografi"),
        tags$dd(meta$geografi %||% "\u2013")
      )
    )
  })

  # Sammanfattningstabell (totalbefolkning per år)
  output$resultat_sammanfattning <- renderTable({
    req(app_kontext$resultat)
    sammanf <- app_kontext$resultat$prognos$sammanfattning
    req(!is.null(sammanf))
    sammanf %>%
      dplyr::mutate(
        Total_befolkning = format(Total_befolkning, big.mark = "\u00a0"),
        Kvinnor          = format(Kvinnor,          big.mark = "\u00a0"),
        Män              = format(Män,               big.mark = "\u00a0")
      ) %>%
      dplyr::rename(`Total befolkning` = Total_befolkning)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "–")

  # Debug-panel (komplett resultatobjekt, exkl. tunga data)
  output$debug_resultat <- renderPrint({
    req(app_kontext$resultat)
    res <- app_kontext$resultat
    cat("=== RESULTAT ===\n")
    cat("Metadata:\n")
    str(res$metadata)
    cat("\nPrognostyp:", res$metadata$prognostyp, "\n")
    cat("Geografi:  ", res$metadata$geografi %||% "\u2013", "\n")
    cat("\nRisktal (antal rader per komponent):\n")
    if (!is.null(res$risktal)) {
      for (nm in names(res$risktal)) {
        cat(sprintf("  %-25s %d rader\n", nm, nrow(res$risktal[[nm]])))
      }
    }
    cat("\nPrognos \u2013 sammanfattning:\n")
    if (!is.null(res$prognos$sammanfattning)) {
      print(res$prognos$sammanfattning, n = Inf)
    }
  })

}

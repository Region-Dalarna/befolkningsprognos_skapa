# data_provider.R
# Hook för datainhämtning – ersätt med riktig DB-koppling när den finns.
#
# Kontraktet: en data_provider-funktion tar emot konfiguration och returnerar
# list(
#   kommun_lista = <samma struktur som i Analytikernatverket/befolkningsprognoser>,
#   riket_lista  = <samma struktur som i Analytikernatverket/befolkningsprognoser>
# )
#
# Kolumnnamnen ska vara i snake_case. normalisera_underlag() hanterar
# automatiskt konvertering av eventuella PascalCase-varianter (DB levererar
# alltid gemener/snake_case):
#   Region   →  region
#   Ålder    →  alder  (Alder → alder)
#   Kön      →  kon    (Kon → kon)
#   År       →  ar     (Ar → ar)
#   Värde    →  varde  (Varde → varde)
#   Variabel →  variabel
#   riket_prognosinvanare_grund  →  riket_prognosinvånare_grund  (listnyckel)
#
# Struktur för kommun_lista (snake_case kolumnnamn):
#   $totfolkmangd          – tibble: region, alder, kon, ar, varde, variabel
#   $medelfolkmangd        – tibble: region, alder, kon, ar, varde
#   $medelfolkmangd_modrar – tibble: region, ar, alder, varde
#   $fodda                 – tibble: region, ar, alder, varde
#   $doda                  – tibble: region, alder, kon, ar, varde
#   $inrikes_inflyttade    – tibble: region, ar, alder, kon, varde
#   $inrikes_utflyttade    – tibble: region, ar, alder, kon, varde
#   $invandring            – tibble: region, ar, alder, kon, varde
#   $utvandring            – tibble: region, ar, alder, kon, varde
#   $inflyttningar_lansgrans_raw – tibble: region, ar, alder, kon, Total, Ovriga_lan
#   $utflyttningar_lansgrans_raw – tibble: region, ar, alder, kon, Total, Ovriga_lan
#
# Struktur för riket_lista (snake_case kolumnnamn):
#   $riket_prognosinvånare_grund – tibble: ar, alder, kon, varde
#   $invandring_riket            – tibble: ar, alder, kon, varde
#   $fodelsetal                  – tibble: ar, alder, varde  (fruktsamhetskvoter)
#   $dodstal                     – tibble: ar, alder, kon, varde, variabel

#' Stub: används som fallback om DB-koppling saknas.
#'
#' @param konfiguration list – konfigurationslista från Shiny
#' @return Genererar ett tydligt fel med instruktioner.
hamta_underlag_stub <- function(konfiguration) {
  stop(paste0(
    "data_provider är inte implementerad ännu.\n\n",
    "Implementera en funktion som returnerar:\n",
    "  list(\n",
    "    kommun_lista = <tibbles enligt kommentarerna i data_provider.R>,\n",
    "    riket_lista  = <tibbles enligt kommentarerna i data_provider.R>\n",
    "  )\n\n",
    "och skicka den som data_provider-argument till kor_prognos():\n",
    "  kor_prognos(konfiguration, data_provider = min_hamta_underlag_funktion)"
  ))
}

#' Hämta underlag från Region Dalarnas interna databas (oppna_data / scb-schema).
#'
#' Använder `shiny_uppkoppling_las()` från Region-Dalarnas hjälpbibliotek.
#' Funktionen sourcear automatiskt `func_shinyappar.R` första gången den anropas.
#'
#' @param konfiguration list – konfigurationslista från Shiny (används ej för
#'   filtrering här; all filtrering sker i riskberäknings- och
#'   prognoskoden).
#' @return list(kommun_lista = ..., riket_lista = ...) enligt kontraktet ovan.
hamta_underlag_db <- function(konfiguration) {

  # Source DB-hjälpfunktionen vid behov (bara en gång per session)
  if (!exists("shiny_uppkoppling_las", mode = "function")) {
    source(
      "https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R",
      encoding = "utf-8",
      echo     = FALSE
    )
  }

  uppkoppling <- shiny_uppkoppling_las("oppna_data")

  # --- Kommuntabeller ---
  kommunlista_namn <- c(
    "medelfolkmangd",
    "medelfolkmangd_modrar",
    "fodda",
    "doda",
    "invandring",
    "utvandring",
    "inrikes_inflyttade",
    "inrikes_utflyttade",
    "totfolkmangd",
    "totfolkmangd_modrar",
    "inflyttningar_lansgrans_raw",
    "utflyttningar_lansgrans_raw"
  )

  kommun_lista <- purrr::set_names(kommunlista_namn) |>
    purrr::imap(~ dplyr::tbl(uppkoppling,
                              dbplyr::in_schema("scb", .x)) |>
                  dplyr::collect())

  # --- Rikstabeller ---
  riketlista_namn <- c(
    "dodstal",
    "fodelsetal",
    "invandring_riket",
    "inrikesflyttningar_riket",
    "riket_prognosinvanare_grund"
  )

  riket_lista <- purrr::set_names(riketlista_namn) |>
    purrr::imap(~ dplyr::tbl(uppkoppling,
                              dbplyr::in_schema("scb", .x)) |>
                  dplyr::collect())

  list(
    kommun_lista = kommun_lista,
    riket_lista  = riket_lista
  )
}

#' Normalisera kolumnnamn och listnycklar i underlagsobjektet till snake_case.
#'
#' DB levererar alltid snake_case (gemener utan svenska tecken), men om data
#' av någon anledning levereras med PascalCase/svenska tecken normaliseras de
#' hit till snake_case som domänkoden förväntar sig.
#'
#' Mappning som utförs automatiskt (PascalCase → snake_case):
#'   Region   →  region
#'   Ålder    →  alder  (Alder → alder)
#'   Kön      →  kon    (Kon → kon)
#'   År       →  ar     (Ar → ar)
#'   Värde    →  varde  (Varde → varde)
#'   Variabel →  variabel
#'   riket_prognosinvanare_grund  →  riket_prognosinvånare_grund  (listnyckel)
#'
#' @param underlag list med $kommun_lista och $riket_lista
#' @return Samma struktur men med normaliserade kolumnnamn och listnycklar.
normalisera_underlag <- function(underlag) {

  # Mappning: lista av möjliga PascalCase-namn → önskat snake_case-namn
  kolumn_mapping <- list(
    "region"   = c("Region"),
    "alder"    = c("Ålder", "Alder"),
    "kon"      = c("Kön", "Kon"),
    "ar"       = c("År", "Ar"),
    "varde"    = c("Värde", "Varde"),
    "variabel" = c("Variabel")
  )

  normalisera_kolumner <- function(tbl) {
    if (!is.data.frame(tbl)) return(tbl)
    for (nytt_namn in names(kolumn_mapping)) {
      gammalt_namn <- intersect(kolumn_mapping[[nytt_namn]], names(tbl))
      if (length(gammalt_namn) > 0 && !(nytt_namn %in% names(tbl))) {
        tbl <- dplyr::rename(tbl, !!nytt_namn := !!gammalt_namn[1])
      }
    }
    tbl
  }

  underlag$kommun_lista <- purrr::map(underlag$kommun_lista, normalisera_kolumner)
  underlag$riket_lista  <- purrr::map(underlag$riket_lista,  normalisera_kolumner)

  # Byt listnyckel utan å-tecken → med å-tecken
  if (!is.null(underlag$riket_lista[["riket_prognosinvanare_grund"]]) &&
      is.null(underlag$riket_lista[["riket_prognosinvånare_grund"]])) {
    underlag$riket_lista[["riket_prognosinvånare_grund"]] <-
      underlag$riket_lista[["riket_prognosinvanare_grund"]]
    underlag$riket_lista[["riket_prognosinvanare_grund"]] <- NULL
  }

  underlag
}

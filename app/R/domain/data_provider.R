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
# Struktur för riket_lista (snake_case kolumnnamn och listnycklar):
#   $riket_prognosinvanare_grund – tibble: ar, alder, kon, varde
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

  # Hjälpfunktion: hämta en tabell, prova alternativa namn (t.ex. med/utan å).
  # Föredrar tabell med rader om flera alternativ träffar (skyddar mot fallet
  # att både ASCII- och å-version finns men en av dem är tom). Returnerar
  # NULL om ingen variant går att hämta.
  hamta_tabell <- function(namn_alternativ) {
    fallback <- NULL
    for (tabnamn in namn_alternativ) {
      res <- tryCatch(
        dplyr::tbl(uppkoppling, dbplyr::in_schema("scb", tabnamn)) |>
          dplyr::collect(),
        error = function(e) NULL
      )
      if (is.null(res)) next
      if (nrow(res) > 0) return(res)
      if (is.null(fallback)) fallback <- res
    }
    fallback
  }

  # --- Kommuntabeller (listnyckel = ASCII-namn; värdet är vektor av möjliga DB-namn) ---
  kommun_definitioner <- list(
    medelfolkmangd               = "medelfolkmangd",
    medelfolkmangd_modrar        = "medelfolkmangd_modrar",
    fodda                        = "fodda",
    doda                         = "doda",
    invandring                   = "invandring",
    utvandring                   = "utvandring",
    inrikes_inflyttade           = "inrikes_inflyttade",
    inrikes_utflyttade           = "inrikes_utflyttade",
    totfolkmangd                 = "totfolkmangd",
    inflyttningar_lansgrans_raw  = "inflyttningar_lansgrans_raw",
    utflyttningar_lansgrans_raw  = "utflyttningar_lansgrans_raw"
  )

  kommun_lista <- purrr::map(kommun_definitioner, hamta_tabell)

  # --- Rikstabeller ---
  riket_definitioner <- list(
    dodstal                      = "dodstal",
    fodelsetal                   = "fodelsetal",
    invandring_riket             = "invandring_riket",
    # DB-tabellen kan heta antingen ASCII eller med å – prova båda.
    riket_prognosinvanare_grund  = c("riket_prognosinvanare_grund",
                                     "riket_prognosinvånare_grund")
  )

  riket_lista <- purrr::map(riket_definitioner, hamta_tabell)

  # --- Sanity-check: ge ett tydligt fel om någon tabell saknas ---
  saknade_kommun <- names(kommun_lista)[vapply(kommun_lista, is.null, logical(1))]
  saknade_riket  <- names(riket_lista)[vapply(riket_lista,  is.null, logical(1))]

  if (length(saknade_kommun) > 0 || length(saknade_riket) > 0) {
    stop(paste0(
      "Följande tabeller kunde inte hämtas från schemat 'scb':\n",
      if (length(saknade_kommun) > 0)
        paste0("  kommun: ", paste(saknade_kommun, collapse = ", "), "\n") else "",
      if (length(saknade_riket) > 0)
        paste0("  riket: ",  paste(saknade_riket,  collapse = ", "), "\n") else "",
      "Kontrollera tabellnamnen i databasen."
    ))
  }

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
#'   riket_prognosinvånare_grund  →  riket_prognosinvanare_grund  (listnyckel,
#'     normaliseras till ASCII)
#'
#' @param underlag list med $kommun_lista och $riket_lista
#' @return Samma struktur men med normaliserade kolumnnamn och listnycklar.
normalisera_underlag <- function(underlag) {

  # Mappning: lista av möjliga PascalCase-namn → önskat snake_case-namn
  kolumn_mapping <- list(
    "region"     = c("Region"),
    "alder"      = c("Ålder", "Alder"),
    "kon"        = c("Kön", "Kon"),
    "ar"         = c("År", "Ar"),
    "varde"      = c("Värde", "Varde"),
    "variabel"   = c("Variabel"),
    "total"      = c("Total"),
    "ovriga_lan" = c("Ovriga_lan", "Övriga_län")
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

  # Normalisera listnyckel med å-tecken → ASCII (om någon variant skulle
  # levereras med å istället för det förväntade ASCII-namnet).
  if (!is.null(underlag$riket_lista[["riket_prognosinvånare_grund"]]) &&
      is.null(underlag$riket_lista[["riket_prognosinvanare_grund"]])) {
    underlag$riket_lista[["riket_prognosinvanare_grund"]] <-
      underlag$riket_lista[["riket_prognosinvånare_grund"]]
    underlag$riket_lista[["riket_prognosinvånare_grund"]] <- NULL
  }

  underlag
}


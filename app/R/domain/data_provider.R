# data_provider.R
# Hook för datainhämtning – ersätt med riktig DB-koppling när den finns.
#
# Kontraktet: en data_provider-funktion tar emot konfiguration och returnerar
# list(
#   kommun_lista = <samma struktur som i Analytikernatverket/befolkningsprognoser>,
#   riket_lista  = <samma struktur som i Analytikernatverket/befolkningsprognoser>
# )
#
# Kolumnnamnen behöver INTE vara normaliserade – normalisera_underlag() hanterar
# automatiskt följande varianter (DB levererar alltid gemener):
#   region   →  Region
#   alder    →  Ålder
#   kon      →  Kön
#   ar       →  År
#   varde    →  Värde
#   variabel →  Variabel
#   riket_prognosinvanare_grund  →  riket_prognosinvånare_grund
#
# Struktur för kommun_lista (DB-kolumnnamn inom parentes):
#   $totfolkmangd          – tibble: Region (region), Ålder (alder), Kön (kon), År (ar), Värde (varde), Variabel (variabel)
#   $medelfolkmangd        – tibble: Region (region), Ålder (alder), Kön (kon), År (ar), Värde (varde)
#   $medelfolkmangd_modrar – tibble: Region (region), År (ar), Ålder (alder), Värde (varde)
#   $fodda                 – tibble: Region (region), År (ar), Ålder (alder), Värde (varde)
#   $doda                  – tibble: Region (region), Ålder (alder), Kön (kon), År (ar), Värde (varde)
#   $inrikes_inflyttade    – tibble: Region (region), År (ar), Ålder (alder), Kön (kon), Värde (varde)
#   $inrikes_utflyttade    – tibble: Region (region), År (ar), Ålder (alder), Kön (kon), Värde (varde)
#   $invandring            – tibble: Region (region), År (ar), Ålder (alder), Kön (kon), Värde (varde)
#   $utvandring            – tibble: Region (region), År (ar), Ålder (alder), Kön (kon), Värde (varde)
#   $inflyttningar_lansgrans_raw – tibble: Region (region), År (ar), Ålder (alder), Kön (kon), Total, Ovriga_lan
#   $utflyttningar_lansgrans_raw – tibble: Region (region), År (ar), Ålder (alder), Kön (kon), Total, Ovriga_lan
#
# Struktur för riket_lista (DB-kolumnnamn inom parentes):
#   $riket_prognosinvånare_grund (riket_prognosinvanare_grund) – tibble: År (ar), Ålder (alder), Kön (kon), Värde (varde)
#   $invandring_riket            – tibble: År (ar), Ålder (alder), Kön (kon), Värde (varde)
#   $fodelsetal                  – tibble: År (ar), Ålder (alder), Värde (varde)  (fruktsamhetskvoter)
#   $dodstal                     – tibble: År (ar), Ålder (alder), Kön (kon), Värde (varde), Variabel (variabel)

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

#' Normalisera kolumnnamn och listnycklar i underlagsobjektet.
#'
#' Hanterar skillnader mellan DB-kolumnnamn (gemener, utan svenska tecken)
#' och de namn som domänkoden förväntar sig (versaler, med svenska tecken).
#'
#' Mappning som utförs automatiskt:
#'   region   →  Region
#'   alder    →  Ålder
#'   kon      →  Kön
#'   ar       →  År
#'   varde    →  Värde
#'   variabel →  Variabel
#'   riket_prognosinvanare_grund  →  riket_prognosinvånare_grund
#'
#' @param underlag list med $kommun_lista och $riket_lista
#' @return Samma struktur men med normaliserade kolumnnamn och listnycklar.
normalisera_underlag <- function(underlag) {

  # Mappning: lista av möjliga DB-namn → önskat kodnamn
  kolumn_mapping <- list(
    "Region"   = c("region"),
    "Ålder"    = c("alder", "Alder"),
    "Kön"      = c("kon",   "Kon"),
    "År"       = c("ar",    "Ar"),
    "Värde"    = c("varde", "Varde"),
    "Variabel" = c("variabel")
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

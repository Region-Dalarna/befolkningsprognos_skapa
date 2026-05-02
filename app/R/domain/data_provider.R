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
# automatiskt följande varianter (se nedan):
#   Alder / alder  →  Ålder
#   Kon   / kon    →  Kön
#   Ar    / ar     →  År
#   Varde / varde  →  Värde
#   variabel       →  Variabel
#   riket_prognosinvanare_grund  →  riket_prognosinvånare_grund
#
# Struktur för kommun_lista (DB-kolumnnamn inom parentes):
#   $totfolkmangd          – tibble: Region, Ålder (Alder), Kön (Kon), År (Ar), Värde (Varde), Variabel
#   $medelfolkmangd        – tibble: Region, Ålder (Alder), Kön (Kon), År (Ar), Värde (Varde)
#   $medelfolkmangd_modrar – tibble: Region, År (Ar), Ålder (Alder), Värde (Varde)
#   $fodda                 – tibble: Region, År (Ar), Ålder (Alder), Värde (Varde)
#   $doda                  – tibble: Region, Ålder (Alder), Kön (Kon), År (Ar), Värde (Varde)
#   $inrikes_inflyttade    – tibble: Region, År (Ar), Ålder (Alder), Kön (Kon), Värde (Varde)
#   $inrikes_utflyttade    – tibble: Region, År (Ar), Ålder (Alder), Kön (Kon), Värde (Varde)
#   $invandring            – tibble: Region, År (Ar), Ålder (Alder), Kön (Kon), Värde (Varde)
#   $utvandring            – tibble: Region, År (Ar), Ålder (Alder), Kön (Kon), Värde (Varde)
#   $inflyttningar_lansgrans_raw – tibble: Region, År (Ar), Ålder (Alder), Kön (Kon), Total, Ovriga_lan
#   $utflyttningar_lansgrans_raw – tibble: Region, År (Ar), Ålder (Alder), Kön (Kon), Total, Ovriga_lan
#
# Struktur för riket_lista (DB-kolumnnamn inom parentes):
#   $riket_prognosinvånare_grund (riket_prognosinvanare_grund) – tibble: År (ar), Ålder (alder), Kön (kon), Värde (varde)
#   $invandring_riket            – tibble: År (ar), Ålder (alder), Kön (kon), Värde (varde)
#   $fodelsetal                  – tibble: År (ar), Ålder (alder), Värde (varde)  (fruktsamhetskvoter)
#   $dodstal                     – tibble: År (ar), Ålder (alder), Kön (kon), Värde (varde), Variabel (variabel)

#' Stub: används tills riktig DB-koppling är implementerad.
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

#' Normalisera kolumnnamn och listnycklar i underlagsobjektet.
#'
#' Hanterar skillnader mellan DB-kolumnnamn (utan svenska tecken / gemener)
#' och de namn som domänkoden förväntar sig (med svenska tecken, versaler).
#'
#' Mappning som utförs automatiskt:
#'   Alder / alder  →  Ålder
#'   Kon   / kon    →  Kön
#'   Ar    / ar     →  År
#'   Varde / varde  →  Värde
#'   variabel       →  Variabel
#'   riket_prognosinvanare_grund  →  riket_prognosinvånare_grund
#'
#' @param underlag list med $kommun_lista och $riket_lista
#' @return Samma struktur men med normaliserade kolumnnamn och listnycklar.
normalisera_underlag <- function(underlag) {

  # Mappning: lista av möjliga DB-namn → önskat kodnamn
  kolumn_mapping <- list(
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

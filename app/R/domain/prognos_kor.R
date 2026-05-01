# prognos_kor.R
# Orkestrerar prognoskörningen i Shiny utan fil-/mapp-skapande.
#
# Förväntat användningssätt:
# - Shiny bygger en konfigurationslista (app_kontext$konfiguration)
# - server_config anropar `kor_prognos(konfiguration, data_provider = ...)`
# - Funktionen returnerar ett resultatobjekt som kan visas i UI och exporteras.

# Liten hjälpfunktion (bas-R) för att kunna skriva x %||% y
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Kör prognos (in-memory)
#'
#' @param konfiguration list med val från Shiny
#'   Obligatoriska fält:
#'     $prognostyp       "enskild" | "regional"
#'     $scenario         "standard" | "alternativ"
#'     $prognos_slut     integer, slutår
#'     $enskild_geografi list(namn, kod)   – krävs om prognostyp == "enskild"
#'     $riskparametrar   list per komponent (antal_ar, viktningstyp, alpha)
#'   Valfria fält (får standardvärden):
#'     $prognos_start    integer – härleds från data om NULL
#'     $alternativ_justeringar – krävs om scenario == "alternativ"
#'     $avrundning       "heltal" | "stokastisk" | "ingen"  (default "heltal")
#'     $bevara_summa_auto_spline_utvandring            (default FALSE)
#'     $bevara_summa_auto_spline_inflyttning_lansgrans (default FALSE)
#'     $bevara_summa_auto_spline_inflyttning           (default TRUE)
#'     $bevara_niva_per_ar_inflyttning                 (default FALSE)
#'
#' @param data_provider funktion(konfiguration) -> list(kommun_lista, riket_lista).
#'   Se app/R/domain/data_provider.R för kontraktet.
#'   Stoppar med tydligt fel om NULL.
#' @param progress valfritt – reserverat för framtida Shiny-progress-bar
#' @return list med metadata, konfiguration, risktal, prognos
kor_prognos <- function(konfiguration,
                        data_provider = NULL,
                        progress = NULL) {
  stopifnot(is.list(konfiguration))

  # ------------------------------------------------------------
  # 1) Validera miniminnehåll
  # ------------------------------------------------------------
  if (is.null(konfiguration$prognostyp)) {
    stop("konfiguration$prognostyp saknas")
  }
  if (!konfiguration$prognostyp %in% c("enskild", "regional")) {
    stop("konfiguration$prognostyp måste vara 'enskild' eller 'regional'")
  }
  if (konfiguration$prognostyp == "regional") {
    stop("Regional prognos är ännu inte implementerad. Välj 'enskild'.")
  }

  # ------------------------------------------------------------
  # 2) Datainhämtning via data_provider (DB-hook)
  # ------------------------------------------------------------
  if (is.null(data_provider)) {
    stop(paste0(
      "data_provider saknas.\n",
      "Skicka in en funktion som returnerar list(kommun_lista, riket_lista).\n",
      "Se app/R/domain/data_provider.R för kontraktet och hamta_underlag_stub ",
      "som exempelstub."
    ))
  }

  message("Hämtar underlag via data_provider...")
  underlag <- data_provider(konfiguration)

  if (!is.list(underlag) ||
      is.null(underlag$kommun_lista) ||
      is.null(underlag$riket_lista)) {
    stop(paste0(
      "data_provider returnerade ett ogiltigt underlag.\n",
      "Förväntat: list(kommun_lista = ..., riket_lista = ...)\n",
      "Se app/R/domain/data_provider.R för den fullständiga strukturen."
    ))
  }

  # ------------------------------------------------------------
  # 3) Riskberäkningar (in-memory)
  # ------------------------------------------------------------
  message("Kör riskberäkningar...")
  risktal <- kor_riskberakningar_in_memory(underlag, konfiguration)

  # ------------------------------------------------------------
  # 4) Prognos (in-memory)
  # ------------------------------------------------------------
  message("Kör prognos...")
  prognos <- kor_prognos_enskild_in_memory(underlag, risktal, konfiguration)

  # ------------------------------------------------------------
  # 5) Bygg och returnera resultatobjekt
  #    (export-vänligt: allt är data frames / listor av data frames)
  # ------------------------------------------------------------
  list(
    metadata = list(
      skapad     = Sys.time(),
      prognostyp = konfiguration$prognostyp,
      scenario   = konfiguration$scenario %||% NA_character_,
      geografi   = konfiguration$enskild_geografi$namn
    ),
    konfiguration = konfiguration,
    risktal       = risktal,   # list av 6 tibbles
    prognos       = prognos    # list: geografi, totalbefolkning, komponenter, sammanfattning
  )
}

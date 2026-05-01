# prognos_kor.R
# Orkestrerar prognoskörningen i Shiny utan fil-/mapp-skapande.
#
# Förväntat användningssätt:
# - Shiny bygger en konfigurationslista (app_kontext$konfiguration)
# - server_config (eller motsv) anropar `kor_prognos(konfiguration)`
# - Funktionen returnerar ett resultatobjekt som kan visas i UI och exporteras.

#' Kör prognos (in-memory)
#'
#' @param konfiguration list med val från Shiny (prognostyp, scenario, period, geografi, parametrar, etc.)
#' @param data_provider valfri funktion/objekt som senare kan användas för DB-inhämtning
#' @param progress valfritt shiny::Progress-objekt eller callback, för framtida UI-progress
#' @return list (resultat) som kan användas av appen
kor_prognos <- function(konfiguration,
                        data_provider = NULL,
                        progress = NULL) {
  stopifnot(is.list(konfiguration))

  # ------------------------------------------------------------
  # 1) Validera miniminnehåll (lättviktigt – Shiny ansvarar för UI-validering)
  # ------------------------------------------------------------
  if (is.null(konfiguration$prognostyp)) {
    stop("konfiguration$prognostyp saknas")
  }
  if (!konfiguration$prognostyp %in% c("enskild", "regional")) {
    stop("konfiguration$prognostyp måste vara 'enskild' eller 'regional'")
  }

  # ------------------------------------------------------------
  # 2) Datainhämtning
  # ------------------------------------------------------------
  # Här ska ni senare koppla på DB. Just nu är detta en tydlig hook.
  # Förväntat output är att efterföljande steg (risk/prognos) hittar
  # det de behöver (helst via explicita argument, annars via objekt i env).
  underlag <- NULL
  if (!is.null(data_provider)) {
    underlag <- data_provider(konfiguration)
  }

  # ------------------------------------------------------------
  # 3) Riskberäkningar + prognos
  # ------------------------------------------------------------
  # TODO: Koppla in era befintliga filer/funktioner här.
  # I Analytikernätverkets repo görs detta via source(...) och
  # globala variabler. Här vill vi i första hand hålla det in-memory.
  #
  # Exempel på framtida struktur:
  #   risk <- kor_riskberakningar(underlag, konfiguration$riskparametrar, scenario = ...)
  #   res  <- kor_prognos_enskild(...) eller kor_prognos_regional(...)

  resultat <- list(
    metadata = list(
      skapad = Sys.time(),
      prognostyp = konfiguration$prognostyp,
      scenario = konfiguration$scenario %||% NA_character_
    ),
    konfiguration = konfiguration,
    underlag = underlag,
    data = NULL
  )

  resultat
}

# Liten hjälpfunktion (bas-R) för att kunna skriva x %||% y
`%||%` <- function(x, y) if (is.null(x)) y else x

# data_provider.R
# Hook för datainhämtning – ersätt med riktig DB-koppling när den finns.
#
# Kontraktet: en data_provider-funktion tar emot konfiguration och returnerar
# list(
#   kommun_lista = <samma struktur som i Analytikernatverket/befolkningsprognoser>,
#   riket_lista  = <samma struktur som i Analytikernatverket/befolkningsprognoser>
# )
#
# Struktur för kommun_lista:
#   $totfolkmangd          – tibble: Region, Ålder, Kön, År, Värde, Variabel
#   $medelfolkmangd        – tibble: Region, Ålder, Kön, År, Värde
#   $medelfolkmangd_modrar – tibble: Region, År, Ålder, Värde
#   $fodda                 – tibble: Region, År, Ålder, Värde
#   $doda                  – tibble: Region, Ålder, Kön, År, Värde
#   $inrikes_inflyttade    – tibble: Region, År, Ålder, Kön, Värde
#   $inrikes_utflyttade    – tibble: Region, År, Ålder, Kön, Värde
#   $invandring            – tibble: Region, År, Ålder, Kön, Värde
#   $utvandring            – tibble: Region, År, Ålder, Kön, Värde
#   $inflyttningar_lansgrans_raw – tibble: Region, År, Ålder, Kön, Total, Ovriga_lan
#   $utflyttningar_lansgrans_raw – tibble: Region, År, Ålder, Kön, Total, Ovriga_lan
#
# Struktur för riket_lista:
#   $riket_prognosinvånare_grund – tibble: År, Ålder, Kön, Värde
#   $invandring_riket            – tibble: År, Ålder, Kön, Värde
#   $fodelsetal                  – tibble: År, Ålder, Värde  (fruktsamhetskvoter)
#   $dodstal                     – tibble: År, Ålder, Kön, Värde, Variabel

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

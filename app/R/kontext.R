app_kontext <- reactiveValues(
  konfiguration  = NULL,              # konfiguration (ersätter konfig-R-fil)
  resultat       = NULL,              # prognosresultat i minnet
  fas            = "konfiguration"    # "konfiguration" | "korning" | "resultat"
)

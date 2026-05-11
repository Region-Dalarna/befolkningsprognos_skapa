# prognos_enskild.R
# In-memory wrapper kring kohort-komponent-prognosen för en enskild geografi,
# baserat på Analytikernatverket/befolkningsprognoser (prognos_engeografi.R).
#
# Inga filer läses från disk och inga RDS sparas –
# allt sker in-memory via argument och returvärde.
#
# OBS: tidyverse-paketen laddas i app.R (ingen library() här).

#' Kör befolkningsprognos för en enskild geografi (in-memory)
#'
#' @param underlag     list med $kommun_lista och $riket_lista
#' @param risktal      list med fodelserisker, dodsrisker, inflyttningsrisker,
#'                     utflyttningsrisker, invandringsrisker, utvandringsrisker
#'                     (output från kor_riskberakningar_in_memory)
#' @param konfiguration list med minst:
#'   $enskild_geografi$namn – namn på geografi (som i region-kolumnen i datan)
#'   $prognos_slut          – slutår (integer)
#'   $prognos_start         – startår (integer, optional: härleds från data om NULL)
#'   $avrundning            – "heltal" | "stokastisk" | "ingen"
#' @return list med geografi, totalbefolkning, komponenter, sammanfattning
kor_prognos_enskild_in_memory <- function(underlag, risktal, konfiguration) {

  set.seed(2025)  # För reproducerbarhet vid stokastisk avrundning

  # --- Lokala variabler som ersätter globaler ---
  AVRUNDNING <- konfiguration$avrundning %||% "heltal"
  geografi   <- konfiguration$enskild_geografi$namn

  if (is.null(geografi) || is.na(geografi) || nchar(trimws(geografi)) == 0) {
    stop("konfiguration$enskild_geografi$namn saknas eller är tomt")
  }

  # Förbered basbefolkning
  basbefolkning_geografi <- underlag$kommun_lista$totfolkmangd %>%
    filter(region == geografi, ar == max(ar)) %>%
    mutate(alder = as.numeric(alder))

  if (nrow(basbefolkning_geografi) == 0) {
    stop(paste("Geografin", geografi, "finns inte i underlaget!"))
  }

  # Härleda startår om det inte anges explicit
  senaste_historiska_ar <- max(as.integer(
    underlag$kommun_lista$totfolkmangd$ar))
  startår <- konfiguration$prognos_start %||% (senaste_historiska_ar + 1L)
  slutår  <- as.integer(konfiguration$prognos_slut)

  # Riksprognosdata
  riksbefolkning_prognos       <- underlag$riket_lista$riket_prognosinvanare_grund
  invandring_till_riket_prognos <- underlag$riket_lista$invandring_riket

  if (!is.character(riksbefolkning_prognos$ar)) {
    riksbefolkning_prognos <- riksbefolkning_prognos %>%
      mutate(ar = as.character(ar))
  }
  if (!is.character(invandring_till_riket_prognos$ar)) {
    invandring_till_riket_prognos <- invandring_till_riket_prognos %>%
      mutate(ar = as.character(ar))
  }

  # Konvertera risktal till tibbles (för säkerhets skull)
  risktal <- risktal %>% purrr::map(as_tibble)

  # ===========================================================
  # HJÄLPFUNKTIONER (stänger in över AVRUNDNING)
  # ===========================================================

  aldra_befolkning <- function(befolkning) {
    aldrad_befolkning <- befolkning %>%
      mutate(alder = alder + 1) %>%
      filter(alder <= 100)

    aldrad_100_plus <- aldrad_befolkning %>%
      filter(alder >= 100) %>%
      group_by(region, kon, ar, variabel) %>%
      summarise(varde = sum(varde), .groups = "drop") %>%
      mutate(alder = 100)

    bind_rows(
      aldrad_befolkning %>% filter(alder < 100),
      aldrad_100_plus
    )
  }

  berakna_fodda <- function(befolkning, fodelserisker_data, prognos_ar) {
    kvinnor_fertil   <- befolkning %>%
      filter(kon == "kvinnor", alder >= 15, alder <= 49)

    fodelserisker_ar <- fodelserisker_data %>%
      filter(ar == prognos_ar, region == unique(befolkning$region))

    if (nrow(fodelserisker_ar) == 0) {
      stop(paste("Inga födelserisker för år", prognos_ar,
                 "och region", unique(befolkning$region)))
    }

    fodda_per_alder <- kvinnor_fertil %>%
      left_join(
        fodelserisker_ar %>% select(region, alder, Fodelserisk = varde),
        by = c("region", "alder")
      ) %>%
      mutate(
        Antal_fodda = varde * Fodelserisk,
        Antal_fodda = replace_na(Antal_fodda, 0)
      ) %>%
      group_by(region) %>%
      summarise(Antal_fodda = sum(Antal_fodda, na.rm = TRUE), .groups = "drop")

    fodda_pojkar <- fodda_per_alder %>%
      mutate(kon = "män", alder = 0, ar = prognos_ar,
             varde = round(Antal_fodda * 0.512, 0),
             variabel = "Total folkmängd") %>%
      select(region, alder, kon, ar, varde, variabel)

    fodda_flickor <- fodda_per_alder %>%
      mutate(kon = "kvinnor", alder = 0, ar = prognos_ar,
             varde = round(Antal_fodda * 0.488, 0),
             variabel = "Total folkmängd") %>%
      select(region, alder, kon, ar, varde, variabel)

    fodda <- bind_rows(fodda_pojkar, fodda_flickor)

    list(
      fodda        = fodda,
      fodda_rapport = fodda %>% mutate(variabel = "Födda")
    )
  }

  berakna_doda <- function(befolkning, dodsrisker_data) {
    prognos_ar       <- unique(befolkning$ar)
    dodsrisker_ar    <- dodsrisker_data %>%
      filter(ar == prognos_ar, region == unique(befolkning$region))

    if (nrow(dodsrisker_ar) == 0) {
      stop(paste("Inga dödsrisker för år", prognos_ar,
                 "och region", unique(befolkning$region)))
    }

    if (AVRUNDNING == "stokastisk") {
      doda <- befolkning %>%
        left_join(
          dodsrisker_ar %>% select(region, kon, alder, Dodsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Dodsrisk = dplyr::case_when(
            is.na(Dodsrisk) & alder > 100 ~ dodsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Dodsrisk
          ),
          Antal_doda = purrr::map2_dbl(varde, Dodsrisk, ~ {
            if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1)
              NA_real_
            else
              tryCatch(rbinom(1, size = .x, prob = .y),
                       warning = function(w) NA_real_,
                       error   = function(e) NA_real_)
          }),
          Antal_doda = replace_na(Antal_doda, 0),
          varde      = Antal_doda,
          variabel   = "Döda"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else if (AVRUNDNING == "heltal") {
      doda <- befolkning %>%
        left_join(
          dodsrisker_ar %>% select(region, kon, alder, Dodsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Dodsrisk = dplyr::case_when(
            is.na(Dodsrisk) & alder > 100 ~ dodsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Dodsrisk
          ),
          Antal_doda = round(varde * Dodsrisk, 0),
          varde      = Antal_doda,
          variabel   = "Döda"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else {  # "ingen"
      doda <- befolkning %>%
        left_join(
          dodsrisker_ar %>% select(region, kon, alder, Dodsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Dodsrisk = dplyr::case_when(
            is.na(Dodsrisk) & alder > 100 ~ dodsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Dodsrisk
          ),
          Antal_doda = varde * Dodsrisk,
          varde      = Antal_doda,
          variabel   = "Döda"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    doda
  }

  berakna_inrikes_inflyttningar <- function(inflyttningsrisker_data,
                                            riksbefolkning, prognos_ar,
                                            geografi) {
    riksbef_ar           <- riksbefolkning %>% filter(ar == prognos_ar)
    inflyttningsrisker_ar <- inflyttningsrisker_data %>%
      filter(ar == prognos_ar, region == geografi)

    if (nrow(inflyttningsrisker_ar) == 0) {
      stop(paste("Inga inflyttningsrisker för år", prognos_ar,
                 "och region", geografi))
    }

    if (AVRUNDNING == "stokastisk") {
      inflyttningar <- riksbef_ar %>%
        select(kon, alder, Riksbefolkning = varde) %>%
        crossing(region = geografi) %>%
        left_join(
          inflyttningsrisker_ar %>%
            select(region, kon, alder, Inflyttningsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_inflyttningar = purrr::map2_dbl(
            Riksbefolkning, Inflyttningsrisk,
            ~ if (!is.na(.x) && !is.na(.y) && .x >= 0 && .y >= 0 && .y <= 1)
              rbinom(1, size = .x, prob = .y)
            else NA_real_
          ),
          Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
          ar      = prognos_ar,
          varde   = Antal_inflyttningar,
          variabel = "Inrikes inflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else if (AVRUNDNING == "heltal") {
      inflyttningar <- riksbef_ar %>%
        select(kon, alder, Riksbefolkning = varde) %>%
        crossing(region = geografi) %>%
        left_join(
          inflyttningsrisker_ar %>%
            select(region, kon, alder, Inflyttningsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_inflyttningar = round(Riksbefolkning * Inflyttningsrisk, 0),
          Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
          ar      = prognos_ar,
          varde   = Antal_inflyttningar,
          variabel = "Inrikes inflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else {  # "ingen"
      inflyttningar <- riksbef_ar %>%
        select(kon, alder, Riksbefolkning = varde) %>%
        crossing(region = geografi) %>%
        left_join(
          inflyttningsrisker_ar %>%
            select(region, kon, alder, Inflyttningsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_inflyttningar = Riksbefolkning * Inflyttningsrisk,
          Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
          ar      = prognos_ar,
          varde   = Antal_inflyttningar,
          variabel = "Inrikes inflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    inflyttningar
  }

  berakna_inrikes_utflyttningar <- function(befolkning, utflyttningsrisker_data) {
    prognos_ar           <- unique(befolkning$ar)
    utflyttningsrisker_ar <- utflyttningsrisker_data %>%
      filter(ar == prognos_ar, region == unique(befolkning$region))

    if (nrow(utflyttningsrisker_ar) == 0) {
      stop(paste("Inga utflyttningsrisker för år", prognos_ar,
                 "och region", unique(befolkning$region)))
    }

    if (AVRUNDNING == "stokastisk") {
      utflyttningar <- befolkning %>%
        left_join(
          utflyttningsrisker_ar %>%
            select(region, kon, alder, Utflyttningsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Utflyttningsrisk = dplyr::case_when(
            is.na(Utflyttningsrisk) & alder > 100 ~
              utflyttningsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Utflyttningsrisk
          ),
          Antal_utflyttningar = purrr::map2_dbl(varde, Utflyttningsrisk, ~ {
            if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1)
              NA_real_
            else
              tryCatch(rbinom(1, size = .x, prob = .y),
                       warning = function(w) NA_real_,
                       error   = function(e) NA_real_)
          }),
          Antal_utflyttningar = replace_na(Antal_utflyttningar, 0),
          varde   = Antal_utflyttningar,
          variabel = "Inrikes utflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else if (AVRUNDNING == "heltal") {
      utflyttningar <- befolkning %>%
        left_join(
          utflyttningsrisker_ar %>%
            select(region, kon, alder, Utflyttningsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Utflyttningsrisk = dplyr::case_when(
            is.na(Utflyttningsrisk) & alder > 100 ~
              utflyttningsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Utflyttningsrisk
          ),
          Utflyttningsrisk    = replace_na(Utflyttningsrisk, 0),
          Antal_utflyttningar = round(varde * Utflyttningsrisk, 0),
          varde   = Antal_utflyttningar,
          variabel = "Inrikes utflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else {  # "ingen"
      utflyttningar <- befolkning %>%
        left_join(
          utflyttningsrisker_ar %>%
            select(region, kon, alder, Utflyttningsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Utflyttningsrisk = dplyr::case_when(
            is.na(Utflyttningsrisk) & alder > 100 ~
              utflyttningsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Utflyttningsrisk
          ),
          Utflyttningsrisk    = replace_na(Utflyttningsrisk, 0),
          Antal_utflyttningar = varde * Utflyttningsrisk,
          varde   = Antal_utflyttningar,
          variabel = "Inrikes utflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    utflyttningar
  }

  berakna_invandring <- function(invandringsrisker_data, invandring_riket,
                                 prognos_ar, geografi) {
    invandring_ar        <- invandring_riket %>% filter(ar == prognos_ar)
    invandringsrisker_ar <- invandringsrisker_data %>%
      filter(ar == prognos_ar, region == geografi)

    if (nrow(invandringsrisker_ar) == 0) {
      stop(paste("Inga invandringsrisker för år", prognos_ar,
                 "och region", geografi))
    }

    if (AVRUNDNING == "stokastisk") {
      invandring <- invandring_ar %>%
        select(kon, alder, Riksinvandring = varde) %>%
        crossing(region = geografi) %>%
        left_join(
          invandringsrisker_ar %>%
            select(region, kon, alder, Invandringsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_invandrare = purrr::map2_dbl(
            Riksinvandring, Invandringsrisk,
            ~ if (!is.na(.x) && !is.na(.y) && .x >= 0 && .y >= 0 && .y <= 1)
              rbinom(1, size = .x, prob = .y)
            else NA_real_
          ),
          Antal_invandrare = replace_na(Antal_invandrare, 0),
          ar      = prognos_ar,
          varde   = Antal_invandrare,
          variabel = "Invandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else if (AVRUNDNING == "heltal") {
      invandring <- invandring_ar %>%
        select(kon, alder, Riksinvandring = varde) %>%
        crossing(region = geografi) %>%
        left_join(
          invandringsrisker_ar %>%
            select(region, kon, alder, Invandringsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_invandrare = round(Riksinvandring * Invandringsrisk, 0),
          Antal_invandrare = replace_na(Antal_invandrare, 0),
          ar      = prognos_ar,
          varde   = Antal_invandrare,
          variabel = "Invandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else {  # "ingen"
      invandring <- invandring_ar %>%
        select(kon, alder, Riksinvandring = varde) %>%
        crossing(region = geografi) %>%
        left_join(
          invandringsrisker_ar %>%
            select(region, kon, alder, Invandringsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_invandrare = Riksinvandring * Invandringsrisk,
          Antal_invandrare = replace_na(Antal_invandrare, 0),
          ar      = prognos_ar,
          varde   = Antal_invandrare,
          variabel = "Invandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    invandring
  }

  berakna_utvandring <- function(befolkning, utvandringsrisker_data) {
    prognos_ar           <- unique(befolkning$ar)
    utvandringsrisker_ar <- utvandringsrisker_data %>%
      filter(ar == prognos_ar, region == unique(befolkning$region))

    if (nrow(utvandringsrisker_ar) == 0) {
      stop(paste("Inga utvandringsrisker för år", prognos_ar,
                 "och region", unique(befolkning$region)))
    }

    if (AVRUNDNING == "stokastisk") {
      utvandring <- befolkning %>%
        left_join(
          utvandringsrisker_ar %>%
            select(region, kon, alder, Utvandringsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Utvandringsrisk = dplyr::case_when(
            is.na(Utvandringsrisk) & alder > 100 ~
              utvandringsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Utvandringsrisk
          ),
          Antal_utvandrare = purrr::map2_dbl(varde, Utvandringsrisk, ~ {
            if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1)
              NA_real_
            else
              tryCatch(rbinom(1, size = .x, prob = .y),
                       warning = function(w) NA_real_,
                       error   = function(e) NA_real_)
          }),
          Antal_utvandrare = replace_na(Antal_utvandrare, 0),
          varde   = Antal_utvandrare,
          variabel = "Utvandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else if (AVRUNDNING == "heltal") {
      utvandring <- befolkning %>%
        left_join(
          utvandringsrisker_ar %>%
            select(region, kon, alder, Utvandringsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Utvandringsrisk = dplyr::case_when(
            is.na(Utvandringsrisk) & alder > 100 ~
              utvandringsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Utvandringsrisk
          ),
          Utvandringsrisk  = replace_na(Utvandringsrisk, 0),
          Antal_utvandrare = round(varde * Utvandringsrisk, 0),
          varde   = Antal_utvandrare,
          variabel = "Utvandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else {  # "ingen"
      utvandring <- befolkning %>%
        left_join(
          utvandringsrisker_ar %>%
            select(region, kon, alder, Utvandringsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Utvandringsrisk = dplyr::case_when(
            is.na(Utvandringsrisk) & alder > 100 ~
              utvandringsrisker_ar %>%
              filter(region == .data$region, kon == .data$kon, alder == 100) %>%
              dplyr::pull(varde) %>% dplyr::first(),
            TRUE ~ Utvandringsrisk
          ),
          Utvandringsrisk  = replace_na(Utvandringsrisk, 0),
          Antal_utvandrare = varde * Utvandringsrisk,
          varde   = Antal_utvandrare,
          variabel = "Utvandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    utvandring
  }

  # ===========================================================
  # HUVUDFUNKTION: KOhort-komponent-prognos
  # ===========================================================
  gora_befolkningsprognos_single <- function(basbefolkning, risktal,
                                             riksbefolkning_prognos,
                                             invandring_till_riket_prognos,
                                             geografi, startår, slutår) {
    alla_resultat <- list()
    message(paste("Geografi:", geografi))
    message(paste("Period:", startår, "-", slutår, "\n"))

    for (prognos_ar in as.character(startår:slutår)) {
      message(paste0("  Beräknar år ", prognos_ar, "..."))

      # 1. Åldra befolkning
      if (prognos_ar == as.character(startår)) {
        aktuell_befolkning <- basbefolkning %>%
          mutate(ar = .env$prognos_ar)
        aktuell_befolkning <- aldra_befolkning(aktuell_befolkning)
      } else {
        aktuell_befolkning <- alla_resultat[[
          as.character(as.numeric(prognos_ar) - 1)]]$befolkning %>%
          mutate(ar = .env$prognos_ar)
        aktuell_befolkning <- aldra_befolkning(aktuell_befolkning)
      }

      # 2. Födda
      fodda_resultat <- berakna_fodda(
        aktuell_befolkning, risktal$fodelserisker, prognos_ar)
      aktuell_befolkning <- bind_rows(
        aktuell_befolkning, fodda_resultat$fodda)

      # 3. Döda
      doda <- berakna_doda(aktuell_befolkning, risktal$dodsrisker)

      # 4. Inrikes inflyttning
      inrikes_inflyttningar <- berakna_inrikes_inflyttningar(
        risktal$inflyttningsrisker, riksbefolkning_prognos, prognos_ar, geografi)

      # 5. Inrikes utflyttning
      inrikes_utflyttningar <- berakna_inrikes_utflyttningar(
        aktuell_befolkning, risktal$utflyttningsrisker)

      # 6. Invandring
      invandring <- berakna_invandring(
        risktal$invandringsrisker, invandring_till_riket_prognos, prognos_ar, geografi)

      # 7. Utvandring
      utvandring <- berakna_utvandring(
        aktuell_befolkning, risktal$utvandringsrisker)

      # 8. Netto
      befolkning_komponenter <- bind_rows(
        fodda_resultat$fodda_rapport, doda,
        inrikes_inflyttningar, inrikes_utflyttningar,
        invandring, utvandring
      )

      befolkning_forandringar <- befolkning_komponenter %>%
        mutate(
          Forändring = dplyr::case_when(
            variabel == "Födda"             ~ 0,
            variabel == "Döda"              ~ -varde,
            variabel == "Inrikes inflyttning" ~ varde,
            variabel == "Inrikes utflyttning" ~ -varde,
            variabel == "Invandring"        ~ varde,
            variabel == "Utvandring"        ~ -varde,
            TRUE                            ~ 0
          )
        ) %>%
        group_by(region, kon, alder, ar) %>%
        summarise(Nettoforändring = sum(Forändring), .groups = "drop")

      ny_befolkning <- aktuell_befolkning %>%
        select(region, kon, alder, ar, varde) %>%
        left_join(befolkning_forandringar,
                  by = c("region", "kon", "alder", "ar")) %>%
        mutate(
          Nettoforändring = replace_na(Nettoforändring, 0),
          Ny_befolkning   = varde + Nettoforändring,
          Ny_befolkning   = round(Ny_befolkning, 0),
          Ny_befolkning   = pmax(0, Ny_befolkning)
        ) %>%
        select(region, kon, alder, ar, varde = Ny_befolkning) %>%
        mutate(variabel = "Total folkmängd")

      alla_resultat[[prognos_ar]] <- list(
        befolkning  = ny_befolkning,
        komponenter = list(
          fodda              = fodda_resultat$fodda_rapport,
          doda               = doda,
          inrikes_inflyttning = inrikes_inflyttningar,
          inrikes_utflyttning = inrikes_utflyttningar,
          invandring         = invandring,
          utvandring         = utvandring
        )
      )

      total_bef  <- sum(ny_befolkning$varde)
      netto_bef  <- sum(befolkning_forandringar$Nettoforändring) +
        sum(fodda_resultat$fodda$varde)
      netto_bef_heltal <- as.integer(round(netto_bef, 0))
      message(paste0("    Total befolkning: ",
                     format(total_bef, big.mark = " "),
                     " (förändring: ",
                     ifelse(netto_bef_heltal >= 0, "+", ""),
                     format(netto_bef_heltal, big.mark = " "), ")"))
    }

    # --- Sammanställ ---
    totalbefolkning <- purrr::map_dfr(
      as.character(startår:slutår),
      ~ alla_resultat[[.x]]$befolkning
    )

    komponenter <- purrr::map(
      setNames(as.character(startår:slutår), as.character(startår:slutår)),
      ~ alla_resultat[[.x]]$komponenter
    )

    sammanfattning <- totalbefolkning %>%
      group_by(ar) %>%
      summarise(
        Total_befolkning = sum(varde),
        Kvinnor          = sum(varde[kon == "kvinnor"]),
        Män              = sum(varde[kon == "män"]),
        .groups = "drop"
      )

    list(
      geografi        = geografi,
      totalbefolkning = totalbefolkning,
      komponenter     = komponenter,
      sammanfattning  = sammanfattning
    )
  }

  # ===========================================================
  # KÖR PROGNOSEN
  # ===========================================================
  message("\n=== STARTAR ENSKILD BEFOLKNINGSPROGNOS (in-memory) ===\n")

  resultat <- gora_befolkningsprognos_single(
    basbefolkning                 = basbefolkning_geografi,
    risktal                       = risktal,
    riksbefolkning_prognos        = riksbefolkning_prognos,
    invandring_till_riket_prognos = invandring_till_riket_prognos,
    geografi                      = geografi,
    startår                       = startår,
    slutår                        = slutår
  )

  message("\nPrognos klar!\n")
  resultat
}

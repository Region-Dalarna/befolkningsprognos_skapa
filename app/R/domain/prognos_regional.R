# prognos_regional.R
# In-memory wrapper kring den regionala kohort-komponent-prognosen, baserat på
# Analytikernatverket/befolkningsprognoser (prognos_regional.R).
#
# Inga filer läses från disk och inga RDS sparas –
# allt sker in-memory via argument och returvärde.
#
# OBS: tidyverse-paketen laddas i app.R (ingen library() här).

#' Kör regional befolkningsprognos (län + kommuner med avstämning)
#'
#' @param underlag     list med $kommun_lista och $riket_lista (snake_case)
#' @param risktal      list med fodelserisker, dodsrisker, inflyttningsrisker,
#'   utflyttningsrisker, invandringsrisker, utvandringsrisker,
#'   inflyttningsandelar_lansgrans, utflyttningsandelar_lansgrans
#'   (output från kor_riskberakningar_in_memory)
#' @param konfiguration list med minst:
#'   $regional_installningar$lan – namn på länet
#'   $prognos_slut                – slutår (integer)
#'   $prognos_start               – startår (integer, optional: härleds från data)
#'   $avrundning                  – "heltal" | "stokastisk" | "ingen"
#'   $dodsfall_fore_aldring       – TRUE/FALSE (default TRUE)
#'   $metod_avstamning_regional   – "minsta_kvadrat" (default), "raking" eller "proportional"
#' @return list med geografi, totalbefolkning, komponenter, sammanfattning,
#'   kommun_resultat (per-kommun-resultat: list[region] = list(totalbefolkning,
#'   komponenter))
kor_prognos_regional_in_memory <- function(underlag, risktal, konfiguration) {

  set.seed(2025)  # För reproducerbarhet vid stokastisk avrundning

  # --- Lokala variabler som ersätter globaler ---
  AVRUNDNING                <- konfiguration$avrundning %||% "heltal"
  dodsfall_fore_aldring     <- isTRUE(konfiguration$dodsfall_fore_aldring %||% TRUE)
  metod_avstamning_regional <- konfiguration$metod_avstamning_regional %||% "minsta_kvadrat"
  lan_namn                  <- konfiguration$regional_installningar$lan

  if (is.null(lan_namn) || is.na(lan_namn) || nchar(trimws(lan_namn)) == 0) {
    stop("konfiguration$regional_installningar$lan saknas eller är tomt")
  }

  kommun_lista <- underlag$kommun_lista
  riket_lista  <- underlag$riket_lista

  # Härleda kommuner från data (alla regioner i kommun_lista utom länet och Riket)
  kommuner <- setdiff(
    unique(as.character(kommun_lista$totfolkmangd$region)),
    c(lan_namn, "Riket")
  )

  if (length(kommuner) == 0) {
    stop(paste("Inga kommuner identifierade för länet", lan_namn))
  }

  # Förbered basbefolkning
  basbefolkning_kommun <- kommun_lista$totfolkmangd %>%
    filter(ar == max(ar)) %>%
    mutate(alder = as.numeric(alder))

  if (!lan_namn %in% unique(basbefolkning_kommun$region)) {
    stop(paste("Länet", lan_namn, "finns inte i data!"))
  }

  basbefolkning_kommun <- basbefolkning_kommun %>%
    filter(region %in% c(lan_namn, kommuner))

  # Härleda startår om det inte anges explicit
  senaste_historiska_ar <- max(as.integer(kommun_lista$totfolkmangd$ar))
  startår <- konfiguration$prognos_start %||% (senaste_historiska_ar + 1L)
  slutår  <- as.integer(konfiguration$prognos_slut)

  # Riksprognosdata
  riksbefolkning_prognos       <- riket_lista$riket_prognosinvanare_grund
  invandring_till_riket_prognos <- riket_lista$invandring_riket

  if (!is.character(riksbefolkning_prognos$ar)) {
    riksbefolkning_prognos <- riksbefolkning_prognos %>%
      mutate(ar = as.character(ar))
  }
  if (!is.character(invandring_till_riket_prognos$ar)) {
    invandring_till_riket_prognos <- invandring_till_riket_prognos %>%
      mutate(ar = as.character(ar))
  }

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
    kvinnor_fertil <- befolkning %>%
      filter(kon == "kvinnor", alder >= 15, alder <= 49)

    fodelserisker_ar <- fodelserisker_data %>%
      filter(ar == prognos_ar)

    if (nrow(fodelserisker_ar) == 0) {
      stop(paste("Inga födelserisker för år", prognos_ar))
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
      fodda         = fodda,
      fodda_rapport = fodda %>% mutate(variabel = "Födda")
    )
  }

  berakna_doda <- function(befolkning, dodsrisker_data) {
    prognos_ar    <- unique(befolkning$ar)
    dodsrisker_ar <- dodsrisker_data %>% filter(ar == prognos_ar)

    if (nrow(dodsrisker_ar) == 0) {
      stop(paste("Inga dödsrisker för år", prognos_ar))
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
                                            riksbefolkning, prognos_ar) {
    riksbef_ar           <- riksbefolkning %>% filter(ar == prognos_ar)
    inflyttningsrisker_ar <- inflyttningsrisker_data %>% filter(ar == prognos_ar)

    if (nrow(inflyttningsrisker_ar) == 0) {
      stop(paste("Inga inflyttningsrisker för år", prognos_ar))
    }

    alla_regioner <- unique(inflyttningsrisker_ar$region)

    if (AVRUNDNING == "stokastisk") {
      inflyttningar <- riksbef_ar %>%
        select(kon, alder, Riksbefolkning = varde) %>%
        crossing(region = alla_regioner) %>%
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
          ar       = prognos_ar,
          varde    = Antal_inflyttningar,
          variabel = "Inrikes inflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else if (AVRUNDNING == "heltal") {
      inflyttningar <- riksbef_ar %>%
        select(kon, alder, Riksbefolkning = varde) %>%
        crossing(region = alla_regioner) %>%
        left_join(
          inflyttningsrisker_ar %>%
            select(region, kon, alder, Inflyttningsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_inflyttningar = round(Riksbefolkning * Inflyttningsrisk, 0),
          Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
          ar       = prognos_ar,
          varde    = Antal_inflyttningar,
          variabel = "Inrikes inflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else {  # "ingen"
      inflyttningar <- riksbef_ar %>%
        select(kon, alder, Riksbefolkning = varde) %>%
        crossing(region = alla_regioner) %>%
        left_join(
          inflyttningsrisker_ar %>%
            select(region, kon, alder, Inflyttningsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_inflyttningar = Riksbefolkning * Inflyttningsrisk,
          Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
          ar       = prognos_ar,
          varde    = Antal_inflyttningar,
          variabel = "Inrikes inflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    inflyttningar
  }

  berakna_inrikes_utflyttningar <- function(befolkning, utflyttningsrisker_data) {
    prognos_ar           <- unique(befolkning$ar)
    utflyttningsrisker_ar <- utflyttningsrisker_data %>% filter(ar == prognos_ar)

    if (nrow(utflyttningsrisker_ar) == 0) {
      stop(paste("Inga utflyttningsrisker för år", prognos_ar))
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
          varde    = Antal_utflyttningar,
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
          varde    = Antal_utflyttningar,
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
          varde    = Antal_utflyttningar,
          variabel = "Inrikes utflyttning"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    utflyttningar
  }

  berakna_invandring <- function(invandringsrisker_data, invandring_riket,
                                 prognos_ar) {
    invandring_ar        <- invandring_riket %>% filter(ar == prognos_ar)
    invandringsrisker_ar <- invandringsrisker_data %>% filter(ar == prognos_ar)

    if (nrow(invandringsrisker_ar) == 0) {
      stop(paste("Inga invandringsrisker för år", prognos_ar))
    }

    alla_regioner <- unique(invandringsrisker_ar$region)

    if (AVRUNDNING == "stokastisk") {
      invandring <- invandring_ar %>%
        select(kon, alder, Riksinvandring = varde) %>%
        crossing(region = alla_regioner) %>%
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
          ar       = prognos_ar,
          varde    = Antal_invandrare,
          variabel = "Invandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else if (AVRUNDNING == "heltal") {
      invandring <- invandring_ar %>%
        select(kon, alder, Riksinvandring = varde) %>%
        crossing(region = alla_regioner) %>%
        left_join(
          invandringsrisker_ar %>%
            select(region, kon, alder, Invandringsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_invandrare = round(Riksinvandring * Invandringsrisk, 0),
          Antal_invandrare = replace_na(Antal_invandrare, 0),
          ar       = prognos_ar,
          varde    = Antal_invandrare,
          variabel = "Invandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

    } else {  # "ingen"
      invandring <- invandring_ar %>%
        select(kon, alder, Riksinvandring = varde) %>%
        crossing(region = alla_regioner) %>%
        left_join(
          invandringsrisker_ar %>%
            select(region, kon, alder, Invandringsrisk = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Antal_invandrare = Riksinvandring * Invandringsrisk,
          Antal_invandrare = replace_na(Antal_invandrare, 0),
          ar       = prognos_ar,
          varde    = Antal_invandrare,
          variabel = "Invandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    invandring
  }

  berakna_utvandring <- function(befolkning, utvandringsrisker_data) {
    prognos_ar           <- unique(befolkning$ar)
    utvandringsrisker_ar <- utvandringsrisker_data %>% filter(ar == prognos_ar)

    if (nrow(utvandringsrisker_ar) == 0) {
      stop(paste("Inga utvandringsrisker för år", prognos_ar))
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
          varde    = Antal_utvandrare,
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
          varde    = Antal_utvandrare,
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
          varde    = Antal_utvandrare,
          variabel = "Utvandring"
        ) %>%
        select(region, kon, alder, ar, varde, variabel)
    }

    utvandring
  }

  # ===========================================================
  # AVSTÄMNING: Flyttningar över länsgräns
  # ===========================================================
  avstam_flyttningar_lansgrans <- function(kommun_flyttningar, lansgransandelar,
                                           lan_flyttningar, lan_namn, kommuner,
                                           ar, flyttningstyp = "inflyttning") {

    if (AVRUNDNING %in% c("heltal", "stokastisk")) {
      # Stokastisk avrundning återanvänder heltal-avstämningen (samma form på data)
      kommun_lansgrans <- kommun_flyttningar %>%
        filter(region %in% kommuner) %>%
        left_join(
          lansgransandelar %>%
            filter(ar == .env$ar) %>%
            select(region, kon, alder, Andel_lansgrans = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Flyttningar_lansgrans = round(varde * Andel_lansgrans, 0)
        ) %>%
        group_by(kon, alder, ar, variabel) %>%
        summarise(
          Kommun_lansgrans_total = sum(Flyttningar_lansgrans, na.rm = TRUE),
          .groups = "drop"
        )

      lan_total <- lan_flyttningar %>%
        filter(region == lan_namn) %>%
        select(kon, alder, ar, variabel, Lan_total = varde)

      justering <- lan_total %>%
        left_join(kommun_lansgrans, by = c("kon", "alder", "ar", "variabel")) %>%
        mutate(
          Kommun_lansgrans_total = replace_na(Kommun_lansgrans_total, 0),
          Justeringsfaktor = dplyr::case_when(
            Kommun_lansgrans_total > 0                       ~ Lan_total / Kommun_lansgrans_total,
            Lan_total > 0 & Kommun_lansgrans_total == 0      ~ NA_real_,
            TRUE                                             ~ 1
          )
        )

      kommun_justerad <- kommun_flyttningar %>%
        filter(region %in% kommuner) %>%
        left_join(
          lansgransandelar %>%
            filter(ar == .env$ar) %>%
            select(region, kon, alder, Andel_lansgrans = varde),
          by = c("region", "kon", "alder")
        ) %>%
        left_join(
          justering %>% select(kon, alder, ar, variabel, Justeringsfaktor, Lan_total),
          by = c("kon", "alder", "ar", "variabel")
        ) %>%
        mutate(
          Flyttningar_inom_lan          = round(varde * (1 - Andel_lansgrans), 0),
          Flyttningar_lansgrans_original = round(varde * Andel_lansgrans, 0),
          Flyttningar_lansgrans_justerad = dplyr::case_when(
            !is.na(Justeringsfaktor) ~
              round(Flyttningar_lansgrans_original * Justeringsfaktor, 0),
            is.na(Justeringsfaktor) & !is.na(Lan_total) & Lan_total > 0 ~
              round(Lan_total / dplyr::n_distinct(region[region %in% kommuner]), 0),
            TRUE ~ Flyttningar_lansgrans_original
          ),
          varde = Flyttningar_inom_lan + Flyttningar_lansgrans_justerad
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

      slutlig_kontroll <- kommun_justerad %>%
        left_join(
          lansgransandelar %>%
            filter(ar == .env$ar) %>%
            select(region, kon, alder, Andel_lansgrans = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(Flyttningar_lansgrans = round(varde * Andel_lansgrans, 0)) %>%
        group_by(kon, alder, ar, variabel) %>%
        summarise(Ny_kommun_lansgrans_total = sum(Flyttningar_lansgrans, na.rm = TRUE),
                  .groups = "drop") %>%
        left_join(lan_total, by = c("kon", "alder", "ar", "variabel")) %>%
        mutate(Slutlig_diff = Lan_total - Ny_kommun_lansgrans_total)

      if (any(abs(slutlig_kontroll$Slutlig_diff) > 0, na.rm = TRUE)) {
        for (i in seq_len(nrow(slutlig_kontroll))) {
          if (!is.na(slutlig_kontroll$Slutlig_diff[i]) &&
              abs(slutlig_kontroll$Slutlig_diff[i]) > 0) {
            storsta_kommun <- kommun_justerad %>%
              filter(
                kon      == slutlig_kontroll$kon[i],
                alder    == slutlig_kontroll$alder[i],
                ar       == slutlig_kontroll$ar[i],
                variabel == slutlig_kontroll$variabel[i]
              ) %>%
              arrange(desc(varde)) %>%
              slice(1)

            if (nrow(storsta_kommun) > 0) {
              kommun_justerad <- kommun_justerad %>%
                mutate(
                  varde = dplyr::case_when(
                    region   == storsta_kommun$region[1] &
                      kon      == storsta_kommun$kon[1] &
                      alder    == storsta_kommun$alder[1] &
                      ar       == storsta_kommun$ar[1] &
                      variabel == storsta_kommun$variabel[1] ~
                      varde + slutlig_kontroll$Slutlig_diff[i],
                    TRUE ~ varde
                  )
                )
            }
          }
        }
      }

      return(kommun_justerad)

    } else if (AVRUNDNING == "ingen") {

      kommun_lansgrans <- kommun_flyttningar %>%
        filter(region %in% kommuner) %>%
        left_join(
          lansgransandelar %>%
            filter(ar == .env$ar) %>%
            select(region, kon, alder, Andel_lansgrans = varde),
          by = c("region", "kon", "alder")
        ) %>%
        mutate(
          Flyttningar_lansgrans = varde * Andel_lansgrans
        ) %>%
        group_by(kon, alder, ar, variabel) %>%
        summarise(
          Kommun_lansgrans_total = sum(Flyttningar_lansgrans, na.rm = TRUE),
          .groups = "drop"
        )

      lan_total <- lan_flyttningar %>%
        filter(region == lan_namn) %>%
        select(kon, alder, ar, variabel, Lan_total = varde)

      justering <- lan_total %>%
        left_join(kommun_lansgrans, by = c("kon", "alder", "ar", "variabel")) %>%
        mutate(
          Kommun_lansgrans_total = replace_na(Kommun_lansgrans_total, 0),
          Justeringsfaktor = dplyr::case_when(
            Kommun_lansgrans_total > 0                       ~ Lan_total / Kommun_lansgrans_total,
            Lan_total > 0 & Kommun_lansgrans_total == 0      ~ NA_real_,
            TRUE                                             ~ 1
          )
        )

      kommun_justerad <- kommun_flyttningar %>%
        filter(region %in% kommuner) %>%
        left_join(
          lansgransandelar %>%
            filter(ar == .env$ar) %>%
            select(region, kon, alder, Andel_lansgrans = varde),
          by = c("region", "kon", "alder")
        ) %>%
        left_join(
          justering %>% select(kon, alder, ar, variabel, Justeringsfaktor, Lan_total),
          by = c("kon", "alder", "ar", "variabel")
        ) %>%
        mutate(
          Flyttningar_inom_lan          = varde * (1 - Andel_lansgrans),
          Flyttningar_lansgrans_original = varde * Andel_lansgrans,
          Flyttningar_lansgrans_justerad = dplyr::case_when(
            !is.na(Justeringsfaktor) ~ Flyttningar_lansgrans_original * Justeringsfaktor,
            is.na(Justeringsfaktor) & !is.na(Lan_total) & Lan_total > 0 ~
              Lan_total / dplyr::n_distinct(region[region %in% kommuner]),
            TRUE ~ Flyttningar_lansgrans_original
          ),
          varde = Flyttningar_inom_lan + Flyttningar_lansgrans_justerad
        ) %>%
        select(region, kon, alder, ar, varde, variabel)

      # Robust avrundning sist (Largest Remainder, NA-säker)
      kommun_final <- kommun_justerad %>%
        dplyr::group_by(kon, alder, ar, variabel) %>%
        dplyr::group_modify(~ {
          x <- .x
          x$varde <- as.numeric(x$varde)
          x$varde[is.na(x$varde)] <- 0

          base   <- floor(x$varde)
          resid  <- x$varde - base
          target <- as.integer(round(sum(x$varde, na.rm = TRUE)))
          need   <- target - sum(base)

          if (!is.na(need) && need != 0 && nrow(x) > 0) {
            k   <- min(abs(need), nrow(x))
            idx <- if (need > 0) order(resid, decreasing = TRUE) else order(resid, decreasing = FALSE)
            idx <- idx[seq_len(k)]
            base[idx] <- base[idx] + ifelse(need > 0, 1L, -1L)
          }

          x$varde <- base
          x
        }) %>%
        dplyr::ungroup()

      return(kommun_final)
    }
  }

  # ===========================================================
  # GENERELL AVSTÄMNINGSFUNKTION (för övriga komponenter)
  # ===========================================================
  avstam_komponent_mot_lan <- function(komponent_data, lan_namn, kommuner,
                                       komponent_namn = "",
                                       metod = "minsta_kvadrat",
                                       max_iter = 50, tol = 1e-6) {
    lan_data    <- komponent_data %>% filter(region == lan_namn)
    kommun_data <- komponent_data %>% filter(region %in% kommuner)

    if (nrow(lan_data) == 0) return(kommun_data)

    kommun_mat <- kommun_data %>%
      mutate(id = paste(region, kon, alder, sep = "_")) %>%
      select(id, region, kon, alder, ar, variabel, varde)

    lan_mal <- lan_data %>%
      group_by(kon, alder, ar, variabel) %>%
      summarise(Lan_total = sum(varde), .groups = "drop")

    kommun_summa <- kommun_mat %>%
      group_by(kon, alder, ar, variabel) %>%
      summarise(Kommun_total = sum(varde), .groups = "drop")

    justeringstabell <- lan_mal %>%
      left_join(kommun_summa, by = c("kon", "alder", "ar", "variabel")) %>%
      mutate(Justeringsfaktor = if_else(Kommun_total > 0, Lan_total / Kommun_total, NA_real_))

    nya_rader <- justeringstabell %>%
      filter(is.na(Kommun_total) & Lan_total > 0) %>%
      crossing(region = kommuner) %>%
      mutate(varde = round(Lan_total / length(kommuner), 0)) %>%
      select(region, kon, alder, ar, variabel, varde)

    if (metod == "raking") {

      df <- kommun_mat %>%
        left_join(justeringstabell %>% select(kon, alder, ar, variabel, Lan_total),
                  by = c("kon", "alder", "ar", "variabel")) %>%
        group_by(region, kon, alder, ar, variabel) %>%
        summarise(varde = sum(varde), Lan_total = first(Lan_total), .groups = "drop")

      iter        <- 0
      konvergerat <- FALSE

      while (iter < max_iter && !konvergerat) {
        iter <- iter + 1

        df <- df %>%
          group_by(kon, ar, variabel) %>%
          mutate(faktor_kon = if_else(sum(varde) > 0, Lan_total[1] / sum(varde), 1),
                 varde = varde * faktor_kon) %>%
          ungroup()

        df <- df %>%
          group_by(alder, ar, variabel) %>%
          mutate(faktor_alder = if_else(sum(varde) > 0, Lan_total[1] / sum(varde), 1),
                 varde = varde * faktor_alder) %>%
          ungroup()

        totalskillnad <- df %>%
          group_by(kon, alder, ar, variabel) %>%
          summarise(Summa = sum(varde), .groups = "drop") %>%
          left_join(lan_mal, by = c("kon", "alder", "ar", "variabel")) %>%
          summarise(diff = sum(abs(Summa - Lan_total), na.rm = TRUE)) %>%
          dplyr::pull(diff)

        konvergerat <- totalskillnad < tol
      }

      befintliga_kombinationer <- df %>%
        select(region, kon, alder, ar, variabel) %>%
        distinct()

      nya_rader_unika <- nya_rader %>%
        anti_join(befintliga_kombinationer,
                  by = c("region", "kon", "alder", "ar", "variabel"))

      kommun_final <- bind_rows(df %>% select(region, kon, alder, ar, variabel, varde),
                                nya_rader_unika)

    } else if (metod == "proportional") {

      kommun_justerad <- kommun_data %>%
        left_join(justeringstabell %>% select(kon, alder, ar, variabel, Justeringsfaktor),
                  by = c("kon", "alder", "ar", "variabel")) %>%
        mutate(varde = if_else(!is.na(Justeringsfaktor),
                               round(varde * Justeringsfaktor),
                               varde)) %>%
        select(-Justeringsfaktor)

      befintliga_kombinationer <- kommun_justerad %>%
        select(region, kon, alder, ar, variabel) %>%
        distinct()

      nya_rader_unika <- nya_rader %>%
        anti_join(befintliga_kombinationer,
                  by = c("region", "kon", "alder", "ar", "variabel"))

      kommun_final <- bind_rows(kommun_justerad, nya_rader_unika)

    } else {  # "minsta_kvadrat" (default)

      kommun_justerad <- kommun_data %>%
        left_join(lan_mal, by = c("kon", "alder", "ar", "variabel")) %>%
        group_by(kon, alder, ar, variabel) %>%
        group_modify(~ {
          komm <- .x
          if (nrow(komm) == 0) return(komm)

          lan_tot <- first(komm$Lan_total)

          if (!is.na(lan_tot) && sum(komm$varde) > 0) {
            n    <- nrow(komm)
            Dmat <- diag(n)
            dvec <- komm$varde
            Amat <- matrix(1, nrow = n)
            bvec <- lan_tot

            res <- try(quadprog::solve.QP(Dmat, dvec, t(Amat), bvec, meq = 1),
                       silent = TRUE)
            if (!inherits(res, "try-error")) {
              komm$varde <- round(res$solution)
            }
          }

          komm
        }) %>%
        ungroup() %>%
        select(region, kon, alder, ar, variabel, varde)

      befintliga_kombinationer <- kommun_justerad %>%
        select(region, kon, alder, ar, variabel) %>%
        distinct()

      nya_rader_unika <- nya_rader %>%
        anti_join(befintliga_kombinationer,
                  by = c("region", "kon", "alder", "ar", "variabel"))

      kommun_final <- bind_rows(kommun_justerad, nya_rader_unika)
    }

    return(kommun_final)
  }

  # ===========================================================
  # HUVUDFUNKTION: REGIONAL KOhort-komponent-prognos
  # ===========================================================
  gora_regional_befolkningsprognos <- function(basbefolkning, risktal,
                                               riksbefolkning_prognos,
                                               invandring_till_riket_prognos,
                                               lan_namn, kommuner,
                                               startår, slutår) {
    alla_resultat <- list()
    message(paste("Län:", lan_namn))
    message(paste("Antal kommuner:", length(kommuner)))
    message(paste("Period:", startår, "-", slutår, "\n"))

    for (prognos_ar in as.character(startår:slutår)) {
      message(paste0("  Beräknar år ", prognos_ar, "..."))

      # 1. Sätt ingående befolkning
      if (prognos_ar == as.character(startår)) {
        ingaende_befolkning <- basbefolkning %>%
          mutate(ar = .env$prognos_ar)
      } else {
        ingaende_befolkning <- alla_resultat[[
          as.character(as.numeric(prognos_ar) - 1)]]$befolkning %>%
          mutate(ar = .env$prognos_ar)
      }

      # 2. Beräkna döda på ingående befolkning ELLER åldra först
      if (dodsfall_fore_aldring) {
        doda <- berakna_doda(ingaende_befolkning, risktal$dodsrisker)
        aktuell_befolkning <- aldra_befolkning(ingaende_befolkning)
      } else {
        aktuell_befolkning <- aldra_befolkning(ingaende_befolkning)
      }

      # 3. Beräkna födda + avstäm
      fodda_resultat <- berakna_fodda(
        aktuell_befolkning, risktal$fodelserisker, prognos_ar)

      fodda_kommun_justerad <- avstam_komponent_mot_lan(
        fodda_resultat$fodda_rapport,
        lan_namn, kommuner, "födda",
        metod = metod_avstamning_regional
      )

      fodda_rapport_final <- bind_rows(
        fodda_resultat$fodda_rapport %>% filter(region == lan_namn),
        fodda_kommun_justerad
      )

      # Lägg till 0-åringar
      fodda_0_aringar <- fodda_rapport_final %>%
        mutate(alder = 0, variabel = "Total folkmängd") %>%
        select(region, alder, kon, ar, varde, variabel)

      aktuell_befolkning <- bind_rows(aktuell_befolkning, fodda_0_aringar)

      # 4. Beräkna döda (om gammal metod), avstäm
      if (!dodsfall_fore_aldring) {
        doda <- berakna_doda(aktuell_befolkning, risktal$dodsrisker)
      }
      doda_kommun_justerad <- avstam_komponent_mot_lan(
        doda, lan_namn, kommuner, "döda", metod = metod_avstamning_regional)
      doda_final <- bind_rows(
        doda %>% filter(region == lan_namn),
        doda_kommun_justerad
      )

      # 5. Beräkna inrikes flyttningar med särskild länsgränshantering
      inrikes_inflyttningar <- berakna_inrikes_inflyttningar(
        risktal$inflyttningsrisker, riksbefolkning_prognos, prognos_ar)

      inrikes_utflyttningar <- berakna_inrikes_utflyttningar(
        aktuell_befolkning, risktal$utflyttningsrisker)

      inflyttningar_kommun_justerad <- avstam_flyttningar_lansgrans(
        inrikes_inflyttningar,
        risktal$inflyttningsandelar_lansgrans,
        inrikes_inflyttningar,
        lan_namn, kommuner, prognos_ar, "inflyttning"
      )

      inrikes_inflyttningar_final <- bind_rows(
        inrikes_inflyttningar %>% filter(region == lan_namn),
        inflyttningar_kommun_justerad
      )

      utflyttningar_kommun_justerad <- avstam_flyttningar_lansgrans(
        inrikes_utflyttningar,
        risktal$utflyttningsandelar_lansgrans,
        inrikes_utflyttningar,
        lan_namn, kommuner, prognos_ar, "utflyttning"
      )

      inrikes_utflyttningar_final <- bind_rows(
        inrikes_utflyttningar %>% filter(region == lan_namn),
        utflyttningar_kommun_justerad
      )

      # 6. Beräkna invandring + avstäm
      invandring <- berakna_invandring(
        risktal$invandringsrisker, invandring_till_riket_prognos, prognos_ar)
      invandring_kommun_justerad <- avstam_komponent_mot_lan(
        invandring, lan_namn, kommuner, "invandring",
        metod = metod_avstamning_regional)
      invandring_final <- bind_rows(
        invandring %>% filter(region == lan_namn),
        invandring_kommun_justerad
      )

      # 7. Beräkna utvandring + avstäm
      utvandring <- berakna_utvandring(
        aktuell_befolkning, risktal$utvandringsrisker)
      utvandring_kommun_justerad <- avstam_komponent_mot_lan(
        utvandring, lan_namn, kommuner, "utvandring",
        metod = metod_avstamning_regional)
      utvandring_final <- bind_rows(
        utvandring %>% filter(region == lan_namn),
        utvandring_kommun_justerad
      )

      # 8. Sammanställ alla komponenter
      befolkning_komponenter <- bind_rows(
        fodda_rapport_final, doda_final,
        inrikes_inflyttningar_final, inrikes_utflyttningar_final,
        invandring_final, utvandring_final
      )

      # 9. Beräkna nettoförändringar
      befolkning_forandringar <- befolkning_komponenter %>%
        mutate(
          Forändring = dplyr::case_when(
            variabel == "Födda"               ~ 0,
            variabel == "Döda"                ~ -varde,
            variabel == "Inrikes inflyttning" ~ varde,
            variabel == "Inrikes utflyttning" ~ -varde,
            variabel == "Invandring"          ~ varde,
            variabel == "Utvandring"          ~ -varde,
            TRUE                              ~ 0
          )
        ) %>%
        group_by(region, kon, alder, ar) %>%
        summarise(Nettoforändring = sum(Forändring), .groups = "drop")

      # 10. Beräkna ny befolkning
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

      # 11. Spara per-årsresultat (komponentnycklar i ASCII)
      alla_resultat[[prognos_ar]] <- list(
        befolkning  = ny_befolkning,
        komponenter = list(
          fodda              = fodda_rapport_final,
          doda               = doda_final,
          inrikes_inflyttning = inrikes_inflyttningar_final,
          inrikes_utflyttning = inrikes_utflyttningar_final,
          invandring         = invandring_final,
          utvandring         = utvandring_final
        )
      )

      total_bef <- sum(ny_befolkning$varde[ny_befolkning$region == lan_namn])
      forandring <- sum(befolkning_forandringar$Nettoforändring[
        befolkning_forandringar$region == lan_namn]) +
        sum(fodda_rapport_final$varde[fodda_rapport_final$region == lan_namn])
      message(paste0("    ", lan_namn, " total: ",
                     format(total_bef, big.mark = " "),
                     " (förändring: ",
                     ifelse(forandring >= 0, "+", ""),
                     format(forandring, big.mark = " "), ")"))
    }

    # --- Sammanställ per region ---
    region_resultat <- list()
    alla_regioner   <- c(lan_namn, kommuner)

    for (region_namn in alla_regioner) {
      region_befolkning  <- tibble()
      region_komponenter <- list()

      for (prognos_ar in as.character(startår:slutår)) {
        region_befolkning <- bind_rows(
          region_befolkning,
          alla_resultat[[prognos_ar]]$befolkning %>%
            filter(region == region_namn)
        )

        region_komponenter[[prognos_ar]] <- lapply(
          alla_resultat[[prognos_ar]]$komponenter,
          function(komp) komp %>% filter(region == region_namn)
        )
      }

      region_resultat[[region_namn]] <- list(
        geografi        = region_namn,
        totalbefolkning = region_befolkning,
        komponenter     = region_komponenter
      )
    }

    region_resultat
  }

  # ===========================================================
  # KÖR PROGNOSEN
  # ===========================================================
  message("\n=== STARTAR REGIONAL BEFOLKNINGSPROGNOS (in-memory) ===\n")

  region_resultat <- gora_regional_befolkningsprognos(
    basbefolkning                 = basbefolkning_kommun,
    risktal                       = risktal,
    riksbefolkning_prognos        = riksbefolkning_prognos,
    invandring_till_riket_prognos = invandring_till_riket_prognos,
    lan_namn                      = lan_namn,
    kommuner                      = kommuner,
    startår                       = startår,
    slutår                        = slutår
  )

  # ===========================================================
  # ANPASSA RESULTATFORMAT FÖR UI:N
  # ===========================================================
  # UI-koden förväntar sig samma struktur som enskild prognos:
  #   list(geografi, totalbefolkning, komponenter, sammanfattning).
  # För regional prognos gör vi länet till primär geografi och bevarar
  # per-kommun-resultaten i $kommun_resultat så att UI kan tillåta byte
  # av geografi.

  lan_resultat <- region_resultat[[lan_namn]]

  totalbefolkning <- lan_resultat$totalbefolkning
  komponenter     <- lan_resultat$komponenter

  sammanfattning <- totalbefolkning %>%
    group_by(ar) %>%
    summarise(
      Total_befolkning = sum(varde),
      Kvinnor          = sum(varde[kon == "kvinnor"]),
      Män              = sum(varde[kon == "män"]),
      .groups = "drop"
    )

  message("\nRegional prognos klar!\n")

  list(
    geografi        = lan_namn,
    totalbefolkning = totalbefolkning,
    komponenter     = komponenter,
    sammanfattning  = sammanfattning,
    kommun_resultat = region_resultat
  )
}

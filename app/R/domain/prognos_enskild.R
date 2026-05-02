# prognos_enskild.R
# In-memory wrapper kring kohort-komponent-prognosen för en enskild geografi,
# baserat på Analytikernatverket/befolkningsprognoser (prognos_engeografi.R).
#
# Inga filer läses från disk och inga RDS sparas –
# allt sker in-memory via argument och returvärde.

library(tidyverse)

#' Kör befolkningsprognos för en enskild geografi (in-memory)
#'
#' @param underlag     list med $kommun_lista och $riket_lista
#' @param risktal      list med fodelserisker, dodsrisker, inflyttningsrisker,
#'                     utflyttningsrisker, invandringsrisker, utvandringsrisker
#'                     (output från kor_riskberakningar_in_memory)
#' @param konfiguration list med minst:
#'   $enskild_geografi$namn – namn på geografi (som i Region-kolumnen i datan)
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
    filter(Region == geografi, År == max(År)) %>%
    mutate(Ålder = as.numeric(Ålder))

  if (nrow(basbefolkning_geografi) == 0) {
    stop(paste("Geografin", geografi, "finns inte i underlaget!"))
  }

  # Härleda startår om det inte anges explicit
  senaste_historiska_ar <- max(as.integer(
    underlag$kommun_lista$totfolkmangd$År))
  startår <- konfiguration$prognos_start %||% (senaste_historiska_ar + 1L)
  slutår  <- as.integer(konfiguration$prognos_slut)

  # Riksprognosdata
  riksbefolkning_prognos       <- underlag$riket_lista$riket_prognosinvånare_grund
  invandring_till_riket_prognos <- underlag$riket_lista$invandring_riket

  if (!is.character(riksbefolkning_prognos$År)) {
    riksbefolkning_prognos <- riksbefolkning_prognos %>%
      mutate(År = as.character(År))
  }
  if (!is.character(invandring_till_riket_prognos$År)) {
    invandring_till_riket_prognos <- invandring_till_riket_prognos %>%
      mutate(År = as.character(År))
  }

  # Konvertera risktal till tibbles (för säkerhets skull)
  risktal <- risktal %>% purrr::map(as_tibble)

  # ===========================================================
  # HJÄLPFUNKTIONER (stänger in över AVRUNDNING)
  # ===========================================================

  aldra_befolkning <- function(befolkning) {
    aldrad_befolkning <- befolkning %>%
      mutate(Ålder = Ålder + 1) %>%
      filter(Ålder <= 100)

    aldrad_100_plus <- aldrad_befolkning %>%
      filter(Ålder >= 100) %>%
      group_by(Region, Kön, År, Variabel) %>%
      summarise(Värde = sum(Värde), .groups = "drop") %>%
      mutate(Ålder = 100)

    bind_rows(
      aldrad_befolkning %>% filter(Ålder < 100),
      aldrad_100_plus
    )
  }

  berakna_fodda <- function(befolkning, fodelserisker_data, prognos_ar) {
    kvinnor_fertil   <- befolkning %>%
      filter(Kön == "kvinnor", Ålder >= 15, Ålder <= 49)

    fodelserisker_ar <- fodelserisker_data %>%
      filter(År == prognos_ar, Region == unique(befolkning$Region))

    if (nrow(fodelserisker_ar) == 0) {
      stop(paste("Inga födelserisker för år", prognos_ar,
                 "och region", unique(befolkning$Region)))
    }

    fodda_per_alder <- kvinnor_fertil %>%
      left_join(
        fodelserisker_ar %>% select(Region, Ålder, Fodelserisk = Värde),
        by = c("Region", "Ålder")
      ) %>%
      mutate(
        Antal_fodda = Värde * Fodelserisk,
        Antal_fodda = replace_na(Antal_fodda, 0)
      ) %>%
      group_by(Region) %>%
      summarise(Antal_fodda = sum(Antal_fodda, na.rm = TRUE), .groups = "drop")

    fodda_pojkar <- fodda_per_alder %>%
      mutate(Kön = "män", Ålder = 0, År = prognos_ar,
             Värde = round(Antal_fodda * 0.512, 0),
             Variabel = "Total folkmängd") %>%
      select(Region, Ålder, Kön, År, Värde, Variabel)

    fodda_flickor <- fodda_per_alder %>%
      mutate(Kön = "kvinnor", Ålder = 0, År = prognos_ar,
             Värde = round(Antal_fodda * 0.488, 0),
             Variabel = "Total folkmängd") %>%
      select(Region, Ålder, Kön, År, Värde, Variabel)

    fodda <- bind_rows(fodda_pojkar, fodda_flickor)

    list(
      fodda        = fodda,
      fodda_rapport = fodda %>% mutate(Variabel = "Födda")
    )
  }

  berakna_doda <- function(befolkning, dodsrisker_data) {
    prognos_ar       <- unique(befolkning$År)
    dodsrisker_ar    <- dodsrisker_data %>%
      filter(År == prognos_ar, Region == unique(befolkning$Region))

    if (nrow(dodsrisker_ar) == 0) {
      stop(paste("Inga dödsrisker för år", prognos_ar,
                 "och region", unique(befolkning$Region)))
    }

    if (AVRUNDNING == "stokastisk") {
      doda <- befolkning %>%
        left_join(
          dodsrisker_ar %>% select(Region, Kön, Ålder, Dodsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Dodsrisk = dplyr::case_when(
            is.na(Dodsrisk) & Ålder > 100 ~ dodsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Dodsrisk
          ),
          Antal_doda = purrr::map2_dbl(Värde, Dodsrisk, ~ {
            if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1)
              NA_real_
            else
              tryCatch(rbinom(1, size = .x, prob = .y),
                       warning = function(w) NA_real_,
                       error   = function(e) NA_real_)
          }),
          Antal_doda = replace_na(Antal_doda, 0),
          Värde      = Antal_doda,
          Variabel   = "Döda"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else if (AVRUNDNING == "heltal") {
      doda <- befolkning %>%
        left_join(
          dodsrisker_ar %>% select(Region, Kön, Ålder, Dodsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Dodsrisk = dplyr::case_when(
            is.na(Dodsrisk) & Ålder > 100 ~ dodsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Dodsrisk
          ),
          Antal_doda = round(Värde * Dodsrisk, 0),
          Värde      = Antal_doda,
          Variabel   = "Döda"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else {  # "ingen"
      doda <- befolkning %>%
        left_join(
          dodsrisker_ar %>% select(Region, Kön, Ålder, Dodsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Dodsrisk = dplyr::case_when(
            is.na(Dodsrisk) & Ålder > 100 ~ dodsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Dodsrisk
          ),
          Antal_doda = Värde * Dodsrisk,
          Värde      = Antal_doda,
          Variabel   = "Döda"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)
    }

    doda
  }

  berakna_inrikes_inflyttningar <- function(inflyttningsrisker_data,
                                            riksbefolkning, prognos_ar,
                                            geografi) {
    riksbef_ar           <- riksbefolkning %>% filter(År == prognos_ar)
    inflyttningsrisker_ar <- inflyttningsrisker_data %>%
      filter(År == prognos_ar, Region == geografi)

    if (nrow(inflyttningsrisker_ar) == 0) {
      stop(paste("Inga inflyttningsrisker för år", prognos_ar,
                 "och region", geografi))
    }

    if (AVRUNDNING == "stokastisk") {
      inflyttningar <- riksbef_ar %>%
        select(Kön, Ålder, Riksbefolkning = Värde) %>%
        crossing(Region = geografi) %>%
        left_join(
          inflyttningsrisker_ar %>%
            select(Region, Kön, Ålder, Inflyttningsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Antal_inflyttningar = purrr::map2_dbl(
            Riksbefolkning, Inflyttningsrisk,
            ~ if (!is.na(.x) && !is.na(.y) && .x >= 0 && .y >= 0 && .y <= 1)
              rbinom(1, size = .x, prob = .y)
            else NA_real_
          ),
          Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
          År      = prognos_ar,
          Värde   = Antal_inflyttningar,
          Variabel = "Inrikes inflyttning"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else if (AVRUNDNING == "heltal") {
      inflyttningar <- riksbef_ar %>%
        select(Kön, Ålder, Riksbefolkning = Värde) %>%
        crossing(Region = geografi) %>%
        left_join(
          inflyttningsrisker_ar %>%
            select(Region, Kön, Ålder, Inflyttningsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Antal_inflyttningar = round(Riksbefolkning * Inflyttningsrisk, 0),
          Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
          År      = prognos_ar,
          Värde   = Antal_inflyttningar,
          Variabel = "Inrikes inflyttning"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else {  # "ingen"
      inflyttningar <- riksbef_ar %>%
        select(Kön, Ålder, Riksbefolkning = Värde) %>%
        crossing(Region = geografi) %>%
        left_join(
          inflyttningsrisker_ar %>%
            select(Region, Kön, Ålder, Inflyttningsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Antal_inflyttningar = Riksbefolkning * Inflyttningsrisk,
          Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
          År      = prognos_ar,
          Värde   = Antal_inflyttningar,
          Variabel = "Inrikes inflyttning"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)
    }

    inflyttningar
  }

  berakna_inrikes_utflyttningar <- function(befolkning, utflyttningsrisker_data) {
    prognos_ar           <- unique(befolkning$År)
    utflyttningsrisker_ar <- utflyttningsrisker_data %>%
      filter(År == prognos_ar, Region == unique(befolkning$Region))

    if (nrow(utflyttningsrisker_ar) == 0) {
      stop(paste("Inga utflyttningsrisker för år", prognos_ar,
                 "och region", unique(befolkning$Region)))
    }

    if (AVRUNDNING == "stokastisk") {
      utflyttningar <- befolkning %>%
        left_join(
          utflyttningsrisker_ar %>%
            select(Region, Kön, Ålder, Utflyttningsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Utflyttningsrisk = dplyr::case_when(
            is.na(Utflyttningsrisk) & Ålder > 100 ~
              utflyttningsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Utflyttningsrisk
          ),
          Antal_utflyttningar = purrr::map2_dbl(Värde, Utflyttningsrisk, ~ {
            if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1)
              NA_real_
            else
              tryCatch(rbinom(1, size = .x, prob = .y),
                       warning = function(w) NA_real_,
                       error   = function(e) NA_real_)
          }),
          Antal_utflyttningar = replace_na(Antal_utflyttningar, 0),
          Värde   = Antal_utflyttningar,
          Variabel = "Inrikes utflyttning"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else if (AVRUNDNING == "heltal") {
      utflyttningar <- befolkning %>%
        left_join(
          utflyttningsrisker_ar %>%
            select(Region, Kön, Ålder, Utflyttningsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Utflyttningsrisk = dplyr::case_when(
            is.na(Utflyttningsrisk) & Ålder > 100 ~
              utflyttningsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Utflyttningsrisk
          ),
          Utflyttningsrisk    = replace_na(Utflyttningsrisk, 0),
          Antal_utflyttningar = round(Värde * Utflyttningsrisk, 0),
          Värde   = Antal_utflyttningar,
          Variabel = "Inrikes utflyttning"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else {  # "ingen"
      utflyttningar <- befolkning %>%
        left_join(
          utflyttningsrisker_ar %>%
            select(Region, Kön, Ålder, Utflyttningsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Utflyttningsrisk = dplyr::case_when(
            is.na(Utflyttningsrisk) & Ålder > 100 ~
              utflyttningsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Utflyttningsrisk
          ),
          Utflyttningsrisk    = replace_na(Utflyttningsrisk, 0),
          Antal_utflyttningar = Värde * Utflyttningsrisk,
          Värde   = Antal_utflyttningar,
          Variabel = "Inrikes utflyttning"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)
    }

    utflyttningar
  }

  berakna_invandring <- function(invandringsrisker_data, invandring_riket,
                                  prognos_ar, geografi) {
    invandring_ar        <- invandring_riket %>% filter(År == prognos_ar)
    invandringsrisker_ar <- invandringsrisker_data %>%
      filter(År == prognos_ar, Region == geografi)

    if (nrow(invandringsrisker_ar) == 0) {
      stop(paste("Inga invandringsrisker för år", prognos_ar,
                 "och region", geografi))
    }

    if (AVRUNDNING == "stokastisk") {
      invandring <- invandring_ar %>%
        select(Kön, Ålder, Riksinvandring = Värde) %>%
        crossing(Region = geografi) %>%
        left_join(
          invandringsrisker_ar %>%
            select(Region, Kön, Ålder, Invandringsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Antal_invandrare = purrr::map2_dbl(
            Riksinvandring, Invandringsrisk,
            ~ if (!is.na(.x) && !is.na(.y) && .x >= 0 && .y >= 0 && .y <= 1)
              rbinom(1, size = .x, prob = .y)
            else NA_real_
          ),
          Antal_invandrare = replace_na(Antal_invandrare, 0),
          År      = prognos_ar,
          Värde   = Antal_invandrare,
          Variabel = "Invandring"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else if (AVRUNDNING == "heltal") {
      invandring <- invandring_ar %>%
        select(Kön, Ålder, Riksinvandring = Värde) %>%
        crossing(Region = geografi) %>%
        left_join(
          invandringsrisker_ar %>%
            select(Region, Kön, Ålder, Invandringsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Antal_invandrare = round(Riksinvandring * Invandringsrisk, 0),
          Antal_invandrare = replace_na(Antal_invandrare, 0),
          År      = prognos_ar,
          Värde   = Antal_invandrare,
          Variabel = "Invandring"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else {  # "ingen"
      invandring <- invandring_ar %>%
        select(Kön, Ålder, Riksinvandring = Värde) %>%
        crossing(Region = geografi) %>%
        left_join(
          invandringsrisker_ar %>%
            select(Region, Kön, Ålder, Invandringsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Antal_invandrare = Riksinvandring * Invandringsrisk,
          Antal_invandrare = replace_na(Antal_invandrare, 0),
          År      = prognos_ar,
          Värde   = Antal_invandrare,
          Variabel = "Invandring"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)
    }

    invandring
  }

  berakna_utvandring <- function(befolkning, utvandringsrisker_data) {
    prognos_ar           <- unique(befolkning$År)
    utvandringsrisker_ar <- utvandringsrisker_data %>%
      filter(År == prognos_ar, Region == unique(befolkning$Region))

    if (nrow(utvandringsrisker_ar) == 0) {
      stop(paste("Inga utvandringsrisker för år", prognos_ar,
                 "och region", unique(befolkning$Region)))
    }

    if (AVRUNDNING == "stokastisk") {
      utvandring <- befolkning %>%
        left_join(
          utvandringsrisker_ar %>%
            select(Region, Kön, Ålder, Utvandringsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Utvandringsrisk = dplyr::case_when(
            is.na(Utvandringsrisk) & Ålder > 100 ~
              utvandringsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Utvandringsrisk
          ),
          Antal_utvandrare = purrr::map2_dbl(Värde, Utvandringsrisk, ~ {
            if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1)
              NA_real_
            else
              tryCatch(rbinom(1, size = .x, prob = .y),
                       warning = function(w) NA_real_,
                       error   = function(e) NA_real_)
          }),
          Antal_utvandrare = replace_na(Antal_utvandrare, 0),
          Värde   = Antal_utvandrare,
          Variabel = "Utvandring"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else if (AVRUNDNING == "heltal") {
      utvandring <- befolkning %>%
        left_join(
          utvandringsrisker_ar %>%
            select(Region, Kön, Ålder, Utvandringsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Utvandringsrisk = dplyr::case_when(
            is.na(Utvandringsrisk) & Ålder > 100 ~
              utvandringsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Utvandringsrisk
          ),
          Utvandringsrisk  = replace_na(Utvandringsrisk, 0),
          Antal_utvandrare = round(Värde * Utvandringsrisk, 0),
          Värde   = Antal_utvandrare,
          Variabel = "Utvandring"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)

    } else {  # "ingen"
      utvandring <- befolkning %>%
        left_join(
          utvandringsrisker_ar %>%
            select(Region, Kön, Ålder, Utvandringsrisk = Värde),
          by = c("Region", "Kön", "Ålder")
        ) %>%
        mutate(
          Utvandringsrisk = dplyr::case_when(
            is.na(Utvandringsrisk) & Ålder > 100 ~
              utvandringsrisker_ar %>%
              filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
              dplyr::pull(Värde) %>% dplyr::first(),
            TRUE ~ Utvandringsrisk
          ),
          Utvandringsrisk  = replace_na(Utvandringsrisk, 0),
          Antal_utvandrare = Värde * Utvandringsrisk,
          Värde   = Antal_utvandrare,
          Variabel = "Utvandring"
        ) %>%
        select(Region, Kön, Ålder, År, Värde, Variabel)
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

    for (ar in as.character(startår:slutår)) {
      message(paste0("  Beräknar år ", ar, "..."))

      # 1. Åldra befolkning
      if (ar == startår) {
        aktuell_befolkning <- basbefolkning %>%
          mutate(År = as.character(as.numeric(År) + 1))
        aktuell_befolkning <- aldra_befolkning(aktuell_befolkning)
      } else {
        aktuell_befolkning <- alla_resultat[[
          as.character(as.numeric(ar) - 1)]]$befolkning %>%
          mutate(År = ar)
        aktuell_befolkning <- aldra_befolkning(aktuell_befolkning)
      }

      # 2. Födda
      fodda_resultat <- berakna_fodda(
        aktuell_befolkning, risktal$fodelserisker, ar)
      aktuell_befolkning <- bind_rows(
        aktuell_befolkning, fodda_resultat$fodda)

      # 3. Döda
      doda <- berakna_doda(aktuell_befolkning, risktal$dodsrisker)

      # 4. Inrikes inflyttning
      inrikes_inflyttningar <- berakna_inrikes_inflyttningar(
        risktal$inflyttningsrisker, riksbefolkning_prognos, ar, geografi)

      # 5. Inrikes utflyttning
      inrikes_utflyttningar <- berakna_inrikes_utflyttningar(
        aktuell_befolkning, risktal$utflyttningsrisker)

      # 6. Invandring
      invandring <- berakna_invandring(
        risktal$invandringsrisker, invandring_till_riket_prognos, ar, geografi)

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
            Variabel == "Födda"             ~ 0,
            Variabel == "Döda"              ~ -Värde,
            Variabel == "Inrikes inflyttning" ~ Värde,
            Variabel == "Inrikes utflyttning" ~ -Värde,
            Variabel == "Invandring"        ~ Värde,
            Variabel == "Utvandring"        ~ -Värde,
            TRUE                            ~ 0
          )
        ) %>%
        group_by(Region, Kön, Ålder, År) %>%
        summarise(Nettoforändring = sum(Forändring), .groups = "drop")

      ny_befolkning <- aktuell_befolkning %>%
        select(Region, Kön, Ålder, År, Värde) %>%
        left_join(befolkning_forandringar,
                  by = c("Region", "Kön", "Ålder", "År")) %>%
        mutate(
          Nettoforändring = replace_na(Nettoforändring, 0),
          Ny_befolkning   = Värde + Nettoforändring,
          Ny_befolkning   = round(Ny_befolkning, 0),
          Ny_befolkning   = pmax(0, Ny_befolkning)
        ) %>%
        select(Region, Kön, Ålder, År, Värde = Ny_befolkning) %>%
        mutate(Variabel = "Total folkmängd")

      alla_resultat[[ar]] <- list(
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

      total_bef  <- sum(ny_befolkning$Värde)
      netto_bef  <- sum(befolkning_forandringar$Nettoforändring) +
        sum(fodda_resultat$fodda$Värde)
      message(paste0("    Total befolkning: ",
                     format(total_bef, big.mark = " "),
                     " (förändring: ",
                     ifelse(netto_bef >= 0, "+", ""),
                     format(netto_bef, big.mark = " "), ")"))
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
      group_by(År) %>%
      summarise(
        Total_befolkning = sum(Värde),
        Kvinnor          = sum(Värde[Kön == "kvinnor"]),
        Män              = sum(Värde[Kön == "män"]),
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

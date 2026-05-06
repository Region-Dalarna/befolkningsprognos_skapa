# risk_kor.R
# In-memory wrapper kring riskberäkningslogiken från
# Analytikernatverket/befolkningsprognoser (prognos_riskberakningar.R).
#
# Inga filer läses från disk och inga RDS sparas –
# allt sker in-memory via argument och returvärde.

library(tidyverse)
library(zoo)

#' Beräkna demografiska risktal in-memory
#'
#' @param underlag  list med $kommun_lista och $riket_lista
#' @param konfiguration list med $riskparametrar, $scenario,
#'   $alternativ_justeringar, $bevara_summa_auto_spline_utvandring,
#'   $bevara_summa_auto_spline_inflyttning_lansgrans,
#'   $bevara_summa_auto_spline_inflyttning,
#'   $bevara_niva_per_ar_inflyttning
#' @return list med fodelserisker, dodsrisker, inflyttningsrisker,
#'   utflyttningsrisker, invandringsrisker, utvandringsrisker
kor_riskberakningar_in_memory <- function(underlag, konfiguration) {

  kommun_lista <- underlag$kommun_lista
  riket_lista  <- underlag$riket_lista

  # --- Lokala variabler som ersätter globaler ---
  AKTIVT_SCENARIO      <- konfiguration$scenario %||% "standard"
  PARAMETRAR           <- konfiguration$riskparametrar
  ALTERNATIV_FRAMSKRIVNING <- konfiguration$alternativ_justeringar

  bevara_summa_auto_spline_utvandring            <-
    isTRUE(konfiguration$bevara_summa_auto_spline_utvandring)
  bevara_summa_auto_spline_inflyttning_lansgrans <-
    isTRUE(konfiguration$bevara_summa_auto_spline_inflyttning_lansgrans)
  bevara_summa_auto_spline_inflyttning           <-
    !isFALSE(konfiguration$bevara_summa_auto_spline_inflyttning)  # default TRUE
  bevara_niva_per_ar_inflyttning                 <-
    isTRUE(konfiguration$bevara_niva_per_ar_inflyttning)

  # ===========================================================
  # HJÄLPFUNKTIONER
  # ===========================================================

  berakna_tidsvikter <- function(antal_ar, viktningstyp, alpha = 0.5) {
    ar_index <- seq(1, antal_ar)
    if (viktningstyp == 1) {
      vikter <- rep(1 / antal_ar, antal_ar)
    } else if (viktningstyp == 2) {
      vikter_raw <- ar_index
      vikter <- vikter_raw / sum(vikter_raw)
    } else if (viktningstyp == 3) {
      if (is.null(alpha) || is.na(alpha) || !is.numeric(alpha) ||
          alpha <= 0 || alpha >= 1) {
        warning(paste("Ogiltig alpha-parameter:", alpha, "– använder 0.5"))
        alpha <- 0.5
      }
      vikter_raw <- alpha * (1 - alpha)^(antal_ar - ar_index)
      if (any(is.na(vikter_raw)) || any(is.infinite(vikter_raw)) ||
          sum(vikter_raw) == 0) {
        warning("Problem med EWMA-beräkning, använder jämn viktning")
        vikter <- rep(1 / antal_ar, antal_ar)
      } else {
        vikter <- vikter_raw / sum(vikter_raw)
      }
    } else {
      stop("Ogiltig viktningstyp. Välj 1 (jämn), 2 (linjär) eller 3 (EWMA).")
    }
    if (any(is.na(vikter)) || abs(sum(vikter) - 1) > 0.001) {
      warning("Problem med viktningsberäkning, använder jämn viktning")
      vikter <- rep(1 / antal_ar, antal_ar)
    }
    return(vikter)
  }

  spline_auto <- function(alder_vektor, varde_vektor, bevara_summa = TRUE) {
    if (length(alder_vektor) < 10) return(varde_vektor)
    valid_idx <- is.finite(varde_vektor) & varde_vektor > 0
    if (sum(valid_idx) < 10) return(varde_vektor)
    original_summa <- sum(varde_vektor[is.finite(varde_vektor)], na.rm = TRUE)
    tryCatch({
      spline_fit <- NULL
      # Fångar Fortran-utskrifter (t.ex. "spar-finding: non-finite value...")
      # så loggen inte spammas vid svåra dataprofiler.
      utskrift <- utils::capture.output(
        spline_fit <- suppressWarnings(stats::smooth.spline(
          alder_vektor[valid_idx],
          varde_vektor[valid_idx],
          cv = FALSE,           # GCV är numeriskt stabilare än leave-one-out CV
          all.knots = TRUE
        )),
        type = "output"
      )
      if (is.null(spline_fit)) return(varde_vektor)
      pred_varden <- stats::predict(spline_fit, alder_vektor)$y
      pred_varden[!is.finite(pred_varden)] <- 0
      pred_varden <- pmax(pred_varden, 0)
      if (bevara_summa && original_summa > 0) {
        ny_summa <- sum(pred_varden, na.rm = TRUE)
        if (ny_summa > 0) pred_varden <- pred_varden * (original_summa / ny_summa)
      }
      return(pred_varden)
    }, error = function(e) {
      warning(paste("Spline-utjämning misslyckades:", e$message))
      return(varde_vektor)
    })
  }

  applicera_scenariojustering <- function(data, komponent_namn) {
    if (AKTIVT_SCENARIO != "alternativ") return(data)
    justeringar <- ALTERNATIV_FRAMSKRIVNING[[komponent_namn]]
    if (is.null(justeringar) || is.null(justeringar$perioder)) return(data)
    justerad_data <- data
    for (period in justeringar$perioder) {
      justerad_data <- justerad_data %>%
        mutate(
          varde = ifelse(
            as.numeric(ar) >= period$från_år & as.numeric(ar) <= period$till_år,
            varde * period$multiplikator,
            varde
          )
        )
    }
    return(justerad_data)
  }

  # ===========================================================
  # 1. FÖDELSERISKER
  # ===========================================================
  berakna_fodelserisker <- function() {
    params     <- PARAMETRAR$fodelserisker
    antal_ar   <- params$antal_ar
    alpha_varde <- if (params$viktningstyp == 3 && !is.null(params$alpha))
      params$alpha else 0.5

    senaste_ar <- kommun_lista$fodda %>%
      dplyr::pull(ar) %>% unique() %>% sort() %>% tail(antal_ar)

    fruktsamhet_raa <- kommun_lista$fodda %>%
      filter(ar %in% senaste_ar, alder >= 15, alder <= 49) %>%
      inner_join(
        kommun_lista$medelfolkmangd_modrar %>% filter(ar %in% senaste_ar),
        by = c("region", "ar", "alder")
      ) %>%
      mutate(
        fruktsamhetskvot = varde.x / varde.y,
        fruktsamhetskvot = ifelse(
          is.infinite(fruktsamhetskvot) | is.nan(fruktsamhetskvot),
          0, fruktsamhetskvot),
        fruktsamhetskvot = pmin(fruktsamhetskvot, 0.5)
      ) %>%
      select(region, ar, alder,
             fruktsamhetskvot, Antal_födda = varde.x, Antal_kvinnor = varde.y)

    tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
    names(tidsvikter) <- senaste_ar

    poolad_data <- fruktsamhet_raa %>%
      group_by(region, alder) %>%
      summarise(
        viktad_antal_fodda   = sum(Antal_födda * tidsvikter[as.character(ar)],
                                   na.rm = TRUE),
        viktad_antal_kvinnor = sum(Antal_kvinnor * tidsvikter[as.character(ar)],
                                   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        poolad_fruktsamhetskvot = viktad_antal_fodda / viktad_antal_kvinnor,
        poolad_fruktsamhetskvot = ifelse(
          is.nan(poolad_fruktsamhetskvot) | is.infinite(poolad_fruktsamhetskvot),
          0, poolad_fruktsamhetskvot),
        poolad_fruktsamhetskvot = pmin(poolad_fruktsamhetskvot, 0.5)
      )

    poolad_data_spline <- poolad_data %>%
      group_by(region) %>% arrange(alder) %>%
      mutate(
        poolad_fruktsamhetskvot_spline = if (first(region) == "Riket")
          poolad_fruktsamhetskvot
        else
          spline_auto(alder, poolad_fruktsamhetskvot, bevara_summa = TRUE)
      ) %>%
      ungroup()

    riket_referens <- poolad_data_spline %>%
      filter(region == "Riket") %>%
      select(alder, riket_fruktsamhetskvot = poolad_fruktsamhetskvot)

    viktad_kvot <- poolad_data_spline %>%
      left_join(riket_referens, by = "alder") %>%
      mutate(
        tidsviktad_kvot = poolad_fruktsamhetskvot / riket_fruktsamhetskvot,
        tidsviktad_kvot = ifelse(
          is.infinite(tidsviktad_kvot) | is.nan(tidsviktad_kvot),
          1, tidsviktad_kvot),
        tidsviktad_kvot = pmin(pmax(tidsviktad_kvot, 0.1), 3)
      ) %>%
      group_by(region) %>% arrange(alder) %>%
      mutate(
        tidsviktad_kvot_spline = if (first(region) == "Riket")
          1.0
        else
          spline_auto(alder, tidsviktad_kvot, bevara_summa = FALSE)
      ) %>%
      ungroup()

    fodelsetal_riksprognos <- riket_lista$fodelsetal %>%
      filter(alder >= 15, alder <= 49)

    fruktsamhet_prognos <- expand_grid(
      region = unique(viktad_kvot$region),
      ar    = unique(fodelsetal_riksprognos$ar)
    ) %>%
      left_join(
        fodelsetal_riksprognos %>% select(ar, alder, Riksvärde = varde),
        by = "ar", relationship = "many-to-many"
      ) %>%
      left_join(
        viktad_kvot %>% select(region, alder, tidsviktad_kvot_spline),
        by = c("region", "alder")
      ) %>%
      mutate(
        varde    = if_else(region == "Riket", Riksvärde,
                           Riksvärde * tidsviktad_kvot_spline),
        kon      = "kvinnor",
        variabel = "Födelserisker"
      ) %>%
      select(region, kon, alder, ar, variabel, varde)

    applicera_scenariojustering(fruktsamhet_prognos, "fodelserisker")
  }

  # ===========================================================
  # 2. DÖDSRISKER
  # ===========================================================
  berakna_dodsrisker <- function() {
    params    <- PARAMETRAR$dodsrisker
    antal_ar  <- params$antal_ar
    alpha_varde <- if (params$viktningstyp == 3 && !is.null(params$alpha))
      params$alpha else 0.5

    gruppera_alder <- function(alder) {
      dplyr::case_when(
        alder == 0                    ~ "0",
        alder >= 1  & alder <= 4     ~ "1-4",
        alder >= 5  & alder <= 89    ~ paste0(
          5 * floor((alder - 5) / 5) + 5, "-",
          5 * floor((alder - 5) / 5) + 9),
        alder >= 90                  ~ "90+"
      )
    }

    berakna_summor <- function(varde, width) {
      zoo::rollapply(varde, width = width, FUN = sum,
                     fill = NA, align = "right")
    }

    aldersmappning <- tibble(
      Åldersgrupp = c("0", "1-4",
                      paste0(seq(5, 85, 5), "-", seq(9, 89, 5)), "90+"),
      alder = list(0, 1:4, 5:9, 10:14, 15:19, 20:24, 25:29, 30:34, 35:39,
                   40:44, 45:49, 50:54, 55:59, 60:64, 65:69, 70:74, 75:79,
                   80:84, 85:89, 90:100)
    ) %>% unnest(alder)

    dodsrisker_summor <- kommun_lista$doda %>%
      mutate(Åldersgrupp = gruppera_alder(alder)) %>%
      group_by(region, kon, Åldersgrupp, ar) %>%
      summarise(varde = sum(varde), .groups = "drop") %>%
      group_by(region, kon, Åldersgrupp) %>%
      arrange(ar) %>%
      mutate(Flerårsgenomsnitt = berakna_summor(varde, antal_ar)) %>%
      ungroup() %>%
      filter(ar == max(ar))

    folkmangd_summor <- kommun_lista$totfolkmangd %>%
      mutate(Åldersgrupp = gruppera_alder(alder)) %>%
      group_by(region, kon, Åldersgrupp, ar) %>%
      summarise(varde = sum(varde), .groups = "drop") %>%
      group_by(region, kon, Åldersgrupp) %>%
      arrange(ar) %>%
      mutate(Flerårsgenomsnitt = berakna_summor(varde, antal_ar)) %>%
      ungroup() %>%
      filter(ar == max(ar))

    dodsrisker_prognosklar <- dodsrisker_summor %>%
      inner_join(
        folkmangd_summor %>%
          select(region, kon, Åldersgrupp, ar, Folkmangd_Flerars = Flerårsgenomsnitt),
        by = c("region", "kon", "Åldersgrupp", "ar")
      ) %>%
      mutate(Flerårsgenomsnitt = Flerårsgenomsnitt / Folkmangd_Flerars) %>%
      select(-Folkmangd_Flerars)

    kontroll_tabell <- dodsrisker_summor %>%
      mutate(Anvand_lansdata = Flerårsgenomsnitt <= 50)

    riket_risker <- dodsrisker_prognosklar %>%
      filter(region == "Riket") %>%
      select(kon, Åldersgrupp, ar, Riket_risk = Flerårsgenomsnitt)

    dodsrisker_prognosklar <- dodsrisker_prognosklar %>%
      left_join(riket_risker, by = c("kon", "Åldersgrupp", "ar")) %>%
      left_join(
        kontroll_tabell %>%
          select(region, kon, Åldersgrupp, ar, Anvand_lansdata),
        by = c("region", "kon", "Åldersgrupp", "ar")
      ) %>%
      mutate(
        Flerårsgenomsnitt = if_else(
          region == "Riket" | !Anvand_lansdata,
          Flerårsgenomsnitt, Riket_risk)
      ) %>%
      select(-Riket_risk, -Anvand_lansdata)

    dodsrisker_riket <- dodsrisker_prognosklar %>%
      filter(region == "Riket") %>%
      select(kon, Åldersgrupp, Dodsrisk_riket = Flerårsgenomsnitt)

    dodsrisker_relativ <- dodsrisker_prognosklar %>%
      left_join(dodsrisker_riket, by = c("kon", "Åldersgrupp")) %>%
      mutate(
        Kvot = dplyr::case_when(
          region == "Riket"                                 ~ 1,
          is.na(Flerårsgenomsnitt / Dodsrisk_riket)        ~ 1,
          TRUE ~ Flerårsgenomsnitt / Dodsrisk_riket
        ),
        Kvot = pmax(0.7, pmin(1.3, Kvot))
      ) %>%
      select(region, kon, Åldersgrupp, Kvot)

    dodsrisker_relativ_ettarsklasser <- dodsrisker_relativ %>%
      left_join(aldersmappning, by = "Åldersgrupp",
                relationship = "many-to-many") %>%
      select(region, kon, alder, Kvot) %>%
      arrange(region, kon, alder)

    dodskvoter_riksprognos_justerad <- riket_lista$dodstal %>%
      mutate(alder = pmin(alder, 100)) %>%
      group_by(ar, kon, alder) %>%
      summarise(
        varde    = mean(varde),
        variabel = dplyr::first(variabel),
        .groups  = "drop"
      )

    kommun_prognos <- expand_grid(
      region = unique(dodsrisker_relativ_ettarsklasser$region),
      ar    = unique(dodskvoter_riksprognos_justerad$ar)
    ) %>%
      left_join(dodskvoter_riksprognos_justerad,
                by = "ar", relationship = "many-to-many") %>%
      left_join(dodsrisker_relativ_ettarsklasser,
                by = c("region", "kon", "alder")) %>%
      mutate(
        Kvot     = if_else(is.na(Kvot), 1, Kvot),
        varde    = if_else(region == "Riket", varde, varde * Kvot),
        variabel = "Dödsrisker"
      ) %>%
      select(region, kon, alder, ar, variabel, varde)

    applicera_scenariojustering(kommun_prognos, "dodsrisker")
  }

  # ===========================================================
  # 3. INFLYTTNINGSRISKER
  # ===========================================================
  berakna_inflyttningsrisker <- function() {
    params    <- PARAMETRAR$inflyttningsrisker
    antal_ar  <- params$antal_ar
    alpha_varde <- if (params$viktningstyp == 3 && !is.null(params$alpha))
      params$alpha else 0.5

    inflyttade                       <- kommun_lista$inrikes_inflyttade %>%
      filter(region != "Riket")
    riket_medelfolkmangd_historisk   <- kommun_lista$medelfolkmangd %>%
      filter(region == "Riket")
    riket_medelfolkmangd_prognos     <- riket_lista$riket_prognosinvanare_grund

    senaste_ar <- inflyttade %>%
      dplyr::pull(ar) %>% unique() %>% sort() %>% tail(antal_ar)

    inflyttningsrisker_raa <- inflyttade %>%
      filter(ar %in% senaste_ar) %>%
      inner_join(
        riket_medelfolkmangd_historisk %>%
          filter(ar %in% senaste_ar) %>%
          select(ar, alder, kon, antal_riket = varde),
        by = c("ar", "alder", "kon")
      ) %>%
      mutate(
        antal_inflyttade   = varde,
        inflyttningsrisk   = antal_inflyttade / antal_riket,
        inflyttningsrisk   = ifelse(
          is.infinite(inflyttningsrisk) | is.nan(inflyttningsrisk),
          0, inflyttningsrisk),
        inflyttningsrisk   = pmin(inflyttningsrisk, 0.5)
      ) %>%
      select(region, ar, alder, kon,
             inflyttningsrisk, antal_inflyttade, antal_riket)

    tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
    names(tidsvikter) <- senaste_ar

    poolad_data <- inflyttningsrisker_raa %>%
      filter(!is.na(antal_inflyttade), !is.na(antal_riket), antal_riket > 0) %>%
      group_by(region, kon, alder) %>%
      summarise(
        viktad_antal_inflyttade = sum(antal_inflyttade * tidsvikter[as.character(ar)],
                                      na.rm = TRUE),
        viktad_antal_riket      = sum(antal_riket * tidsvikter[as.character(ar)],
                                      na.rm = TRUE),
        poolad_inflyttningsrisk = viktad_antal_inflyttade / viktad_antal_riket,
        .groups = "drop"
      ) %>%
      mutate(
        poolad_inflyttningsrisk = ifelse(
          is.nan(poolad_inflyttningsrisk) | is.infinite(poolad_inflyttningsrisk),
          0, poolad_inflyttningsrisk),
        poolad_inflyttningsrisk = pmin(poolad_inflyttningsrisk, 0.5)
      )

    poolad_data_spline <- poolad_data %>%
      group_by(region, kon) %>% arrange(alder) %>%
      mutate(
        poolad_inflyttningsrisk_spline = spline_auto(
          alder, poolad_inflyttningsrisk,
          bevara_summa = bevara_summa_auto_spline_inflyttning)
      ) %>%
      ungroup()

    alla_regioner <- unique(poolad_data_spline$region)
    prognos_ar    <- unique(riket_medelfolkmangd_prognos$ar)

    if (bevara_niva_per_ar_inflyttning) {
      risk_age <- poolad_data %>%
        select(region, kon, alder, ospline = poolad_inflyttningsrisk) %>%
        left_join(
          poolad_data_spline %>%
            select(region, kon, alder, spline = poolad_inflyttningsrisk_spline),
          by = c("region", "kon", "alder")
        )

      scalefac <- risk_age %>%
        tidyr::crossing(ar = unique(riket_medelfolkmangd_prognos$ar)) %>%
        left_join(
          riket_medelfolkmangd_prognos %>% select(ar, alder, kon, n = varde),
          by = c("ar", "alder", "kon")
        ) %>%
        group_by(region, kon, ar) %>%
        summarise(
          num    = sum(ospline * n, na.rm = TRUE),
          denom  = sum(spline  * n, na.rm = TRUE),
          faktor = dplyr::if_else(denom > 0, num / denom, 1),
          .groups = "drop"
        )

      inflyttningsrisker_prognos <- expand_grid(
        region = unique(poolad_data_spline$region),
        ar    = unique(riket_medelfolkmangd_prognos$ar)
      ) %>%
        left_join(
          riket_medelfolkmangd_prognos %>%
            select(ar, alder, kon, Riksbefolkning = varde),
          by = "ar", relationship = "many-to-many"
        ) %>%
        left_join(
          poolad_data_spline %>%
            select(region, alder, kon, spline = poolad_inflyttningsrisk_spline),
          by = c("region", "alder", "kon")
        ) %>%
        left_join(scalefac %>% select(region, kon, ar, faktor),
                  by = c("region", "kon", "ar")) %>%
        mutate(
          varde    = spline * dplyr::coalesce(faktor, 1),
          variabel = "Inflyttningsrisker"
        ) %>%
        select(region, kon, alder, ar, variabel, varde)

    } else {
      inflyttningsrisker_prognos <- expand_grid(
        region = alla_regioner,
        ar    = prognos_ar
      ) %>%
        left_join(
          riket_medelfolkmangd_prognos %>%
            select(ar, alder, kon, Riksbefolkning = varde),
          by = "ar", relationship = "many-to-many"
        ) %>%
        left_join(
          poolad_data_spline %>%
            select(region, alder, kon, poolad_inflyttningsrisk_spline),
          by = c("region", "alder", "kon")
        ) %>%
        mutate(
          varde    = poolad_inflyttningsrisk_spline,
          variabel = "Inflyttningsrisker"
        ) %>%
        select(region, kon, alder, ar, variabel, varde)
    }

    applicera_scenariojustering(inflyttningsrisker_prognos, "inflyttningsrisker")
  }

  # ===========================================================
  # 4. UTFLYTTNINGSRISKER
  # ===========================================================
  berakna_utflyttningsrisker <- function() {
    params    <- PARAMETRAR$utflyttningsrisker
    antal_ar  <- params$antal_ar
    alpha_varde <- if (params$viktningstyp == 3 && !is.null(params$alpha))
      params$alpha else 0.5

    utflyttade    <- kommun_lista$inrikes_utflyttade
    medelfolkmangd <- kommun_lista$medelfolkmangd

    senaste_ar <- utflyttade %>%
      mutate(ar = as.numeric(as.character(ar))) %>%
      dplyr::pull(ar) %>% unique() %>% sort() %>% tail(antal_ar)

    utflyttningsrisker_raa <- utflyttade %>%
      mutate(ar = as.numeric(as.character(ar))) %>%
      filter(ar %in% senaste_ar) %>%
      inner_join(
        medelfolkmangd %>%
          mutate(ar = as.numeric(as.character(ar))) %>%
          filter(ar %in% senaste_ar) %>%
          select(region, ar, alder, kon, antal_befolkning = varde),
        by = c("region", "ar", "alder", "kon")
      ) %>%
      mutate(
        antal_utflyttade = varde,
        utflyttningsrisk = antal_utflyttade / antal_befolkning,
        utflyttningsrisk = replace_na(utflyttningsrisk, 0),
        utflyttningsrisk = pmin(utflyttningsrisk, 0.5)
      ) %>%
      select(region, ar, alder, kon,
             utflyttningsrisk, antal_utflyttade, antal_befolkning)

    tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
    names(tidsvikter) <- as.character(senaste_ar)

    poolad_data <- utflyttningsrisker_raa %>%
      filter(!is.na(antal_utflyttade), !is.na(antal_befolkning),
             antal_befolkning > 0) %>%
      group_by(region, kon, alder) %>%
      summarise(
        viktad_antal_utflyttade  = sum(antal_utflyttade *
                                         tidsvikter[as.character(ar)],
                                       na.rm = TRUE),
        viktad_antal_befolkning  = sum(antal_befolkning *
                                         tidsvikter[as.character(ar)],
                                       na.rm = TRUE),
        poolad_utflyttningsrisk  = viktad_antal_utflyttade /
          viktad_antal_befolkning,
        .groups = "drop"
      ) %>%
      mutate(
        poolad_utflyttningsrisk = ifelse(
          is.nan(poolad_utflyttningsrisk) | is.infinite(poolad_utflyttningsrisk),
          0, poolad_utflyttningsrisk),
        poolad_utflyttningsrisk = pmin(poolad_utflyttningsrisk, 0.5)
      )

    poolad_data_spline <- poolad_data %>%
      group_by(region, kon) %>% arrange(alder) %>%
      mutate(
        poolad_utflyttningsrisk_spline = if (dplyr::first(region) == "Riket")
          poolad_utflyttningsrisk
        else
          spline_auto(alder, poolad_utflyttningsrisk, bevara_summa = FALSE)
      ) %>%
      ungroup()

    prognos_ar <- unique(riket_lista$riket_prognosinvanare_grund$ar)

    utflyttningsrisker_prognos <- poolad_data_spline %>%
      select(region, kon, alder, varde = poolad_utflyttningsrisk_spline) %>%
      crossing(ar = prognos_ar) %>%
      mutate(variabel = "Utflyttningsrisker") %>%
      select(region, kon, alder, ar, variabel, varde)

    applicera_scenariojustering(utflyttningsrisker_prognos, "utflyttningsrisker")
  }

  # ===========================================================
  # 5. INVANDRINGSRISKER
  # ===========================================================
  berakna_invandringsrisker <- function() {
    params    <- PARAMETRAR$invandringsrisker
    antal_ar  <- params$antal_ar
    alpha_varde <- if (params$viktningstyp == 3 && !is.null(params$alpha))
      params$alpha else 0.5

    invandrade             <- kommun_lista$invandring
    riket_invandrade       <- invandrade %>% filter(region == "Riket")
    riket_invandring_prognos <- riket_lista$invandring_riket %>%
      mutate(region = "Riket") %>%
      select(region, ar, alder, kon, varde)

    senaste_ar <- invandrade %>%
      dplyr::pull(ar) %>% unique() %>% sort() %>% tail(antal_ar)

    invandringsrisker_raa <- invandrade %>%
      filter(ar %in% senaste_ar) %>%
      inner_join(
        riket_invandrade %>%
          filter(ar %in% senaste_ar) %>%
          select(ar, alder, kon, antal_riket = varde),
        by = c("ar", "alder", "kon")
      ) %>%
      mutate(
        antal_invandrade = varde,
        invandringsrisk  = antal_invandrade / antal_riket,
        invandringsrisk  = ifelse(
          is.infinite(invandringsrisk) | is.nan(invandringsrisk),
          0, invandringsrisk),
        invandringsrisk  = pmin(invandringsrisk, 1.0)
      ) %>%
      select(region, ar, alder, kon,
             invandringsrisk, antal_invandrade, antal_riket)

    tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
    names(tidsvikter) <- senaste_ar

    poolad_data <- invandringsrisker_raa %>%
      filter(!is.na(antal_invandrade), !is.na(antal_riket), antal_riket > 0) %>%
      group_by(region, kon, alder) %>%
      summarise(
        viktad_antal_invandrade = sum(antal_invandrade *
                                        tidsvikter[as.character(ar)],
                                      na.rm = TRUE),
        viktad_antal_riket      = sum(antal_riket *
                                        tidsvikter[as.character(ar)],
                                      na.rm = TRUE),
        poolad_invandringsrisk  = viktad_antal_invandrade / viktad_antal_riket,
        .groups = "drop"
      ) %>%
      mutate(
        poolad_invandringsrisk = ifelse(
          is.nan(poolad_invandringsrisk) | is.infinite(poolad_invandringsrisk),
          0, poolad_invandringsrisk),
        poolad_invandringsrisk = pmin(poolad_invandringsrisk, 1.0)
      )

    poolad_data_spline <- poolad_data %>%
      group_by(region, kon) %>% arrange(alder) %>%
      mutate(
        poolad_invandringsrisk_spline = if (dplyr::first(region) == "Riket")
          poolad_invandringsrisk
        else
          spline_auto(alder, poolad_invandringsrisk, bevara_summa = FALSE)
      ) %>%
      ungroup()

    alla_regioner <- unique(poolad_data_spline$region)
    prognos_ar    <- unique(riket_invandring_prognos$ar)

    invandringsrisker_prognos <- expand_grid(
      region = alla_regioner,
      ar    = prognos_ar
    ) %>%
      left_join(
        riket_invandring_prognos %>%
          select(ar, alder, kon, invandring_riket = varde),
        by = "ar", relationship = "many-to-many"
      ) %>%
      left_join(
        poolad_data_spline %>%
          select(region, alder, kon, poolad_invandringsrisk_spline),
        by = c("region", "alder", "kon")
      ) %>%
      mutate(
        varde    = poolad_invandringsrisk_spline,
        variabel = "Invandringsrisker"
      ) %>%
      select(region, kon, alder, ar, variabel, varde)

    applicera_scenariojustering(invandringsrisker_prognos, "invandringsrisker")
  }

  # ===========================================================
  # 6. UTVANDRINGSRISKER
  # ===========================================================
  berakna_utvandringsrisker <- function() {
    params    <- PARAMETRAR$utvandringsrisker
    antal_ar  <- params$antal_ar
    alpha_varde <- if (params$viktningstyp == 3 && !is.null(params$alpha))
      params$alpha else 0.5

    utvandrade    <- kommun_lista$utvandring
    medelfolkmangd <- kommun_lista$medelfolkmangd

    senaste_ar <- utvandrade %>%
      mutate(ar = as.numeric(as.character(ar))) %>%
      dplyr::pull(ar) %>% unique() %>% sort() %>% tail(antal_ar)

    utvandringsrisker_raa <- utvandrade %>%
      mutate(ar = as.numeric(as.character(ar))) %>%
      filter(ar %in% senaste_ar) %>%
      inner_join(
        medelfolkmangd %>%
          mutate(ar = as.numeric(as.character(ar))) %>%
          filter(ar %in% senaste_ar) %>%
          select(region, ar, alder, kon, antal_befolkning = varde),
        by = c("region", "ar", "alder", "kon")
      ) %>%
      mutate(
        antal_utvandrade = varde,
        utvandringsrisk  = antal_utvandrade / antal_befolkning,
        utvandringsrisk  = replace_na(utvandringsrisk, 0),
        utvandringsrisk  = pmin(utvandringsrisk, 0.5)
      ) %>%
      select(region, ar, alder, kon,
             utvandringsrisk, antal_utvandrade, antal_befolkning)

    tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
    names(tidsvikter) <- as.character(senaste_ar)

    poolad_data <- utvandringsrisker_raa %>%
      filter(!is.na(antal_utvandrade), !is.na(antal_befolkning),
             antal_befolkning > 0) %>%
      group_by(region, kon, alder) %>%
      summarise(
        viktad_antal_utvandrade = sum(antal_utvandrade *
                                        tidsvikter[as.character(ar)],
                                      na.rm = TRUE),
        viktad_antal_befolkning = sum(antal_befolkning *
                                        tidsvikter[as.character(ar)],
                                      na.rm = TRUE),
        poolad_utvandringsrisk  = viktad_antal_utvandrade /
          viktad_antal_befolkning,
        .groups = "drop"
      ) %>%
      mutate(
        poolad_utvandringsrisk = ifelse(
          is.nan(poolad_utvandringsrisk) | is.infinite(poolad_utvandringsrisk),
          0, poolad_utvandringsrisk),
        poolad_utvandringsrisk = pmin(poolad_utvandringsrisk, 0.5)
      )

    poolad_data_spline <- poolad_data %>%
      group_by(region, kon) %>% arrange(alder) %>%
      mutate(
        poolad_utvandringsrisk_spline = if (dplyr::first(region) == "Riket")
          poolad_utvandringsrisk
        else
          spline_auto(alder, poolad_utvandringsrisk,
                      bevara_summa = bevara_summa_auto_spline_utvandring)
      ) %>%
      ungroup()

    prognos_ar <- unique(riket_lista$riket_prognosinvanare_grund$ar)

    utvandringsrisker_prognos <- poolad_data_spline %>%
      select(region, kon, alder, varde = poolad_utvandringsrisk_spline) %>%
      crossing(ar = prognos_ar) %>%
      mutate(variabel = "Utvandringsrisker") %>%
      select(region, kon, alder, ar, variabel, varde)

    applicera_scenariojustering(utvandringsrisker_prognos, "utvandringsrisker")
  }

  # ===========================================================
  # 7. LÄNSGRÄNSANDELAR (för regional avstämning)
  #    Andel = ovriga_lan / total, poolad över historiska år och
  #    expanderad till prognosår (en andel per region/kon/alder/ar).
  # ===========================================================
  berakna_lansgransandelar <- function(raw_data, antal_ar = 5,
                                       variabel_namn = "Andel länsgräns") {
    if (is.null(raw_data) || nrow(raw_data) == 0) return(tibble())

    senaste_ar <- raw_data %>%
      dplyr::pull(ar) %>% unique() %>% sort() %>% tail(antal_ar)

    poolad <- raw_data %>%
      filter(.data$ar %in% senaste_ar, region != "Riket") %>%
      group_by(region, kon, alder) %>%
      summarise(
        total      = sum(.data$total,      na.rm = TRUE),
        ovriga_lan = sum(.data$ovriga_lan, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        andel = if_else(.data$total > 0, .data$ovriga_lan / .data$total, 0),
        andel = pmin(pmax(.data$andel, 0), 1)
      ) %>%
      select(region, kon, alder, varde = andel)

    prognos_ar <- unique(riket_lista$riket_prognosinvanare_grund$ar)

    poolad %>%
      crossing(ar = prognos_ar) %>%
      mutate(variabel = variabel_namn) %>%
      select(region, kon, alder, ar, variabel, varde)
  }

  # ===========================================================
  # KÖR ALLA RISKBERÄKNINGAR OCH RETURNERA IN-MEMORY
  # ===========================================================
  message("\n==== STARTAR RISKBERÄKNINGAR (in-memory) ====\n")

  fodelserisker   <- berakna_fodelserisker()   %>% filter(region != "Riket")
  message("✓ Födelserisker klara")

  dodsrisker      <- berakna_dodsrisker()      %>% filter(region != "Riket")
  message("✓ Dödsrisker klara")

  inflyttningsrisker <- berakna_inflyttningsrisker()
  message("✓ Inflyttningsrisker klara")

  utflyttningsrisker <- berakna_utflyttningsrisker() %>% filter(region != "Riket")
  message("✓ Utflyttningsrisker klara")

  invandringsrisker  <- berakna_invandringsrisker()  %>% filter(region != "Riket")
  message("✓ Invandringsrisker klara")

  utvandringsrisker  <- berakna_utvandringsrisker()  %>% filter(region != "Riket")
  message("✓ Utvandringsrisker klara")

  inflyttningsandelar_lansgrans <- berakna_lansgransandelar(
    kommun_lista$inflyttningar_lansgrans_raw,
    antal_ar      = PARAMETRAR$inflyttningsrisker$antal_ar %||% 5,
    variabel_namn = "Andel inflyttning över länsgräns"
  )
  utflyttningsandelar_lansgrans <- berakna_lansgransandelar(
    kommun_lista$utflyttningar_lansgrans_raw,
    antal_ar      = PARAMETRAR$utflyttningsrisker$antal_ar %||% 5,
    variabel_namn = "Andel utflyttning över länsgräns"
  )
  message("✓ Länsgränsandelar klara")

  message("\n==== RISKBERÄKNINGAR KLARA ====\n")

  list(
    fodelserisker                 = fodelserisker,
    dodsrisker                    = dodsrisker,
    inflyttningsrisker            = inflyttningsrisker,
    utflyttningsrisker            = utflyttningsrisker,
    invandringsrisker             = invandringsrisker,
    utvandringsrisker             = utvandringsrisker,
    inflyttningsandelar_lansgrans = inflyttningsandelar_lansgrans,
    utflyttningsandelar_lansgrans = utflyttningsandelar_lansgrans
  )
}

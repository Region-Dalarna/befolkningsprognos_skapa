# diagram_funktioner.R
# Plot-hjälpfunktioner för resultatsidan.
#
# Porterade och anpassade från Analytikernatverket/befolkningsprognoser/prognos_app.R.
# Anpassningarna:
#   - Referensappen har data uppdelat per geografi (lista av geografier).
#     Här används istället det in-memory-returvärde som kor_prognos() producerar
#     för EN geografi: prognos$totalbefolkning, prognos$komponenter[[ar]]$...
#   - Komponentnycklar i vår domänkod: fodda, doda, inrikes_inflyttning,
#     inrikes_utflyttning, invandring, utvandring  (utan svenska specialtecken).
#   - Risktal hämtas som tibble direkt ur risktal-listan (risktal$fodelserisker etc.).

library(ggplot2)
library(ggiraph)
library(dplyr)
library(tidyr)
library(scales)

# ===========================================================
# HJÄLPFUNKTIONER – VIKTER OCH JUSTERINGSTEXTER
# ===========================================================

#' Formatera viktningsinformation
formatera_vikter <- function(antal_ar, viktningstyp, alpha = 0.5) {
  ar_index <- seq(1, antal_ar)

  if (viktningstyp == 1) {
    vikter <- rep(1 / antal_ar, antal_ar)
    metod  <- "Jämn viktning"
  } else if (viktningstyp == 2) {
    vikter_raw <- ar_index
    vikter <- vikter_raw / sum(vikter_raw)
    metod  <- "Linjär viktning"
  } else if (viktningstyp == 3) {
    vikter_raw <- alpha * (1 - alpha)^(antal_ar - ar_index)
    vikter <- vikter_raw / sum(vikter_raw)
    metod  <- paste0("EWMA (alpha = ", alpha, ")")
  } else {
    vikter <- rep(1 / antal_ar, antal_ar)
    metod  <- "Okänd viktning"
  }

  # År-vektorn är approximativ för visning: baseras på innevarande år minus
  # antal historiska år. Syftet är att ge ett ungefärligt årsintervall i
  # metod-informationspanelen – exakta år finns i datatabellerna.
  ar_nu     <- as.numeric(format(Sys.Date(), "%Y"))
  ar_vektor <- seq(ar_nu - antal_ar, ar_nu - 1)
  vikt_text <- paste(ar_vektor, sprintf("%.1f%%", vikter * 100),
                     sep = ": ", collapse = ", ")
  list(metod = metod, vikter = vikt_text)
}

#' Formatera alternativjusteringar som läsbar text
formatera_justeringar <- function(justeringar) {
  if (is.null(justeringar) || length(justeringar$perioder) == 0) {
    return("Inga justeringar")
  }
  text_delar <- vapply(justeringar$perioder, function(period) {
    procent <- (period$multiplikator - 1) * 100
    sprintf("%d\u2013%d: %+.0f%%", period$från_år, period$till_år, procent)
  }, character(1))
  paste(text_delar, collapse = ", ")
}

# ===========================================================
# HJÄLPFUNKTIONER – HISTORISKA RISKTAL
# ===========================================================

#' Beräkna historiska risktal för specifika år ur rådata i kommun_lista.
#'
#' @param kommun_lista  list med historiska underlagstabeller
#' @param risk_typ      "Födelserisker" | "Dödsrisker" | "Inflyttningsrisker" |
#'                      "Utflyttningsrisker" | "Invandringsrisker" | "Utvandringsrisker"
#' @param geografi_namn Character – regionnamn att filtrera på
#' @param valda_ar      Character-vektor med år att beräkna för
berakna_historiska_risker_for_ar <- function(kommun_lista, risk_typ,
                                              geografi_namn, valda_ar) {
  valda_ar <- as.character(valda_ar)

  if (risk_typ == "Födelserisker") {
    historisk_risk <- kommun_lista$fodda %>%
      mutate(År = as.character(År)) %>%
      filter(Region == geografi_namn, År %in% valda_ar,
             Ålder >= 15, Ålder <= 49) %>%
      inner_join(
        kommun_lista$medelfolkmangd_modrar %>%
          mutate(År = as.character(År)) %>%
          filter(Region == geografi_namn, År %in% valda_ar),
        by = c("Region", "År", "Ålder")
      ) %>%
      mutate(
        Värde    = ifelse(Värde.y > 0, Värde.x / Värde.y, 0),
        Värde    = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde    = pmin(Värde, 0.5),
        Kön      = "kvinnor",
        Variabel = "Födelserisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)

  } else if (risk_typ == "Dödsrisker") {
    historisk_risk <- kommun_lista$doda %>%
      mutate(År = as.character(År)) %>%
      filter(Region == geografi_namn, År %in% valda_ar) %>%
      inner_join(
        kommun_lista$totfolkmangd %>%
          mutate(År = as.character(År)) %>%
          filter(Region == geografi_namn, År %in% valda_ar),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde    = ifelse(Värde.y > 0, Värde.x / Värde.y, 0),
        Värde    = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde    = pmin(Värde, 0.5),
        Variabel = "Dödsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)

  } else if (risk_typ == "Inflyttningsrisker") {
    riket_bef <- kommun_lista$medelfolkmangd %>%
      mutate(År = as.character(År)) %>%
      filter(Region == "Riket", År %in% valda_ar)

    historisk_risk <- kommun_lista$inrikes_inflyttade %>%
      mutate(År = as.character(År)) %>%
      filter(Region == geografi_namn, År %in% valda_ar) %>%
      inner_join(
        riket_bef %>% select(År, Ålder, Kön, antal_riket = Värde),
        by = c("År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde    = ifelse(antal_riket > 0, Värde / antal_riket, 0),
        Värde    = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde    = pmin(Värde, 0.5),
        Variabel = "Inflyttningsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)

  } else if (risk_typ == "Utflyttningsrisker") {
    historisk_risk <- kommun_lista$inrikes_utflyttade %>%
      mutate(År = as.character(År)) %>%
      filter(Region == geografi_namn, År %in% valda_ar) %>%
      inner_join(
        kommun_lista$medelfolkmangd %>%
          mutate(År = as.character(År)) %>%
          filter(Region == geografi_namn, År %in% valda_ar) %>%
          select(Region, År, Ålder, Kön, antal_bef = Värde),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde    = ifelse(antal_bef > 0, Värde / antal_bef, 0),
        Värde    = replace_na(Värde, 0),
        Värde    = pmin(Värde, 0.5),
        Variabel = "Utflyttningsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)

  } else if (risk_typ == "Invandringsrisker") {
    riket_inv <- kommun_lista$invandring %>%
      mutate(År = as.character(År)) %>%
      filter(Region == "Riket", År %in% valda_ar)

    historisk_risk <- kommun_lista$invandring %>%
      mutate(År = as.character(År)) %>%
      filter(Region == geografi_namn, År %in% valda_ar) %>%
      inner_join(
        riket_inv %>% select(År, Ålder, Kön, antal_riket = Värde),
        by = c("År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde    = ifelse(antal_riket > 0, Värde / antal_riket, 0),
        Värde    = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde    = pmin(Värde, 1.0),
        Variabel = "Invandringsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)

  } else if (risk_typ == "Utvandringsrisker") {
    historisk_risk <- kommun_lista$utvandring %>%
      mutate(År = as.character(År)) %>%
      filter(Region == geografi_namn, År %in% valda_ar) %>%
      inner_join(
        kommun_lista$medelfolkmangd %>%
          mutate(År = as.character(År)) %>%
          filter(Region == geografi_namn, År %in% valda_ar) %>%
          select(Region, År, Ålder, Kön, antal_bef = Värde),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde    = ifelse(antal_bef > 0, Värde / antal_bef, 0),
        Värde    = replace_na(Värde, 0),
        Värde    = pmin(Värde, 0.5),
        Variabel = "Utvandringsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)

  } else {
    return(tibble())
  }

  if (nrow(historisk_risk) > 0) {
    historisk_risk <- historisk_risk %>%
      filter(!is.na(Värde), !is.na(Ålder), !is.na(År))
  }
  historisk_risk
}

# ===========================================================
# RISKTAL – DATA OCH PLOT
# ===========================================================

#' Kombinera prognosrisktal med beräknade historiska risktal för valda år.
#'
#' @param risk_data     Tibble – t.ex. risktal$fodelserisker (Region, Kön, Ålder, År, Variabel, Värde)
#' @param geografi_namn Character – regionnamn
#' @param valda_ar      Character-vektor med valda år (historiska och/eller prognosår)
#' @param kommun_lista  list – historiska underlagstabeller (krävs för historiska år)
#' @param risk_typ      Character – risktyp-sträng för berakna_historiska_risker_for_ar()
skapa_risk_data_multi <- function(risk_data, geografi_namn, valda_ar,
                                  kommun_lista = NULL, risk_typ = NULL) {
  if (is.null(risk_data) || nrow(risk_data) == 0) return(NULL)
  if (!geografi_namn %in% risk_data$Region) return(NULL)

  valda_ar  <- as.character(valda_ar)
  risk_data <- risk_data %>% mutate(År = as.character(År))

  prognos_ar_i_data   <- unique(risk_data$År)
  valda_prognos_ar    <- valda_ar[valda_ar %in% prognos_ar_i_data]
  valda_historiska_ar <- valda_ar[!valda_ar %in% prognos_ar_i_data]

  prognos_data <- risk_data %>%
    filter(Region == geografi_namn, År %in% valda_prognos_ar) %>%
    mutate(Typ = "Prognos") %>%
    arrange(År, Ålder)

  historisk_data <- tibble()
  if (length(valda_historiska_ar) > 0 &&
      !is.null(kommun_lista) &&
      !is.null(risk_typ)) {
    historisk_data <- tryCatch({
      berakna_historiska_risker_for_ar(
        kommun_lista, risk_typ, geografi_namn, valda_historiska_ar) %>%
        mutate(Typ = "Historisk", År = as.character(År)) %>%
        arrange(År, Ålder)
    }, error = function(e) {
      warning(paste("Kunde inte beräkna historiska risktal:", e$message))
      tibble()
    })
  }

  bind_rows(historisk_data, prognos_data) %>%
    mutate(År = as.character(År)) %>%
    filter(!is.na(Värde), !is.na(Ålder), !is.na(År)) %>%
    arrange(År, Ålder)
}

#' Skapa riskplot (ggplot) med historiska och prognosrisktal per ålder och år.
#'
#' @param data      Tibble från skapa_risk_data_multi()
#' @param titel     Diagramtitel
#' @param y_label   Y-axelmärkning
#' @param valt_kon  "Båda" | "kvinnor" | "män"
skapa_risk_plot_multi <- function(data, titel, y_label = "Risk",
                                  valt_kon = "Båda") {
  tom_plot <- function(subtitle = "Ingen data tillgänglig") {
    ggplot() +
      labs(title = titel, subtitle = subtitle) +
      theme_minimal()
  }

  if (is.null(data) || nrow(data) == 0) return(tom_plot())

  data <- data %>% mutate(År = as.character(År))

  # Könsfiltrering
  er_fodelserisk <- any(grepl("Födelserisker", data$Variabel, ignore.case = TRUE))
  if (!er_fodelserisk) {
    if (valt_kon == "Båda" && "Kön" %in% names(data)) {
      return(tom_plot("Välj ett specifikt kön för att se data (Kvinnor eller Män)"))
    }
    if (valt_kon != "Båda" && "Kön" %in% names(data)) {
      data <- data %>% filter(Kön == valt_kon)
    }
  }

  data <- data %>% filter(!is.na(Värde), !is.na(Ålder), !is.na(År))
  if (nrow(data) == 0) return(tom_plot())

  historisk_data <- data %>% filter(Typ == "Historisk")
  prognos_data   <- data %>% filter(Typ == "Prognos")

  n_hist <- length(unique(historisk_data$År))
  n_prog <- length(unique(prognos_data$År))

  hist_colors <- if (n_hist > 0)
    colorRampPalette(c("#B0B0B0", "#606060"))(n_hist) else character(0)
  prog_colors <- if (n_prog > 0)
    colorRampPalette(c("#4A90E2", "#1E5BA8"))(n_prog) else character(0)

  all_years  <- c(if (n_hist > 0) sort(unique(historisk_data$År)) else character(0),
                  if (n_prog > 0) sort(unique(prognos_data$År)) else character(0))
  all_colors <- c(hist_colors, prog_colors)
  names(all_colors) <- all_years

  p <- ggplot(data = data, aes(x = Ålder, y = Värde))

  if (nrow(historisk_data) > 0) {
    p <- p + geom_line(data = historisk_data,
                       aes(group = År, color = År),
                       linewidth = 0.8, alpha = 0.5)
  }
  if (nrow(prognos_data) > 0) {
    p <- p + geom_line(data = prognos_data,
                       aes(group = År, color = År),
                       linewidth = 1.8, alpha = 0.7)
  }

  subtitle_text <- if (er_fodelserisk)
    paste("Endast kvinnor 15-49 år | Kön:", valt_kon)
  else
    paste("Kön:", valt_kon)

  p <- p +
    scale_color_manual(values = all_colors, name = "År") +
    labs(title = titel, subtitle = subtitle_text,
         x = "Ålder", y = y_label) +
    theme_minimal() +
    theme(
      plot.title           = element_text(size = 14, face = "bold"),
      plot.subtitle        = element_text(size = 10, color = "gray50"),
      panel.grid.minor     = element_blank(),
      panel.grid.major     = element_line(color = "gray90"),
      legend.position      = "right",
      legend.background    = element_rect(fill = "white", color = NA),
      legend.key           = element_rect(fill = "white", color = NA)
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))

  if (er_fodelserisk) {
    p <- p + scale_x_continuous(breaks = seq(15, 50, by = 5), limits = c(15, 49))
  } else {
    p <- p + scale_x_continuous(breaks = seq(0, 100, by = 10))
  }

  if (grepl("risk", y_label, ignore.case = TRUE)) {
    p <- p + scale_y_continuous(labels = percent_format(accuracy = 0.1))
  }

  p
}

# ===========================================================
# DEMOGRAFISKA KOMPONENTER ÖVER TID
# ===========================================================

#' Bygg tidsserie-data (historik + prognos) för en demografisk komponent.
#'
#' @param prognos         list – app_kontext$resultat$prognos
#'   (geografi, totalbefolkning, komponenter, sammanfattning)
#' @param komponent_typ   Character – "Födda" | "Döda" | "Födelsenetto" |
#'   "Inrikes inflyttade" | "Inrikes utflyttade" | "Inrikes flyttnetto" |
#'   "Invandrade" | "Utvandrade" | "Utrikes flyttnetto" |
#'   "Total befolkning" | "Total befolkningsförändring"
#' @param kommun_lista    list – historiska underlagstabeller (NULL om saknas)
#' @param antal_historiska_ar  integer – max antal historiska år att inkludera
skapa_komponent_data <- function(prognos, komponent_typ,
                                 kommun_lista = NULL,
                                 antal_historiska_ar = 10) {
  if (is.null(prognos)) return(NULL)

  geografi_namn  <- prognos$geografi
  alla_prognos_ar <- sort(unique(prognos$totalbefolkning$År))

  # --- Prognosdata ---
  prognos_data <- tibble()
  for (ar in alla_prognos_ar) {
    ar_komp <- prognos$komponenter[[ar]]

    varde <- switch(
      komponent_typ,
      "Födda"                    = sum(ar_komp$fodda$Värde, na.rm = TRUE),
      "Döda"                     = sum(ar_komp$doda$Värde, na.rm = TRUE),
      "Födelsenetto"             = {
        sum(ar_komp$fodda$Värde, na.rm = TRUE) -
          sum(ar_komp$doda$Värde, na.rm = TRUE)
      },
      "Inrikes inflyttade"       = sum(ar_komp$inrikes_inflyttning$Värde, na.rm = TRUE),
      "Inrikes utflyttade"       = sum(ar_komp$inrikes_utflyttning$Värde, na.rm = TRUE),
      "Inrikes flyttnetto"       = {
        sum(ar_komp$inrikes_inflyttning$Värde, na.rm = TRUE) -
          sum(ar_komp$inrikes_utflyttning$Värde, na.rm = TRUE)
      },
      "Invandrade"               = sum(ar_komp$invandring$Värde, na.rm = TRUE),
      "Utvandrade"               = sum(ar_komp$utvandring$Värde, na.rm = TRUE),
      "Utrikes flyttnetto"       = {
        sum(ar_komp$invandring$Värde, na.rm = TRUE) -
          sum(ar_komp$utvandring$Värde, na.rm = TRUE)
      },
      "Total befolkning"         = {
        sum(prognos$totalbefolkning$Värde[
          prognos$totalbefolkning$År == ar], na.rm = TRUE)
      },
      "Total befolkningsförändring" = {
        foregaende_ar <- as.character(as.numeric(ar) - 1)
        aktuell_bef   <- sum(prognos$totalbefolkning$Värde[
          prognos$totalbefolkning$År == ar], na.rm = TRUE)
        if (foregaende_ar %in% alla_prognos_ar) {
          fore_bef <- sum(prognos$totalbefolkning$Värde[
            prognos$totalbefolkning$År == foregaende_ar], na.rm = TRUE)
          aktuell_bef - fore_bef
        } else if (!is.null(kommun_lista) && "totfolkmangd" %in% names(kommun_lista)) {
          hist_bef <- kommun_lista$totfolkmangd %>%
            filter(Region == geografi_namn,
                   as.character(År) == foregaende_ar) %>%
            summarise(n = sum(Värde, na.rm = TRUE)) %>%
            pull(n)
          if (length(hist_bef) > 0 && !is.na(hist_bef)) aktuell_bef - hist_bef
          else NA_real_
        } else NA_real_
      },
      NA_real_
    )

    prognos_data <- bind_rows(prognos_data,
                              tibble(År = as.numeric(ar),
                                     Värde = varde,
                                     Komponent = komponent_typ,
                                     Dataserie = "Prognos"))
  }

  # --- Historisk data ---
  historisk_data <- tibble()
  if (!is.null(kommun_lista)) {
    historisk_data <- switch(
      komponent_typ,
      "Födda" = if ("fodda" %in% names(kommun_lista))
        kommun_lista$fodda %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(Komponent = komponent_typ, Dataserie = "Historisk"),

      "Döda" = if ("doda" %in% names(kommun_lista))
        kommun_lista$doda %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(Komponent = komponent_typ, Dataserie = "Historisk"),

      "Födelsenetto" = if ("fodda" %in% names(kommun_lista) &&
                            "doda" %in% names(kommun_lista)) {
        fodda_h <- kommun_lista$fodda %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Fodda = sum(Värde, na.rm = TRUE), .groups = "drop")
        doda_h  <- kommun_lista$doda %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Doda = sum(Värde, na.rm = TRUE), .groups = "drop")
        fodda_h %>% left_join(doda_h, by = "År") %>%
          mutate(Värde = Fodda - Doda, Komponent = komponent_typ,
                 Dataserie = "Historisk") %>%
          select(År, Värde, Komponent, Dataserie)
      },

      "Inrikes inflyttade" = if ("inrikes_inflyttade" %in% names(kommun_lista))
        kommun_lista$inrikes_inflyttade %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(Komponent = komponent_typ, Dataserie = "Historisk"),

      "Inrikes utflyttade" = if ("inrikes_utflyttade" %in% names(kommun_lista))
        kommun_lista$inrikes_utflyttade %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(Komponent = komponent_typ, Dataserie = "Historisk"),

      "Inrikes flyttnetto" = if ("inrikes_inflyttade" %in% names(kommun_lista) &&
                                  "inrikes_utflyttade" %in% names(kommun_lista)) {
        inf_h <- kommun_lista$inrikes_inflyttade %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Infl = sum(Värde, na.rm = TRUE), .groups = "drop")
        utf_h <- kommun_lista$inrikes_utflyttade %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Utf = sum(Värde, na.rm = TRUE), .groups = "drop")
        inf_h %>% left_join(utf_h, by = "År") %>%
          mutate(Värde = Infl - Utf, Komponent = komponent_typ, Dataserie = "Historisk") %>%
          select(År, Värde, Komponent, Dataserie)
      },

      "Invandrade" = if ("invandring" %in% names(kommun_lista))
        kommun_lista$invandring %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(Komponent = komponent_typ, Dataserie = "Historisk"),

      "Utvandrade" = if ("utvandring" %in% names(kommun_lista))
        kommun_lista$utvandring %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(Komponent = komponent_typ, Dataserie = "Historisk"),

      "Utrikes flyttnetto" = if ("invandring" %in% names(kommun_lista) &&
                                  "utvandring" %in% names(kommun_lista)) {
        inv_h <- kommun_lista$invandring %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Inv = sum(Värde, na.rm = TRUE), .groups = "drop")
        utv_h <- kommun_lista$utvandring %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Utv = sum(Värde, na.rm = TRUE), .groups = "drop")
        inv_h %>% left_join(utv_h, by = "År") %>%
          mutate(Värde = Inv - Utv, Komponent = komponent_typ, Dataserie = "Historisk") %>%
          select(År, Värde, Komponent, Dataserie)
      },

      "Total befolkning" = if ("totfolkmangd" %in% names(kommun_lista))
        kommun_lista$totfolkmangd %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(Komponent = komponent_typ, Dataserie = "Historisk"),

      "Total befolkningsförändring" = if ("totfolkmangd" %in% names(kommun_lista))
        kommun_lista$totfolkmangd %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Bef = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          arrange(År) %>%
          mutate(Värde = Bef - lag(Bef)) %>%
          filter(!is.na(Värde)) %>%
          mutate(Komponent = komponent_typ, Dataserie = "Historisk") %>%
          select(År, Värde, Komponent, Dataserie),

      tibble()  # default – okänd komponenttyp
    )

    if (is.null(historisk_data)) historisk_data <- tibble()

    if (nrow(historisk_data) > 0) {
      max_hist <- max(as.numeric(historisk_data$År))
      min_hist <- max_hist - antal_historiska_ar + 1
      historisk_data <- historisk_data %>%
        filter(as.numeric(År) >= min_hist) %>%
        mutate(År = as.numeric(År))
    }
  }

  bind_rows(historisk_data, prognos_data) %>%
    filter(!is.na(Värde))
}

#' Skapa interaktiv ggiraph-linjeplot för en demografisk komponent över tid.
#'
#' @param data   Tibble från skapa_komponent_data()
#' @param titel  Diagramtitel
skapa_komponent_plot <- function(data, titel) {
  if (is.null(data) || nrow(data) == 0) {
    return(girafe(
      ggobj   = ggplot() +
        labs(title = titel, subtitle = "Ingen data tillgänglig") +
        theme_minimal()
    ))
  }

  prognos_ar <- data %>% filter(Dataserie == "Prognos") %>% pull(År)
  brytpunkt  <- if (length(prognos_ar) > 0) min(prognos_ar) - 0.5 else NA

  p <- ggplot(data, aes(x = År, y = Värde, color = Dataserie, group = Dataserie)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0("År: ", År, "\nVärde: ", round(Värde))),
      size = 2
    ) +
    scale_color_manual(values = c("Historisk" = "black", "Prognos" = "blue")) +
    labs(title = titel, x = "År", y = "Antal", color = "Dataserie") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(size = 10),
      axis.title      = element_text(size = 9)
    )

  if (!is.na(brytpunkt)) {
    p <- p + geom_vline(xintercept = brytpunkt, linetype = "dashed",
                        color = "darkgray", alpha = 0.7)
  }

  girafe(
    ggobj      = p,
    width_svg  = if (grepl("Total", titel)) 7.9 else 5.0,
    height_svg = 3.5,
    options    = list(
      opts_toolbar(saveaspng = FALSE),
      opts_sizing(rescale = FALSE)
    )
  )
}

# ===========================================================
# 1-ÅRSKLASSER – DATA OCH PLOT
# ===========================================================

#' Bygg åldersklassdata för ett eller flera år (historik + prognos).
#'
#' @param prognos         list – app_kontext$resultat$prognos
#' @param komponent_typ   Character – se skapa_komponent_data()
#' @param valda_ar        Character-vektor med valda år
#' @param kommun_lista    list – historiska underlagstabeller (NULL om saknas)
#' @param fodelserisker   Tibble med födelserisker (krävs för "Födda efter moderns ålder")
skapa_ettarsklass_data <- function(prognos, komponent_typ, valda_ar,
                                   kommun_lista = NULL,
                                   fodelserisker = NULL) {
  if (is.null(prognos)) return(NULL)

  geografi_namn  <- prognos$geografi
  prognos_ar_i_data  <- names(prognos$komponenter)
  valda_prognos_ar   <- valda_ar[valda_ar %in% prognos_ar_i_data]

  # --- Prognosdata ---
  prognos_data <- tibble()
  for (ar in valda_prognos_ar) {
    ar_komp <- prognos$komponenter[[ar]]

    ar_data <- switch(
      komponent_typ,

      "Födda efter moderns ålder" = {
        kv_fertil <- prognos$totalbefolkning %>%
          filter(År == ar, Kön == "kvinnor", Ålder >= 15, Ålder <= 49) %>%
          select(Ålder, Antal_kv = Värde)

        if (!is.null(fodelserisker)) {
          fr_ar <- fodelserisker %>%
            filter(Region == geografi_namn, År == ar) %>%
            select(Ålder, Fodelserisk = Värde)
          kv_fertil %>%
            left_join(fr_ar, by = "Ålder") %>%
            mutate(Värde     = replace_na(Antal_kv * Fodelserisk, 0),
                   År        = ar,
                   Komponent = komponent_typ,
                   Dataserie = "Prognos") %>%
            select(Ålder, År, Värde, Komponent, Dataserie)
        } else tibble()
      },

      "Döda" = ar_komp$doda %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos"),

      "Inrikes inflyttade" = ar_komp$inrikes_inflyttning %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos"),

      "Inrikes utflyttade" = ar_komp$inrikes_utflyttning %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos"),

      "Inrikes flyttnetto" = {
        inf <- ar_komp$inrikes_inflyttning %>%
          group_by(Ålder, År) %>% summarise(Infl = sum(Värde, na.rm = TRUE), .groups = "drop")
        utf <- ar_komp$inrikes_utflyttning %>%
          group_by(Ålder, År) %>% summarise(Utf = sum(Värde, na.rm = TRUE), .groups = "drop")
        inf %>% left_join(utf, by = c("Ålder", "År")) %>%
          mutate(Värde = Infl - Utf, Komponent = komponent_typ, Dataserie = "Prognos") %>%
          select(Ålder, År, Värde, Komponent, Dataserie)
      },

      "Invandrade" = ar_komp$invandring %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos"),

      "Utvandrade" = ar_komp$utvandring %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos"),

      "Utrikes flyttnetto" = {
        inv <- ar_komp$invandring %>%
          group_by(Ålder, År) %>% summarise(Inv = sum(Värde, na.rm = TRUE), .groups = "drop")
        utv <- ar_komp$utvandring %>%
          group_by(Ålder, År) %>% summarise(Utv = sum(Värde, na.rm = TRUE), .groups = "drop")
        inv %>% left_join(utv, by = c("Ålder", "År")) %>%
          mutate(Värde = Inv - Utv, Komponent = komponent_typ, Dataserie = "Prognos") %>%
          select(Ålder, År, Värde, Komponent, Dataserie)
      },

      "Total befolkning" = prognos$totalbefolkning %>%
        filter(År == ar) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos"),

      tibble()
    )

    prognos_data <- bind_rows(prognos_data, ar_data)
  }

  if (nrow(prognos_data) > 0) {
    prognos_data <- prognos_data %>% mutate(År = as.numeric(År))
  }

  # --- Historisk data ---
  historisk_data <- tibble()
  if (!is.null(kommun_lista)) {
    historiska_ar_tillg <- setdiff(valda_ar, prognos_ar_i_data)

    historisk_data <- switch(
      komponent_typ,

      "Födda efter moderns ålder" = if (!is.null(historiska_ar_tillg) &&
                                         length(historiska_ar_tillg) > 0 &&
                                         "fodda" %in% names(kommun_lista) &&
                                         "totfolkmangd" %in% names(kommun_lista)) {
        fodda_tot <- kommun_lista$fodda %>% filter(Region == geografi_namn) %>%
          group_by(År) %>% summarise(Totalt = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År)) %>% filter(År %in% as.numeric(historiska_ar_tillg))

        kv_fertil <- kommun_lista$totfolkmangd %>%
          filter(Region == geografi_namn, Kön == "kvinnor", Ålder >= 15, Ålder <= 49) %>%
          group_by(År, Ålder) %>%
          summarise(Antal_kv = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År)) %>% filter(År %in% as.numeric(historiska_ar_tillg))

        # Normalfördelning (medel=30, sd=5) ger en rimlig approximation av
        # hur födslar fördelar sig på moderns ålder (15-49 år) när exakta
        # åldersspecifika födelsedata saknas för historiska år.
        alder_fordeln <- tibble(Ålder = 15:49,
                                Vikt  = dnorm(15:49, mean = 30, sd = 5)) %>%
          mutate(Vikt = Vikt / sum(Vikt))

        kv_fertil %>%
          inner_join(fodda_tot, by = "År") %>%
          left_join(alder_fordeln, by = "Ålder") %>%
          group_by(År) %>%
          mutate(Total_vikt = sum(Antal_kv * Vikt, na.rm = TRUE),
                 Andel = (Antal_kv * Vikt) / Total_vikt,
                 Värde = Totalt * Andel,
                 Komponent = komponent_typ, Dataserie = "Historisk") %>%
          ungroup() %>%
          select(Ålder, År, Värde, Komponent, Dataserie)
      },

      "Döda" = if ("doda" %in% names(kommun_lista))
        kommun_lista$doda %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År), Komponent = komponent_typ, Dataserie = "Historisk") %>%
          filter(År %in% as.numeric(valda_ar)),

      "Inrikes inflyttade" = if ("inrikes_inflyttade" %in% names(kommun_lista))
        kommun_lista$inrikes_inflyttade %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År), Komponent = komponent_typ, Dataserie = "Historisk") %>%
          filter(År %in% as.numeric(valda_ar)),

      "Inrikes utflyttade" = if ("inrikes_utflyttade" %in% names(kommun_lista))
        kommun_lista$inrikes_utflyttade %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År), Komponent = komponent_typ, Dataserie = "Historisk") %>%
          filter(År %in% as.numeric(valda_ar)),

      "Inrikes flyttnetto" = if ("inrikes_inflyttade" %in% names(kommun_lista) &&
                                  "inrikes_utflyttade" %in% names(kommun_lista)) {
        inf_h <- kommun_lista$inrikes_inflyttade %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Infl = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År)) %>% filter(År %in% as.numeric(valda_ar))
        utf_h <- kommun_lista$inrikes_utflyttade %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Utf = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År)) %>% filter(År %in% as.numeric(valda_ar))
        inf_h %>% left_join(utf_h, by = c("Ålder", "År")) %>%
          mutate(Värde = Infl - Utf, Komponent = komponent_typ, Dataserie = "Historisk") %>%
          select(Ålder, År, Värde, Komponent, Dataserie)
      },

      "Invandrade" = if ("invandring" %in% names(kommun_lista))
        kommun_lista$invandring %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År), Komponent = komponent_typ, Dataserie = "Historisk") %>%
          filter(År %in% as.numeric(valda_ar)),

      "Utvandrade" = if ("utvandring" %in% names(kommun_lista))
        kommun_lista$utvandring %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År), Komponent = komponent_typ, Dataserie = "Historisk") %>%
          filter(År %in% as.numeric(valda_ar)),

      "Utrikes flyttnetto" = if ("invandring" %in% names(kommun_lista) &&
                                  "utvandring" %in% names(kommun_lista)) {
        inv_h <- kommun_lista$invandring %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Inv = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År)) %>% filter(År %in% as.numeric(valda_ar))
        utv_h <- kommun_lista$utvandring %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Utv = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År)) %>% filter(År %in% as.numeric(valda_ar))
        inv_h %>% left_join(utv_h, by = c("Ålder", "År")) %>%
          mutate(Värde = Inv - Utv, Komponent = komponent_typ, Dataserie = "Historisk") %>%
          select(Ålder, År, Värde, Komponent, Dataserie)
      },

      "Total befolkning" = if ("totfolkmangd" %in% names(kommun_lista))
        kommun_lista$totfolkmangd %>% filter(Region == geografi_namn) %>%
          group_by(Ålder, År) %>% summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
          mutate(År = as.numeric(År), Komponent = komponent_typ, Dataserie = "Historisk") %>%
          filter(År %in% as.numeric(valda_ar)),

      tibble()
    )

    if (is.null(historisk_data)) historisk_data <- tibble()
  }

  bind_rows(historisk_data, prognos_data) %>%
    filter(!is.na(Värde)) %>%
    mutate(År = as.numeric(År), Ålder = as.numeric(Ålder))
}

#' Skapa åldersklass-plot (ggplot, statisk) med en linje per valt år.
#'
#' @param data   Tibble från skapa_ettarsklass_data()
#' @param titel  Diagramtitel
skapa_ettarsklass_plot <- function(data, titel) {
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() +
             labs(title = titel, subtitle = "Ingen data tillgänglig") +
             theme_minimal())
  }

  historisk_data <- data %>% filter(Dataserie == "Historisk")
  prognos_data   <- data %>% filter(Dataserie == "Prognos")

  n_hist <- length(unique(historisk_data$År))
  n_prog <- length(unique(prognos_data$År))

  hist_colors <- if (n_hist > 0)
    colorRampPalette(c("#B0B0B0", "#606060"))(n_hist) else character(0)
  prog_colors <- if (n_prog > 0)
    colorRampPalette(c("#4A90E2", "#1E5BA8"))(n_prog) else character(0)

  all_years  <- c(sort(unique(historisk_data$År)), sort(unique(prognos_data$År)))
  all_colors <- c(hist_colors, prog_colors)
  names(all_colors) <- as.character(all_years)

  p <- ggplot()

  if (nrow(historisk_data) > 0) {
    p <- p + geom_line(data = historisk_data,
                       aes(x = Ålder, y = Värde,
                           group = År, color = as.character(År)),
                       linewidth = 0.8, alpha = 0.5)
  }
  if (nrow(prognos_data) > 0) {
    p <- p + geom_line(data = prognos_data,
                       aes(x = Ålder, y = Värde,
                           group = År, color = as.character(År)),
                       linewidth = 1.8, alpha = 0.7)
  }

  p <- p +
    scale_color_manual(values = all_colors, name = "År") +
    labs(title = titel,
         subtitle = if (grepl("moderns", titel, ignore.case = TRUE))
           "Antal födda fördelat på moderns ålder" else "",
         x = "Ålder", y = "Antal") +
    theme_minimal() +
    theme(
      plot.title        = element_text(size = 14, face = "bold"),
      plot.subtitle     = element_text(size = 10, color = "gray50"),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "gray90"),
      legend.position   = "right",
      legend.background = element_rect(fill = "white", color = NA),
      legend.key        = element_rect(fill = "white", color = NA)
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))

  if (grepl("moderns", titel, ignore.case = TRUE)) {
    p <- p + scale_x_continuous(breaks = seq(15, 50, by = 5), limits = c(15, 49))
  } else {
    p <- p + scale_x_continuous(breaks = seq(0, 100, by = 10))
  }

  if (grepl("netto|f\u00f6r\u00e4ndring", titel, ignore.case = TRUE)) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed",
                        color = "darkgray", alpha = 0.7)
  }

  p
}

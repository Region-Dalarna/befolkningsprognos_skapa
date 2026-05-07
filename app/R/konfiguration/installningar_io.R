# ------------------------------------------------------------------
# installningar_io.R
#
# Spara/ladda användarens antaganden (viktningsparametrar) och
# eventuellt reviderade komponenter (alternativa justeringsperioder)
# som JSON-fil.
#
# Tre publika funktioner:
#   - bygg_installningar_objekt(input, alt_perioder) -> list (för JSON)
#   - validera_installningar(obj) -> list(ok = TRUE/FALSE, fel = chr)
#   - applicera_installningar(obj, session, alt_perioder) -> invisible()
# ------------------------------------------------------------------

INSTALLNINGAR_SCHEMA         <- "befolkningsprognos-installningar"
INSTALLNINGAR_SCHEMA_VERSION <- 1L

# --- Decimalhjälpare ----------------------------------------------
# parse_decimal: tar emot text eller numeriskt värde, accepterar både
# komma och punkt som decimaltecken, returnerar numeriskt.
parse_decimal <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.numeric(x)) {
    v <- as.numeric(x[1])
    return(if (is.na(v)) default else v)
  }
  s <- trimws(as.character(x[1]))
  if (!nzchar(s) || toupper(s) == "NA") return(default)
  s <- gsub(",", ".", s, fixed = TRUE)
  v <- suppressWarnings(as.numeric(s))
  if (length(v) == 0 || is.na(v)) default else v
}

# format_decimal: numeriskt -> sträng med komma som decimaltecken.
format_decimal <- function(x, decimals = 2) {
  if (is.null(x) || length(x) == 0) return("")
  v <- x[1]
  if (is.na(v) || !is.finite(v)) return("")
  s <- formatC(v, digits = decimals, format = "f")
  gsub(".", ",", s, fixed = TRUE)
}


KOMP_IDS <- c(
  "fodelserisker",
  "dodsrisker",
  "inflyttningsrisker",
  "utflyttningsrisker",
  "invandringsrisker",
  "utvandringsrisker"
)


# --- Hjälp: bygg objekt från nuvarande UI-state -------------------
bygg_installningar_objekt <- function(input, alt_perioder) {

  riskparametrar <- lapply(
    setNames(KOMP_IDS, KOMP_IDS),
    function(id) {
      vtyp <- suppressWarnings(as.integer(input[[paste0("risk_vtyp_", id)]]))
      ar   <- suppressWarnings(as.integer(input[[paste0("risk_ar_",   id)]]))
      out  <- list(
        antal_ar     = ar,
        viktningstyp = vtyp
      )
      if (!is.na(vtyp) && vtyp == 3L) {
        out$alpha <- parse_decimal(input[[paste0("risk_alpha_", id)]], default = 0.33)
      }
      out
    }
  )

  alt_aktivera <- isTRUE(input$alt_aktivera)

  resultat <- list(
    schema         = INSTALLNINGAR_SCHEMA,
    schema_version = INSTALLNINGAR_SCHEMA_VERSION,
    skapad         = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    skapad_av      = "Befolkningsprognos-appen",

    riskparametrar = riskparametrar,
    alt_aktivera   = alt_aktivera
  )

  # Inkludera reviderade komponenter endast om aktiverade
  if (alt_aktivera) {
    prognos_slut <- suppressWarnings(as.integer(input$prognos_slut))
    if (length(prognos_slut) == 0 || is.na(prognos_slut)) {
      prognos_slut <- 2050L
    }

    resultat$alternativ_justeringar <- lapply(
      setNames(KOMP_IDS, KOMP_IDS),
      function(id) {
        df <- alt_perioder[[id]]()
        perioder <- lapply(seq_len(nrow(df)), function(i) {
          # Läs aktuella värden från input (användarens senaste inmatning),
          # fall tillbaka till df-värdet om input inte är tillgängligt.
          fr_in  <- input[[paste0("alt_fran_", id, "_", i)]]
          til_in <- input[[paste0("alt_till_", id, "_", i)]]
          mu_in  <- input[[paste0("alt_mult_", id, "_", i)]]

          fr  <- suppressWarnings(as.integer(if (!is.null(fr_in))  fr_in  else df$fran[i]))
          til <- suppressWarnings(as.integer(if (!is.null(til_in)) til_in else df$till[i]))
          mu  <- parse_decimal(if (!is.null(mu_in)) mu_in else df$mult[i],
                               default = as.numeric(df$mult[i]))
          if (is.na(til)) til <- prognos_slut
          list(
            fran_ar       = fr,
            till_ar       = til,
            multiplikator = mu
          )
        })
        list(perioder = perioder)
      }
    )
  }

  resultat
}


# --- Validering ---------------------------------------------------
# Returnerar list(ok = logical, fel = character vector med felmeddelanden)
validera_installningar <- function(obj) {

  fel <- character(0)
  lagg_fel <- function(msg) fel <<- c(fel, msg)

  if (!is.list(obj)) {
    return(list(ok = FALSE, fel = "Filen innehåller inte en giltig inställningsstruktur."))
  }

  # Schema
  schema <- obj$schema
  if (is.null(schema) || !identical(schema, INSTALLNINGAR_SCHEMA)) {
    lagg_fel(sprintf(
      "Fel filtyp: förväntade schema='%s', hittade '%s'.",
      INSTALLNINGAR_SCHEMA,
      if (is.null(schema)) "(saknas)" else as.character(schema)
    ))
  }

  ver <- obj$schema_version
  if (is.null(ver) || suppressWarnings(as.integer(ver)) != INSTALLNINGAR_SCHEMA_VERSION) {
    lagg_fel(sprintf(
      "Schemaversion stöds inte: förväntade %d, hittade '%s'.",
      INSTALLNINGAR_SCHEMA_VERSION,
      if (is.null(ver)) "(saknas)" else as.character(ver)
    ))
  }

  # Om grundläggande schema-fält saknas: avbryt direkt
  if (length(fel) > 0) {
    return(list(ok = FALSE, fel = fel))
  }

  # --- riskparametrar ---
  rp <- obj$riskparametrar
  if (!is.list(rp)) {
    lagg_fel("Sektion 'riskparametrar' saknas eller är felaktig.")
  } else {
    saknade_komp <- setdiff(KOMP_IDS, names(rp))
    if (length(saknade_komp) > 0) {
      lagg_fel(paste(
        "Riskparametrar saknas för:",
        paste(saknade_komp, collapse = ", ")
      ))
    }

    for (id in intersect(KOMP_IDS, names(rp))) {
      p <- rp[[id]]
      if (!is.list(p)) {
        lagg_fel(sprintf("Riskparametrar för '%s' har fel struktur.", id))
        next
      }

      ar <- suppressWarnings(as.integer(p$antal_ar))
      if (length(ar) == 0) ar <- NA_integer_
      if (is.na(ar) || ar < 3L || ar > 20L) {
        lagg_fel(sprintf("'%s': antal_ar måste vara ett heltal mellan 3 och 20.", id))
      }

      vtyp <- suppressWarnings(as.integer(p$viktningstyp))
      if (length(vtyp) == 0) vtyp <- NA_integer_
      if (is.na(vtyp) || !(vtyp %in% c(1L, 2L, 3L))) {
        lagg_fel(sprintf("'%s': viktningstyp måste vara 1, 2 eller 3.", id))
      } else if (vtyp == 3L) {
        if (is.null(p$alpha)) {
          lagg_fel(sprintf("'%s': alpha krävs när viktningstyp = 3 (EWMA).", id))
        } else {
          a <- suppressWarnings(as.numeric(p$alpha))
          if (length(a) == 0) a <- NA_real_
          if (is.na(a) || a < 0.1 || a > 0.9) {
            lagg_fel(sprintf("'%s': alpha måste vara mellan 0,1 och 0,9.", id))
          }
        }
      }
    }
  }

  # --- alt_aktivera (frivilligt, default FALSE) ---
  if (!is.null(obj$alt_aktivera)) {
    if (is.na(suppressWarnings(as.logical(obj$alt_aktivera)))) {
      lagg_fel("Fältet 'alt_aktivera' måste vara TRUE eller FALSE.")
    }
  }

  # --- alternativ_justeringar (helt frivilligt — valideras bara om de finns) ---
  aj <- obj$alternativ_justeringar
  if (!is.null(aj)) {
    if (!is.list(aj)) {
      lagg_fel("Sektion 'alternativ_justeringar' har fel struktur.")
    } else {
      saknade <- setdiff(KOMP_IDS, names(aj))
      if (length(saknade) > 0) {
        lagg_fel(paste(
          "Reviderade komponenter saknas för:",
          paste(saknade, collapse = ", ")
        ))
      }

      for (id in intersect(KOMP_IDS, names(aj))) {
        komp <- aj[[id]]
        if (!is.list(komp) || is.null(komp$perioder) || !is.list(komp$perioder)) {
          lagg_fel(sprintf("'%s': sektionen 'perioder' saknas eller är felaktig.", id))
          next
        }
        if (length(komp$perioder) < 1) {
          lagg_fel(sprintf("'%s': minst en period krävs.", id))
          next
        }

        for (i in seq_along(komp$perioder)) {
          per <- komp$perioder[[i]]
          if (!is.list(per)) {
            lagg_fel(sprintf("'%s' period %d: fel struktur.", id, i))
            next
          }
          fr  <- suppressWarnings(as.integer(per$fran_ar))
          til <- suppressWarnings(as.integer(per$till_ar))
          mu  <- suppressWarnings(as.numeric(per$multiplikator))
          # NULL → as.integer(NULL) ger length 0; tvinga till NA-skalär
          if (length(fr)  == 0) fr  <- NA_integer_
          if (length(til) == 0) til <- NA_integer_
          if (length(mu)  == 0) mu  <- NA_real_

          if (is.na(fr)) {
            lagg_fel(sprintf("'%s' period %d: 'fran_ar' saknas eller är ogiltig.", id, i))
          }
          if (is.na(til)) {
            lagg_fel(sprintf("'%s' period %d: 'till_ar' saknas eller är ogiltig.", id, i))
          }
          if (!is.na(fr) && !is.na(til) && til < fr) {
            lagg_fel(sprintf(
              "'%s' period %d: till_ar (%d) får inte vara mindre än fran_ar (%d).",
              id, i, til, fr
            ))
          }
          if (is.na(mu) || mu < 0.5 || mu > 1.5) {
            lagg_fel(sprintf(
              "'%s' period %d: multiplikator måste vara ett tal mellan 0,5 och 1,5.",
              id, i
            ))
          }
        }
      }
    }
  }

  list(ok = length(fel) == 0, fel = fel)
}


# --- Applicera inställningar på Shiny-state ----------------------
applicera_installningar <- function(obj, session, alt_perioder) {

  # 1. Riskparametrar
  for (id in KOMP_IDS) {
    p    <- obj$riskparametrar[[id]]
    ar   <- as.integer(p$antal_ar)
    vtyp <- as.integer(p$viktningstyp)

    updateNumericInput(session, paste0("risk_ar_",   id), value = ar)
    updateSelectInput( session, paste0("risk_vtyp_", id), selected = as.character(vtyp))

    if (!is.na(vtyp) && vtyp == 3L && !is.null(p$alpha)) {
      updateTextInput(session, paste0("risk_alpha_", id),
                      value = format_decimal(parse_decimal(p$alpha), decimals = 2))
    }
  }

  # 2. Reviderade komponenter — skriv dataframe per komponent
  if (!is.null(obj$alternativ_justeringar)) {
    for (id in KOMP_IDS) {
      perioder <- obj$alternativ_justeringar[[id]]$perioder
      df <- do.call(rbind, lapply(perioder, function(per) {
        data.frame(
          fran = as.integer(per$fran_ar),
          till = as.integer(per$till_ar),
          mult = as.numeric(per$multiplikator),
          stringsAsFactors = FALSE
        )
      }))
      alt_perioder[[id]](df)
    }
  }

  # 3. Aktivera/avaktivera reviderings-panelen sist
  updateCheckboxInput(session, "alt_aktivera",
                      value = isTRUE(as.logical(obj$alt_aktivera)))

  invisible()
}

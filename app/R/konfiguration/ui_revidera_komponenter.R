revidera_komponent_ui <- function(id, etikett) {
  card(
    card_header(etikett),
    fluidRow(
      column(
        4,
        numericInput(
          paste0("alt_fran_", id),
          "Från år",
          value = 2030,
          min = 2026,
          step = 1
        )
      ),
      column(
        4,
        numericInput(
          paste0("alt_till_", id),
          "Till år",
          value = 2040,
          min = 2026,
          step = 1
        )
      ),
      column(
        4,
        numericInput(
          paste0("alt_mult_", id),
          "Multiplikator",
          value = 1.00,
          min = 0.5,
          max = 1.5,
          step = 0.01
        )
      )
    )
  )
}

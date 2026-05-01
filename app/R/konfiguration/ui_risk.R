tags <- shiny::tags

risk_ui <- function(id,
                    etikett,
                    default_vtyp = 3,
                    default_ar = 7,
                    default_alpha = 0.33) {

  fluidRow(
    style = "margin-bottom:2px; align-items:center;",
    column(
      3,
      tags$div(
        style="font-size:0.85em; font-weight:600; padding-top:6px;",
        etikett
      )
    ),
    column(
      2,
      numericInput(
        paste0("risk_ar_", id),
        NULL,
        value = default_ar,
        min = 3,
        max = 20,
        step = 1
      )
    ),
    column(
      3,
      selectInput(
        paste0("risk_vtyp_", id),
        NULL,
        choices = c(
          "1 – Jämn"   = 1,
          "2 – Linjär" = 2,
          "3 – EWMA"   = 3
        ),
        selected = default_vtyp
      )
    ),
    column(
      3,
      conditionalPanel(
        condition = sprintf("input['risk_vtyp_%s'] == '3'", id),
        numericInput(
          paste0("risk_alpha_", id),
          NULL,
          value = if (is.na(default_alpha)) 0.33 else default_alpha,
          min = 0.1,
          max = 0.9,
          step = 0.01
        )
      )
    )
  )
}

ui_resultat <- function(app_kontext) {
  tagList(
    h3("Resultat"),

    fluidRow(
      column(
        6,
        card(
          card_header("Metadata"),
          uiOutput("resultat_metadata")
        )
      ),
      column(
        6,
        card(
          card_header("Befolkningsutveckling (sammanfattning)"),
          tableOutput("resultat_sammanfattning")
        )
      )
    ),

    fluidRow(
      column(
        12,
        card(
          card_header("Detaljerat resultatobjekt (debug)"),
          verbatimTextOutput("debug_resultat")
        )
      )
    )
  )
}

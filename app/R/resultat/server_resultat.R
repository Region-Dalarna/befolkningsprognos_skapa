server_resultat <- function(input, output, session, app_kontext) {

  output$debug_resultat <- renderPrint({
    req(app_kontext$resultat)
    app_kontext$resultat
  })

}

library(pxweb)


hamtaregtab <- function(){

  # Hämta tabell med regioner
  url_adress <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"

  hamtad_regionkod <- hamta_giltiga_varden_fran_tabell(url_adress, "region")
  hamtad_region <- hamta_giltiga_varden_fran_tabell(url_adress, "region", klartext = TRUE)

  regdf <- data.frame(regionkod = hamtad_regionkod, region = hamtad_region)

  return(regdf)
}


hamta_giltiga_varden_fran_tabell <- function(api_url, variabel, klartext = FALSE, kod_OCH_klartext = FALSE){

  # om vi skickar med en metadata-lista (som vi får genom att köra "px_meta <- pxweb_get(api_url)") så använder vi den direkt
  # om det är en url så hämtar vi data med den
  metadata <- if(is.list(api_url)) api_url else pxweb_get(api_url)

  ind <- which(tolower(sapply(metadata$variables, "[[", 1)) == tolower(variabel))  # vi kollar om skickad variabel finns som kod
  if (length(ind) < 1) ind <- which(tolower(sapply(metadata$variables, "[[", 2)) == tolower(variabel)) # om inte variabeln finns som kod så kollar vi om den finns som text

  if (length(ind) > 0){
    # välj om vi ska hämta koder eller klartext
    if (klartext) retur_varden <- metadata$variables[[ind]]$valueTexts else retur_varden <- metadata$variables[[ind]]$values
    if (kod_OCH_klartext) retur_varden <- data.frame(kod = metadata$variables[[ind]]$values, klartext = metadata$variables[[ind]]$valueTexts)
  } else {
    warning("Variabeln ", variabel, " hittades inte i tabellen med url: ", api_url, ". Kontrollera stavningen eller att variabeln finns i aktuell tabell.")
    retur_varden <- NULL
  }
  return(retur_varden)
}

# Enkel cache så att nätverksanropet bara görs en gång per Shiny-session
.geo_cache <- NULL

hamta_geografi_val <- function() {

  if (!is.null(.geo_cache)) return(.geo_cache)

  #source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

  geo_df <- hamtaregtab()

  con <- shiny_uppkoppling_las("oppna_data")                                                  # skapa anslutning
  geo_df <- tbl(con, dbplyr::in_schema("scb", "totfolkmangd")) %>%      # använd dbplyr för att hämta delar av tabellen
    distinct(regionkod, region) %>%
    collect()                                                               # transformera till WGS84 för användning i leaflet
  DBI::dbDisconnect(con)


  rk <- as.character(geo_df$regionkod)
  rk_num <- suppressWarnings(as.integer(rk))
  rk <- ifelse(!is.na(rk_num) & rk_num < 100, sprintf("%02d", rk_num),
               ifelse(!is.na(rk_num) & rk_num < 10000, sprintf("%04d", rk_num), rk))
  geo_df$regionkod <- rk

  geo_df <- geo_df[geo_df$regionkod != "00", , drop = FALSE]

  is_lan <- nchar(geo_df$regionkod) == 2

  lan_val    <- setNames(geo_df$regionkod[is_lan],  geo_df$region[is_lan])
  kommun_val <- setNames(geo_df$regionkod[!is_lan], geo_df$region[!is_lan])

  result <- list(
    lan_val = lan_val,
    enskild_val = list(
      "Län"     = lan_val,
      "Kommun"  = kommun_val
    )
  )

  .geo_cache <<- result
  result
}

hamta_geografi_val <- function() {

  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

  geo_df <- hamtaregtab()

  rk <- as.character(geo_df$regionkod)
  rk_num <- suppressWarnings(as.integer(rk))
  rk <- ifelse(!is.na(rk_num) & rk_num < 100, sprintf("%02d", rk_num),
               ifelse(!is.na(rk_num) & rk_num < 10000, sprintf("%04d", rk_num), rk))
  geo_df$regionkod <- rk

  geo_df <- geo_df[geo_df$regionkod != "00", , drop = FALSE]

  is_lan <- nchar(geo_df$regionkod) == 2

  lan_val <- setNames(geo_df$regionkod[is_lan], geo_df$region[is_lan])
  kommun_val <- setNames(geo_df$regionkod[!is_lan], geo_df$region[!is_lan])

  list(
    lan_val = lan_val,
    enskild_val = list(
      "Län" = lan_val,
      "Kommun" = kommun_val
    )
  )
}

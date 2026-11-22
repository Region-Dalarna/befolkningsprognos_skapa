# _dependencies.R – läses av renv::dependencies(), körs aldrig
library(DBI)
library(sf)
library(RPostgres)
# ... lägg till alla paket appen använder, även de som laddas via source()
library(shiny.telemetry) # anvands av skapa_telemetry() i rdshinyappar - maste finnas i renv.lock
library(rdshinyappar)   # skapa_telemetry() och shiny_uppkoppling_las()

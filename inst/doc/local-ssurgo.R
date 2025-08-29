## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = isTRUE(try(local_NASIS_defined(), silent = TRUE)) ||
           !as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = "TRUE")),
  message = FALSE,
  warning = FALSE,
  fig.height = 7,
  fig.width = 7
)

## -----------------------------------------------------------------------------
# library(soilDB)
# 
# gpkg_dir <- tempdir()
# 
# AREASYMBOLS <- c("KS129", "KS187")
# 
# ssurgo_zip <- downloadSSURGO(
#   areasymbol = AREASYMBOLS,
#   destdir = gpkg_dir
# )

## ----warning=FALSE, message=FALSE---------------------------------------------
# # Create a local GeoPackage from the downloaded ZIP
# gpkg_path <- file.path(gpkg_dir, "ssurgo.gpkg")
# 
# createSSURGO(
#   gpkg_path,
#   exdir = gpkg_dir
# )

## -----------------------------------------------------------------------------
# library(DBI)
# library(RSQLite)
# 
# # Connect to the GeoPackage
# con <- dbConnect(SQLite(), gpkg_path)
# 
# # List available tables
# dbListTables(con)

## -----------------------------------------------------------------------------
# dbListFields(con, "mapunit")

## -----------------------------------------------------------------------------
# dbGetQuery(con, "SELECT * FROM mapunit LIMIT 5")

## -----------------------------------------------------------------------------
# query <- "
# SELECT mu.musym, mu.muname, c.compname, c.comppct_r
# FROM mapunit mu
# JOIN component c ON mu.mukey = c.mukey
# LIMIT 10
# "
# dbGetQuery(con, query)

## -----------------------------------------------------------------------------
# library(sf)
# 
# # Read spatial map units
# spatial_mu <- st_read(gpkg_path, layer = "mupolygon")
# 
# spatial_mu
# 
# # Plot the map units
# plot(st_geometry(spatial_mu))

## -----------------------------------------------------------------------------
# library(sf)
# 
# # Read spatial map units
# spatial_mu <- sf::st_read(gpkg_path, layer = "mupolygon")
# 
# # Read tabular data
# mapunit <- dbReadTable(con, "mapunit")
# component <- dbReadTable(con, "component")
# 
# # Disconnect when done
# dbDisconnect(con)

## -----------------------------------------------------------------------------
# # Get dominant component per map unit
# dominant_comp <- aggregate(
#   comppct_r ~ mukey,
#   data = component,
#   max
# )
# 
# # Join with spatial data
# spatial_mu$comppct_r <- dominant_comp$comppct_r[match(spatial_mu$mukey, dominant_comp$mukey)]
# 
# # Plot (small subset of extent)
# plot(
#   spatial_mu["comppct_r"],
#   main = "Dominant Component Percentage (comppct_r)",
#   breaks = seq(0, 100, 10),
#   key.pos = 4,
#   border = NA,
#   pal = hcl.colors(10)
# )

## -----------------------------------------------------------------------------
# # Get most common hydgrp per mukey
# hydgrp_tab <- aggregate(
#   hydgrp ~ mukey,
#   data = component,
#   function(x) names(sort(table(x), decreasing = TRUE))[1]
# )
# 
# # Convert to ordered factor
# hydgrp_tab[[2]] <- NASISChoiceList(hydgrp_tab)[[2]]
# 
# # Join with spatial data
# spatial_mu$hydgrp <- hydgrp_tab$hydgrp[match(spatial_mu$mukey, hydgrp_tab$mukey)]
# 
# spatial_mu
# 
# # Plot
# plot(
#   spatial_mu["hydgrp"],
#   main = "Hydrologic Group (hydgrp)",
#   key.pos = 4,
#   border = NA,
#   pal = rev(hcl.colors(7))
# )

## -----------------------------------------------------------------------------
# # Get most common drainage class per mukey
# drainage_tab <- aggregate(
#   drainagecl ~ mukey,
#   data = component,
#   function(x) names(sort(table(x), decreasing = TRUE))[1]
# )
# 
# # Convert to ordered factor
# drainage_tab[[2]] <- NASISChoiceList(drainage_tab)[[2]]
# 
# # Join with spatial data
# spatial_mu$drainagecl <- drainage_tab$drainagecl[match(spatial_mu$mukey, drainage_tab$mukey)]
# 
# # Plot
# plot(
#   spatial_mu["drainagecl"],
#   main = "Drainage Class (drainagecl)",
#   border = NA,
#   key.pos = 4,
#   pal = rev(hcl.colors(8))
# )


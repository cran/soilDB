## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = !as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = "TRUE")),
  message = FALSE,
  warning = FALSE,
  fig.height = 7,
  fig.width = 7
)

## ----download-ssurgo----------------------------------------------------------
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

## ----create-ssurgo, warning=FALSE, message=FALSE------------------------------
# # Create a local GeoPackage from the downloaded ZIP
# gpkg_path <- file.path(gpkg_dir, "ssurgo.gpkg")
# 
# createSSURGO(
#   gpkg_path,
#   exdir = gpkg_dir
# )

## ----connect-db---------------------------------------------------------------
# library(DBI)
# library(RSQLite)
# 
# # Connect to the GeoPackage
# con <- dbConnect(SQLite(), gpkg_path)
# 
# # List available tables
# dbListTables(con)

## ----list-fields--------------------------------------------------------------
# dbListFields(con, "mapunit")

## ----basic-query--------------------------------------------------------------
# dbGetQuery(con, "SELECT * FROM mapunit LIMIT 5")

## ----join-query---------------------------------------------------------------
# query <- "
# SELECT mu.musym, mu.muname, c.compname, c.comppct_r
# FROM mapunit mu
# JOIN component c ON mu.mukey = c.mukey
# LIMIT 10
# "
# dbGetQuery(con, query)

## ----sf-mupolygon-------------------------------------------------------------
# library(sf)
# 
# # Read spatial map units
# spatial_mu <- st_read(gpkg_path, layer = "mupolygon")
# 
# spatial_mu
# 
# # Plot the map units
# plot(st_geometry(spatial_mu))

## ----dbi-read-table-----------------------------------------------------------
# # Read tabular data
# mapunit <- dbReadTable(con, "mapunit")
# head(mapunit)
# 
# component <- dbReadTable(con, "component")
# head(component)
# 
# # Disconnect when done
# dbDisconnect(con)

## ----manual-dominant-component------------------------------------------------
# # Calculate dominant component per map unit
# dominant_comp <- aggregate(
#   comppct_r ~ mukey,
#   data = component,
#   max
# )
# head(dominant_comp)
# 
# # Match dominant component value for each mukey with mukey of spatial data
# spatial_mu$domcomppct_r <- dominant_comp$comppct_r[match(spatial_mu$mukey, dominant_comp$mukey)]

## ----plot-domcomppct----------------------------------------------------------
# # Visualize
# plot(
#   spatial_mu["domcomppct_r"],
#   main = "Dominant Component Percentage (domcomppct_r)",
#   breaks = seq(0, 100, 10),
#   key.pos = 4,
#   border = NA,
#   pal = hcl.colors(10)
# )

## ----get-sda-prop-hyd---------------------------------------------------------
# component_properties <- c("hydgrp", "drainagecl")
# 
# # Get most common hydgrp and drainagecl per mukey
# hyd_tab <- get_SDA_property(
#   component_properties,
#   method = "dominant condition",
#   dsn = gpkg_path,
#   WHERE = "1=1"
# )
# head(hyd_tab)

## ----nasis-choice-list--------------------------------------------------------
# # Convert to factor
# hyd_tab[component_properties] <- NASISChoiceList(hyd_tab[component_properties])
# 
# # Inspect result
# str(hyd_tab)

## ----join-example-------------------------------------------------------------
# # Join with spatial data
# spatial_mu <- merge(spatial_mu, hyd_tab)
# 
# # Inspect
# str(spatial_mu)

## ----plot-hsg-----------------------------------------------------------------
# plot(
#   spatial_mu["hydgrp"],
#   main = "Hydrologic Group (hydgrp)",
#   key.pos = 4,
#   border = NA,
#   pal = rev(hcl.colors(7))
# )

## ----plot-drainagecl----------------------------------------------------------
# plot(
#   spatial_mu["drainagecl"],
#   main = "Drainage Class (drainagecl)",
#   border = NA,
#   key.pos = 4,
#   pal = rev(hcl.colors(8))
# )


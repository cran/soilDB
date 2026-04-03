## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = isTRUE(try(local_NASIS_defined(), silent = TRUE)) ||
           !as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = "TRUE")),
  message = FALSE,
  warning = FALSE,
  fig.width = 7 
)

## ----libraries----------------------------------------------------------------
# library(soilDB)
# library(sf)

## ----SDA-spatialQuery---------------------------------------------------------
# aoi <- sf::st_as_sfc(sf::st_bbox(c(
#   xmin = -120.85,
#   xmax = -120.775,
#   ymin = 37.975,
#   ymax = 38.0
# ), crs = 4326))
# 
# # Query SDA for map unit polygons cropped to the AOI
# soil_polygons <- SDA_spatialQuery(
#   aoi,
#   what = "mupolygon"
# )
# 
# plot(
#   soil_polygons["mukey"],
#   main = "SSURGO Map Units from SDA_spatialQuery"
# )
# head(soil_polygons)

## ----SDA-spatialQuery-geomIntersect-------------------------------------------
# # Query SDA for map unit polygons cropped to the AOI
# soil_polygons <- SDA_spatialQuery(
#   aoi,
#   what = "mupolygon",
#   geomIntersection = TRUE
# )
# 
# plot(
#   soil_polygons["mukey"],
#   main = "Cropped SSURGO Map Units from SDA_spatialQuery"
# )
# head(soil_polygons)

## ----fetch-SDA-spatial--------------------------------------------------------
# mu_ssurgo <- fetchSDA_spatial(
#   unique(soil_polygons$mukey),
#   by.col = "mukey",
#   add.fields = c("legend.areaname",
#                  "mapunit.muname",
#                  "mapunit.farmlndcl")
# )
# 
# plot(mu_ssurgo["mukey"], main = "SSURGO Map Units from fetchSDA_spatial")
# head(mu_ssurgo)

## ----fetch-SDA-spatial-statsgo------------------------------------------------
# statsgo_polygons <- SDA_spatialQuery(
#   aoi,
#   what = "mupolygon",
#   db = "STATSGO",
#   geomIntersection = TRUE
# )
# 
# plot(statsgo_polygons["mukey"], main = "STATSGO Map Units from SDA_spatialQuery")
# head(statsgo_polygons)
# 
# mu_statsgo <- fetchSDA_spatial(
#   unique(statsgo_polygons$mukey),
#   by.col = "mukey",
#   db = "STATSGO",
#   add.fields = c("legend.areaname", "mapunit.muname", "mapunit.farmlndcl")
# )
# 
# plot(mu_statsgo["mukey"], main = "STATSGO Map Units from fetchSDA_spatial")
# head(mu_statsgo)

## ----fetch-SDA-spatial-ssa----------------------------------------------------
# ssas <- SDA_spatialQuery(aoi, what = "areasymbol")
# ssas
# 
# ssa <- fetchSDA_spatial(
#   ssas$areasymbol,
#   by.col = "areasymbol",
#   geom.src = "sapolygon",
#   add.fields = c("legend.areaname")
# )
# 
# plot(ssa["areasymbol"], main = "SSURGO Soil Survey Area from SDA")
# head(ssa)

## ----load-spatial, eval = FALSE-----------------------------------------------
# ssurgo_path <- "data/soilmu_a.gpkg"  # Replace with your actual path
# soil_polygons <- sf::st_read(ssurgo_path)
# head(soil_polygons)

## ----extract-mukey------------------------------------------------------------
# mukeys <- unique(soil_polygons$mukey)

## ----query-sda----------------------------------------------------------------
# eco_data <- get_SDA_coecoclass(mukeys = mukeys)
# head(eco_data)

## ----query-sda-domcomp--------------------------------------------------------
# eco_data_domcond <- get_SDA_coecoclass(
#   mukeys = mukeys,
#   method = "dominant component"
# )
# head(eco_data_domcond)

## ----query-sda-domcond--------------------------------------------------------
# eco_data_domcond <- get_SDA_coecoclass(
#   mukeys = mukeys,
#   method = "dominant condition"
# )
# head(eco_data_domcond)

## ----join-data----------------------------------------------------------------
# soil_polygons <- merge(
#   soil_polygons,
#   eco_data_domcond,
#   by = "mukey"
# )

## ----plot, fig.height = 4-----------------------------------------------------
# plot(
#   soil_polygons["ecoclassid"],
#   main = "Dominant Ecological Site by Map Unit"
# )

## ----export, eval=FALSE-------------------------------------------------------
# sf::st_write(
#   soil_polygons,
#   "ecosite_dominant.gpkg",
#   delete_dsn = TRUE
# )

## ----cleanup, include=FALSE---------------------------------------------------
# unlink("ecosite_dominant.gpkg")


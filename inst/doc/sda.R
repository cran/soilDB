## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = !as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = "TRUE")),
  message = FALSE,
  warning = FALSE,
  fig.height = 7,
  fig.width = 7
)

## ----load-packages------------------------------------------------------------
# library(soilDB)

## ----suggested-packages2, eval=FALSE------------------------------------------
# install.packages(c("sf", "terra"))

## ----basic-query-example------------------------------------------------------
# # Simple query to get the first 5 map units from an area
# q <- "
# SELECT TOP 5
#   mapunit.mukey,
#   mapunit.nationalmusym,
#   legend.areasymbol,
#   mapunit.musym,
#   mapunit.muname
# FROM legend
# INNER JOIN mapunit ON mapunit.lkey = legend.lkey
# WHERE legend.areasymbol = 'CA630'
# ORDER BY mapunit.musym
# "
# 
# result <- SDA_query(q)
# head(result)

## ----sda-query-error----------------------------------------------------------
# result <- SDA_query(q)
# 
# # check if the result is an error response
# if (!inherits(result, 'try-error')){
#   head(result)
# } else {
#   # stop with error message extracted from result
#   stop("Error occured! ", result[1])
# }

## ----sda-query-error-backoff--------------------------------------------------
# n_tries <- 1
# while (n_tries <= 3){
#   result <- SDA_query(q)
# 
#   # check if the result is an error response
#   if (!inherits(result, 'try-error')){
#     head(result)
# 
#     # if no error, break out of the loop
#     break
#   } else {
#     # on error, increment the number of tries
#     # and sleep (time in seconds)
#     Sys.sleep(exp(n_tries))
# 
#     n_tries <- n_tries + 1
#     if (n_tries > 3) {
#       stop("Error occured! ", result[1])
#     }
#   }
# }

## ----basic-join-example-------------------------------------------------------
# # Get map unit, component, and horizon data
# q <- "
# SELECT TOP 10
#   mu.mukey,
#   leg.areasymbol,
#   mu.musym,
#   co.compname,
#   co.comppct_r,
#   ch.hzname,
#   ch.hzdept_r,
#   ch.hzdepb_r,
#   ch.claytotal_r
# FROM legend AS leg
# INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
# INNER JOIN component AS co ON co.mukey = mu.mukey
# LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
# WHERE leg.areasymbol = 'CA630'
#   AND co.majcompflag = 'Yes'
#   AND ch.claytotal_r IS NOT NULL
# ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
# "
# 
# result <- SDA_query(q)
# head(result, 10)

## ----inner-join-complete-data-------------------------------------------------
# # These relationships are always present in SSURGO:
# # - mapunit always belongs to a legend (survey area)
# # - component always belongs to a mapunit
# q <- "
# SELECT TOP 10
#   leg.areasymbol,
#   mu.musym,
#   co.compname,
#   co.comppct_r
# FROM legend AS leg
# INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
# INNER JOIN component AS co ON co.mukey = mu.mukey
# WHERE leg.areasymbol = 'CA630'
# ORDER BY mu.musym, co.comppct_r DESC
# "
# 
# result <- SDA_query(q)
# 
# # Every row represents a valid component with all parent relationships
# head(result)

## ----left-join-components-without-horizons------------------------------------
# # Some components have NO horizons defined
# # INNER JOIN would exclude these components entirely
# # LEFT JOIN preserves components even without horizon data
# q <- "
# SELECT
#   leg.areasymbol,
#   mu.mukey,
#   mu.musym,
#   co.compname,
#   co.comppct_r,
#   ch.chkey,
#   ch.hzdept_r,
#   ch.hzdepb_r
# FROM legend AS leg
# INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
# INNER JOIN component AS co ON co.mukey = mu.mukey
# LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
# WHERE leg.areasymbol = 'CA630'
#   AND co.majcompflag = 'Yes'
# ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
# "
# 
# result <- SDA_query(q)
# 
# # Check for components without horizons:
# # Rows with NA in horizon columns indicate a component with no horizon data
# if (is.data.frame(result) && nrow(result) > 0) {
#   components_no_horizons <- result[is.na(result$chkey), ]
#   if (nrow(components_no_horizons) > 0) {
#     cat('Found', nrow(components_no_horizons), 'components without horizon data:\n')
#     print(components_no_horizons[, c('musym', 'compname', 'comppct_r')])
#   }
# }

## ----compare-inner-join-drops-components--------------------------------------
# # Using INNER JOIN on horizons drops components that lack horizon data
# q_inner <- "
# SELECT
#   leg.areasymbol,
#   mu.mukey,
#   mu.musym,
#   co.compname,
#   co.comppct_r,
#   ch.desgnmaster,
#   ch.hzdept_r,
#   ch.hzdepb_r
# FROM legend AS leg
# INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
# INNER JOIN component AS co ON co.mukey = mu.mukey
# INNER JOIN chorizon AS ch ON ch.cokey = co.cokey
# WHERE leg.areasymbol = 'CA630'
#   AND co.majcompflag = 'Yes'
# ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
# "
# 
# result_inner <- SDA_query(q_inner)
# 
# # number of rows in LEFT JOIN result
# nrow(result)
# 
# # number of rows in INNER JOIN result
# nrow(result_inner)

## ----left-join-horizons-without-fragments-------------------------------------
# # Some horizons have NO rock fragment data recorded
# # LEFT JOIN preserves horizons even without fragment information
# q <- "
# SELECT
#   mu.musym,
#   co.compname,
#   ch.desgnmaster,
#   ch.hzdept_r,
#   ch.hzdepb_r,
#   rf.fragsize_h,
#   rf.fragvol_r
# FROM legend AS leg
# INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
# INNER JOIN component AS co ON co.mukey = mu.mukey
# LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
# LEFT JOIN chfrags AS rf ON rf.chkey = ch.chkey
# WHERE leg.areasymbol = 'CA630'
#   AND co.majcompflag = 'Yes'
# ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
# "
# 
# result <- SDA_query(q)
# 
# # Check for horizons without fragments:
# if (is.data.frame(result) && nrow(result) > 0) {
#   horizons_no_frags <- result[!duplicated(result$chkey) & is.na(result$fragsize_h), ]
#   if (nrow(horizons_no_frags) > 0) {
#     cat('Found', nrow(horizons_no_frags), 'horizons without rock fragment data:\n')
#     print(horizons_no_frags[, c('musym', 'compname', 'desgnmaster', 'fragvol_r')])
#   }
# }

## ----aggregation-example------------------------------------------------------
# # Get component statistics per map unit
# q <- "
# SELECT
#   mu.mukey,
#   mu.musym,
#   mu.muname,
#   COUNT(co.cokey) AS n_components,
#   SUM(co.comppct_r) AS total_comppct,
#   AVG(co.comppct_r) AS avg_comppct
# FROM legend AS leg
# INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
# INNER JOIN component AS co ON co.mukey = mu.mukey
# WHERE leg.areasymbol = 'CA630'
# GROUP BY mu.mukey, mu.musym, mu.muname
# ORDER BY mu.musym
# "
# 
# result <- SDA_query(q)
# head(result)

## ----inspect-horizon-data-----------------------------------------------------
# q <- "
# SELECT TOP 10
#   component.compname,
#   ch.hzname,
#   ch.hzdept_r,
#   ch.hzdepb_r,
#   ch.texturerv,
#   ch.horztype,
#   ch.claytotal_r,
#   ch.om_r
# FROM chorizon AS ch
# INNER JOIN component ON component.cokey = ch.cokey AND taxsubgrp LIKE '%Histic%'
# ORDER BY component.cokey, hzdept_r
# "
# 
# result <- SDA_query(q)
# result

## ----filtering-horizon-data---------------------------------------------------
# q <- "
# SELECT TOP 10
#   ch.hzname,
#   ch.hzdept_r,
#   ch.hzdepb_r,
#   ch.texturerv,
#   ch.horztype,
#   ch.claytotal_r,
#   ch.om_r
# FROM chorizon AS ch
# WHERE
#   -- 1. Exclude Bedrock
#   ch.texturerv NOT IN ('BR', 'WB', 'UWB')
# 
#   -- 2. Exclude Dry Organic / Duff
#   AND ISNULL(ch.horztype, '') != 'Dry Organic'
#   AND ch.texturerv NOT IN ('SPM', 'MPM', 'HPM')
# ORDER BY cokey, hzdept_r
# "
# 
# result <- SDA_query(q)
# head(result)

## ----organic-kfact------------------------------------------------------------
# q <- "
# SELECT
#   COUNT(chkey) AS n,
#   kwfact,
#   kffact
# FROM chorizon
# WHERE desgnmaster = 'O' AND hzdept_r = 0
# GROUP BY kwfact, kffact
# ORDER BY n DESC
# "
# result <- SDA_query(q)
# result$percent <- round(prop.table(result$n) * 100, 1)
# head(result, 10)

## ----rock-fragment-basic------------------------------------------------------
# # Query horizons with their rock fragment content
# q <- "
# SELECT TOP 20
#   mu.musym,
#   co.compname,
#   ch.hzname,
#   ch.hzdept_r,
#   ch.hzdepb_r,
#   rf.fragkind,
#   rf.fragsize_h,
#   rf.fragvol_r
# FROM legend AS leg
# INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
# INNER JOIN component AS co ON co.mukey = mu.mukey
# LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
# LEFT JOIN chfrags AS rf ON rf.chkey = ch.chkey
# WHERE leg.areasymbol = 'CA630'
#   AND co.majcompflag = 'Yes'
# ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r, rf.fragsize_h
# "
# 
# result <- SDA_query(q)
# head(result, 20)

## ----rock-fragment-aggregation------------------------------------------------
# # Total rock fragment content per horizon
# q <- "
# SELECT
#   mu.musym,
#   co.compname,
#   co.comppct_r,
#   ch.hzname,
#   ch.hzdept_r,
#   ch.hzdepb_r,
#   SUM(rf.fragvol_r) AS calc_fragvoltot_r,
#   COUNT(DISTINCT rf.fragsize_h) AS n_frag_classes
# FROM legend AS leg
# INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
# INNER JOIN component AS co ON co.mukey = mu.mukey
# LEFT JOIN chorizon AS ch ON ch.cokey = co.cokey
# LEFT JOIN chfrags AS rf ON rf.chkey = ch.chkey
# WHERE leg.areasymbol = 'CA630'
#   AND co.majcompflag = 'Yes'
# GROUP BY mu.musym, co.compname, co.comppct_r, ch.hzname,
#          ch.hzdept_r, ch.hzdepb_r, ch.chkey
# ORDER BY mu.musym, co.comppct_r DESC, ch.hzdept_r
# "
# 
# result <- SDA_query(q)
# head(result)

## ----rock-fragment-via-property-----------------------------------------------
# # Get rock fragment weight percentages (3-10 inch class)
# frag_3_10 <- get_SDA_property(
#   property = "Rock Fragments 3 - 10 inches - Rep Value",  # equivalent to 'frag3to10_r'
#   method = "Dominant Component (Numeric)", # dominant component, weighted average by depth
#   areasymbols = "CA630",
#   top_depth = 0,
#   bottom_depth = 100  # depth range for averaging
# )
# 
# head(frag_3_10)
# 
# # get fragment volume (fragvoltot_l, fragvoltot_r, fragvoltot_h)
# fragvol_total <- get_SDA_property(
#   property = "fragvoltot_r",
#   method = "Weighted Average", # weighted by component percentage and depth
#   top_depth = 0,
#   bottom_depth = 100,
#   areasymbols = "CA630"
# )
# 
# head(fragvol_total)

## ----chunking-example---------------------------------------------------------
# large_mukey_list <- 461994:466995
# 
# # Divide into chunks of 1000
# chunks <- soilDB::makeChunks(large_mukey_list, 1000)
# 
# # Now loop through chunks
# results_list <- lapply(split(large_mukey_list, chunks), function(chunk_keys) {
# 
#   # Build IN clause
#   in_clause <- soilDB::format_SQL_in_statement(chunk_keys)
# 
#   # construct query
#   q <- sprintf(
#     "SELECT mukey, musym, muname FROM mapunit WHERE mukey IN %s", in_clause
#   )
#     cat(sprintf("Chunk mukey %d to %d\n", min(chunk_keys), max(chunk_keys)))
#   SDA_query(q)
# })
# 
# # Combine results
# all_results <- do.call(rbind, results_list)

## ----chunked-property-example-------------------------------------------------
# test_areasymbols <- c("CA067", "CA077", "CA632", "CA644", "CA630", "CA649")
# 
# # Chunk into groups of 5
# chunks <- soilDB::makeChunks(test_areasymbols, 2)
# 
# results_list <- lapply(split(test_areasymbols, chunks), function(chunk_keys) {
# 
#   # Build IN clause
#   in_clause <- soilDB::format_SQL_in_statement(chunk_keys)
# 
#   q <- sprintf("
#     SELECT
#       mu.mukey,
#       mu.musym,
#       mu.muname,
#       co.cokey,
#       co.compname,
#       SUM(hzdepb_r - hzdept_r) AS sum_hz_thickness
#     FROM legend AS leg
#     INNER JOIN mapunit AS mu ON mu.lkey = leg.lkey
#     INNER JOIN component AS co ON co.mukey = mu.mukey
#     INNER JOIN chorizon AS ch ON ch.cokey = co.cokey
#     WHERE leg.areasymbol IN %s
#     GROUP BY mu.mukey, mu.musym, mu.muname, co.cokey, co.compname
#   ", in_clause)
# 
#   cat(sprintf("Chunk %s completed\n", paste0(chunk_keys, collapse = ", ")))
#   SDA_query(q)
# })
# 
# all_results <- do.call(rbind, results_list)
# head(all_results)

## ----spatial-wkt-example------------------------------------------------------
# library(sf)
# 
# # Define a bounding box for a region of interest
# # (xmin, ymin, xmax, ymax)
# bbox <- c(-120.9, 37.7, -120.8, 37.8)
# bbox_sf <- st_as_sfc(st_bbox(c(
#   xmin = bbox[1],
#   ymin = bbox[2],
#   xmax = bbox[3],
#   ymax = bbox[4]
# ), crs = 4326))
# 
# wkt <- st_as_text(bbox_sf)
# 
# wkt

## ----high-level-spatial-example1----------------------------------------------
# # Example: Retrieve map unit polygons that intersect a bounding box
# # This handles the WKT conversion and query construction automatically
# # geomIntersection = TRUE clips the polygons to the bounding box
# 
# polygons <- SDA_spatialQuery(bbox_sf, what = "mupolygon", geomIntersection = TRUE)
# polygons

## ----high-level-spatial-example2----------------------------------------------
# # Example: get some legend and mapunit-level tabular data at a point
# point_sf <- sf::st_as_sf(data.frame(wkt = "POINT (-120.85 37.75)"),
#                          wkt = "wkt",
#                          crs = "EPSG:4236")
# ssa <- SDA_spatialQuery(
#   point_sf,
#   what = "areasymbol"
# )
# ssa
# 
# mu <- SDA_spatialQuery(
#   point_sf,
#   what = "mukey"
# )
# mu

## ----spatial-sda-query-example------------------------------------------------
# # Step 1: Get the mukeys that intersect the bounding box
# q <- sprintf("
#   SELECT DISTINCT mu.mukey
#   FROM mapunit AS mu
#   INNER JOIN mupolygon AS mp ON mp.mukey = mu.mukey
#   WHERE mp.mupolygongeo.STIntersects(geometry::STGeomFromText('%s', 4326)) = 1
# ", wkt)
# 
# spatial_result <- SDA_query(q)
# head(spatial_result, 5)

## ----sda-helper-function------------------------------------------------------
# # Input MUST be WKT in WGS84 (EPSG:4326)
# q <- "SELECT * FROM SDA_Get_Mukey_from_intersection_with_WktWgs84('POINT(-120.9 37.7)')"
# result <- SDA_query(q)

## ----sda-helper-function-geometry---------------------------------------------
# # Step 2: Get Geometry from Map Unit Key (mukey)
# # Useful for retrieving the polygon boundary for a specific map unit
# target_mukey <- 461994
# 
# q <- sprintf("SELECT * FROM SDA_Get_MupolygonWktWgs84_from_Mukey('%s')", target_mukey)
# 
# # Result contains the mukey and the geometry in WKT format
# geometry_df <- SDA_query(q)
# 
# head(geometry_df)

## -----------------------------------------------------------------------------
# if (requireNamespace("sf", quietly = TRUE) &&
#    !is.null(geometry_df)) {
#   # Parse WKT column (usually named 'mupolygongeo' in mupolygon table, but 'MupolygonWktWgs84' in TVF result)
#   poly_sf <- sf::st_as_sfc(geometry_df$MupolygonWktWgs84, crs = 4326)
# 
#   plot(poly_sf, main = paste("Geometry for mukey=", target_mukey))
# }

## ----spatial-property-integration---------------------------------------------
# # Step 2: Use the mukeys to fetch tabular data
# # First, get mukeys in bounding box
# spatial_mukeys <- unique(spatial_result$mukey)
# 
# # Then query properties for those mukeys
# if (length(spatial_mukeys) > 0) {
#   clay_in_bbox <- get_SDA_property(
#     property = "Total Clay - Rep Value",
#     method = "Weighted Average",
#     mukeys = spatial_mukeys,
#     top_depth = 0,
#     bottom_depth = 50
#   )
# 
#   head(clay_in_bbox)
# }

## ----get_sda_property_category------------------------------------------------
# # Get taxonomic classification from dominant component
# tax_order <- get_SDA_property(
#   property = "Taxonomic Suborder",
#   method = "Dominant Component (Category)",
#   areasymbols = "CA630"
# )
# 
# head(tax_order)

## ----get_sda_property_numeric-------------------------------------------------
# # Get bulk density at 0-25 cm depth
# bulk_density <- get_SDA_property(
#   property = c("dbthirdbar_l", "dbthirdbar_r", "dbthirdbar_h"),
#   method = "Dominant Component (Numeric)",
#   areasymbols = "CA630",
#   top_depth = 0,
#   bottom_depth = 25
# )
# 
# head(bulk_density)

## ----get_sda_property_weighted------------------------------------------------
# # Get weighted average clay content (0-50 cm)
# clay_avg <- get_SDA_property(
#   property = "Total Clay - Rep Value",
#   method = "Weighted Average",
#   areasymbols = "CA630",
#   top_depth = 0,
#   bottom_depth = 50,
#   include_minors = FALSE
# )
# 
# head(clay_avg)

## ----get_sda_property_minmax--------------------------------------------------
# # Get maximum sand content in any component
# sand_max <- get_SDA_property(
#   property = "Total Sand - Rep Value",
#   method = "Min/Max",
#   areasymbols = "CA630",
#   FUN = "MAX",
#   top_depth = 0,
#   bottom_depth = 50
# )
# 
# head(sand_max)

## ----get_sda_property_dominant_condition--------------------------------------
# # Get dominant drainage class condition
# drain_dominant <- get_SDA_property(
#   property = "Drainage Class",
#   method = "Dominant Condition",
#   areasymbols = "CA630"
# )
# 
# head(drain_dominant)

## ----get_sda_property_none----------------------------------------------------
# # Get all pH values by component and horizon
# ph_all <- get_SDA_property(
#   property = "pH 1:1 water - Rep Value",
#   method = "None",
#   areasymbols = "CA630"
# )
# 
# head(ph_all)

## ----get_sda_property_query_string--------------------------------------------
# q <- get_SDA_property(
#   property = "Taxonomic Suborder",
#   method = "Dominant Component (Category)",
#   areasymbols = "CA630",
#   query_string = TRUE
# )
# 
# # View first 500 characters of query
# cat(substr(q, 1, 500), "...\n")

## ----get_sda_interpretation_dominant_component--------------------------------
# # Get forestry ratings for dominant component
# for_ratings <- get_SDA_interpretation(
#   rulename = c("FOR - Potential Seedling Mortality",
#                "FOR - Road Suitability (Natural Surface)"),
#   method = "Dominant Component",
#   areasymbols = "CA630"
# )
# 
# head(for_ratings)

## ----get_sda_interpretation_dominant_condition--------------------------------
# # Get dominant engineering interpretation
# eng_ratings <- get_SDA_interpretation(
#   rulename = "ENG - Septic Tank Absorption Fields",
#   method = "Dominant Condition",
#   areasymbols = "CA649"
# )
# 
# head(eng_ratings)

## ----get_sda_interpretation_weighted_average----------------------------------
# # Get weighted average agricultural rating
# agr_weighted <- get_SDA_interpretation(
#   rulename = "AGR - Winter Wheat Yield (MT)",
#   method = "Weighted Average",
#   areasymbols = "MT041",
#   include_minors = TRUE
# )
# 
# head(agr_weighted)

## ----get_sda_interpretation_none----------------------------------------------
# # Get all component-level ratings
# all_ratings <- get_SDA_interpretation(
#   rulename = "FOR - Mechanical Planting Suitability",
#   method = "None",
#   areasymbols = "CA630"
# )
# 
# head(all_ratings)

## ----get_sda_interpretation_wide_reason---------------------------------------
# # Get detailed ratings with reasons pivoted
# detailed <- get_SDA_interpretation(
#   rulename = "FOR - Mechanical Planting Suitability",
#   method = "Dominant Component",
#   areasymbols = "CA630",
#   wide_reason = TRUE
# )
# 
# head(detailed)

## ----get_sda_hydric_mapunit---------------------------------------------------
# hydric_class <- get_SDA_hydric(
#   areasymbols = c("CA077", "CA630"),
#   method = "MAPUNIT"
# )
# 
# head(hydric_class)
# 
# # Check unique ratings
# unique(hydric_class$HYDRIC_RATING)

## ----get_sda_hydric_dominant_component----------------------------------------
# hydric_dom <- get_SDA_hydric(
#   areasymbols = "CA630",
#   method = "DOMINANT COMPONENT"
# )
# 
# head(hydric_dom)

## ----get_sda_hydric_dominant_condition----------------------------------------
# hydric_cond <- get_SDA_hydric(
#   mukeys = c(461994, 461995, 462205),
#   method = "DOMINANT CONDITION"
# )
# 
# head(hydric_cond)

## ----get_sda_hydric_none------------------------------------------------------
# hydric_all <- get_SDA_hydric(
#   areasymbols = "CA630",
#   method = "NONE"
# )
# 
# head(hydric_all)

## ----get_sda_muaggatt---------------------------------------------------------
# muagg <- get_SDA_muaggatt(
#   areasymbols = "CA630"
# )
# 
# head(muagg)
# str(muagg)

## ----get_sda_muaggatt_filter--------------------------------------------------
# # Get aggregate attributes for specific mukeys
# muagg_filtered <- get_SDA_muaggatt(
#   mukeys = c(461994, 461995, 463264)
# )
# 
# head(muagg_filtered)

## ----get_sda_pmgroupname_dominant_component-----------------------------------
# pm_dom <- get_SDA_pmgroupname(
#   areasymbols = "CA630",
#   method = "DOMINANT COMPONENT",
#   simplify = FALSE
# )
# 
# head(pm_dom)

## ----get_sda_pmgroupname_dominant_condition-----------------------------------
# pm_cond <- get_SDA_pmgroupname(
#   mukeys = c(461994, 461995, 462205),
#   method = "DOMINANT CONDITION"
# )
# 
# head(pm_cond)

## ----get_sda_pmgroupname_simplify---------------------------------------------
# # Simplified (broader groups)
# pm_simple <- get_SDA_pmgroupname(
#   mukeys = c(461994, 461995),
#   simplify = TRUE
# )
# 
# head(pm_simple)
# 
# # Detailed (specific groups)
# pm_detailed <- get_SDA_pmgroupname(
#   mukeys = c(461994, 461995),
#   simplify = FALSE
# )
# 
# head(pm_detailed)

## ----get_sda_coecoclass_none--------------------------------------------------
# eco_none <- get_SDA_coecoclass(
#   method = "None",
#   areasymbols = "CA630"
# )
# 
# head(eco_none)

## ----get_sda_coecoclass_dominant_component------------------------------------
# eco_dom <- get_SDA_coecoclass(
#   method = "Dominant Component",
#   areasymbols = "CA630"
# )
# 
# head(eco_dom)

## ----get_sda_coecoclass_dominant_condition------------------------------------
# eco_cond <- get_SDA_coecoclass(
#   method = "Dominant Condition",
#   mukeys = c(461994, 461995, 462205),
#   ecoclasstypename = "NRCS Forestland Site"
# )
# 
# head(eco_cond)

## ----get_sda_coecoclass_all---------------------------------------------------
# eco_all <- get_SDA_coecoclass(
#   method = "All",
#   mukeys = c(461994, 461995),
#   threshold = 5
# )
# 
# head(eco_all)

## ----get_sda_coecoclass_ecoclasstypename--------------------------------------
# # Get rangeland sites only
# eco_range <- get_SDA_coecoclass(
#   method = "Dominant Condition",
#   areasymbols = "CA630",
#   ecoclasstypename = "NRCS Rangeland Site"
# )
# 
# head(eco_range)

## ----get_sda_cosurfmorph_3d---------------------------------------------------
# # Get geomorphic position by component name
# geom_3d <- get_SDA_cosurfmorph(
#   table = "cosurfmorphgc",
#   areasymbols = "CA630"
# )
# 
# head(geom_3d)

## ----get_sda_cosurfmorph_hillslope--------------------------------------------
# # Get hillslope position aggregated by area symbol
# geom_hill <- get_SDA_cosurfmorph(
#   table = "cosurfmorphhpp",
#   by = "areasymbol",
#   areasymbols = "CA630"
# )
# 
# head(geom_hill)

## ----get_sda_cosurfmorph_surface_shape----------------------------------------
# # Get surface shape classes
# geom_shape <- get_SDA_cosurfmorph(
#   table = "cosurfmorphss",
#   areasymbols = "CA649"
# )
# 
# head(geom_shape)

## ----get_sda_cosurfmorph_microrelief------------------------------------------
# # Get microrelief by component name
# geom_micro <- get_SDA_cosurfmorph(
#   table = "cosurfmorphmr",
#   areasymbols = "CA630"
# )
# 
# head(geom_micro)

## ----fetchsda-example---------------------------------------------------------
# library(aqp)
# 
# # Query soil components by areasymbol and musym
# s <- fetchSDA(WHERE = "areasymbol = 'IN005' AND musym = 'MnpB2'")
# 
# # The result is a SoilProfileCollection
# s
# 
# # Check the horizon data
# head(horizons(s))

## ----fetchldm-example-area----------------------------------------------------
# # Fetch KSSL data for a specific soil survey area (CA630)
# # 'what' argument specifies we are searching by 'area_code'
# # 'tables' argument specifies which data tables to include (defaults to core tables)
# ldm_data <- fetchLDM("CA630", what = "area_code")
# 
# # The result is a SoilProfileCollection
# ldm_data
# 
# # Inspect site data
# head(site(ldm_data))

## ----fetchldm-example-taxon---------------------------------------------------
# # Fetch lab data where the correlated or sampled name is 'Musick' or 'Holland'
# # CASE statement handles differences between correlated and sampled names
# ldm_taxon <- fetchLDM(WHERE = "(CASE WHEN corr_name IS NOT NULL
#                                 THEN LOWER(corr_name)
#                                 ELSE LOWER(samp_name)
#                             END) IN ('musick', 'holland')")
# 
# ldm_taxon

## ----fetchldm-example-tables--------------------------------------------------
# # Fetch physical properties for soils correlated as "Typic Argialbolls"
# ldm_phys <- fetchLDM(x = "Typic Argialbolls",
#                      what = "corr_taxsubgrp",
#                      tables = "lab_physical_properties")
# 
# # Inspect the available horizon data columns
# names(horizons(ldm_phys))

## ----integration-example-1----------------------------------------------------
# # Get clay content using three different methods
# methods <- c("Dominant Component (Numeric)", "Weighted Average", "Max")
# 
# clay_results <- data.frame()
# 
# for (method in methods) {
#   result <- get_SDA_property(
#     property = "Total Clay - Rep Value",
#     method = method,
#     areasymbols = "CA630",
#     top_depth = 0,
#     bottom_depth = 50
#   )
# 
#   result$method <- method
#   clay_results <- rbind(clay_results, result)
# }
# 
# # compare methods for a single map unit
# subset(clay_results, mukey == 1865918)

## ----integration-example-2----------------------------------------------------
# # Get drainage class and hydrologic group
# drain_hydro <- get_SDA_property(
#   property = c("Drainage Class", "Hydrologic Group"),
#   method = "Dominant Condition",
#   areasymbols = "CA630"
# )
# 
# # Get an engineering interpretation
# eng_interp <- get_SDA_interpretation(
#   rulename = "ENG - Septic Tank Absorption Fields",
#   method = "Dominant Condition",
#   areasymbols = "CA630"
# )
# 
# # Explicitly merge on mukey (and other shared columns to avoid duplication)
# suitability <- merge(drain_hydro, eng_interp,
#                      by = c("mukey", "areasymbol", "musym", "muname"))
# 
# head(suitability)

## ----integration-example-3----------------------------------------------------
# # Get a generalized mapunit-level hydric classification
# # see ?get_SDA_hydric for details on method="mapunit"
# hydric_stat <- get_SDA_hydric(
#   areasymbols = "CA077",
#   method = "MAPUNIT"
# )
# 
# wet_interp <- get_SDA_interpretation(
#   rulename = "WMS - Excavated Ponds (Aquifer-fed)",
#   method = "Dominant Condition",
#   areasymbols = "CA077"
# )
# 
# wetland_assess <- merge(hydric_stat, wet_interp,
#                         by = c("mukey", "areasymbol", "musym", "muname"),
#                         all.x = TRUE)
# 
# subset(wetland_assess, rating_WMSExcavatedPondsAquiferfed < 0.6)

## ----integration-example-4----------------------------------------------------
# # Get properties for two areas
# props_ca630 <- get_SDA_property(
#   property = "Total Clay - Rep Value",
#   method = "Dominant Component (Numeric)",
#   areasymbols = "CA630"
# )
# 
# props_ca649 <- get_SDA_property(
#   property = "Total Clay - Rep Value",
#   method = "Dominant Component (Numeric)",
#   areasymbols = "CA649"
# )
# 
# # Combine
# all_props <- rbind(props_ca630, props_ca649)
# 
# # Aggregate by area symbol
# area_summary <- aggregate(
#   claytotal_r ~ areasymbol,
#   data = all_props,
#   FUN = function(x) {
#     c(
#       mean = mean(x, na.rm = TRUE),
#       median = median(x, na.rm = TRUE),
#       sd = sd(x, na.rm = TRUE),
#       n = sum(!is.na(x))
#     )
#   }
# )
# 
# area_summary

## ----integration-example-5----------------------------------------------------
# # Get all component properties without aggregation
# clay_by_comp <- get_SDA_property(
#   property = "Total Clay - Rep Value",
#   method = "None",
#   areasymbols = "CA630",
#   top_depth = 0,
#   bottom_depth = 25,
#   include_minors = TRUE
# )
# 
# head(clay_by_comp)
# 
# # Calculate average clay by major vs. minor components
# clay_summary <- aggregate(
#   claytotal_r ~ majcompflag,
#   data = clay_by_comp,
#   FUN = mean
# )
# 
# clay_summary


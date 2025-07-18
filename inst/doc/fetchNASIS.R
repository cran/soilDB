## ----include = FALSE----------------------------------------------------------
soilDBdata_pedon_dsn <- system.file("extdata/fetchNASIS_pedons.sqlite", package = "soilDBdata")

calcdef <- structure(list(fetchNASIS_Column = c("hzID", "texture_class", "d_r", "d_g", "d_b", 
"d_hue", "d_value", "d_chroma", "d_sigma", "m_r", "m_g", "m_b", 
"m_hue", "m_value", "m_chroma", "m_sigma", "moist_soil_color", 
"dry_soil_color", "soil_color", "fine_gravel", "gravel", "cobbles", 
"stones", "boulders", "channers", "flagstones", "parafine_gravel", 
"paragravel", "paracobbles", "parastones", "paraboulders", "parachanners", 
"paraflagstones", "unspecified", "total_frags_pct_nopf", "total_frags_pct", 
"art_fgr", "art_gr", "art_cb", "art_st", "art_by", "art_ch", 
"art_fl", "art_unspecified", "total_art_pct", "huartvol_cohesive", 
"huartvol_penetrable", "huartvol_innocuous", "huartvol_persistent", 
"surface_fine_gravel", "surface_fgravel", "surface_gravel", "surface_cobbles", 
"surface_stones", "surface_boulders", "surface_channers", "surface_flagstones", 
"surface_parafine_gravel", "surface_paragravel", "surface_paracobbles", 
"surface_parastones", "surface_paraboulders", "surface_parachanners", 
"surface_paraflagstones", "surface_unspecified", "surface_total_frags_pct_nopf", 
"surface_total_frags_pct", "slope_shape", "landform_string", 
"landscape_string", "microfeature_string", "geomicrorelief_string", 
"selection_method", "es_selection_method", "ochric.epipedon", 
"cambic.horizon", "lithic.contact", "mollic.epipedon", "argillic.horizon", 
"redox.concentrations", "andic.soil.properties", "secondary.carbonates", 
"sapric.soil.materials", "aquic.conditions", "reduced.matrix", 
"albic.horizon", "spodic.horizon", "glossic.horizon", "spodic.materials", 
"lithologic.discontinuity", "densic.materials", "umbric.epipedon", 
"albic.materials"), ColumnDescription = c("Internal horizon row ID (unique) calculated in aqp SoilProfileCollection objects", "Texture class of fine-earth fraction (`phtexture.texcl`). If missing, then the in lieu class is used (`phtexture.lieutex`).",
"Dry Color R", "Dry Color G", "Dry Color B", "Dry Color Hue", 
"Dry Color Value", "Dry Color Chroma", "Dry Color Sigma", "Moist Color R", 
"Moist Color G", "Moist Color B", "Moist Color Hue", "Moist Color Value", 
"Moist Color Chroma", "Moist Color Sigma", "Moist Munsell Soil Color", 
"Dry Munsell Soil Color", "Default Munsell Soil Color; Moist by default, based on `fetchNASIS(soilColorState=\"moist\")`", 
"Rock fragment volume (%) in 2 - 5 mm size range", "Rock fragment volume (%) in 2 - 75 mm size range (includes fine_gravel)", 
"Rock fragment volume (%) in the 75 - 250 mm size range", "Rock fragment volume (%) in the 250 - 600 mm size range", 
"Rock fragment volume (%) in the >600mm size range", "Flat rock fragment volume (%) in the 2 - 150 mm size range", 
"Flat rock fragment volume (%) in the >150 mm size range", "Pararock fragment volume (%) in 2 - 5 mm size range", 
"Pararock fragment volume (%) in 2 - 75 mm size range (includes parafine_gravel)", 
"Pararock fragment volume (%) in the 75 - 250 mm size range", 
"Pararock fragment volume (%) in the 250 - 600 mm size range", 
"Pararock fragment volume (%) in the >600mm size range", "Flat pararock fragment volume (%) in the 2 - 150 mm size range", 
"Flat pararock fragment volume (%) in the >150 mm size range", 
"Rock fragment volume (%) with unspecified/incomplete fragsize ", 
"Total rock fragment volume (%), excluding parafragments", "Total rock fragment volume (%), including parafragments", 
"Artifact volume (%) in 2 - 5 mm size range", "Artifact volume (%) in 2 - 75 mm size range (includes fine_gravel)", 
"Artifact volume (%) in the 75 - 250 mm size range", "Artifact volume (%) in the 250 - 600 mm size range", 
"Artifact volume (%) in the >600mm size range", "Flat artifact volume (%) in the 2 - 150 mm size range", 
"Flat artifact volume (%) in the >150 mm size range", "Artifact volume (%) with unspecified/incomplete size specification", 
"Total artifact volume (%)", "Cohesive artifact volume (%)", 
"Penetrable artifact volume (%)", "Innocuous artifact volume (%)", 
"Persistent artifact volume (%)", "Surface rock fragment volume (%) in 2 - 5 mm size range", 
"Deprecated alias of surface_fine_gravel", "Surface rock fragment volume (%) in 2 - 75 mm size range (includes fine_gravel)", 
"Surface rock fragment volume (%) in the 75 - 250 mm size range", 
"Surface rock fragment volume (%) in the 250 - 600 mm size range", 
"Surface rock fragment volume (%) in the >600mm size range", 
"Surface flat rock fragment volume (%) in the 2 - 150 mm size range", 
"Surface flat rock fragment volume (%) in the >150 mm size range", 
"Surface pararock fragment volume (%) in 2 - 5 mm size range", 
"Surface pararock fragment volume (%) in 2 - 75 mm size range (includes parafine_gravel)", 
"Surface pararock fragment volume (%) in the 75 - 250 mm size range", 
"Surface pararock fragment volume (%) in the 250 - 600 mm size range", 
"Surface pararock fragment volume (%) in the >600mm size range", 
"Surface flat pararock fragment volume (%) in the 2 - 150 mm size range", 
"Surface flat pararock fragment volume (%) in the >150 mm size range", 
"Surface fragment volume (%) with unspecified/incomplete size specification", 
"Total surface fragment volume (%), excluding parafragments", 
"Total surface fragment volume (%), including parafragments", 
"Calculated factor where levels are concatenation of site.shapeacross + \" / \" + site.shapedown", 
"Concatenated landform string via `soilDB:::.formatLandformString()`", 
"Concatenated landscape string via `soilDB:::.formatLandformString()`", 
"Concatenated microfeature string via `soilDB:::.formatLandformString()`", 
"Concatenated microrelief string via `soilDB:::.formatLandformString()`", 
"Pedon Taxonomic History record selection method from `soilDB:::.pickBestTaxHistory()`", 
"Ecological Site History record selection method from `soilDB:::.pickBestEcosite()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`", 
"Presence/absence. Calculated from Pedon Diagnostic Horizon Table with `soilDB:::.diagHzLongtoWide()`"
), RelatedFunction = c("`get_hz_data_from_NASIS_db()`", "`get_hz_data_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_colors_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_colors_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_colors_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_colors_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_colors_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_colors_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_colors_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_colors_from_NASIS_db()`", "`get_colors_from_NASIS_db()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyFragmentData()`", "`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", "`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", "`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", "`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", "`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", "`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", 
"`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", "`get_hz_data_from_NASIS_db()`, `simplifyArtifactData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`, `simplifyFragmentData()`", 
"`get_site_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`", 
"`get_extended_data_from_NASIS_db()`", "`get_extended_data_from_NASIS_db()`"
), Deprecated = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
FALSE, FALSE, FALSE, FALSE)), row.names = c(NA, -93L), class = "data.frame")

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = FALSE,
  eval = (nchar(soilDBdata_pedon_dsn) > 0) && file.exists(soilDBdata_pedon_dsn) && requireNamespace("RSQLite", quietly = TRUE) && requireNamespace("aqp", quietly = TRUE)
)

## ----setup, include = FALSE---------------------------------------------------
# library(soilDB)
# 
# # get fetchNASIS result and categorize columns by table source
# f <- fetchNASIS(dsn = soilDBdata_pedon_dsn)
# x <- get_NASIS_table_metadata(dsn = soilDBdata_pedon_dsn)
# y <- get_NASIS_table_name_by_purpose(c("area", "site", "pedon", "transect",
#                                        "metadata", "lookup", "nasis"))

## ----message = FALSE, results = 'asis'----------------------------------------
# mldx <- names(f) %in% x$ColumnPhysicalName
# nasis_names <- names(f)[mldx]
# soildb_names <- names(f)[!mldx]
# 
# # fetchNASIS does not hit these tables; ignore them (some column names overlap)
# nasis_tables <- y[!y %in% c("transectestcomposition",
#                             "ncsspedonlabdata",
#                             "ncsslayerlabdata")]
# 
# # these are 1:1 aliases of a NASIS column (from="pedons")
# aliases <- c(genhz = "phorizon.dspcomplayerid",
#              clay = "phorizon.claytotest",
#              silt = "phorizon.silttotest",
#              sand = "phorizon.sandtotest",
#              texture_class = "phorizon.texcl",
#              y_std = "site.latstddecimaldegrees",
#              x_std = "site.longstddecimaldegrees",
#              pedon_id = "pedon.upedonid",
#              describer = "pedon.descname",
#              site_id = "site.usiteid",
#              elev_field = "site.elev",
#              slope_field = "site.slope",
#              aspect_field = "site.aspect",
#              obs_date = "siteobs.obsdate",
#              es_classifier = "siteecositehistory.classifier")
# alias_tnames <- gsub("(.*)\\..*|([^.]*)", "\\1\\2", aliases)
# alias_pnames <- gsub(".*\\.(.*)|([^.]*)", "\\1\\2", aliases)
# 
# res <- subset(x,
#               TablePhysicalName %in% nasis_tables &
#               ColumnPhysicalName %in% c(nasis_names, alias_pnames),
#                     select = c("TablePhysicalName",
#                                "TableDescription",
#                                "ColumnPhysicalName",
#                                "ColumnDescription"))
# res$fetchNASIS_Column <- res$ColumnPhysicalName
# ressub <- data.frame(TablePhysicalName = alias_tnames,
#                      ColumnPhysicalName = alias_pnames,
#                      fetchNASIS_Column = names(aliases))
# newalias <- merge(data.table::data.table(res[,1:3]), ressub, all.x = TRUE, sort = FALSE)$fetchNASIS_Column
# res$fetchNASIS_Column[!is.na(newalias)] <- na.omit(newalias)
# 
# res$ColumnDescription <- gsub("\r\n\r\n", " ", res$ColumnDescription)
# rownames(res) <- NULL
# res2 <- split(res, res$TablePhysicalName)
# # cat("# Duplicates")
# # knitr::kable(res[duplicated(res$ColumnPhysicalName),])
# 
# makeTable <- function(d) {
#   cat(paste0("# ", "`", d$TablePhysicalName[1], "`"), "\n")
#   cat("\n")
#   cat(d$TableDescription[1], "\n")
#   cat("\n")
#   d$Alias <- ifelse(d$ColumnPhysicalName == d$fetchNASIS_Column, "No", "Yes")
#   colnames(d)[4] <- "Column Description"
#   colnames(d)[5] <- "`fetchNASIS()` Column"
#   d$`Physical Name` <- paste0(d$TablePhysicalName, ".", d$ColumnPhysicalName)
#   cat(knitr::kable(d[c(5:7, 4)], row.names = FALSE), sep = "\n")
#   cat("\n")
#   cat("\n")
# }
# x <- lapply(res2, makeTable)
# 
# # Missing / undefined calculated values
# cat("# Calculated", "\n", "\n")
# ldx <- !names(f) %in% res$fetchNASIS_Column
# res3 <- res[0,][seq_len(sum(ldx)),]
# res3$fetchNASIS_Column <- names(f)[ldx]
# res3$ColumnPhysicalName <- NULL
# res3$ColumnDescription <- "TODO"
# calcdef$RelatedFunction <- sapply(strsplit(calcdef$RelatedFunction, ","), function(z) { paste0(paste0("`", trimws(z), "`"), collapse = ", ") })
# res4 <- merge(res3[, 4, drop = FALSE], calcdef, by = "fetchNASIS_Column")
# knitr::kable(res4[complete.cases(res4), ], row.names = FALSE)
# cat(" \n")
# 
# # dbQueryNASIS(NASIS(), "SELECT siteecositehistory.classifier AS [siteecositehistory.classifier] FROM siteecositehistory") |>
# #   head() |>
# #   View()
# 
# # custom calculated
# # - color (dry/moist, RGB/HVC+sigma, dry_soil_color, moist_soil_color, soil_color)
# # - horizon fragments (fine_gravel, gravel, cobbles, stones, boulders, channers, flagstones + para, total with and w/o para)
# # - horizon artifacts (fine_gravel, gravel, cobbles, stones, boulders, channers, flagstones + para, total cohesive/penetrable/innocuous)
# # - surface fragments (fine_gravel, gravel, cobbles, stones, boulders, channers, flagstones + para, total with and w/o para)
# # - presence/absence of diagnostic features (TRUE/FALSE feature name with " " replaced with ".")
# # - landform, landscape, microfeature and geomicrorelief strings (concatenated landform names)
# 
# # these should not be used any more
# # x/y (calculated long/lat), texture_class (replaced by texcl), surface_fgravel (replaced by surface_fine_gravel)


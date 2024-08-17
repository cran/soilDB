## ----setup, echo=FALSE, results='hide', warning=FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.align = 'center',
  fig.width = 8,
  fig.height = 6, 
  dpi = 36,
  tidy = FALSE,
  verbose = FALSE,
  # run when NASIS is defined or when R_SOILDB_SKIP_LONG_EXAMPLES is FALSE
  eval = isTRUE(try(local_NASIS_defined(), silent = TRUE)) ||
           !as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = "TRUE"))
)
options(width = 100, stringsAsFactors = FALSE)

## ----eval = FALSE---------------------------------------------------------------------------------
#  install.packages(c('soilDB', 'terra', 'sf'))

## ----eval = FALSE---------------------------------------------------------------------------------
#  install.packages(c('soilDB', 'terra', 'sf'),
#    repos = c('https://ncss-tech.r-universe.dev',
#              'https://rspatial.r-universe.dev',
#              'https://r-spatial.r-universe.dev')
#  )

## ----eval = FALSE---------------------------------------------------------------------------------
#  # select gSSURGO grid, 30m resolution
#  x <- mukey.wcs(aoi = aoi, db = 'gssurgo', ...)
#  
#  # select gNATSGO grid, 30m resolution
#  x <- mukey.wcs(aoi = aoi, db = 'gnatsgo', ...)
#  
#  # select RSS grid, 10m resolution
#  x <- mukey.wcs(aoi = aoi, db = 'RSS', ...)
#  
#  # select STATSGO2 grid, 300m resolution
#  x <- mukey.wcs(aoi = aoi, db = 'statsgo', ...)

## ----eval = FALSE---------------------------------------------------------------------------------
#  # select various ISSR-800 grids, details below
#  x <- ISSR800.wcs(aoi = aoi, var = 'paws')

## ----fig.width = 5, fig.height = 5----------------------------------------------------------------
library(terra)
library(soilDB)

# example point, WGS84 coordinates
p <- vect(
  data.frame(
    lon = -118.55639,
    lat = 36.52578
  ),
  crs = "EPSG:4326"
)

# 1000m buffer applied to WGS84 coordinate
# radius defined in meters
b <- buffer(p, 1000)

# query WCS
# result is in EPSG:5070
mu <- mukey.wcs(b, db = 'gSSURGO')

# inspect
plot(mu, legend = FALSE, axes = FALSE, main = metags(mu)['description'])

# add buffer, after transforming to mukey grid CRS
plot(project(b, "EPSG:5070"), add = TRUE)

# add original point, after transforming to mukey grid CRS
plot(project(p, "EPSG:5070"), add = TRUE, pch = 16)

## ----fig.width = 8, fig.height = 7----------------------------------------------------------------
library(sf)
library(soilDB)
library(terra)

# paste the five coordinates comprising the BBOX polygon here
bb <- '-118.6609 36.4820,-118.6609 36.5972,-118.3979 36.5972,-118.3979 36.4820,-118.6609 36.4820'

# convert WKT string -> sfc geometry
wkt <- sprintf('POLYGON((%s))', bb)
x <- st_as_sfc(wkt)

# set coordinate reference system as GCS/WGS84
st_crs(x) <- 4326

# query WCS
mu <- mukey.wcs(x, db = 'gSSURGO')

# inspect
plot(mu, legend = FALSE, axes = FALSE, main = metags(mu)['description'])

# add original BBOX, after transforming to mukey grid CRS
plot(st_transform(x, 5070), add = TRUE)

## -------------------------------------------------------------------------------------------------
# make a bounding box and assign a CRS (4326: GCS, WGS84)
a <- st_bbox(
  c(xmin = -114.16, xmax = -114.08, ymin = 47.65, ymax = 47.68), 
  crs = st_crs(4326)
)

# fetch gSSURGO map unit keys at native resolution (30m)
mu <- mukey.wcs(aoi = a, db = 'gssurgo')

# check:
print(mu)

plot(
  mu, 
  main = 'gSSURGO map unit keys',
  sub = 'Albers Equal Area Projection',
  axes = FALSE, 
  legend = FALSE
)

## -------------------------------------------------------------------------------------------------
# because mu is a SpatRaster, result is a SpatVector object (GCS WGS84)
p <- SDA_spatialQuery(mu, what = 'mupolygon', geomIntersection = TRUE)

## -------------------------------------------------------------------------------------------------
p <- project(p, crs(mu))

## ----fig.width = 8, fig.height = 7----------------------------------------------------------------
plot(mu,
     main = 'gSSURGO Grid (WCS)\nSSURGO Polygons (SDA)',
     axes = FALSE,
     legend = FALSE)
plot(p, add = TRUE, border = 'white')
mtext('CONUS Albers Equal Area Projection (EPSG:5070)', side = 1, line = 1)

## -------------------------------------------------------------------------------------------------
# make a bounding box (in California) and assign a CRS (GCS WGS84 / EPSG:4326)
a.CA <- st_bbox(c(
  xmin = -121,
  xmax = -120,
  ymin = 37,
  ymax = 38
), crs = st_crs(4326))

# fetch gSSURGO map unit keys at ~800m
# nearest-neighbor resampling = this is a "preview"
# result is a SpatRaster object
x.800 <- mukey.wcs(aoi = a.CA, db = 'gssurgo', res = 800)

plot(
  x.800,
  main = 'A Preview of gSSURGO Map Unit Keys',
  sub = 'Albers Equal Area Projection (800m)\nnearest-neighbor resampling',
  axes = FALSE,
  legend = FALSE
)

## ----fig.width=8, fig.height=6--------------------------------------------------------------------
# Coweeta Hydrologic Laboratory extent; specified in EPSG:5070
a <- st_bbox(
  c(xmin = 1129000, xmax = 1135000, ymin = 1403000, ymax = 1411000), 
  crs = st_crs(5070)
)

# convert boundary sf polygon
a <- st_as_sfc(a)

# gSSURGO grid: 30m resolution
(x <- mukey.wcs(a, db = 'gSSURGO', res = 30))

# gNATSGO grid: 30m resolution
(y <- mukey.wcs(a, db = 'gNATSGO', res = 30))

# RSS grid: 10m resolution
(z <- mukey.wcs(a, db = 'RSS', res = 10))

# graphical comparison
par(mfcol = c(1, 3))


# gSSURGO
plot(
  x,
  axes = FALSE,
  legend = FALSE,
  main = metags(x)['description']
)
plot(a, add = TRUE)

# gNATSGO
plot(
  y,
  axes = FALSE,
  legend = FALSE,
  main = metags(y)['description']
)
plot(a, add = TRUE)

# RSS
plot(
  z,
  axes = FALSE,
  legend = FALSE,
  main = metags(z)['description'],
  ext = x
)
plot(a, add = TRUE)

## ----fig.width=8, fig.height=6--------------------------------------------------------------------
(statsgo <- mukey.wcs(a, db = 'statsgo', res = 300))

# graphical comparison
par(mfcol = c(1, 2))

# gSSURGO
plot(
  x,
  axes = FALSE,
  legend = FALSE,
  main = metags(x)['description']
)

# STATSGO
plot(
  statsgo,
  axes = FALSE,
  legend = FALSE,
  main = metags(statsgo)['description']
)

## ----fig.width = 6.5, fig.height=5----------------------------------------------------------------
# paste your BBOX text here
bb <- '-159.7426 21.9059,-159.7426 22.0457,-159.4913 22.0457,-159.4913 21.9059,-159.7426 21.9059'

# convert WKT string -> sfc geometry
wkt <- sprintf('POLYGON((%s))', bb)
x <- st_as_sfc(wkt, crs = 4326)

# query WCS
mu <- mukey.wcs(x, db = 'hi_ssurgo')

# make NA (the ocean) blue
plot(
  mu,
  legend = FALSE,
  axes = FALSE,
  main = metags(mu)['description'],
  colNA = 'royalblue'
)

## ----eval=FALSE, include=FALSE--------------------------------------------------------------------
#  # # check mu names
#  # .is <- format_SQL_in_statement(cats(mu)[[1]]$mukey)
#  # .sql <- sprintf("SELECT mukey, muname FROM mapunit WHERE mukey IN %s", .is)
#  # knitr::kable(SDA_query(.sql))

## ----fig.width = 6.5, fig.height=5----------------------------------------------------------------
# paste your BBOX text here
bb <- '-65.7741 18.1711,-65.7741 18.3143,-65.5228 18.3143,-65.5228 18.1711,-65.7741 18.1711'

# convert WKT string -> sfc geometry
wkt <- sprintf('POLYGON((%s))', bb)
x <- st_as_sfc(wkt, crs = 4326)

# query WCS
mu <- mukey.wcs(x, db = 'pr_ssurgo')

# make missing data (NA; the ocean) blue
plot(
  mu,
  legend = FALSE,
  axes = FALSE,
  main = metags(mu)['description'],
  colNA = 'royalblue'
)

## ----eval=FALSE, include=FALSE--------------------------------------------------------------------
#  # # check mu names
#  # .is <- format_SQL_in_statement(cats(mu)[[1]]$mukey)
#  # .sql <- sprintf("SELECT mukey, muname FROM mapunit WHERE mukey IN %s", .is)
#  # knitr::kable(SDA_query(.sql))

## -------------------------------------------------------------------------------------------------
# make a bounding box and assign a CRS (4326: GCS, WGS84)
a <- st_bbox(
  c(xmin = -114.16, xmax = -114.08, ymin = 47.65, ymax = 47.68), 
  crs = st_crs(4326)
)

# convert bbox to sf geometry
a <- st_as_sfc(a)

# fetch gSSURGO map unit keys at native resolution (~30m)
mu <- mukey.wcs(aoi = a, db = 'gssurgo')

## ----fig.width=8----------------------------------------------------------------------------------
# copy example grid
mu2 <- mu

# extract raster attribute table for thematic mapping
(rat <- cats(mu2)[[1]])

# optionally use convenience function:
# * returns all fields from muaggatt table
# * along with map unit name
# tab <- get_SDA_muaggatt(mukeys = as.numeric(rat$mukey), query_string = TRUE)

.sql <- paste0(
  "SELECT mukey, aws050wta, aws0100wta FROM muaggatt WHERE mukey IN ",
  format_SQL_in_statement(as.numeric(rat$mukey))
)

# run query, result is a data.frame
tab <- SDA_query(.sql)

# check
head(tab)

# set raster categories
levels(mu2) <- tab

# convert grid + RAT -> stack of property grids
aws <- catalyze(mu2)

# plot, set a common range [0, 20] for both layers
plot(
  aws,
  axes = FALSE,
  cex.main = 0.7,
  main = c(
    'Plant Available Water Storage (cm)\nWeighted Mean over Components, 0-50cm',
    'Plant Available Water Storage (cm)\nWeighted Mean over Components, 0-100cm'
  ),
  range = c(0, 20)
)

## -------------------------------------------------------------------------------------------------
# copy example grid
mu2 <- mu

# extract RAT for thematic mapping
rat <- cats(mu2)[[1]]

rules <- c('ENG - Construction Materials; Roadfill',
           'AWM - Irrigation Disposal of Wastewater')

tab <- get_SDA_interpretation(
  rulename = rules, 
  method = "Weighted Average", 
  mukeys = as.numeric(rat$mukey)
)

# check
head(tab)

# set ordered factor levels (for nice label/legend order)
tab$class_ENGConstructionMaterialsRoadfill <- factor(
  tab$class_ENGConstructionMaterialsRoadfill,
  levels = c(
    'Not suited',
    'Poorly suited',
    'Moderately suited',
    'Moderately well suited',
    'Well suited',
    'Not Rated'
  ),
  ordered = TRUE
)

par(mar = c(4, 12, 3, 3))
boxplot(
  rating_ENGConstructionMaterialsRoadfill ~ class_ENGConstructionMaterialsRoadfill,
  cex.main = 0.7,
  main = 'ENG - Construction Materials; Roadfill',
  ylab = "",
  data = tab,
  horizontal = TRUE, # fuzzy ratings on X axis
  las = 1            # rotate axis labels 90 degrees
)

## ----fig.width=8----------------------------------------------------------------------------------
vars <- c(
  'rating_ENGConstructionMaterialsRoadfill',
  'rating_AWMIrrigationDisposalofWastewater'
)

# set raster categories
levels(mu2) <- tab[, c('mukey', vars)]

rating <- catalyze(mu2)

# inspect
plot(
  rating,
  axes = FALSE,
  cex.main = 0.7,
  main = c(
    'Construction Materials; Roadfill\nWeighted Mean over Components',
    'Irrigation Disposal of Wastewater\nWeighted Mean over Components'
  )
)

## ----fig.width = 8, fig.height = 6----------------------------------------------------------------
# copy example grid
mu2 <- mu

# extract RAT for thematic mapping
rat <- cats(mu2)[[1]]

tab <- get_SDA_property(property = 'Corrosion of Steel', 
                        method = 'DOMINANT CONDITION',
                        mukeys = as.integer(rat$mukey),
                        miscellaneous_areas = TRUE)

# get soil data viewer standard colors for corsteel
cols <- get_SDV_legend_elements("attributecolumnname = 'corsteel'")

# set raster categories
levels(mu2) <- tab[, c('mukey', 'corsteel')]

# set active category
activeCat(mu2) <- 'corsteel'

# plot
plot(
  mu2,
  col = cols$hex[na.omit(match(unique(tab$corsteel), cols$label))],
  axes = FALSE,
  legend = "topleft"
)

## -------------------------------------------------------------------------------------------------
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=36.57666,-96.70175,z14
# make a bounding box and assign a CRS (4326: GCS, WGS84)
a <- st_bbox(
  c(xmin = -96.7696, xmax = -96.6477, 
    ymin = 36.5477, ymax = 36.6139), 
  crs = st_crs(4326)
)

# fetch gSSURGO map unit keys at native resolution (~30m)
mu <- mukey.wcs(aoi = a, db = 'gssurgo')

plot(
  mu, 
  legend = FALSE, 
  axes = FALSE, 
  cex.main = 0.7,
  main = 'gSSURGO Map Unit Key Grid'
)

## ----fig.width = 8, fig.height = 6----------------------------------------------------------------
# copy example grid
mu2 <- mu

# extract RAT for thematic mapping
rat <- cats(mu2)[[1]]

# simplified parent material group name
tab <- get_SDA_pmgroupname(mukeys = as.integer(rat$mukey),
                           miscellaneous_areas = TRUE)

# set raster categories
levels(mu2) <- tab[, c('mukey', 'pmgroupname')]

# set active category
activeCat(mu2) <- 'pmgroupname'

plot(mu2, legend = "topleft", axes = FALSE)

## ----fig.width = 8, fig.height = 6----------------------------------------------------------------
# copy example grid
mu2 <- mu

# extract RAT for thematic mapping
rat <- cats(mu2)[[1]]

# simplified parent material group name
tab <- get_SDA_hydric(mukeys = as.integer(rat$mukey))

levels(mu2) <- tab[, c('mukey', 'HYDRIC_RATING')]

# set active category 
activeCat(mu2) <- 'HYDRIC_RATING'
plot(mu2, legend = "topleft", axes = FALSE)

## -------------------------------------------------------------------------------------------------
# extract RAT for thematic mapping
rat <- cats(mu)[[1]]

# variables of interest
vars <- c("dbthirdbar_r", "awc_r", "ph1to1h2o_r")

# get / aggregate specific horizon-level properties from SDA
# be sure to see the manual page for this function
tab <- get_SDA_property(property = vars,
                        method = "Dominant Component (Numeric)", 
                        mukeys = as.integer(rat$mukey),
                        top_depth = 0,
                        bottom_depth = 25)


# check
head(tab)

# convert areasymbol into a factor easy plotting later
tab$areasymbol <- factor(tab$areasymbol)

# set raster categories
levels(mu) <- tab[, c('mukey', vars)]

# list variables in the RAT
names(cats(mu)[[1]])

# convert categories associated with keys to values
mu2 <- catalyze(mu)

## ----fig.width = 6, fig.height = 4----------------------------------------------------------------
plot(mu2$awc_r)

## -------------------------------------------------------------------------------------------------
plot(mu2[['dbthirdbar_r']], cex.main = 0.7,
     main = '1/3 Bar Bulk Density (g/cm^3)\nDominant Component\n0-25cm')

plot(mu2[['awc_r']], cex.main = 0.7,
     main = 'AWC (cm/cm)\nDominant Component\n0-25cm')

plot(mu2[['ph1to1h2o_r']], cex.main = 0.7,
     main = 'pH 1:1 H2O\nDominant Component\n0-25cm')

## -------------------------------------------------------------------------------------------------
# extract a BBOX like this from SoilWeb by pressing "b"
bb <- '-91.6853 36.4617,-91.6853 36.5281,-91.5475 36.5281,-91.5475 36.4617,-91.6853 36.4617'
wkt <- sprintf('POLYGON((%s))', bb)

# init sf object from WKT
x <- st_as_sfc(wkt, crs = 4326)

# get gSSURGO grid here
mu <- mukey.wcs(aoi = x, db = 'gssurgo')

# note SSA boundary
plot(mu, legend = FALSE, axes = FALSE)

## ----fig.width = 8, fig.height = 6----------------------------------------------------------------
# extract RAT for thematic mapping
rat <- cats(mu)[[1]]

# variables of interest
vars <- c("sandtotal_r", "silttotal_r", "claytotal_r")

# get thematic data from SDA
# dominant component
# depth-weighted average
# sand, silt, clay (RV)
tab <-  get_SDA_property(property = vars,
                         method = "Dominant Component (Numeric)", 
                         mukeys = as.integer(rat$mukey),
                         top_depth = 25,
                         bottom_depth = 50) 

# check
head(tab)

# set raster categories
levels(mu) <- tab[, c('mukey', vars)]

# convert mukey grid + RAT -> stack of numerical grids
# retaining only sand, silt, clay via [[vars]]
ssc <- catalyze(mu)

# create a copy of the grid
texture.class <- ssc[[1]]
names(texture.class) <- 'soil.texture'

# assign soil texture classes for the fine earth fraction
# using sand and clay percentages
values(texture.class) <- aqp::ssc_to_texcl(
  sand = values(ssc[['sandtotal_r']]), 
  clay = values(ssc[['claytotal_r']]), 
  droplevels = FALSE
)
r <- c(ssc, texture.class)

# graphical check
plot(
  r,
  cex.main = 0.7,
  main = paste0(names(r), " - 25-50cm\nDominant Component")
)


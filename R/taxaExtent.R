
#' @title Get SoilWeb 800m Major Component Soil Taxonomy Grids
#' 
#' @description This function downloads a generalized representation of the geographic extent of any single taxon from the top 4 levels of Soil Taxonomy, or taxa matching a given formative element used in Great Group or subgroup taxa. Data are provided by SoilWeb, ultimately sourced from the current SSURGO snapshot. Data are returned as \code{raster} objects representing area proportion falling within 800m cells. Currently area proportions are based on major components only. Data are only available in CONUS and returned using an Albers Equal Area / NAD83(2011) coordinate reference system (EPSG: 5070).
#' 
#' @param x single taxon label (e.g. `haploxeralfs`) or formative element (e.g. `pale`), case-insensitive
#' 
#' @param level the taxonomic level within the top 4 tiers of Soil Taxonomy, one of `'order'`, `'suborder'`, `'greatgroup'`, `'subgroup'`
#' 
#' @param formativeElement logical, search using formative elements instead of taxon label
#' 
#' @param timeout time that we are willing to wait for a response, in seconds
#' 
#' @param as_Spatial Return raster (`RasterLayer`) classes? Default: `FALSE`.
#' 
#' @return a `SpatRaster` object (or `RasterLayer` when `as_Spatial=TRUE`)
#' 
#' @author D.E. Beaudette and A.G. Brown
#' 
#' @details See the [Geographic Extent of Soil Taxa](https://ncss-tech.github.io/AQP/soilDB/taxa-extent.html) tutorial for more detailed examples.
#' 
#' ## Taxon Queries
#' 
#' Taxon labels can be conveniently extracted from the `"ST_unique_list"` sample data, provided by the [SoilTaxonomy package](https://github.com/ncss-tech/SoilTaxonomy).
#' 
#' ## Formative Element Queries
#' 
#' ### Greatgroup:
#' The following labels are used to access taxa containing the following formative elements (in parentheses)
#' 
#' * acr: (acro/acr) extreme weathering
#' * alb: (alb) presence of an albic horizon
#' * anhy: (anhy) very dry
#' * anthra: (anthra) presence of an anthropic epipedon
#' * aqu: (aqui/aqu) wetness
#' * argi: (argi) presence of an argillic horizon
#' * calci: (calci) presence of a calcic horizon
#' * cry: (cryo/cry) cryic STR
#' * dur: (duri/dur) presence of a duripan
#' * dystr: (dystro/dystr) low base saturation
#' * endo: (endo) ground water table
#' * epi: (epi) perched water table
#' * eutr: (eutro/eutr) high base saturation
#' * ferr: (ferr) presence of Fe
#' * fibr: (fibr) least decomposed stage
#' * fluv: (fluv) flood plain
#' * fol: (fol) mass of leaves
#' * fragi: (fragi) presence of a fragipan
#' * fragloss: (fragloss) presence of a fragipan and glossic horizon
#' * frasi: (frasi) not salty
#' * fulv: (fulvi/fulv) dark brown with organic carbon
#' * glac: (glac) presence of ice lenses
#' * gloss: (glosso/gloss) presence of a glossic horizon
#' * gypsi: (gypsi) presence of a gypsic horizon
#' * hal: (hal) salty
#' * hemi: (hemi) intermediate decomposition
#' * hist: (histo/hist) organic soil material
#' * hum: (humi/hum) presence of organic carbon
#' * hydr: (hydro/hydr) presence of water
#' * kandi: (kandi) presence of a kandic horizon
#' * kanhap: (kanhaplo/kanhap) thin kandic horizon
#' * luvi: (luvi) illuvial organic material
#' * melan: (melano/melan) presence of a melanic epipedon
#' * moll: (molli/moll) presence of a mollic epipedon
#' * natr: (natri/natr) presence of a natric horizon
#' * pale: (pale) excessive development
#' * petr: (petro/petr) petrocalcic horizon
#' * plac: (plac) presence of a thin pan
#' * plagg: (plagg) presence of a plaggen epipedon
#' * plinth: (plinth) presence of plinthite
#' * psamm: (psammo/psamm) sandy texture
#' * quartzi: (quartzi) high quartz content
#' * rhod: (rhodo/rhod) dark red colors
#' * sal: (sali/sal) presence of a salic horizon
#' * sapr: (sapr) most decomposed stage
#' * sombri: (sombri) presence of a sombric horizon
#' * sphagno: (sphagno) presence of sphagnum moss
#' * sulf: (sulfo/sulfi/sulf) presence of sulfides or their oxidation products
#' * torri: (torri) torric/aridic SMR
#' * ud: (udi/ud) udic SMR
#' * umbr: (umbri/umbr) presence of an umbric epipedon
#' * ust: (usti/ust) ustic SMR
#' * verm: (verm) wormy, or mixed by animals
#' * vitr: (vitri/vitr) presence of glass
#' * xer: (xero/xer) xeric SMR
#' 
#' 
#' ### Subgroup:
#' The following labels are used to access taxa containing the following formative elements (in parenthesis).
#' 
#' * abruptic: (abruptic) abrupt textural change
#' * acric: (acric) low apparent CEC
#' * aeric: (aeric) more aeration than typic subgroup
#' * albaquic: (albaquic) presence of albic minerals, wetter than typic subgroup
#' * albic: (albic) presence of albic minerals
#' * alfic: (alfic) presence of an argillic or kandic horizon
#' * alic: (alic) high extractable Al content
#' * anionic: (anionic) low CEC or positively charged
#' * anthraquic: (anthraquic) human controlled flooding as in paddy rice culture
#' * anthropic: (anthropic) an anthropic epipedon
#' * aquic: (aquic) wetter than typic subgroup
#' * arenic: (arenic) 50-100cm sandy textured surface
#' * argic: (argic) argillic horizon
#' * aridic: (aridic) more aridic than typic subgroup
#' * calcic: (calcic) presence of a calcic horizon
#' * chromic: (chromic) high chroma colors
#' * cumulic: (cumulic) thickened epipedon
#' * duric: (duric) presence of a duripan
#' * durinodic: (durinodic) presence of durinodes
#' * dystric: (dystric) lower base saturation percentage
#' * entic: (entic) minimal surface/subsurface development
#' * eutric: (eutric) higher base saturation percentage
#' * fibric: (fibric) >25cm of fibric material
#' * fluvaquentic: (fluvaquentic) wetter than typic subgroup, evidence of stratification
#' * fragiaquic: (fragiaquic) presence of fragic properties, wetter than typic subgroup
#' * fragic: (fragic) presence of fragic properties
#' * glacic: (glacic) presence of ice lenses or wedges
#' * glossaquic: (glossaquic) interfingered horizon boundaries, wetter than typic subgroup
#' * glossic: (glossic) interfingered horizon boundaries
#' * grossarenic: (grossarenic) >100cm sandy textured surface
#' * gypsic: (gypsic) presence of gypsic horizon
#' * halic: (halic) salty
#' * haplic: (haplic) central theme of subgroup concept
#' * hemic: (hemic) >25cm of hemic organic material
#' * humic: (humic) higher organic matter content
#' * hydric: (hydric) presence of water
#' * kandic: (kandic) low activity clay present
#' * lamellic: (lamellic) presence of lamellae
#' * leptic: (leptic) thinner than typic subgroup
#' * limnic: (limnic) presence of a limnic layer
#' * lithic: (lithic) shallow lithic contact present
#' * natric: (natric) presence of sodium
#' * nitric: (nitric) presence of nitrate salts
#' * ombroaquic: (ombroaquic) surface wetness
#' * oxyaquic: (oxyaquic) water saturated but not reduced
#' * pachic: (pachic) epipedon thicker than typic subgroup
#' * petrocalcic: (petrocalcic) presence of a petrocalcic horizon
#' * petroferric: (petroferric) presence of petroferric contact
#' * petrogypsic: (petrogypsic) presence of a petrogypsic horizon
#' * petronodic: (petronodic) presence of concretions and/or nodules
#' * placic: (placic) presence of a placic horizon
#' * plinthic: (plinthic) presence of plinthite
#' * rhodic: (rhodic) darker red colors than typic subgroup
#' * ruptic: (ruptic) intermittent horizon
#' * salic: (salic) presence of a salic horizon
#' * sapric: (sapric) >25cm of sapric organic material
#' * sodic: (sodic) high exchangeable Na content
#' * sombric: (sombric) presence of a sombric horizon
#' * sphagnic: (sphagnic) sphagnum organic material
#' * sulfic: (sulfic) presence of sulfides
#' * terric: (terric) mineral substratum within 1 meter
#' * thapto: (thaptic/thapto) presence of a buried soil horizon
#' * turbic: (turbic) evidence of cryoturbation
#' * udic: (udic) more humid than typic subgroup
#' * umbric: (umbric) presence of an umbric epipedon
#' * ustic: (ustic) more ustic than typic subgroup
#' * vermic: (vermic) animal mixed material
#' * vitric: (vitric) presence of glassy material
#' * xanthic: (xanthic) more yellow than typic subgroup
#' * xeric: (xeric) more xeric than typic subgroup
#' @export
#' @examplesIf curl::has_internet() && requireNamespace("terra")
#' \dontrun{
#'   library(terra)
#'   
#'   # soil order
#'   taxa <- 'vertisols'
#'   x <- taxaExtent(taxa, level = 'order')
#'   
#'   # suborder
#'   taxa <- 'ustalfs'
#'   x <- taxaExtent(taxa, level = 'suborder')
#'   
#'   # greatgroup
#'   taxa <- 'haplohumults'
#'   x <- taxaExtent(taxa, level = 'greatgroup')
#'   
#'   # subgroup
#'   taxa <- 'Typic Haploxerepts'
#'   x <- taxaExtent(taxa, level = 'subgroup')
#'   
#'   # greatgroup formative element
#'   taxa <- 'psamm'
#'   x <- taxaExtent(taxa, level = 'greatgroup', formativeElement = TRUE)
#'   
#'   # subgroup formative element
#'   taxa <- 'abruptic'
#'   x <- taxaExtent(taxa, level = 'subgroup', formativeElement = TRUE)
#'   
#'   # coarsen for faster plotting
#'   a <- terra::aggregate(x, fact = 5, na.rm = TRUE)
#'   
#'   # quick evaluation of the result
#'   terra::plot(a, axes = FALSE)
#' }
taxaExtent <- function(x, level = c('order', 'suborder', 'greatgroup', 'subgroup'),
                       formativeElement = FALSE, timeout = 60,
                       as_Spatial = getOption('soilDB.return_Spatial', default = FALSE)) {
 
  ## sanity checks
  
  # legal levels
  level <- match.arg(level)
  
  # base URL
  base.url <- 'https://casoilresource.lawr.ucdavis.edu/taxa-grid-cache'
  
  # main branch
  # formative element query
  if (formativeElement) {
    
    # formative elements are only available at the greatgroup / subgroup for now
    if (level %in% c('order', 'suborder')) {
      stop('formative element queries are only supported for greatgroup and subgroup taxa', call. = FALSE)
    }
    
    # convert formative element level to path
    subdir <- switch(
      level,
      greatgroup = 'gg',
      subgroup = 'sg'
    )
    
    # full URL
    u <- URLencode(
      sprintf(
        '%s/fm/%s/%s.tif', 
        base.url,
        subdir, 
        x
      )
    )
    
  } else {
    # taxon query
    
    # encode taxon name: spaces -> underscores
    x <- gsub(pattern = ' ', replacement = '_', x = tolower(x), fixed = TRUE)
    
    # convert taxa level to path
    subdir <- switch(
      level,
      order = 'taxorder',
      suborder = 'taxsuborder',
      greatgroup = 'taxgrtgroup',
      subgroup = 'taxsubgrp'
    )
    
    # full URL
    u <- URLencode(
      sprintf(
        '%s/%s/%s.tif', 
        base.url,
        subdir, 
        x
      )
    )
    
  }
  
  # init temp files
  tf <- tempfile(fileext = '.tif')
  
  # safely download GeoTiff file
  # Mac / Linux: file automatically downloaded via binary transfer
  # Windows: must manually specify binary transfer
  res <- tryCatch(
    suppressWarnings(
      curl::curl_download(url = u, destfile = tf, handle = .soilDB_curl_handle(timeout = timeout), quiet = TRUE, mode = 'wb')
    ),
    error = function(e) {e}
  )
  
  # trap errors
  if (inherits(res, 'error')) {
    message('no data returned')
    return(NULL)
  }

  # init SpatRaster
  r <- terra::rast(tf)
  
  # load all values into memory
  terra::values(r) <- terra::values(r)
  
  # remove tempfile 
  unlink(tf)
  
  # transfer layer name
  # conversion of '_' -> ' ' only meaningful in taxon query
  names(r) <- gsub(pattern = '_', replacement = ' ', x = x, fixed = TRUE)
  
  # make CRS explicit
  terra::crs(r) <- 'EPSG:5070'
  
  if (as_Spatial) {
    if (requireNamespace("raster", quietly = TRUE)) {
      r <- raster::raster(r) 
    } else {
      stop("Package `raster` is required to return raster data as a RasterLayer object with as_Spatial=TRUE")
    }
  }
  
  return(r)
  
}


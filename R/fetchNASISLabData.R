# convenience function for loading most commonly used information from local NASIS database


#' Get NCSS Pedon laboratory data from NASIS
#'
#' Fetch KSSL laboratory pedon/horizon layer data from a local NASIS database,
#' return as a SoilProfileCollection object.
#'
#' @param SS fetch data from the currently loaded selected set in NASIS or from
#' the entire local database (default: `TRUE`)#'
#' @param dsn Optional: path to local SQLite database containing NASIS
#' table structure; default: `NULL`
#'
#' @return a SoilProfileCollection object
#'
#' @author J.M. Skovlin and D.E. Beaudette
#' @seealso \code{\link{get_labpedon_data_from_NASIS_db}}
#' @keywords manip
#' @export fetchNASISLabData
fetchNASISLabData <- function(SS = TRUE, dsn = NULL) {

  # check if NASIS local DB instance/ODBC data source is available
  .soilDB_test_NASIS_connection(dsn = dsn)
  
  if (!requireNamespace("aqp")) {
    stop("package 'aqp' is required", call. = FALSE)
  }
  
	# 1. load data in pieces, results are DF objects
	s <- get_labpedon_data_from_NASIS_db(SS = SS, dsn = dsn)
	h <- get_lablayer_data_from_NASIS_db(SS = SS, dsn = dsn)

  # stop if selected set is not loaded
  if (nrow(h) == 0 | nrow(s) == 0)
    stop('Selected set is missing either the NCSS Pedon or NSSS Layer Lab Data table', call. = FALSE)

	# fix some common problems
	# replace missing lower boundaries
	missing.lower.depth.idx <- which(!is.na(h$hzdept) & is.na(h$hzdepb))
  if (length(missing.lower.depth.idx) > 0) {
    message(paste0('replacing missing lower horizon depths with top depth + 1cm ... [',
                  length(missing.lower.depth.idx), ' horizons]'))
    h$hzdepb[missing.lower.depth.idx] <- h$hzdept[missing.lower.depth.idx] + 1
  }

  ## TODO: what to do with multiple samples / hz?
	# test for bad horizonation... flag
	message('finding horizonation errors ...')
	h.test <-	aqp::checkHzDepthLogic(h, c('hzdept', 'hzdepb'), idname = 'ncsspedonlabdataiid', fast = TRUE)

	# which are the good (valid) ones?
	# good.ids <- as.character(h.test$ncsspedonlabdataiid[which(h.test$valid)])
	bad.ids <- as.character(h.test$ncsspedonlabdataiid[which(!h.test$valid)])
  bad.pedon.ids <- s$upedonid[which(s$labpeiid %in% bad.ids)]

	# upgrade to SoilProfilecollection
	aqp::depths(h) <- ncsspedonlabdataiid ~ hzdept + hzdepb

	## TODO: this will fail in the presence of duplicates
	# add site data to object
	s$labpeiid <- NULL
	aqp::site(h) <- s # left-join via ncsspedonlabdataiid

	# set NASIS-specific horizon identifier
	aqp::hzidname(h) <- 'ncsslayerlabdataiid'

	# 7. save and mention bad pedons
	assign('bad.labpedon.ids', value = bad.pedon.ids, envir = get_soilDB_env())
	if (length(bad.pedon.ids) > 0)
		message("horizon errors detected, use `get('bad.labpedon.ids', envir=get_soilDB_env())` for a list of pedon IDs")

	# done
	return(h)
}

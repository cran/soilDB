fetchPedonPC <- function(dsn) {
	
	# 1. load data in pieces
	site_data <- get_site_data_from_pedon_db(dsn)
	hz_data <- get_hz_data_from_pedon_db(dsn)
	color_data <- get_colors_from_pedon_db(dsn)
	extended_data <- get_extended_data_from_pedon_db(dsn)
	
	# 2. join pieces
	# horizon + hz color: all horizons
	h <- join(hz_data, color_data, by='phiid', type='left')
	
	## TODO: this creates 2 columns of 'pedon_id', temp work-around is to remove it from site data
	# (hz + color) + site: only those with horizon data
	f <- join(h, site_data[, -which(names(site_data) == 'pedon_id')], by='peiid', type='inner')
	
	
	# 3. fix some common problems
	# replace missing lower boundaries
	cat('replacing missing lower boundaries ...\n')
	f$hzdepb[!is.na(f$hzdept) & is.na(f$hzdepb)] <- f$hzdept[!is.na(f$hzdept) & is.na(f$hzdepb)]
	
	# test for bad horizonation... flag, and remove
	cat('finding horizonation errors ...\n')
	f.test <- ddply(f, 'pedon_id', test_hz_logic, topcol='hzdept', bottomcol='hzdepb', strict=TRUE)
	
	# which are the good (valid) ones?
	good.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass)])
	bad.pedon.ids <- as.character(f.test$pedon_id[which(f.test$hz_logic_pass == FALSE)])
	
	# keep the good ones
	f <- subset(f, pedon_id %in% good.pedon.ids)
	
	# 4. upgrade to SoilProfilecollection
	depths(f) <- pedon_id ~ hzdept + hzdepb
	
	# move site data into @site
	site(f) <- ~ peiid + site_id + siteiid + hillslope_pos + x + y + datum + elev + slope + aspect + plantassocnm + bedrckdepth + bedrock_kind + bedrock_hardness + describer + psctopdepth + pscbotdepth + obs_date + pedon_purpose + pedon_type + pedlabsampnum
	
	# 5. convert colors... in the presence of missing color data
	f$soil_color <- rep(NA, times=nrow(horizons(f)))
	idx <- complete.cases(f$m_r)
	f$soil_color[idx] <- with(horizons(f)[idx, ], rgb(m_r, m_g, m_b)) # moist colors
	
	# 6. merge-in extended data:
	# replace horizons with hz + fragment summary
	horizons(f) <- join(horizons(f), extended_data$frag_summary, by='phiid', type='left')
	
	# add diagnostic boolean data into @site
	site(f) <- extended_data$diagHzBoolean
	
	# load diagnostic horizons into @diagnostic: note that this requires one additional join
	diagnostic_hz(f) <- join(site(f)[, c('pedon_id','peiid')], extended_data$diagnostic, by='peiid', type='left')
	
	# load best-guess optimal records from taxhistory
	best.tax.data <- ddply(extended_data$taxhistory, 'peiid', pickBestTaxHistory)
	site(f) <- best.tax.data
	
	# 7. mention bad pedons
	if(length(bad.pedon.ids) > 0)
		cat(paste('horizon errors in:', paste(bad.pedon.ids, collapse=','), '\n'))
	
	# done
	return(f)
}
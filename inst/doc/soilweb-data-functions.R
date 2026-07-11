## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = !as.logical(Sys.getenv("R_SOILDB_SKIP_LONG_EXAMPLES", unset = "TRUE")),
  message = FALSE,
  warning = FALSE,
  fig.height = 7,
  fig.width = 7,
  fig.retina = 2
)

## ----get-data, echo=FALSE-----------------------------------------------------
# # get source data for explanations
# library(soilDB)
# library(aqp)
# 
# x <- fetchOSD(c('miami', 'pierre', 'tristan', 'lucy', 'musick'), extended = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # typical invocation
# library(soilDB)
# library(aqp)
# 
# x <- fetchOSD(c('lucy'), extended = TRUE)
# 
# # access metadata
# x$soilweb.metadata

## ----eval=FALSE---------------------------------------------------------------
# # series names listed in "competing" have the family classification as "series"
# head(x$competing)

## ----echo=FALSE---------------------------------------------------------------
# knitr::kable(head(x$competing))

## ----eval=FALSE---------------------------------------------------------------
# # series names listed in "gas" are geographically associated with "series"
# head(x$geog_assoc_soils)

## ----echo=FALSE---------------------------------------------------------------
# # series names listed in "gas" are geographically associated with "series"
# knitr::kable(head(x$geog_assoc_soils))

## ----eval=FALSE---------------------------------------------------------------
# .mlra <- x$mlra[x$mlra$series == 'LUCY', ]
# .mlra[order(.mlra$membership, decreasing = TRUE), ]

## ----echo=FALSE---------------------------------------------------------------
# .mlra <- x$mlra[x$mlra$series == 'LUCY', ]
# knitr::kable(.mlra[order(.mlra$membership, decreasing = TRUE), ], digits = 2, row.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
# # hillslope position
# head(x$hillpos)
# # geomorphic component: hills
# head(x$geomcomp)
# # geomorphic component: mountains
# head(x$mtnpos)
# # geomorphic component: terraces
# head(x$terrace)
# # geomorphic component: flats
# head(x$flats)
# # surface curvature across slope
# head(x$shape_across)
# # surface curvature down slope
# head(x$shape_down)

## ----echo=FALSE---------------------------------------------------------------
# knitr::kable(head(x$hillpos), digits = 2, caption = 'hillslope position')
# 
# knitr::kable(head(x$geomcomp), digits = 2, caption = 'geomorphic component: hills')
# 
# knitr::kable(head(x$mtnpos), digits = 2, caption = 'geomorphic component: mountains')
# 
# knitr::kable(head(x$terrace), digits = 2, caption = 'geomorphic component: terraces')
# 
# knitr::kable(head(x$flats), digits = 2, caption = 'geomorphic component: flats')
# 
# knitr::kable(head(x$shape_across), digits = 2, caption = 'surface curvature across-slope')
# 
# knitr::kable(head(x$shape_down), digits = 2, caption = 'surface curvature down-slope')

## ----eval=FALSE---------------------------------------------------------------
# sib <- siblings('PIERRE', only.major = TRUE)
# head(sib$sib)

## ----echo=FALSE---------------------------------------------------------------
# sib <- siblings('PIERRE', only.major = TRUE)
# knitr::kable(sib$sib)

## ----eval=FALSE---------------------------------------------------------------
# head(x$NCCPI)

## ----echo=FALSE---------------------------------------------------------------
# knitr::kable(head(x$NCCPI), digits = 2)

## ----eval=FALSE---------------------------------------------------------------
# head(x$ecoclassid)

## ----echo=FALSE---------------------------------------------------------------
# knitr::kable(head(x$ecoclassid), digits = 2)

## ----eval=FALSE---------------------------------------------------------------
# head(x$pmkind)
# head(x$pmorigin)

## ----echo=FALSE---------------------------------------------------------------
# knitr::kable(head(x$pmkind), digits = 2)
# knitr::kable(head(x$pmorigin), digits = 2)

## ----eval=FALSE---------------------------------------------------------------
# head(x$climate.annual)
# 
# head(x$climate.monthly)

## ----echo=FALSE---------------------------------------------------------------
# knitr::kable(head(x$climate.annual), digits = 0)
# 
# knitr::kable(head(x$climate.monthly), digits = 0)

## ----eval=FALSE---------------------------------------------------------------
# head(x$geomorphons)

## ----echo=FALSE---------------------------------------------------------------
# knitr::kable(head(x$geomorphons), digits = 2, caption = 'geomorphon summary')


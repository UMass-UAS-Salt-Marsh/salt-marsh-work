#------------------------------------------------------------------------------#
# Ground elevation source comparison — rr (Red River)
#------------------------------------------------------------------------------#
#
# Calls report_ground_sources() which renders
# rmd/ground_source_comparison.Rmd for this site.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(pathtools)

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/data/paths.yml")

#------------------------------------------------------------------------------#
# Red River
#------------------------------------------------------------------------------#

site <- "rr"

# Best CSF stems from Phase 1 evaluation (lowest overall RMSE).
# Spring: RMSE 0.174 m, bias +0.165 m
# Summer: RMSE 0.245 m, bias +0.221 m
best_csf_spring <- list(csf_threshold = 0.01, csf_res = 0.1,
                        csf_rigidness = 2, raster_res = 0.25)
best_csf_summer <- list(csf_threshold = 0.12, csf_res = 0.2,
                        csf_rigidness = 2, raster_res = 0.25)

# Must match whatever lidar/03.R used to produce best_csf_spring
# and best_csf_summer.
target_epsg <- 6491L

sources <- c(
   lidar_spring = get_path("ground_raster", c(
      list(site = site, date = "2022_05_14", target_epsg = target_epsg),
      best_csf_spring
   )),
   lidar_summer = get_path("ground_raster", c(
      list(site = site, date = "2022_08_10", target_epsg = target_epsg),
      best_csf_summer
   )),
   photo_spring_hesai = get_path("photo_dem", site = site,
                                 source = "spring_hesai"),
   photo_spring_mica  = get_path("photo_dem", site = site,
                                 source = "spring_mica"),
   photo_summer       = get_path("photo_dem", site = site,
                                 source = "summer"),
   massgis            = get_path("massgis_tile", tile = "19TDG412612")
)

# Render report
report_ground_sources(sources = sources, site = site)




#------------------------------------------------------------------------------#
# Old Town Hill
#------------------------------------------------------------------------------#

site <- "oth"
sources <- c(
   may_14_ortho   = get_path("photo_dem", site = site,
                             source = "may_14_ortho"),
   may_02_ortho   = get_path("photo_dem", site = site,
                             source = "may_02_ortho"),
   massgis_346737 = get_path("massgis_tile", tile = "19TCH346737"),
   massgis_346735 = get_path("massgis_tile", tile = "19TCH346735")
)

report_ground_sources(sources = sources, site = site)




#------------------------------------------------------------------------------#
# Welfleet:
#------------------------------------------------------------------------------#
site <- "wel"
sources <- c(
   may_22_ortho   = get_path("photo_dem", site = site,
                             source = "may_22_ortho"),
   massgis_415636 = get_path("massgis_tile", tile = "19TDG415636"),
   massgis_417636 = get_path("massgis_tile", tile = "19TDG417636")
)

report_ground_sources(sources = sources, site = site)

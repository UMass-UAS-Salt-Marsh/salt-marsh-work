#------------------------------------------------------------------------------#
# Ground elevation source comparison — rr (Red River)
#------------------------------------------------------------------------------#
#
# Calls report_ground_sources() which renders
# rmd/ground_source_comparison.Rmd for this site.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

#------------------------------------------------------------------------------#
# Red River
#------------------------------------------------------------------------------#

site <- "rr"

# Best CSF stems from Phase 1 evaluation (lowest overall RMSE).
# Spring: RMSE 0.174 m, bias +0.165 m
# Summer: RMSE 0.245 m, bias +0.221 m
best_csf_spring <- "csf_th0.01_res0.1_rgd2_0.25m.tif"
best_csf_summer <- "csf_th0.12_res0.2_rgd2_0.25m.tif"

# Must match whatever lidar/02.R used to produce best_csf_spring
# and best_csf_summer.
target_epsg <- 6491L

sources <- c(
   lidar_spring = file.path(
      "E:/uas_scratch/lidar", site, "2022_05_14",
      paste0("zzzraster_epsg", target_epsg), best_csf_spring
   ),
   lidar_summer = file.path(
      "E:/uas_scratch/lidar", site, "2022_08_10",
      paste0("zzzraster_epsg", target_epsg), best_csf_summer
   ),
   photo_spring_hesai = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Red River/Orthos and DEMs 2022/26May2022/Low/",
      "26May2022_RED_Low_HesaiRGB_DEM.tif"
   ),
   photo_spring_mica = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Red River/Orthos and DEMs 2022/26May2022/Low/",
      "26May22_RR_Low_Mica_DEM.tif"
   ),
   photo_summer = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Red River/Orthos and DEMs 2022/10Aug2022/Low/",
      "10Aug22_RR_Low_Mavic_DEM.tif"
   ),
   massgis = paste0(
      "X:/scratch/bcompton/LiDAR/",
      "be_19TDG412612/be_19TDG412612.tif"
   )
)

# Render report
report_ground_sources(sources = sources, site = site)




#------------------------------------------------------------------------------#
# Old Town Hill
#------------------------------------------------------------------------------#

site <- "oth"
sources <- c(
   may_14_ortho    = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Old Town Hill/Orthos and DEMs 2022/14May2022/",
      "14May2022_OTH_Low_HESAI_DEM.tif"
   ),
   may_02_ortho    = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Old Town Hill/Orthos and DEMs 2022/02May2022/",
      "02May2022_OTH_Low_Mavic_DEM.tif"
   ),
   massgis_346737  = "X:/scratch/bcompton/LiDAR/be_19TCH346737.tif",
   massgis_346735  = "X:/scratch/bcompton/LiDAR/be_19TCH346735.tif"
)

report_ground_sources(sources = sources, site = site)




#------------------------------------------------------------------------------#
# Welfleet:
#------------------------------------------------------------------------------#
site <- "wel"
sources <- c(
   may_22_ortho   = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Wellfleet Bay/Orthos and DEMs 2022/20May2022/Low/",
      "20May22_WEL_Low_Hesai_DEM.tif"
   ),
   massgis_415636 = "X:/scratch/bcompton/LiDAR/be_19TDG415636.tif",
   massgis_417636 = "X:/scratch/bcompton/LiDAR/be_19TDG417636.tif"
)

report_ground_sources(sources = sources, site = site)

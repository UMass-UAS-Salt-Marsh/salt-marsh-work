library(terra)
library(sf)
library(RSAGA)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(leaflet)


####Fillings sinks

#read in dtm
dtm <- terra::rast("/Users/emily/Library/CloudStorage/OneDrive-UniversityofMassachusetts/salt_marsh_data/Testing repo/salt_marsh_work/hydrology/overland_flow/for overlflow output rasters/RR_May_CSF2012_Thin25cm_TriNN25cm_Clipped_005.tif")

#read in mask to make boundary line
boundary<- st_read("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/Lines_RR_Boundary_08Aug2019.shp")

#adding buffer
buffer<- 0.5
line_buf<- st_buffer(line_bound, buffer)

#read in outflow point
gap_vector<- st_read("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/Water Flow Routing/CSF2012_Thin25cm_TriNN25cm_005/Single_Outlet_Point.shp")

#plot to see dtm + boundary + gap
ggplot() +
   geom_spatraster(data = dtm) +
   geom_sf(data = boundary) +
   geom_sf(data = gap_vector)

#make raster for boundary
wall <- terra::rasterize(boundary, dtm, values = 4, touched = TRUE,
                         background = 0)
gaps <- terra::rasterize(gap_vector, wall, value = 1, background
                         = 0, touched = TRUE)
wall[gaps == 1] <- 0 # zero out the gaps in the wall
wall_dtm <- dtm + wall

plot(wall_dtm)

out_wall_dtm<- writeRaster(wall_dtm, "out_wall_dtm.tif",
                           overwrite = TRUE)



#setting env with rsaga, the path is where you have downloaded rsaga to your
#computer and modules are found within the folder to rsaga. This might be specific
#to your computer so you might have to look up how to do this if this doesn't work.
env <- rsaga.env(
   path = "/Applications/SAGA.app/Contents/MacOS",
   modules = "/Applications/SAGA.app/Contents/libs"
)


# Write to a temporary file for SAGA to process for fill.sinks
writeRaster(out_wall_dtm, "temp_dem.sdat", overwrite = TRUE)

#to fill sinks
filled_dem <- rsaga.fill.sinks(
   in.dem = "temp_dem.sdat",
   out.dem = "filled_dem.sdat",
   minslope = .000001,
   method = "wang.liu.2006",
   env = env
)

#write out filled dem
filled_sdat_path <- rast("/Users/emily/Library/CloudStorage/OneDrive-UniversityofMassachusetts/salt_marsh_data/Testing repo/salt_marsh_work/hydrology/overland_flow/for overlflow output rasters/filled_dem.sdat")
writeRaster(filled_sdat_path, "filled_dem.tif", overwrite = TRUE)

#showing fill dem as a .tif
fill_dem<- terra::rast("/Users/emily/Library/CloudStorage/OneDrive-UniversityofMassachusetts/salt_marsh_data/Testing repo/salt_marsh_work/hydrology/overland_flow/for overlflow output rasters/filled_dem.tif")
plot(fill_dem)


####this stuff is to verify what to did above


##compared to prior filled dem
check_fill<- rast("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/SinksFilled_Red_May2022_CSF2012_Thin25cm_TriNN25cm.sdat")
plot(check_fill)

check_fill <- resample(check_fill, fill_dem) 
check_diff <- check_fill - fill_dem
plot(check_diff)
plot(check_diff < .005 & check_diff > - 0.005)

##compared to orginal dem
fill_depth <- fill_dem - dtm
plot(fill_depth)
plot(fill_depth != 0)




####carve channels section (Originally made for culvert but change object names
####just be aware to change it in other places in the script)


# filled dtm read in
dem_in <- terra::rast("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/Data_AllSites/Original Raster/UAS LiDAR DTM/RR/RR_26May2022_CSF2012_DTM_clipped_v3.tif")


###Identify all cells in the ditch (This method can also be used for burning in stream channels
#instead of a culvert/ditch)


#read in ditch (this was made in QGIS, a shapefile where the channel was)
cul_shp <- st_read("/Users/emily/Library/CloudStorage/OneDrive-UniversityofMassachusetts/salt_marsh_data/Testing repo/salt_marsh_work/hydrology/overland_flow/RR_culvert_cross.shp")

#covert to line to points
cul_shp_pts <- cul_shp|>
   st_cast ("POINT")

#define start point, depends on the first point of the channel 
#you are looking at(usually first row/id number, use whole numbers for slice 
#aka "id 1.64 = 65" or "1.23 = 24")
st_xy_cul <- cul_shp_pts |>
   slice(1)

#define end point, depends on the last point of the channel 
#you are looking at (number in slice corresponds to left-most id/row number in table)
en_xy_cul<- cul_shp_pts |>
   slice(4)

#split start coordinates into x and y columns
st_coords_cul <- st_coordinates(st_xy_cul) %>%
   cbind(id = st_xy_cul$id)

#split end points into x and y columns
en_coords_cul <- st_coordinates(en_xy_cul) %>%
   cbind(id = en_xy_cul$id)

#defining x and y points for everything
x1<- st_coords_cul[1,1]
x2<- en_coords_cul[1,1]

y1<- st_coords_cul[1,2]
y2<- en_coords_cul[1,2]


burn_culvert <- function(x1, x2, y1, y2, dem_in, dem_out){
   
   #Calculate length of ditch
   length <- sqrt((x2-x1)^2 + (y2 - y1)^2)
   
   #extract z coordinates (height values for elevation) from read in DEM, the values 
   #extracted will be at the start and end points that you made before 
   #Ex: (start = x1, y1) and (end = x2, y2)
   z1 <- terra::extract(dem_in , st_xy_cul, ID = FALSE)|>
      as.numeric()
   z2 <- terra::extract(dem_in , en_xy_cul, ID = FALSE)|>
      as.numeric()
   
   #Calculate elevation difference 
   height <- z2-z1
   
   #Rasterize line using DEM as reference to define geometry
   #add part to make a linestring from X1,2 and Y1,2 and switch out with cul_shp
   
   dem_out <- rasterize(cul_shp, dem_in , field = 1, touches = TRUE)
   
   # Convert ditch cells to points
   cul_points <- cells(dem_out)
   cul_cells <- which(values(dem_out) == 1)
   
   # Get coordinates of cell centers
   cul_coords <- xyFromCell(dem_out, cul_cells)
   
   #splitting coordinates into x and y values
   dx <- cul_coords[,1]
   dy <- cul_coords[,2]
   
   #Calculate distance of culvert from start
   dist <- sqrt((dx - x1)^2 + (dy - y1)^2)
   
   #Elevation of each point
   elev <- z1 + height * dist / length 
   print(elev)
   
   #Rasterize points into DEM using elevation as value for each cell
   dem_out <- rasterize(cul_coords, dem_in , values = elev, update = TRUE)
   
   #visualize the channel
   if (FALSE) {
      v <- values(dem_elev) 
      v[!is.na(v)]
      
      m <- leaflet()|>
         addProviderTiles("OpenStreetMap")|>
         addRasterImage(dem_out)
      m
   }
   
   return(dem_out)
}









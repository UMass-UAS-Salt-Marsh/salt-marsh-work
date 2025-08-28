library(terra)

#reading in DEM
dem <- terra::rast("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/Data_AllSites/Original Raster/
                   UAS Photogrammetry DEM/OTH/27Apr2021_OTH_Low_RGB_DEM.tif")


#for calculating slope 
terra::terrain(dem)

rast(dem)
#showing dem
plot(dem)

#All white parts are NA values - no need to interpolate na values (not sure)



library(terra)
library(sf)
library(stars)
library(RSAGA)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(tidyverse)


####Fillings sinks



#read in dtm
dtm <- terra::rast("/Users/emily/Library/CloudStorage/OneDrive-UniversityofMassachusetts/salt_marsh_data/Testing repo/salt_marsh_work/hydrology/overland_flow/for overlflow output rasters/RR_May_CSF2012_Thin25cm_TriNN25cm_Clipped_005.tif")

plot(dtm)

View(dtm)
#read in mask to make boundary line
boundary<- st_read("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/Lines_RR_Boundary_08Aug2019.shp")


#adding buffer
buffer<- 0.5
line_buf<- st_buffer(line_bound, buffer)

#read in outflow point
gap_vector<- st_read("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/Water Flow Routing/CSF2012_Thin25cm_TriNN25cm_005/Single_Outlet_Point.shp")

plot(gap_vector)
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

#setting env with rsaga (works)
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

plot(sqrt(fill_depth))

plot(hist(fill_depth))

or_dem <- rast("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/RR_May_CSF2012_Thin25cm_TriNN25cm_Clipped_005.tif")
cross <- st_read("/Users/emily/Library/CloudStorage/OneDrive-UniversityofMassachusetts/salt_marsh_data/Testing repo/salt_marsh_work/hydrology/overland_flow/RR_culvert_cross.shp")

cross_raster <- rasterize(cross, fill_depth, touches = TRUE)
sum(values(cross_raster) == 1, na.rm = TRUE) 
plot(cross_raster)
plot(or_dem)
crs(or_dem)

crs(fill_dem)




####carve channels section


library(terra)
library(sf)
library(dplyr)

# filled dtm read in
dem_in <- terra::rast("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/Data_AllSites/Original Raster/UAS LiDAR DTM/RR/RR_26May2022_CSF2012_DTM_clipped_v3.tif")

#read in channel network 
stream_net <- st_read("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/Water Flow Routing/CSF2012_Thin25cm_TriNN25cm_005/Channels_005.shp")
View(stream_net)
plot(stream_net)





####define start and end points of a specific channel (BELOW) 


#define a specific channel (SEGMENT_ID is the channel number)
channel <- filter(stream_net, SEGMENT_ID == 1)

#converts line to point values and geometry values
points <- channel |>
   st_cast("POINT")

#define start point, depends on the first point of the channel 
#you are looking at(usually first row/id number, use whole numbers for slice 
#aka "id 1.64 = 65" or "1.23 = 24")
st_xy <- points |>
   slice(1)
#define end point, depends on the last point of the channel 
#you are looking at (number in slice corresponds to left-most id/row number in table)
en_xy<- points |>
   slice(65)

#split start points into x and y points
st_coords <- st_coordinates(st_xy) %>%
   cbind(id = st_xy$id)
print(st_coords)

#using the printed values from the last chunk copy and paste the X and Y values
#to define them as X1 and Y1 (1 is for start point)
X1 <- 413216.4
Y1 <- 4613426


#split end points into x and y points
en_coords <- st_coordinates(en_xy) %>%
   cbind(id = en_xy$id)
print(en_coords)

#using the printed values from the last chunk copy and paste the X and Y values
#to define them as X2 and Y2 (2 is for end point)
X2 <- 413232.1
Y2 <- 4613429

#extract z coordinates (height values for elevation) from read in DEM, the values 
#extracted will be at the start and end points that you made before 
#Ex: (start = X1, Y1) and (end = X2, Y2)
Z1 <- terra::extract(dem_in , st_xy)
Z2 <- terra::extract(dem_in , en_xy)

#Calculate length of channel (for EM: the channel length from the collected table
#is a different length than what I calculated with line below 
#from table: "18.07107".   what I calculated: "15.98405")
ch_length <- sqrt((X2-X1)^2 + (Y2 - Y1)^2)
print(ch_length)

#Calculate elevation difference (for EM: is a negative number, should it be?)
height <- Z2-Z1
print(height)






###Identify all cells in the ditch


#read in ditch (this was made in QGIS, a shapefile where the ditch/culvert was)
cul_shp <- st_read("/Users/emily/Library/CloudStorage/OneDrive-UniversityofMassachusetts/salt_marsh_data/Testing repo/salt_marsh_work/hydrology/overland_flow/RR_culvert_cross.shp")
dem_in <- 

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

#split start points into x and y points
st_coords_cul <- st_coordinates(st_xy_cul) %>%
   cbind(id = st_xy_cul$id)
print(st_coords_cul)

#split end points into x and y points
en_coords_cul <- st_coordinates(en_xy_cul) %>%
   cbind(id = en_xy_cul$id)
print(en_coords_cul)

#defineing x and y points for everything
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
   
   cul_ras <- rasterize(cul_shp, dem_in , field = 1, touches = TRUE)
   
   # Convert ditch cells to points
   cul_points <- cells(cul_ras)
   
   # Get coordinates of cell centers
   cul_coords <- xyFromCell(cul_ras, cul_cells)
   
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
   
   #visualize
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








#######below all using flowdem package


library(flowdem);library(terra)

#Read the raster data
dem <- terra::rast("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/Data_AllSites/Original Raster/UAS LiDAR DTM/RR/RR_26May2022_CSF2012_DTM_clipped_v3.tif")
boundary<- st_read("/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share/UAS Data Collection/Red River/RR_Analysis/Hydrology/RR_WaterFlowRouting/Lines_RR_Boundary_08Aug2019.shp")

ggplot() +
   geom_spatraster(data = dem) +
   geom_sf(data = boundary) 
#Fill DEM and leave surfaces flat
dem_fill <- fill(dem, epsilon = FALSE)

#Fill DEM and apply a gradient on flat surfaces to ensure flow
#Note: When writing DEMs filled with epsilon to file, 
#set the datatype to FLT8S in terra::writeRaster()

dem_FLT<- writeRaster(dem_fill, "dem_FLT.tif", 
                      datatype = "FLT8S", 
                      overwrite = TRUE)

dem_fill_eps <- fill(dem_FLT, epsilon = TRUE)

#Calculate flowdirections
dem_dirs <- dirs(dem_FLT, mode="d8")

#Fill DEM and delineate coastal drainage basins simultaneously
dem_fill_basins <- fill_basins(dem_FLT)

#Breach DEM, that is, resolve depression by "carving" through obstacles
dem_breach <- breach(dem_FLT)

#Use fill with epsilon on breached DEM to resolve flats and ensure drainage
dem_breach_fill_eps <- fill(dem_breach, epsilon = TRUE)

plot(dem_breach_fill_eps)

#showing plots for Impact of breaching and Impact of Filling (https://github.com/KennethTM/flowdem)
dem_breach_diff <- dem - dem_breach
dem_breach_diff[dem_breach_diff == 0] <- NA


dem_fill_diff <- dem - dem_fill_eps
dem_fill_diff[dem_fill_diff == 0] <- NA

par(mfrow = c(1, 2))
plot(dem_breach_diff, main = "Impact of breaching", col=hcl.colors(50, rev = TRUE))
plot(dem_fill_diff, main = "Impact of filling", col=hcl.colors(50, rev = TRUE))   

#Get flow directions using the filled DEM
dem_dir <- dirs(dem_breach_fill_eps, mode="d8")

#Get flow accumulation
dem_acc <- accum(dem_dir, mode="d8")

#Flow accumulation can be used for stream delineation, 
#e.g using a threshold of 100 contributing cells (100*100*100 = 1 km2)
dem_streams <- dem_acc > 100

#plot of stream delineation (looks a bit out of place but want to check)
plot(dem_streams, col=c("grey", "dodgerblue"), legend=FALSE)

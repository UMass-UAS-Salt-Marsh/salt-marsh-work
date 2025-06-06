
# This is initial exploritory code to test R's lidar processing tools and
# figure out what our workflow should bd

library(lidR)


# St up a catalog in this case it's one image but it allows
# processing in chunks
ctg <- readLAScatalog("lidar/data/")

# Set (square) chunk size in meters
opt_chunk_size(ctg) <- 200
# And buffer to read around chunk when processing
opt_chunk_buffer(ctg) <- 20


# creates sidecar .lax index file - I don't think it uses
# or incorporates any of the chunk info but does speed up
# reads of parts of the file
lidR:::catalog_laxindex(ctg)

# Plot the chunks
plot(ctg, chunk_pattern = TRUE, mapview = TRUE)

# Set output file for processing
# function documentation sucks but from the examples:
#  {*} indicates input file name
#  {id}  indicates chunk id
#  {XLEFT}, {YBOTTOM}  coordinates of chunk edges
opt_output_files(ctg) <- ""  # lidar/data/output/{id}_{xleft}_{y_bottom}_class"



#
xll <- 413600
width <- 50
yll <- 4613600
height <- 50

focal <- lidR::clip_rectangle(ctg,xleft = xll, xright = xll + width,
                              ybottom = yll, ytop = yll + height )



# This algorythm calculates the mean distance to the k closest neighbors
# and if that distances is m times the mean distance for all points
# considers it an outlier.


# plot(noise, color = "Classification")
filtered <- focal |>
  filter_last() |>
  classify_noise(algorithm = sor(k=10, m = 10)) |>
  filter_poi(Classification != LASNOISE)

cat(npoints(focal) - npoints(filtered), " points removed\n")

# plot(filtered)

# Progressive morphological filter (pmf)
if(FALSE){

  ground <- classify_ground(filtered, algorithm = pmf(ws = 3, th = c(0.2)), last_returns = TRUE)
  plot(ground, color = "Classification")
}

# Cloth Simulation Filter
# Ward's thesis used three rounds of csf with parameters:
# resolution:  0.05, 0.10, 0.20
# threshold:  0.03,  0.06, 0.12
# iterations: 500, 500, 500
# resolution doubled in each round


res <- c(0.1, 0.1, 0.20)
threshold <- c(0.01, 0.06, 0.12)
rigidness <- 2


level <- 1
ground_class <- classify_ground(filtered, last_returns = TRUE,
                            algorithm = csf(class_threshold = threshold[level],
                                            cloth_resolution = res[level],
                                            rigidness = rigidness))
# plot(ground_class, color = "Classification")

ground <- filter_poi(ground_class, Classification == 2)
cat(nrow(ground_class) - nrow(ground), " non-ground points dropped")
cat(nrow(ground), " ground points")

# plot(ground)



# Horizontal and vertical transects for plotting
e <- ext(ground_class)
xmid <- mean(e[c(1, 2)])
ymid <- mean(e[c(3, 4)])
vp1 <- c(xmid, e[3])
vp2 <- c(xmid, e[4])
hp1 <- c(e[1], ymid)
hp2 <- c(e[2], ymid)
vtr <- clip_transect(ground_class, vp1, vp2, width = 0.1, xz = TRUE)
htr <- clip_transect(ground_class, hp1, hp2, width = 0.1, xz = TRUE)

# plot(htr, color = "Classification")

# Horizontal and vertical transects for plotting
e <- ext(ground_class)
xmid <- mean(e[c(1, 2)])
ymid <- mean(e[c(3, 4)])
vp1 <- c(xmid, e[3])
vp2 <- c(xmid, e[4])
hp1 <- c(e[1], ymid)
hp2 <- c(e[2], ymid)
vtr <- clip_transect(ground_class, vp1, vp2, width = 0.1, xz = TRUE)
htr <- clip_transect(ground_class, hp1, hp2, width = 0.1, xz = TRUE)

library(ggplot2)

htr |>
  payload() |>
  dplyr::mutate(Classification = as.factor(Classification)) |>
  ggplot(aes(X,Z, color = Classification)) +
  geom_point(size = 0.5)  +
  theme_minimal() +
  ggplot2::scale_color_manual(values =  c(rgb(1, 0, 0, 1),
                                          rgb(0,0,0, 1)))


library(terra)

rasterize_terrain(ground, res = 1, )





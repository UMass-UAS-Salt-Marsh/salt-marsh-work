
# This is initial exploratory code to test R's lidar processing tools and
# figure out what our workflow should be

library(lidR)

paths <- list()
paths$data_dir <- "lidar/data/"
paths$gcp <- "lidar/data/RedRiver_11May2022.csv"

# Start up a catalog in this case it's one image but it allows
# processing in chunks
ctg <- readLAScatalog(data_dir)

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



# This algorithm calculates the mean distance to the k closest neighbors
# and if that distances is m times the mean distance for all points
# considers it an outlier.


# plot(noise, color = "Classification")
filtered <- focal |>
  filter_last() |>
  classify_noise(algorithm = sor(k=10, m = 10)) |>
  filter_poi(Classification != LASNOISE)

n_lost <- npoints(focal) - npoints(filtered)
cat(n_lost, " points removed (", 
  round(n_lost / npoints(focal) * 100, 3), "%)\n")

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


res <- c(0.1, 0.1, 0.20, 0.05)
threshold <- c(0.01, 0.06, 0.12, .005)
rigidness <- 2


level <- 4
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
length <- 3

width = .1
xmid <- mean(e[c(1, 2)])
ymid <- mean(e[c(3, 4)])
vp1 <- c(xmid, e[3])
vp2 <- c(xmid, e[4])
hp1 <- c(e[1], ymid)
hp2 <- c(e[2], ymid)

# Whole focal tile
vtr <- clip_transect(ground_class, vp1, vp2, width = width, xz = TRUE)
htr <- clip_transect(ground_class, hp1, hp2, width = width, xz = TRUE)

# Narrow strip of defined length near center
vtr <- clip_transect(ground_class, c(xmid, ymid - length/2), c(xmid, ymid + length/2), width = width, xz = TRUE)
htr <- clip_transect(ground_class, c(xmid - length/2, ymid), c(xmid + length/2, ymid), width = width, xz = TRUE)
library(ggplot2)

htr |>
  payload() |>
  dplyr::mutate(Classification = as.factor(Classification)) |>
  ggplot(aes(X,Z, color = Classification)) +
  geom_point(size = 0.5)  +
  theme_minimal() +
  ggplot2::scale_color_manual(values =  c(rgb(1, 0, 0, 1),
                                          rgb(0,0,0, 1))) + 
  geom_vline(xintercept = seq(0, length, by = width), color = rgb(0, 0, 0, .2))
   
   
vtr |>
   payload() |>
   dplyr::mutate(Classification = as.factor(Classification)) |>
   ggplot(aes(X,Z, color = Classification)) +
   geom_point(size = 0.5)  +
   theme_minimal() +
   ggplot2::scale_color_manual(values =  c(rgb(1, 0, 0, 1),
                                           rgb(0,0,0, 1))) 

library(terra)

r <- rasterize_terrain(ground, res = .5, )



plot(r)


# Ground control points
gcp <- readr::read_csv(paths$gcp)





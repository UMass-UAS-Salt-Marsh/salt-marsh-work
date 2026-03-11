#  Start of running - code scrap

paths <- readr::read_csv("lidar/data/paths.csv")

paths$file_size <- file.size(paths$path)

path <- paths$path[paths$site == "nor"][1]

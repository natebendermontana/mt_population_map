remotes::install_github("https://github.com/tylermorganwall/rayshader")
#remotes::install_github("https://github.com/tylermorganwall/rayrender")
library(av)
library(here)
library(sf)
library(tigris)
library(tidyverse)
library(glue)
library(stars)
library(rayshader)
library(rayrender)
library(MetBrewer)
library(colorspace)
library(gifski)

data_path <- here("data", "kontur_population_US_20231101.gpkg")
data <- st_read(data_path)

# load states 
st <- states()

# Create a mask for MT
montana <- st |> 
  filter(NAME == "Montana") |> 
  st_transform(crs = st_crs(data))

# check with map
montana |>
  ggplot() +
  geom_sf()

# do intersection on data to limit kontur to florida
st_montana_temp <- st_intersection(data, montana)

st_montana_temp %>% 
  ggplot() +
  geom_sf()

# get state boundary
# address <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip"
# destdir <- tempdir()
# utils::download.file(file.path(address), zip_file <- tempfile())
# utils::unzip(zip_file, exdir = destdir)
# land <- st_read(glue("{destdir}/ne_10m_land.shp")) |>
#   st_transform(crs = st_crs(data))
# 
# mt_land <- st_intersection(st_montana_temp, land)
# mt_land %>% 
#   ggplot() +
#   geom_sf()
# 
# # original
# mt_buff <- mt_land |>
#   st_cast("MULTILINESTRING") |>
#   st_buffer(1609.34, endCapStyle = "FLAT", joinStyle = "MITRE") |>
#   mutate(population = 1) |>
#   rename(geom = geometry)

# new
mt_buff <- montana |>
  st_cast("MULTILINESTRING") |>
  st_buffer(1609.34, endCapStyle = "FLAT", joinStyle = "MITRE") |>
  mutate(population = 1) |>
  rename(geom = geometry)
mt_buff %>% 
  ggplot() +
  geom_sf()
###

mt_buff |>
  ggplot() +
  geom_sf()

st_montana <- bind_rows(st_montana_temp, mt_buff)

# define aspect ratio based on bounding box
bb <- st_bbox(st_montana)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# check by plotting points
st_montana |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = top_left) +
  geom_sf(data = bottom_right, color = "red")

# handle conditions of width or height being the longer side
if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

# convert to raster so we can then convert to matrix
size <- 1000 #5000

# original
# montana_rast <- st_rasterize(st_montana, 
#                              nx = floor(size * w_ratio),
#                              ny = floor(size * h_ratio))

# new
mt_rast <- st_rasterize(st_montana %>% 
                          select(population, geom),
                        nx = floor(size * w_ratio), ny = floor(size * h_ratio))


mat <- matrix(mt_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette
c1 <- met.brewer("Hiroshige")
c1 <- rev(c1)
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 3)(256)
swatchplot(texture)

# plot that 3d thing!
try(rgl::close3d())
mat |>
  height_shade(texture = texture) |>
  plot_3d(heightmap = mat,
          zscale = 35, #25 / 5
          solid = FALSE,
          shadowdepth = 0,
          windowsize = c(800, 800),
          background = "grey")

# set the camera details after building the 3d object in the window
render_camera(theta = -15, phi = 20, zoom = .9)


#### Create high-quality graphic ####

outfile <- "images/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    # creates a placeholder png file if outfile doesn't exist, to ensure a valid file exists for
    # render_highquality to write to
    png::writePNG(matrix(1), target = outfile) 
  }
  render_highquality(
    filename = outfile,
    # number of iterations; more increases the quality
    samples = 50,
    preview = F,
    interactive = FALSE,
    lightdirection = 210,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(600, 100),
    width = 800, #low end: 2500
    height = 800 #low end: 6000
  )
  end_time <- Sys.time()
  diff <- as.numeric(difftime(end_time, start_time, units = "mins"))
  cat(crayon::cyan(sprintf("Elapsed time: %.2f minutes", diff)), "\n")
}

#### Animated version ####
anim_outfile <- "images/test"

render_movie(
  filename = "images/test.mp4",
  type = "orbit",
  frames = 360*2,  # Number of frames in the animation
  fps = 30,
  phi = 25,
  theta = -20,
  zoom = .85
)

phi_vec <- seq(0, 25, length.out = 360*2)
theta_vec <- seq(0, -20, length.out = 360*2)

render_movie(
  filename = "images/test_custom.mp4",
  type = "custom",
  frames = 360*2,  # Number of frames in the animation
  fps = 30,
  phi = phi_vec,
  theta = theta_vec,
  zoom = .85
)

####
#### sample animation ####
####


render_camera(theta = -15, phi = 20, zoom = .9)

scene <- render_highquality(cache_filename = "mb1", 
                     return_scene = T)

camera_pos = list(c(100,100,100),c(100,100,200),c(100,100,300),c(100,100,350),c(100,100,400))

motion <- generate_camera_motion(positions = camera_pos, 
                                frames=120, type = "cubic")

anim_outfile <- "images/anim_test"


{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  render_animation(filename = anim_outfile,
                   scene=scene,
                   camera_motion = motion, 
                   #environment_light = "data/rosendal_park_sunset_4k.hdr",
                   samples=5,
                   width = 800,
                   height=800)
  
  av::av_encode_video(glue::glue("images/anim_test{1:60}.png"), 
                      output = "anim/anim_test.mp4", framerate = 60)
  end_time <- Sys.time()
  diff <- as.numeric(difftime(end_time, start_time, units = "mins"))
  cat(crayon::cyan(end_time), "\n")
  cat(crayon::cyan(sprintf("Elapsed time: %.2f minutes", diff)), "\n")
}

####


n_frames <- 2
zscale_val <- 35
zoom_val <- .9
theta_val <- seq(0, 360, length.out = n_frames)
phi_val <- 35

outdir <- "anim/"

# generate .png frame images
img_frames <- paste0(outdir, "drain", seq_len(n_frames), ".png")
{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    try(rgl::close3d())
    mat %>%
      height_shade(texture = texture) %>% 
      plot_3d(heightmap = mat,
              solid = FALSE,
              shadowdepth = 0,
              theta = theta_val[i],
              phi = phi_val,
              zscale = zscale_val,
              zoom = zoom_val,
              windowsize = c(800, 600),
              background = "white")
    #render_snapshot(img_frames[i])
    render_highquality(img_frames[i],
                       # number of iterations; more increases the quality
                       samples = 5,
                       preview = F,
                       interactive = FALSE,
                       lightdirection = 210,
                       lightaltitude = c(20, 80),
                       lightcolor = c(c1[2], "white"),
                       lightintensity = c(600, 100))
    rgl::clear3d()
    end_time <- Sys.time()
    diff <- as.numeric(difftime(end_time, start_time, units = "mins"))
    cat(crayon::cyan(end_time), "\n")
    cat(crayon::cyan(sprintf("Elapsed time: %.2f minutes", diff)), "\n")
  }
}
# build gif
magick::image_write_gif(magick::image_read(img_frames), 
                        path = "anim/test.gif", 
                        delay = 6/n_frames)




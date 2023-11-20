library(magick)
library(here)
library(MetBrewer)
library(colorspace)
library(glue)
library(stringr)

# load image
img_path <- here("images", "final_plot.png")
img <- image_read(img_path)

colors <- met.brewer("OKeeffe2")
swatchplot(colors)
text_color <- darken(colors[7], .25)
swatchplot(text_color)

# docktrin custom font not working. Something wrong with magick package setup? 
#fontpath <- here("data", "docktrin.ttf")

annot <- glue("This map shows the population density of Montana. ",
              "Population estimates are bucketed into 400 meter (about 1/4 mile) ",
              "hexagons.") %>% 
  str_wrap(50)

img %>% 
  image_crop(gravity = "center",
             geometry = "2500x2000") %>%
  image_annotate("Montana Population Density",
                 gravity = "northwest",
                 location = "+100+275",
                 color = text_color,
                 font = "Academy Engraved LET",
                 weight = 700,
                 size = 140
                 ) %>% 
  image_annotate(annot,
                 gravity = "center",
                 location = "+500+700",
                 color = text_color,
                 font = "Academy Engraved LET",
                 size = 50) %>%
  image_annotate("Data: Kontur Population (Released Nov 1, 2023)",
                 gravity = "west",
                 location = "+100+900",
                 font = "Academy Engraved LET",
                 color = alpha(text_color, .5),
                 size = 40) %>%
  image_annotate("Graphic by Nate Bender — tenpeaksbeforelunch.com",
                 gravity = "west",
                 location = "+100+950",
                 font = "Academy Engraved LET",
                 color = alpha(text_color, .5),
                 size = 40) %>%
  image_write("images/titled_final_plot.png")


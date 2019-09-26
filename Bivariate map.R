#Creating Bivariate map of vechile access and distance to store
#based on code by Time Grossenbacher

library(rstudioapi)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots ##I'm using 3.4, cowplot isn't available for this version, but I'm pretty sure that V3.5 will mess up my code.
library(rmarkdown)

# create 3 buckets for gini
quantiles_NoCar1mi <- atlas_Polk %>%
  pull(per_NoCar1mi) %>%
  quantile(probs = seq(0, 1, length.out = 3))
View(quantiles_PovertyRate)
# create 3 buckets for mean income
quantiles_PovertyRate <- atlas_Polk %>%
  pull(PovertyRate) %>%
  quantile(probs = seq(0, 1, length.out = 3))

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high inequality, high income
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low inequality, high income
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium inequality, medium income
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high inequality, low income
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low inequality, low income
) %>%
  gather("group", "fill")


# cut into groups defined above and join fill
Bivar_map <- atlas_Polk %>%
  mutate(
    NoCar1mi_quantiles = cut(
      per_NoCar1mi,
      breaks = quantiles_NoCar1mi,
      include.lowest = TRUE
    ),
    PovertyRate_quantiles = cut(
      PovertyRate,
      breaks = quantiles_PovertyRate,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(NoCar1mi_quantiles), "-",
      as.numeric(PovertyRate_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group") 
View(Bivar_map)

map <- ggplot(data = Bivar_map) +
geom_sf(
  aes(
    fill = fill
  ),
  # use thin white stroke for municipalities
  color = "white",
  size = 0.1
) +
  scale_fill_identity() +
  labs(title = "Intersection of Poverty and lack of Car")
  theme_minimal() +
  xlab(" ") + 
  ylab(" ") +
  geom_text_repel(data = Markers, aes(x = latitude, y = longitude, label = Markers$marker), size = 4,
                  point.padding = .25)
  
#Draw Legend
# separate the groups
  bivariate_color_scale %<>%
    separate(group, into = c("NoCar1mi", "PovertyRate"), sep = " - ") %>%
    mutate(NoCar1mi = as.integer(NoCar1mi),
           PovertyRate = as.integer(PovertyRate))
  
  View(bivariate_color_scale)
  
  legend <- ggplot() +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = NoCar1mi,
        y = PovertyRate,
        fill = fill)
    ) +
    scale_fill_identity() +
    labs(x = "Percentage without a car ??????",
         y = "Poverty Rate ??????") +
    theme_minimal() +
    # make font small enough
    theme(
      axis.title = element_text(size = 6)
    ) +
      # quadratic tiles
    coord_fixed()

  #Draw map and legend
  ggdraw() +
    draw_plot(Bivar_map, 0, 0, 1, 1) +
    draw_plot(legend, 0.05, 0.075, 0.2, 0.2)  
  # error: no package called cowplot??
  
  ggplot(data = Bivar_map) +
    geom_sf(
      aes(
        fill = fill
      ),
      # use thin white stroke for municipalities
      color = "white",
      size = 0.1
    ) +
    scale_fill_identity() +
    labs(title = "Intersection of Poverty and lack of Car") +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = NoCar1mi,
        y = PovertyRate,
        fill = fill))
      
    #legend addition 
    ggplot() +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = NoCar1mi,
        y = PovertyRate,
        fill = fill)
    ) +
    scale_fill_identity() +
    labs(x = "Percentage without a car ??????",
         y = "Poverty Rate ??????") +
    # make font small enough
    theme(
      axis.title = element_text(size = 6)
    ) +
    # quadratic tiles
    coord_fixed()
  
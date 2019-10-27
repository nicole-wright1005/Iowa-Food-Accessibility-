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
library(tidycensus)
library(ggrepel)

census_api_key("6bfbcee4b4c37a9683ac9c0e1d174d5f0ad67674", install = TRUE, overwrite = TRUE)
census_key <- "6bfbcee4b4c37a9683ac9c0e1d174d5f0ad67674"

geo_data_tract <- get_decennial(geography = "tract", 
                                variables = "P001001", 
                                state = "IA",
                                year = 2010, 
                                geometry = TRUE, 
                                cache_table = TRUE, 
                                shift_geo = FALSE, 
                                key = census_key) 


atlas <- read_csv("D:/OAITI/Food_Atlas/data/Food_Access_Atlas.csv") %>%
  filter(State == "Iowa") %>%
  rename(GEOID = CensusTract) %>%
  left_join(geo_data_tract %>%
              dplyr::select(GEOID, geometry), by = "GEOID") 

atlas_Polk <- atlas %>%
  dplyr::select(GEOID, State, County, Urban, POP2010, PovertyRate, lahunv1share, lablack1share, HUNVFlag, LILATracts_Vehicle, TractHUNV, TractLOWI, LILATracts_1And10, geometry) %>%
  filter(County == "Polk") %>%
  mutate(PR = cut(PovertyRate, breaks = c(0, 5, 10, 20, 30, 40, 50),
                  labels = c("<5", "5-10", "10-20", "20-30", "30-40", "40-50"))) %>%
  mutate(per_NoCar1mi = lahunv1share * 100,  
         FD1 = cut(per_NoCar1mi, breaks = c(0.00000, 2.5, 5, 10),
                   labels = c("<2.5", "2.5-5", "5-10")))
##Add more regions...
Markers <- tribble(
  ~marker, ~latitude, ~longitude,
  "Capital", -93.63035, 41.586693)

Markers_sf <- Markers %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

##No car and >1mi from supermarket == Low Food Access
# create 3 buckets for No car and >1mi from
quantiles_NoCar1mi <- atlas_Polk %>%
  pull(per_NoCar1mi) %>%
  quantile(probs = c(0, .5, .75, 1))

# create 3 buckets forPoverty Rate (PR)
quantiles_PovertyRate <- atlas_Polk %>%
  pull(PovertyRate) %>%
  quantile(probs =c(0, .5, .75, 1))

# create color scale that encodes two variables
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#574249", # High Low Food Access (LFA)
  "2 - 3" = "#985356",
  "1 - 3" = "#c85a5a", # low LFA, high Poverty Rate (PR)
  "3 - 2" = "#627f8c",
  "2 - 2" = "#ad9ea5", # medium LFA, medium PR
  "1 - 2" = "#e4acac",
  "3 - 1" = "#64acbe", # high LFA, low PR
  "2 - 1" = "#b0d5df",
  "1 - 1" = "#e8e8e8" # low LFA, low PR
) %>%
  gather("group", "fill")
#e8e8e8
# cut into groups defined above and join fill
Bivar_map <- atlas_Polk %>%
  mutate(NoCar1mi_quantiles = cut(
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
  # so each municipality knows its hex value based on the combined LFA and PR
  # income value
  left_join(bivariate_color_scale, by = "group") 

b_map <- ggplot(data = Bivar_map) +
geom_sf(
  aes(geometry = geometry,
    fill = fill
  ),
  # use thin white stroke for municipalities
  color = "white",
  size = 0.1
) +
  scale_fill_identity() +
  labs(title = "Intersection of Poverty and Low Food Access for Polk County",
       caption = "Data from 2015 Food Access Research Atlas available at ers.usda.gov") +
  theme_minimal() +
  xlab(" ") + 
  ylab(" ") +
  theme(
    axis.text = element_blank(), panel.grid = element_blank()
  )

##Where is my point??

#Draw Legend
# separate the groups
  bivariate_color_scale %<>%
    separate(group, into = c("NoCar1mi", "PovertyRate"), sep = " - ") %>%
    mutate(NoCar1mi = as.integer(NoCar1mi),
           PovertyRate = as.integer(PovertyRate))
  

 l_plot <- ggplot() +
   geom_tile(
    data = bivariate_color_scale,
      mapping = aes(
        x = NoCar1mi,
        y = PovertyRate,
        fill = fill)
    ) +
   scale_fill_identity() +
   labs(x = "Low Food Access",
        y = "Poverty Rate") +
   theme_minimal() +
   theme(axis.title = element_text(size = 8))
   
 
c_map <- b_map + 
   geom_sf(data = DM_poly_test, alpha = 0, size = .25) 

ggdraw() +
  draw_plot(c_map, 0, 0, 1, 1) +
  draw_plot(l_plot, 0.06, 0.035, 0.2, 0.2) 

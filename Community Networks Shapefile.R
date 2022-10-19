setwd("C:\\Users\\user\\path\\to\\communitynetworksdashboard")

library(data.table)
library(tigris)
library(tidyverse)
library(readxl)
library(sf)
library(s2)
s2_available = !inherits(try(sf_use_s2(TRUE), silent=TRUE), "try-error")
library(leaflet)
library(textclean)

##Import Files

Community_Networks <- read_excel("FINAL_SNAP-Ed Community Networks_5.13.21.xlsx", sheet = "Networks")

IL_tracts_sf <- tracts(state = "IL", year = "2020")

IL_counties_sf <- counties(state = "IL", year = "2020")

IL_places_sf <- places(state = "IL", year = "2020")

Unit_Regions <- read_excel("SNAP-Ed Regions GIS.xlsx")

#Create Network Shapefiles

IL_places_sf$NAME <- gsub('De Pue', 'DePue', IL_places_sf$NAME, fixed=TRUE)

IL_places_sf <- IL_places_sf %>% filter(GEOID != 1728950) #Remove Georgetown near Macomb from Danville Area network (inconsequential dupe names: Willowbrook, Wilmington, Windsor)

commuity_network_cols <- c("ID#", "Name of the SNAP-Ed Community Network", "Unit", "Reshaped Census Tracts", "Updated City/County descriptors", "Is City Network", "Is County Network", "Network, no programming")

reshaped_community_networks <- Community_Networks[commuity_network_cols] %>%
  filter(!is.na(Community_Networks$`Reshaped Census Tracts`)) %>%
  separate_rows("Reshaped Census Tracts", sep=",")
reshaped_community_networks$"Reshaped Census Tracts" <- trimws(reshaped_community_networks$"Reshaped Census Tracts", which = c("both"))
reshaped_community_networks <- inner_join(IL_tracts_sf, reshaped_community_networks, by = c("GEOID" = "Reshaped Census Tracts"))

reshaped_community_networks <- reshaped_community_networks %>% rename(network_name = "Name of the SNAP-Ed Community Network")

reshaped_community_networks2 <- reshaped_community_networks  %>%
  group_by(network_name) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() 

FCS_Community_Networks <- Community_Networks %>% separate_rows("Updated City/County descriptors", sep=",")

FCS_Community_Networks$`Updated City/County descriptors` <- trimws(FCS_Community_Networks$`Updated City/County descriptors`, which = c("both"))

county_networks <- filter(FCS_Community_Networks[commuity_network_cols], FCS_Community_Networks$`Is County Network` == "Y" &
                            is.na(FCS_Community_Networks$"Reshaped Census Tracts"))
county_networks$`Updated City/County descriptors` <- gsub(' County', '', county_networks$`Updated City/County descriptors`, fixed=TRUE)

county_networks <- inner_join(IL_counties_sf, county_networks, by = c("NAME" = "Updated City/County descriptors"))

city_networks <- filter(FCS_Community_Networks[commuity_network_cols], FCS_Community_Networks$`Is City Network` == "Y" &
                          is.na(FCS_Community_Networks$"Reshaped Census Tracts"))
city_networks <- inner_join(IL_places_sf, city_networks, by = c("NAME" = "Updated City/County descriptors"))

network_cols1 <- c("NAME", "ID#", "Name of the SNAP-Ed Community Network", "Unit", "Is City Network", "Is County Network", "Network, no programming", "geometry")       
fcs_networks_sf <- rbind(county_networks[network_cols1], city_networks[network_cols1])

fcs_networks_sf <-  fcs_networks_sf %>% rename(network_name = "Name of the SNAP-Ed Community Network")

fcs_networks_sf2 <- fcs_networks_sf  %>%
  group_by(network_name) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

snap_ed_networks_sf  <- rbind(fcs_networks_sf2, reshaped_community_networks2)

snap_ed_networks_sf  <- left_join(snap_ed_networks_sf, Community_Networks[commuity_network_cols], by = c(network_name = "Name of the SNAP-Ed Community Network"))

snap_ed_networks_sf  <- left_join(snap_ed_networks_sf, Unit_Regions[c("Unit number", "Region")] %>% distinct(), by = c("Unit"= "Unit number"))

names(snap_ed_networks_sf) <- mgsub(
  colnames(snap_ed_networks_sf),
  c(
    "ID#",
    "Unit",
    "Reshaped Census Tracts",
    "Is City Network",
    "Is County Network",
    "Updated City/County descriptors",
    "Network, no programming",
    "Region"
  ),
  c(
    "id",
    "unit",
    "census_tracts",
    "city_network",
    "county_network",
    "cities_counties",
    "no_programming",
    "region"
  )
)

snap_ed_networks_sf[!is.na(snap_ed_networks_sf$census_tracts), "cities_counties"] <- NA

snap_ed_networks_sf <- snap_ed_networks_sf %>% mutate(city_network = if_else(city_network == "Y", TRUE, FALSE, missing = FALSE))

snap_ed_networks_sf <- snap_ed_networks_sf %>% mutate(county_network = if_else(county_network == "Y", TRUE, FALSE, missing = FALSE))

snap_ed_networks_sf <- snap_ed_networks_sf %>% select(- no_programming) #networks without programming haven't been reshaped

#Works with warnings

#dir.create("snap_ed_networks_sf.shp")

#st_write(snap_ed_networks_sf, "snap_ed_networks_sf.shp/snap_ed_networks_sf.shp", delete_layer = TRUE)


# Works without warnings

st_write(obj = snap_ed_networks_sf, dsn = "snap_ed_networks_sf.gpkg", layer = 'snap_ed_networks_sf', delete_layer = TRUE)

# sf1 <- st_read("./snap_ed_networks_sf_old.gpkg")
# 
# sf2 <- st_read("./snap_ed_networks_sf.gpkg")
# 
# all.equal(sf1, sf2)

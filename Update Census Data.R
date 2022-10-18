setwd("C:\\Users\\user\\path\\to\\communitynetworksdashboard")

library(tidycensus)
library(tigris)
library(readxl)
# library(data.table)
library(dplyr)
library(stringr)
library(tidyr)        
library(sf)
library(s2)
s2_available = !inherits(try(sf_use_s2(TRUE), silent=TRUE), "try-error")
library(textclean)
library(arrow)

out_dir <- "./Community Networks Dashboard - FY22/"


# snap_ed_networks_sf

## Import Files

Community_Networks <- read_excel("FINAL_SNAP-Ed Community Networks_5.13.21.xlsx", sheet = "Networks")

IL_tracts_sf <- tracts(state = "IL", year = "2020")

IL_counties_sf <- counties(state = "IL", year = "2020")

IL_places_sf <- places(state = "IL", year = "2020")

Unit_Regions <- read_excel("SNAP-Ed Regions GIS.xlsx")

# Create Network Shapefiles

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

# Shapefile without Census data merged

# st_write(obj = snap_ed_networks_sf, dsn = "snap_ed_networks_sf.gpkg", layer = 'snap_ed_networks_sf', delete_layer = TRUE)


# Import S1701 Poverty data

acs_st_vars_lookup <- load_variables(2020, "acs5/subject", cache = TRUE)

S1701_var_ids <-
  data.frame(
    name_e =
      c(
        "S1701_C01_001E",
        "S1701_C01_041E",
        "S1701_C01_003E",
        "S1701_C01_004E",
        "S1701_C01_007E",
        "S1701_C01_008E",
        "S1701_C01_010E",
        "S1701_C02_003E",
        "S1701_C02_004E",
        "S1701_C02_007E",
        "S1701_C02_008E",
        "S1701_C02_010E",
        "S1701_C01_011E",
        "S1701_C01_012E",
        "S1701_C02_011E",
        "S1701_C02_012E",
        "S1701_C01_013E",
        "S1701_C01_014E",
        "S1701_C01_015E",
        "S1701_C01_016E",
        "S1701_C01_017E",
        "S1701_C01_018E",
        "S1701_C01_019E",
        "S1701_C01_020E",
        "S1701_C02_001E",
        "S1701_C02_013E",
        "S1701_C02_014E",
        "S1701_C02_015E",
        "S1701_C02_016E",
        "S1701_C02_017E",
        "S1701_C02_018E",
        "S1701_C02_019E",
        "S1701_C02_020E"
      )
  )

S1701_var_ids$name <- str_sub(S1701_var_ids$name, 1, end=-2)

S1701_Poverty_Tracts <- get_acs(
  geography = "tract",
  variables = S1701_var_ids$name,
  state = "IL",
  year = 2020,
  output = "wide"
) %>% 
  select(-ends_with("M"))

S1701_var_labels <- left_join(S1701_var_ids, acs_st_vars_lookup , by = "name")$label

S1701_Poverty_Tracts <-
  S1701_Poverty_Tracts %>% rename_at(vars(S1701_var_ids$name_e), ~S1701_var_labels)

S1701_Poverty_Places <-
  get_acs(
    geography = "place",
    variables = S1701_var_ids$name,
    state = "IL",
    year = 2020,
    output = "wide"
  ) %>% 
  select(-ends_with("M"))

S1701_Poverty_Places <-
  S1701_Poverty_Places %>% rename_at(vars(S1701_var_ids$name_e), ~S1701_var_labels)

unit_counties <- read_excel("C:\\Users\\jstadni2\\Box\\FCS Data Analyst\\Illinois Extension Unit Counties.xlsx", sheet = "Unit Counties")
unit_counties$County  <- gsub('DeWitt', 'De Witt', unit_counties$County, fixed=TRUE)
unit_counties <- unit_counties %>% mutate(County = replace(County, County=="Dupage", "DuPage"))

# SNAP Eligible Individuals/% Tracts Layer

S1701_Poverty_Tracts <-
  S1701_Poverty_Tracts %>% rename(
    total_population = 'Estimate!!Total!!Population for whom poverty status is determined',
    individuals_income_below_185_percent_poverty_level = 'Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level',
    total_population_under_5_years = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years',
    total_population_5_to_17_years = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years',
    total_population_18_to_34_years = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years',
    total_population_35_to_64_years = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years',
    total_population_65_years_and_over = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over',
    below_poverty_level_under_5_years = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years',
    below_poverty_level_5_to_17_years = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years',
    below_poverty_level_18_to_34_years = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years',
    below_poverty_level_35_to_64_years = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years',
    below_poverty_level_65_years_and_over = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over',
    total_population_male = 'Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male',
    total_population_female = 'Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female',
    below_poverty_level_male = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male',
    below_poverty_level_female = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female',
    total_population_white = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone',
    total_population_black_or_african_american = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone',
    total_population_american_indian_and_alaska_native = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone',
    total_population_asian = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone',
    total_population_native_hawaiian_and_other_pacific_islander = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone',
    total_population_some_other_race = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone',
    total_population_two_or_more_races = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races',
    total_population_hispanic_or_latino_origin = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)',
    below_poverty_level_total_population = 'Estimate!!Below poverty level!!Population for whom poverty status is determined',
    below_poverty_white = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone',
    below_poverty_black_or_african_american = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone',
    below_poverty_american_indian_and_alaska_native = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone',
    below_poverty_asian = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone',
    below_poverty_native_hawaiian_and_other_pacific_islander = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone',
    below_poverty_some_other_race = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone',
    below_poverty_two_or_more_races = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races',
    below_poverty_hispanic_or_latino_origin = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)'
  ) %>%
  separate('NAME',
           c("census_tract", "county", "state"),
           sep = ", ")

S1701_Poverty_Tracts$county <- gsub(' County', '', S1701_Poverty_Tracts$county, fixed=TRUE)

C_Map_Poverty <- S1701_Poverty_Tracts[ ,c('GEOID',
                                          'census_tract',
                                          'county',
                                          'state',
                                          'total_population',
                                          'individuals_income_below_185_percent_poverty_level')]

C_Map_Poverty$census_tract <- gsub('Census Tract ', '', C_Map_Poverty$census_tract, fixed=TRUE)


C_Map_Poverty$snap_eligibility_percent<- round(100 * (C_Map_Poverty$individuals_income_below_185_percent_poverty_level / C_Map_Poverty$total_population))

IL_tracts_sf_merged <- merge(IL_tracts_sf, C_Map_Poverty, by = "GEOID")

IL_tracts_sf_merged <- IL_tracts_sf_merged %>% select(all_of(c("GEOID", "county", "census_tract", "individuals_income_below_185_percent_poverty_level", "total_population", "snap_eligibility_percent")))

st_write(obj = IL_tracts_sf_merged, dsn = paste0(out_dir, "IL_tracts_sf_merged.gpkg"), layer = 'IL_tracts_sf_merged', delete_layer = TRUE)

# Test IL_tracts_sf_merged 

# all.equal(st_read("./Community Networks Dashboard - FY22/IL_tracts_sf_merged.gpkg"), IL_tracts_sf_merged)

# SNAP-Ed Community Networks Layer

reshaped_networks <- select(as.data.frame(snap_ed_networks_sf[, c("census_tracts", "network_name", "unit")]), -geometry) %>%
  filter(!is.na(census_tracts)) %>%
  separate_rows(census_tracts, sep=", ")

# reshaped_networks$AFFGEOID <- paste0("1400000US", reshaped_networks$census_tracts)

reshaped_networks_eligbility <-
  left_join(S1701_Poverty_Tracts, reshaped_networks, by = c("GEOID" = "census_tracts")) %>%
  filter(!is.na(network_name)) %>%
  group_by(network_name) %>%
  summarise(
    total_population = sum(total_population),
    individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)
  ) %>% ungroup()

reshaped_networks_eligbility$snap_eligibility_percent <- round(100 * reshaped_networks_eligbility$individuals_income_below_185_percent_poverty_level / reshaped_networks_eligbility$total_population)

county_networks <- select(as.data.frame(snap_ed_networks_sf[snap_ed_networks_sf$county_network == TRUE, c("network_name", "cities_counties", "unit")]), -geometry) #%>%
#separate_rows(cities_counties, sep=", ")

county_networks$cities_counties <- gsub(' County', '', county_networks$cities_counties)

county_networks_eligbility <-
  left_join(S1701_Poverty_Tracts, county_networks, by = c("county" = "cities_counties")) %>%
  filter(!is.na(network_name)) %>%
  group_by(network_name) %>%
  summarise(
    total_population = sum(total_population),
    individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)
  ) %>% ungroup()

county_networks_eligbility$snap_eligibility_percent <- round(100 * county_networks_eligbility$individuals_income_below_185_percent_poverty_level / county_networks_eligbility$total_population)

S1701_Poverty_Places <-
  S1701_Poverty_Places %>% rename(
    total_population = 'Estimate!!Total!!Population for whom poverty status is determined',
    individuals_income_below_185_percent_poverty_level = 'Estimate!!Total!!Population for whom poverty status is determined!!ALL INDIVIDUALS WITH INCOME BELOW THE FOLLOWING POVERTY RATIOS!!185 percent of poverty level',
    total_population_under_5_years = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years',
    total_population_5_to_17_years = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years',
    total_population_18_to_34_years = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years',
    total_population_35_to_64_years = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years',
    total_population_65_years_and_over = 'Estimate!!Total!!Population for whom poverty status is determined!!AGE!!65 years and over',
    below_poverty_level_under_5_years = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!Under 5 years',
    below_poverty_level_5_to_17_years = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!Under 18 years!!5 to 17 years',
    below_poverty_level_18_to_34_years = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!18 to 34 years',
    below_poverty_level_35_to_64_years = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!18 to 64 years!!35 to 64 years',
    below_poverty_level_65_years_and_over = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!AGE!!65 years and over',
    total_population_male = 'Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Male',
    total_population_female = 'Estimate!!Total!!Population for whom poverty status is determined!!SEX!!Female',
    below_poverty_level_male = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Male',
    below_poverty_level_female = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!SEX!!Female',
    total_population_white = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone',
    total_population_black_or_african_american = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone',
    total_population_american_indian_and_alaska_native = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone',
    total_population_asian = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone',
    total_population_native_hawaiian_and_other_pacific_islander = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone',
    total_population_some_other_race = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone',
    total_population_two_or_more_races = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races',
    total_population_hispanic_or_latino_origin = 'Estimate!!Total!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)',
    below_poverty_level_total_population = 'Estimate!!Below poverty level!!Population for whom poverty status is determined',
    below_poverty_white = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!White alone',
    below_poverty_black_or_african_american = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Black or African American alone',
    below_poverty_american_indian_and_alaska_native = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!American Indian and Alaska Native alone',
    below_poverty_asian = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Asian alone',
    below_poverty_native_hawaiian_and_other_pacific_islander = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Native Hawaiian and Other Pacific Islander alone',
    below_poverty_some_other_race = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Some other race alone',
    below_poverty_two_or_more_races = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Two or more races',
    below_poverty_hispanic_or_latino_origin = 'Estimate!!Below poverty level!!Population for whom poverty status is determined!!RACE AND HISPANIC OR LATINO ORIGIN!!Hispanic or Latino origin (of any race)'
  ) %>%
  separate('NAME',
           c("geographic_area_name", "state"),
           sep = ", ")

S1701_Poverty_Places$geographic_area_name <- mgsub(S1701_Poverty_Places$geographic_area_name, c(" city", " CDP", " village", " town"), "")
S1701_Poverty_Places$geographic_area_name  <- gsub('De Pue', 'DePue', S1701_Poverty_Places$geographic_area_name, fixed=TRUE)
S1701_Poverty_Places <- S1701_Poverty_Places %>% filter(GEOID != "1728950") #Remove Georgetown near Macomb from Danville Area network (inconsequential dupe names: Willowbrook, Wilmington, Windsor)

city_networks <- select(as.data.frame(snap_ed_networks_sf[snap_ed_networks_sf$city_network == TRUE, c("network_name", "cities_counties", "unit")]), -geometry) %>%
  separate_rows(cities_counties, sep=", ")

city_networks_eligbility <-
  left_join(S1701_Poverty_Places, city_networks, by = c("geographic_area_name" = "cities_counties")) %>%
  filter(!is.na(network_name)) %>%
  group_by(network_name) %>%
  summarise(
    total_population = sum(total_population),
    individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)
  ) %>% ungroup()

city_networks_eligbility$snap_eligibility_percent <- round(100 * city_networks_eligbility$individuals_income_below_185_percent_poverty_level / city_networks_eligbility$total_population)

networks_eligbility <- rbind(reshaped_networks_eligbility, county_networks_eligbility)
networks_eligbility <- rbind(networks_eligbility , city_networks_eligbility)

snap_ed_networks_sf <-
  snap_ed_networks_sf %>% left_join(networks_eligbility , by = c("network_name"))

st_write(obj = snap_ed_networks_sf, dsn = paste0(out_dir, "snap_ed_networks_sf.gpkg"), layer = 'snap_ed_networks_sf', delete_layer = TRUE)

# Test snap_ed_networks_sf

# all.equal(st_read("./Community Networks Dashboard - FY22/snap_ed_networks_sf.gpkg"), snap_ed_networks_sf)


# Import community profile data

S2201_var_ids <-
  data.frame(
    name_e =
      c(
        "S2201_C03_001E",
        "S2201_C03_025E",
        "S2201_C03_026E",
        "S2201_C03_027E",
        "S2201_C03_028E",
        "S2201_C03_029E",
        "S2201_C03_030E",
        "S2201_C03_031E",
        "S2201_C03_032E"
      )
  )

S2201_var_ids$name <- str_sub(S2201_var_ids$name, 1, end=-2)

S2201_SNAP_Tracts <-
  get_acs(
    geography = "tract",
    variables = S2201_var_ids$name,
    state = "IL",
    year = 2020,
    output = "wide"
  ) %>% 
  select(-ends_with("M"))

S2201_var_labels <- left_join(S2201_var_ids, acs_st_vars_lookup , by = "name")$label

S2201_SNAP_Tracts <-
  S2201_SNAP_Tracts %>% rename_at(vars(S2201_var_ids$name_e), ~S2201_var_labels)

S2201_SNAP_Places <-
  get_acs(
    geography = "place",
    variables = S2201_var_ids$name,
    state = "IL",
    year = 2020,
    output = "wide"
  ) %>% 
  select(-ends_with("M"))

S2201_SNAP_Places <-
  S2201_SNAP_Places %>% rename_at(vars(S2201_var_ids$name_e), ~S2201_var_labels)

S1602_var_ids <-
  data.frame(
    name_e =
      c(
        "S1602_C01_001E",
        "S1602_C03_001E",
        "S1602_C03_002E",
        "S1602_C03_003E",
        "S1602_C03_004E",
        "S1602_C03_005E"
      )
  )

S1602_var_ids$name <- str_sub(S1602_var_ids$name, 1, end=-2)

S1602_LEP_Tracts <-
  get_acs(
    geography = "tract",
    variables = S1602_var_ids$name,
    state = "IL",
    year = 2020,
    output = "wide"
  ) %>% 
  select(-ends_with("M"))

S1602_var_labels <- left_join(S1602_var_ids, acs_st_vars_lookup , by = "name")$label

S1602_LEP_Tracts <-
  S1602_LEP_Tracts %>% rename_at(vars(S1602_var_ids$name_e), ~S1602_var_labels)

S1602_LEP_Places <-
  get_acs(
    geography = "place",
    variables = S1602_var_ids$name,
    state = "IL",
    year = 2020,
    output = "wide"
  ) %>% 
  select(-ends_with("M"))

S1602_LEP_Places <-
  S1602_LEP_Places %>% rename_at(vars(S1602_var_ids$name_e), ~S1602_var_labels)

## Community Profile

### SNAP Recipient Households by Race/Ethnicity

S2201_var_labels <- left_join(S2201_var_ids, acs_st_vars_lookup , by = "name")$label

names(S2201_SNAP_Tracts) <- mgsub(colnames(S2201_SNAP_Tracts), c(" alone",
                                                                 " (of any race)",
                                                                 "Estimate!!Households receiving food stamps/SNAP!!",
                                                                 "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!",
                                                                 " "),
                                  c("",
                                    "",
                                    "total_snap_recipient_households_",
                                    "total_snap_recipient_households_",
                                    "_"))
names(S2201_SNAP_Tracts) <- tolower(names(S2201_SNAP_Tracts))

snap_recipient_households_demo_tracts <- left_join(S2201_SNAP_Tracts, reshaped_networks, by = c("geoid" = "census_tracts"))

snap_recipient_households_demo_tracts$name <- snap_recipient_households_demo_tracts$name %>% mgsub(c("Census Tract ", " County"	), "")
snap_recipient_households_demo_tracts  <- snap_recipient_households_demo_tracts  %>% separate('name', c("census_tract","county", "state"), sep = ", ") 

snap_recipient_households_demo_tracts <- left_join(snap_recipient_households_demo_tracts, county_networks, by = c("county" = "cities_counties"))

snap_recipient_households_demo_tracts <-
  snap_recipient_households_demo_tracts %>% mutate(
    network_name.x = ifelse(is.na(network_name.x), network_name.y, network_name.x),
    unit.x = ifelse(is.na(unit.x), unit.y, unit.x)
  ) %>%
  select(-c(network_name.y, unit.y)) %>%
  rename(network_name = 'network_name.x',
         unit = 'unit.x')

snap_recipient_households_demo_tracts$total_snap_recipient_households_no_hispanic_or_latino_origin <- snap_recipient_households_demo_tracts$total_snap_recipient_households_households - snap_recipient_households_demo_tracts$total_snap_recipient_households_hispanic_or_latino_origin

snap_recipient_households_demo_tracts <- snap_recipient_households_demo_tracts %>%
  pivot_longer(
    cols = starts_with("total_snap_recipient_households_"),
    names_to = "demo",
    names_prefix = "total_snap_recipient_households_",
    values_to = "total_snap_recipient_households",
    values_drop_na = TRUE
  )

snap_recipient_households_demo_tracts <- left_join(snap_recipient_households_demo_tracts[snap_recipient_households_demo_tracts$demo != "households", ],
                                                   S2201_SNAP_Tracts[, c("geoid", "total_snap_recipient_households_households")],
                                                   by = c("geoid")) %>%
  rename(snap_recipient_households = total_snap_recipient_households,
         total_snap_recipient_households = total_snap_recipient_households_households)

snap_recipient_households_demo_tracts$demo <- gsub("_", " ", snap_recipient_households_demo_tracts$demo) %>% str_to_title()


snap_recipient_households_demo_tracts <- left_join(snap_recipient_households_demo_tracts, unit_counties, by = c("county" = "County"))

snap_recipient_households_demo_tracts <- snap_recipient_households_demo_tracts %>% mutate(unit = ifelse(is.na(unit), as.character(snap_recipient_households_demo_tracts$"Unit #"), unit)) %>%
  select(- "Unit #")

write_feather(
  snap_recipient_households_demo_tracts,
  paste0(out_dir, "snap_recipient_households_demo_tracts.feather"),
)

# Test snap_recipient_households_demo_tracts

# all.equal(
#   read_feather(paste0(out_dir, "snap_recipient_households_demo_tracts.feather")),
#   snap_recipient_households_demo_tracts
#   )

S2201_SNAP_Places <- S2201_SNAP_Places  %>% separate(NAME, c("geographic_area_name", "state"), sep = ", ")

names(S2201_SNAP_Places) <- mgsub(colnames(S2201_SNAP_Places), c(" alone",
                                                                 " (of any race)",
                                                                 "Estimate!!Households receiving food stamps/SNAP!!",
                                                                 "Estimate!!Households receiving food stamps/SNAP!!Households!!RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!",
                                                                 " "),
                                  c("",
                                    "",
                                    "total_snap_recipient_households_",
                                    "total_snap_recipient_households_",
                                    "_"))
names(S2201_SNAP_Places) <- tolower(names(S2201_SNAP_Places))

snap_recipient_households_demo_cities <- S2201_SNAP_Places

snap_recipient_households_demo_cities$total_snap_recipient_households_no_hispanic_or_latino_origin <- snap_recipient_households_demo_cities$total_snap_recipient_households_households - snap_recipient_households_demo_cities$total_snap_recipient_households_hispanic_or_latino_origin

snap_recipient_households_demo_cities <- snap_recipient_households_demo_cities %>%
  pivot_longer(
    cols = starts_with("total_snap_recipient_households_"),
    names_to = "demo",
    names_prefix = "total_snap_recipient_households_",
    values_to = "total_snap_recipient_households",
    values_drop_na = TRUE
  )
snap_recipient_households_demo_cities <- left_join(snap_recipient_households_demo_cities[snap_recipient_households_demo_cities$demo != "households", ],
                                                   S2201_SNAP_Places[, c("geoid", "total_snap_recipient_households_households")],
                                                   by = "geoid") %>%
  rename(snap_recipient_households = total_snap_recipient_households,
         total_snap_recipient_households = total_snap_recipient_households_households)

snap_recipient_households_demo_cities$geographic_area_name <- mgsub(snap_recipient_households_demo_cities$geographic_area_name, c(" city", " CDP", " village", " town"), "")
snap_recipient_households_demo_cities$geographic_area_name  <- gsub('De Pue', 'DePue', snap_recipient_households_demo_cities$geographic_area_name, fixed=TRUE)
snap_recipient_households_demo_cities <- snap_recipient_households_demo_cities %>% filter(geoid != "1728950")
snap_recipient_households_demo_cities$demo <- gsub("_", " ", snap_recipient_households_demo_cities$demo) %>% str_to_title()

snap_recipient_households_demo_cities <- left_join(snap_recipient_households_demo_cities,
                                                   city_networks,
                                                   by = c(geographic_area_name = "cities_counties"))

# snap_recipient_households_demo_cities <- left_join(snap_recipient_households_demo_cities, unit_counties, by = c("county" = "County")) %>%
#   rename(unit = "Unit #")

write_feather(
  snap_recipient_households_demo_cities,
  paste0(out_dir, "snap_recipient_households_demo_cities.feather"),
)

# Test snap_recipient_households_demo_cities

# all.equal(
#   read_feather(paste0(out_dir, "snap_recipient_households_demo_cities.feather")),
#   snap_recipient_households_demo_cities
#   )

#Poverty Status of Individuals by Age

poverty_individuals_age_tracts1 <- S1701_Poverty_Tracts[, c("GEOID",
                                                            "county",
                                                            "state",
                                                            "total_population_under_5_years",
                                                            "total_population_5_to_17_years",
                                                            "total_population_18_to_34_years",
                                                            "total_population_35_to_64_years",
                                                            "total_population_65_years_and_over")] %>%
  pivot_longer(
    cols = starts_with("total_population_"),
    names_to = "age",
    names_prefix = "total_population_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_age_tracts1$poverty_status <- "Total Population"

poverty_individuals_age_tracts2 <- S1701_Poverty_Tracts[, c("GEOID",
                                                            "county",
                                                            "state",
                                                            "below_poverty_level_under_5_years",
                                                            "below_poverty_level_5_to_17_years",
                                                            "below_poverty_level_18_to_34_years",
                                                            "below_poverty_level_35_to_64_years",
                                                            "below_poverty_level_65_years_and_over")] %>%
  pivot_longer(
    cols = starts_with("below_poverty_level_"),
    names_to = "age",
    names_prefix = "below_poverty_level_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_age_tracts2$poverty_status <- "Below 100% Poverty Level"

poverty_individuals_age_tracts <- rbind(poverty_individuals_age_tracts1, poverty_individuals_age_tracts2)
poverty_individuals_age_tracts <- left_join(poverty_individuals_age_tracts, S1701_Poverty_Tracts[, c("GEOID", "total_population", "individuals_income_below_185_percent_poverty_level")], by=c("GEOID"))
poverty_individuals_age_tracts$age <- mgsub(poverty_individuals_age_tracts$age, unique(poverty_individuals_age_tracts$age), c("Under 5", "5 to 17", "18 to 34", "35 to 64", "65 and Over"))

poverty_individuals_age_tracts <- left_join(poverty_individuals_age_tracts, reshaped_networks, by = c("GEOID" = "census_tracts"))

poverty_individuals_age_tracts<- left_join(poverty_individuals_age_tracts, county_networks, by = c("county" = "cities_counties"))

poverty_individuals_age_tracts <-
  poverty_individuals_age_tracts %>% mutate(
    network_name.x = ifelse(is.na(network_name.x), network_name.y, network_name.x),
    unit.x = ifelse(is.na(unit.x), unit.y, unit.x)
  ) %>%
  select(-c(network_name.y, unit.y)) %>%
  rename(network_name = 'network_name.x',
         unit = 'unit.x')

poverty_individuals_age_tracts <- left_join(poverty_individuals_age_tracts, unit_counties, by = c("county" = "County"))

poverty_individuals_age_tracts <- poverty_individuals_age_tracts %>% mutate(unit = ifelse(is.na(unit), as.character(poverty_individuals_age_tracts$"Unit #"), unit)) %>%
  select(- "Unit #")

write_feather(
  poverty_individuals_age_tracts,
  paste0(out_dir, "poverty_individuals_age_tracts.feather"),
)

# Test poverty_individuals_age_tracts

# all.equal(
#   read_feather(paste0(out_dir, "poverty_individuals_age_tracts.feather")),
#   poverty_individuals_age_tracts
#   )

poverty_individuals_age_cities1 <- S1701_Poverty_Places[, c("GEOID",
                                                            "geographic_area_name",
                                                            "state",
                                                            "total_population_under_5_years",
                                                            "total_population_5_to_17_years",
                                                            "total_population_18_to_34_years",
                                                            "total_population_35_to_64_years",
                                                            "total_population_65_years_and_over")] %>%
  pivot_longer(
    cols = starts_with("total_population_"),
    names_to = "age",
    names_prefix = "total_population_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_age_cities1$poverty_status <- "Total Population"

poverty_individuals_age_cities2 <- S1701_Poverty_Places[, c("GEOID",
                                                            "geographic_area_name",
                                                            "state",
                                                            "below_poverty_level_under_5_years",
                                                            "below_poverty_level_5_to_17_years",
                                                            "below_poverty_level_18_to_34_years",
                                                            "below_poverty_level_35_to_64_years",
                                                            "below_poverty_level_65_years_and_over")] %>%
  pivot_longer(
    cols = starts_with("below_poverty_level_"),
    names_to = "age",
    names_prefix = "below_poverty_level_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_age_cities2$poverty_status <- "Below 100% Poverty Level"

poverty_individuals_age_cities <- rbind(poverty_individuals_age_cities1, poverty_individuals_age_cities2)
poverty_individuals_age_cities <- left_join(poverty_individuals_age_cities, S1701_Poverty_Places[, c("GEOID", "total_population", "individuals_income_below_185_percent_poverty_level")], by=c("GEOID"))
poverty_individuals_age_cities$age <- mgsub(poverty_individuals_age_cities$age, unique(poverty_individuals_age_cities$age), c("Under 5", "5 to 17", "18 to 34", "35 to 64", "65 and Over"))

poverty_individuals_age_cities <- left_join(poverty_individuals_age_cities,
                                            city_networks,
                                            by = c(geographic_area_name = "cities_counties"))

write_feather(
  poverty_individuals_age_cities,
  paste0(out_dir, "poverty_individuals_age_cities.feather"),
)

# Test poverty_individuals_age_cities

# all.equal(
#   read_feather(paste0(out_dir, "poverty_individuals_age_cities.feather")),
#   poverty_individuals_age_cities
# )

#Poverty Status of Individuals by Sex

poverty_individuals_sex_tracts1 <- S1701_Poverty_Tracts[, c("GEOID",
                                                            "county",
                                                            "state",
                                                            "total_population_male",
                                                            "total_population_female")] %>%
  pivot_longer(
    cols = starts_with("total_population_"),
    names_to = "sex",
    names_prefix = "total_population_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_sex_tracts1$poverty_status <- "Total Population"

poverty_individuals_sex_tracts2 <- S1701_Poverty_Tracts[, c("GEOID",
                                                            "county",
                                                            "state",
                                                            "below_poverty_level_male",
                                                            "below_poverty_level_female")] %>%
  pivot_longer(
    cols = starts_with("below_poverty_level_"),
    names_to = "sex",
    names_prefix = "below_poverty_level_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_sex_tracts2$poverty_status <- "Below Poverty Level"

poverty_individuals_sex_tracts <- rbind(poverty_individuals_sex_tracts1, poverty_individuals_sex_tracts2)
poverty_individuals_sex_tracts <- left_join(poverty_individuals_sex_tracts, S1701_Poverty_Tracts[, c("GEOID", "total_population", "individuals_income_below_185_percent_poverty_level")], by=c("GEOID"))
poverty_individuals_sex_tracts$sex <- poverty_individuals_sex_tracts$sex %>% str_to_title()

poverty_individuals_sex_tracts <- left_join(poverty_individuals_sex_tracts, reshaped_networks, by = c("GEOID" = "census_tracts"))

poverty_individuals_sex_tracts <- left_join(poverty_individuals_sex_tracts, county_networks, by = c("county" = "cities_counties"))

poverty_individuals_sex_tracts <-
  poverty_individuals_sex_tracts %>% mutate(
    network_name.x = ifelse(is.na(network_name.x), network_name.y, network_name.x),
    unit.x = ifelse(is.na(unit.x), unit.y, unit.x)
  ) %>%
  select(-c(network_name.y, unit.y)) %>%
  rename(network_name = 'network_name.x',
         unit = 'unit.x')

poverty_individuals_sex_tracts <- left_join(poverty_individuals_sex_tracts, unit_counties, by = c("county" = "County"))

poverty_individuals_sex_tracts <- poverty_individuals_sex_tracts %>% mutate(unit = ifelse(is.na(unit), as.character(poverty_individuals_sex_tracts$"Unit #"), unit)) %>%
  select(- "Unit #")

write_feather(
  poverty_individuals_sex_tracts,
  paste0(out_dir, "poverty_individuals_sex_tracts.feather"),
)

# Test poverty_individuals_sex_tracts

# all.equal(
#   read_feather(paste0(out_dir, "poverty_individuals_sex_tracts.feather")),
#   poverty_individuals_sex_tracts
# )

poverty_individuals_sex_cities1 <- S1701_Poverty_Places[, c("GEOID",
                                                            "geographic_area_name",
                                                            "state",
                                                            "total_population_male",
                                                            "total_population_female")] %>%
  pivot_longer(
    cols = starts_with("total_population_"),
    names_to = "sex",
    names_prefix = "total_population_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_sex_cities1$poverty_status <- "Total Population"

poverty_individuals_sex_cities2 <- S1701_Poverty_Places[, c("GEOID",
                                                            "geographic_area_name",
                                                            "state",
                                                            "below_poverty_level_male",
                                                            "below_poverty_level_female")] %>%
  pivot_longer(
    cols = starts_with("below_poverty_level_"),
    names_to = "sex",
    names_prefix = "below_poverty_level_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_sex_cities2$poverty_status <- "Below Poverty Level"

poverty_individuals_sex_cities <- rbind(poverty_individuals_sex_cities1, poverty_individuals_sex_cities2)
poverty_individuals_sex_cities <- left_join(poverty_individuals_sex_cities, S1701_Poverty_Places[, c("GEOID", "total_population", "individuals_income_below_185_percent_poverty_level")], by=c("GEOID"))
poverty_individuals_sex_cities$sex <- poverty_individuals_sex_cities$sex %>% str_to_title()

poverty_individuals_sex_cities <- left_join(poverty_individuals_sex_cities,
                                            city_networks,
                                            by = c(geographic_area_name = "cities_counties"))
write_feather(
  poverty_individuals_sex_cities,
  paste0(out_dir, "poverty_individuals_sex_cities.feather"),
)

# Test poverty_individuals_sex_cities

# all.equal(
#   read_feather(paste0(out_dir, "poverty_individuals_sex_cities.feather")),
#   poverty_individuals_sex_cities
# )

#LEP Households by Language

S1602_LEP_Tracts <- S1602_LEP_Tracts %>% rename(total_households = 'Estimate!!Total!!All households',
                                                total_lep_households = 'Estimate!!Limited English-speaking households!!All households',
                                                spanish = 'Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Spanish',
                                                other_indo_european_languages = 'Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other Indo-European languages',
                                                asian_and_pacific_island_languages = 'Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Asian and Pacific Island languages',
                                                other_languages = 'Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other languages')

S1602_LEP_Tracts$NAME <- S1602_LEP_Tracts$NAME %>% mgsub(c("Census Tract ", " County"	), "")
S1602_LEP_Tracts <- S1602_LEP_Tracts  %>% separate(NAME, c("census_tract","county", "state"), sep = ", ")

lep_households_tracts <- left_join(S1602_LEP_Tracts, reshaped_networks, by = c("GEOID" = "census_tracts"))

lep_households_tracts <- left_join(lep_households_tracts, county_networks, by = c("county" = "cities_counties"))

lep_households_tracts <-
  lep_households_tracts %>% mutate(
    network_name.x = ifelse(is.na(network_name.x), network_name.y, network_name.x),
    unit.x = ifelse(is.na(unit.x), unit.y, unit.x)
  ) %>%
  select(-c(network_name.y, unit.y)) %>%
  rename(network_name = 'network_name.x',
         unit = 'unit.x')

lep_households_tracts <- left_join(lep_households_tracts, unit_counties, by = c("county" = "County"))

lep_households_tracts <- lep_households_tracts %>% mutate(unit = ifelse(is.na(unit), as.character(lep_households_tracts$"Unit #"), unit)) %>%
  select(- "Unit #")

lep_households_tracts <- lep_households_tracts  %>% pivot_longer(
  cols = c("spanish", "other_indo_european_languages", "asian_and_pacific_island_languages", "other_languages"),
  names_to = "language",
  values_to = "households",
  values_drop_na = TRUE
)

lep_households_tracts$language <-  gsub("_", " ", lep_households_tracts$language, fixed=TRUE) %>% str_to_title()

write_feather(
  lep_households_tracts,
  paste0(out_dir, "lep_households_tracts.feather"),
)

# Test lep_households_tracts

# all.equal(
#   read_feather(paste0(out_dir, "lep_households_tracts.feather")),
#   lep_households_tracts
# )

S1602_LEP_Places <- S1602_LEP_Places %>% rename(total_households = 'Estimate!!Total!!All households',
                                                total_lep_households = 'Estimate!!Limited English-speaking households!!All households',
                                                spanish = 'Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Spanish',
                                                other_indo_european_languages = 'Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other Indo-European languages',
                                                asian_and_pacific_island_languages = 'Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Asian and Pacific Island languages',
                                                other_languages = 'Estimate!!Limited English-speaking households!!All households!!Households speaking --!!Other languages')

S1602_LEP_Places <- S1602_LEP_Places  %>% separate(NAME, c("geographic_area_name", "state"), sep = ", ")
S1602_LEP_Places$geographic_area_name <- S1602_LEP_Places$geographic_area_name %>% mgsub(c(" city", " CDP", " village", " town"), "")
S1602_LEP_Places$geographic_area_name  <- gsub('De Pue', 'DePue', S1602_LEP_Places$geographic_area_name, fixed=TRUE)
S1602_LEP_Places <- S1602_LEP_Places %>% filter(GEOID != "1728950")

lep_households_cities <- left_join(S1602_LEP_Places,
                                   city_networks,
                                   by = c(geographic_area_name = "cities_counties"))

lep_households_cities <- lep_households_cities %>% pivot_longer(
  cols = c("spanish", "other_indo_european_languages", "asian_and_pacific_island_languages", "other_languages"),
  names_to = "language",
  values_to = "households",
  values_drop_na = TRUE
)

lep_households_cities$language <-  gsub("_", " ", lep_households_cities$language, fixed=TRUE) %>% str_to_title()

write_feather(
  lep_households_cities,
  paste0(out_dir, "lep_households_cities.feather"),
)

# Test lep_households_cities

# all.equal(
#   read_feather(paste0(out_dir, "lep_households_cities.feather")),
#   lep_households_cities
# )

#Poverty Status of Individuals by Race/Ethnicity

poverty_individuals_demo_tracts1 <- S1701_Poverty_Tracts[, c("GEOID",
                                                             "county",
                                                             "state",
                                                             "total_population",
                                                             "total_population_white",
                                                             "total_population_black_or_african_american",
                                                             "total_population_american_indian_and_alaska_native",
                                                             "total_population_asian",
                                                             "total_population_native_hawaiian_and_other_pacific_islander",
                                                             "total_population_some_other_race",
                                                             "total_population_two_or_more_races",
                                                             "total_population_hispanic_or_latino_origin")]

poverty_individuals_demo_tracts1$total_population_no_hispanic_or_latino_origin <- poverty_individuals_demo_tracts1$total_population - poverty_individuals_demo_tracts1$total_population_hispanic_or_latino_origin
poverty_individuals_demo_tracts1 <- poverty_individuals_demo_tracts1 %>% select(-total_population)

poverty_individuals_demo_tracts1 <- poverty_individuals_demo_tracts1 %>%
  pivot_longer(
    cols = starts_with("total_population_"),
    names_to = "demo",
    names_prefix = "total_population_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_demo_tracts1$poverty_status <- "Total Population"

poverty_individuals_demo_tracts2 <- S1701_Poverty_Tracts[, c("GEOID",
                                                             "county",
                                                             "state",
                                                             "below_poverty_level_total_population",
                                                             "below_poverty_white",
                                                             "below_poverty_black_or_african_american",
                                                             "below_poverty_american_indian_and_alaska_native",
                                                             "below_poverty_asian",
                                                             "below_poverty_native_hawaiian_and_other_pacific_islander",
                                                             "below_poverty_some_other_race",
                                                             "below_poverty_two_or_more_races",
                                                             "below_poverty_hispanic_or_latino_origin")]

poverty_individuals_demo_tracts2$below_poverty_no_hispanic_or_latino_origin <- poverty_individuals_demo_tracts2$below_poverty_level_total_population - poverty_individuals_demo_tracts2$below_poverty_hispanic_or_latino_origin
poverty_individuals_demo_tracts2 <- poverty_individuals_demo_tracts2 %>% select(-below_poverty_level_total_population)

poverty_individuals_demo_tracts2 <- poverty_individuals_demo_tracts2 %>%
  pivot_longer(
    cols = starts_with("below_poverty_"),
    names_to = "demo",
    names_prefix = "below_poverty_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_demo_tracts2$poverty_status <- "Below 100% Poverty Level"

poverty_individuals_demo_tracts <- rbind(poverty_individuals_demo_tracts1, poverty_individuals_demo_tracts2)
poverty_individuals_demo_tracts <- left_join(poverty_individuals_demo_tracts, S1701_Poverty_Tracts[, c("GEOID", "total_population")], by=c("GEOID"))
poverty_individuals_demo_tracts$demo <- gsub("_", " ", poverty_individuals_demo_tracts$demo) %>% str_to_title()

poverty_individuals_demo_tracts <- left_join(poverty_individuals_demo_tracts, reshaped_networks, by = c("GEOID" = "census_tracts"))

poverty_individuals_demo_tracts<- left_join(poverty_individuals_demo_tracts, county_networks, by = c("county" = "cities_counties"))

poverty_individuals_demo_tracts <-
  poverty_individuals_demo_tracts %>% mutate(
    network_name.x = ifelse(is.na(network_name.x), network_name.y, network_name.x),
    unit.x = ifelse(is.na(unit.x), unit.y, unit.x)
  ) %>%
  select(-c(network_name.y, unit.y)) %>%
  rename(network_name = 'network_name.x',
         unit = 'unit.x')

poverty_individuals_demo_tracts <- left_join(poverty_individuals_demo_tracts, unit_counties, by = c("county" = "County"))

poverty_individuals_demo_tracts <- poverty_individuals_demo_tracts %>% mutate(unit = ifelse(is.na(unit), as.character(poverty_individuals_demo_tracts$"Unit #"), unit)) %>%
  select(- "Unit #")

write_feather(
  poverty_individuals_demo_tracts,
  paste0(out_dir, "poverty_individuals_demo_tracts.feather"),
)

# Test poverty_individuals_demo_tracts

# all.equal(
#   read_feather(paste0(out_dir, "poverty_individuals_demo_tracts.feather")),
#   poverty_individuals_demo_tracts
# )

poverty_individuals_demo_cities1 <- S1701_Poverty_Places[, c("GEOID",
                                                             "geographic_area_name",
                                                             "state",
                                                             "total_population",
                                                             "total_population_white",
                                                             "total_population_black_or_african_american",
                                                             "total_population_american_indian_and_alaska_native",
                                                             "total_population_asian",
                                                             "total_population_native_hawaiian_and_other_pacific_islander",
                                                             "total_population_some_other_race",
                                                             "total_population_two_or_more_races",
                                                             "total_population_hispanic_or_latino_origin")] 

poverty_individuals_demo_cities1$total_population_no_hispanic_or_latino_origin <- poverty_individuals_demo_cities1$total_population - poverty_individuals_demo_cities1$total_population_hispanic_or_latino_origin
poverty_individuals_demo_cities1 <- poverty_individuals_demo_cities1 %>% select(-total_population)

poverty_individuals_demo_cities1 <- poverty_individuals_demo_cities1 %>%
  pivot_longer(
    cols = starts_with("total_population_"),
    names_to = "demo",
    names_prefix = "total_population_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_demo_cities1$poverty_status <- "Total Population"

poverty_individuals_demo_cities2 <- S1701_Poverty_Places[, c("GEOID",
                                                             "geographic_area_name",
                                                             "state",
                                                             "below_poverty_level_total_population",
                                                             "below_poverty_white",
                                                             "below_poverty_black_or_african_american",
                                                             "below_poverty_american_indian_and_alaska_native",
                                                             "below_poverty_asian",
                                                             "below_poverty_native_hawaiian_and_other_pacific_islander",
                                                             "below_poverty_some_other_race",
                                                             "below_poverty_two_or_more_races",
                                                             "below_poverty_hispanic_or_latino_origin")] 

poverty_individuals_demo_cities2$below_poverty_no_hispanic_or_latino_origin <- poverty_individuals_demo_cities2$below_poverty_level_total_population - poverty_individuals_demo_cities2$below_poverty_hispanic_or_latino_origin
poverty_individuals_demo_cities2 <- poverty_individuals_demo_cities2 %>% select(-below_poverty_level_total_population)

poverty_individuals_demo_cities2 <- poverty_individuals_demo_cities2 %>%
  pivot_longer(
    cols = starts_with("below_poverty_"),
    names_to = "demo",
    names_prefix = "below_poverty_",
    values_to = "individuals",
    values_drop_na = TRUE
  )

poverty_individuals_demo_cities2$poverty_status <- "Below 100% Poverty Level"

poverty_individuals_demo_cities <- rbind(poverty_individuals_demo_cities1, poverty_individuals_demo_cities2)
poverty_individuals_demo_cities <- left_join(poverty_individuals_demo_cities, S1701_Poverty_Places[, c("GEOID", "total_population")], by=c("GEOID"))
poverty_individuals_demo_cities$demo <- gsub("_", " ", poverty_individuals_demo_cities$demo) %>% str_to_title()

poverty_individuals_demo_cities <- left_join(poverty_individuals_demo_cities,
                                             city_networks,
                                             by = c(geographic_area_name = "cities_counties"))

write_feather(
  poverty_individuals_demo_cities,
  paste0(out_dir, "poverty_individuals_demo_cities.feather"),
)

# Test poverty_individuals_demo_cities

# all.equal(
#   read_feather(paste0(out_dir, "poverty_individuals_demo_cities.feather")),
#   poverty_individuals_demo_cities
# )
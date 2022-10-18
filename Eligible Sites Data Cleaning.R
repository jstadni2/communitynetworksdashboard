setwd("C:\\Users\\user\\path\\to\\communitynetworksdashboard")

library(data.table)
library(readxl)

wic_sites <- fread("wic_sites_geocodio.csv")

fcrc_sites <- fread("fcrc_sites_geocodio.csv")

FQHCs <- read_excel("FQHCs.xlsx")

schools_sites <- fread("eligible_schools_geocodio.csv")

head_start_centers <- fread("head_start_centers.csv")

homeless_shelters <- fread("homeless_shelters_geocodio.csv")

## Eligible Sites Datacleaning

eligible_sites_fcrc <- fcrc_sites[, c("site_name", "site_address", "site_city", "site_state", "site_zip", "Latitude", "Longitude")] %>% 
  rename(latitude = Latitude,
         longitude = Longitude)
eligible_sites_fcrc$site_type <- "Family Community Resource Center"

eligible_sites_fqhc <- FQHCs[FQHCs$"Grant #" != '[No Data]' & FQHCs$"Services Delivered at Site" == "Yes", 
                             c("Site Name", "Address", "City", "State", "ZIP", "Longitude", "Latitude")] %>%
  rename(site_name = "Site Name",
         site_address = Address,
         site_city = City,
         site_state = State,
         site_zip = ZIP,
         latitude = Latitude,
         longitude = Longitude)
eligible_sites_fqhc$site_type <- "Federally Qualified Health Center"

eligible_sites_wic <- wic_sites[, c("site_name", "site_address", "site_city", "site_state", "site_zip", "Latitude", "Longitude")] %>% 
  rename(latitude = Latitude,
         longitude = Longitude)
eligible_sites_wic$site_zip <- as.character(eligible_sites_wic$site_zip)
eligible_sites_wic$site_type <- "Women, Infants, and Children (WIC)"

eligible_sites_schools <- schools_sites[, c("site_name", "site_address", "site_city", "site_state", "site_zip", "Latitude", "Longitude")] %>% 
  rename(latitude = Latitude,
         longitude = Longitude)
eligible_sites_schools$site_type <- "Schools"

eligible_sites_head_starts <- head_start_centers[, c("name", "addressLineOne", "city", "state", "zipFive", "latitude", "longitude")] %>%
  rename(site_name = name,
         site_address = addressLineOne,
         site_city = city,
         site_state = state,
         site_zip = zipFive)
eligible_sites_head_starts$site_zip <- as.character(eligible_sites_head_starts$site_zip)
eligible_sites_head_starts$site_type <- "Head Start Centers"

eligible_sites_homeless_shelters <- homeless_shelters[, c("Site Name", "Address", "City", "State", "ZIP", "Latitude", "Longitude")] %>%
  rename(site_name = "Site Name",
         site_address = Address,
         site_city = City,
         site_state = State,
         site_zip = ZIP,
         latitude = Latitude,
         longitude = Longitude)
eligible_sites_homeless_shelters$site_type <- "Homeless Shelters"

eligible_sites <- bind_rows(eligible_sites_fcrc, eligible_sites_fqhc, eligible_sites_wic, eligible_sites_schools, eligible_sites_head_starts, eligible_sites_homeless_shelters)

write.csv(eligible_sites, "eligible_sites.csv")
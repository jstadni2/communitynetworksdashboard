#setwd("C:\\Users\\user\\path\\to\\communitynetworksdashboard\\CommunityNetworksDashboard")

#install.packages(c("data.table", "tidyverse", "sf", "tmaptools", "classInt", "RColorBrewer", "leaflet"))
#install.packages('textclean')

library(arrow)
library(data.table)
library(tidyverse)
library(readxl)
library(sf)
library(s2)
s2_available = !inherits(try(sf_use_s2(TRUE), silent=TRUE), "try-error")
library(tmaptools)
library(classInt)
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)
library(textclean)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leafem)
library(fontawesome) #devtools::install_github("rstudio/fontawesome")
library(shinyjs)

##Import map data and shape files

snap_ed_networks_sf <- st_read("snap_ed_networks_sf.gpkg")

IL_tracts_sf_merged <- st_read("IL_tracts_sf_merged.gpkg")

eligible_sites <- fread("eligible_sites.csv")

sites <- read_excel("Site_Export.xlsx", sheet = "Site Data")

PA <- read_excel("Program_Activities_Export.xlsx", sheet = "Program Activity Data")

IA <- read_excel("Indirect_Activity_Export.xlsx", sheet = "Indirect Activity Data")
IA_IC <- read_excel("Indirect_Activity_Export.xlsx", sheet = "Intervention Channels")

PSE <- read_excel("PSE_Site_Activity_Export.xlsx", sheet = "PSE Data")

Coa <- read_excel("Coalition_Export.xlsx", sheet = "Coalition Data")
Coa_Members <- read_excel("Coalition_Export.xlsx", sheet = "Members")

Part <- read_excel("Partnership_Export.xlsx", sheet = "Partnership Data")

unit_counties <- read_excel("Illinois Extension Unit Counties.xlsx", sheet = "Unit Counties")

##Import community profile data

poverty_individuals_age_tracts <-
  read_feather("poverty_individuals_age_tracts.feather")
poverty_individuals_age_cities <-
  read_feather("poverty_individuals_age_cities.feather")

poverty_individuals_sex_tracts <-
  read_feather("poverty_individuals_sex_tracts.feather")
poverty_individuals_sex_cities <-
  read_feather("poverty_individuals_sex_cities.feather")

poverty_individuals_demo_tracts <-
  read_feather("poverty_individuals_demo_tracts.feather")
poverty_individuals_demo_cities <-
  read_feather("poverty_individuals_demo_cities.feather")

snap_recipient_households_demo_tracts <-
  read_feather("snap_recipient_households_demo_tracts.feather")
snap_recipient_households_demo_cities <-
  read_feather("snap_recipient_households_demo_cities.feather")

lep_households_tracts <-
  read_feather("lep_households_tracts.feather")
lep_households_cities <-
  read_feather("lep_households_cities.feather")

adult_obesity <- fread("2021 County Health Rankings Illinois Data - v1 - Ranked Measure Data.csv",
                       skip=1,
                       select = c('FIPS',
                                  'State',
                                  'County',
                                  '% Adults with Obesity'                                             
                       ))

food_insecurity <- fread("MMG2020_2018Data_ToShare.csv", skip=1)

# Community Networks Map

## PEARS sites Datacleaning

PA <- PA[PA$program_areas == "SNAP-Ed" & !grepl("TEST", PA$name), c("program_id", "name", "program_areas", "site_id")]
sites_PA <- PA %>% count(site_id, name="program_activities")

IA <- IA[IA$program_area == "SNAP-Ed" & !grepl("TEST", IA$title), c("activity_id", "title", "program_area")]
IA_IC <- IA_IC[c("activity_id", "channel_id", "site_id")]
sites_IA <- left_join(IA, IA_IC, by = "activity_id") %>%
  filter(!is.na(site_id)) %>% #Should every IA have site?
  distinct(activity_id, site_id) %>% 
  count(site_id, name="indirect_activities")

PSE <- PSE[PSE$program_area == "SNAP-Ed" & !grepl("TEST", PSE$name), c("pse_id", "name", "program_area", "site_id")] #PSE with null name?
sites_PSE <- PSE %>% count(site_id, name="PSE_site_activities")

Coa <- Coa[Coa$program_area == "SNAP-Ed" & !grepl("TEST", Coa$coalition_name), c("coalition_id", "coalition_name", "program_area")]
Coa_Members <- Coa_Members[c("coalition_id", "member_id", "site_id")]
sites_Coa <- left_join(Coa, Coa_Members, by = "coalition_id") %>%
  filter(!is.na(site_id))%>% #some site data is all null
  distinct(coalition_id, site_id) %>% 
  count(site_id, name="coalitions")

Part <- Part[Part$program_area == "SNAP-Ed" & !grepl("TEST", Part$partnership_name), c("partnership_id", "partnership_name", "program_area", "site_id")]
sites_Part <- Part %>% count(site_id, name="partnerships")

site_programming <- sites[c("site_id", "site_name", "city__county", "latitude", "longitude")] %>%
  left_join(sites_PA, by="site_id") %>%
  left_join(sites_IA, by="site_id") %>%
  left_join(sites_PSE, by="site_id") %>%
  left_join(sites_Coa, by="site_id") %>%
  left_join(sites_Part, by="site_id") %>%
  filter(!is.na(program_activities) | !is.na(PSE_site_activities) | !is.na(coalitions) | !is.na(partnerships))

unit_counties <- unit_counties %>% mutate(County = replace(County, County=="Dupage", "DuPage")) # consolidate these line during refactor
unit_counties$County  <- gsub('DeWitt', 'De Witt', unit_counties$County, fixed=TRUE)
site_programming <- left_join(site_programming, unit_counties, by = c("city__county" = "County")) 
site_programming <- site_programming %>% filter(!is.na(site_programming$"Unit #"))

## Leaflet Map Visuals

pal1 <- colorNumeric("Blues", domain=IL_tracts_sf_merged$snap_eligibility_percent)
pal2 <- colorNumeric("Reds", domain=IL_tracts_sf_merged$individuals_income_below_185_percent_poverty_level)
pal3 <- colorFactor(
  palette = "Spectral",
  domain=snap_ed_networks_sf$network_name)

popup_sb1 <- paste0("County: \n", as.character(IL_tracts_sf_merged$county), 
                    "</br/> Census Tract: \n", as.character(IL_tracts_sf_merged$census_tract), 
                    "</br/> # of Eligible Individuals: \n", as.character(IL_tracts_sf_merged$individuals_income_below_185_percent_poverty_level), 
                    "</br/> % of Individuals Below 185% of poverty level (SNAP-Ed Eligibility): \n", as.character(IL_tracts_sf_merged$snap_eligibility_percent))

# legend html generator:
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:1px; margin: 0;'> Site Legend </h4>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 0px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 5px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}

IconSet <- awesomeIconList(
  "Programming" = makeAwesomeIcon(icon = 'circle', markerColor = 'green', library='fa'),
  "FCRC" = makeAwesomeIcon(icon = 'users', markerColor = 'purple', library='fa'),
  "FQHC" = makeAwesomeIcon(icon = 'medkit', markerColor = 'red', library='fa'),
  "Head Start Centers" = makeAwesomeIcon(icon = 'child', markerColor = 'darkred', library='fa'),
  "Homeless Shelters" = makeAwesomeIcon(icon = 'home', markerColor = 'cadetblue', library='fa'),  
  "Schools" = makeAwesomeIcon(text = fa('school'), markerColor = 'orange', library='fa'),  
  "WIC" = makeAwesomeIcon(icon = 'female', markerColor = 'blue', library='fa')
)

# Community Profile

## Adult Obesity

adult_obesity <- adult_obesity %>% rename(fips = 'FIPS',
                                          state = 'State',
                                          county = 'County',
                                          percent_adults_obesity = '% Adults with Obesity')

## Food Insecurity

food_insecurity <- food_insecurity %>% rename(county = "County, State",
                                              food_insecurity_rate = "2018 Food Insecurity Rate",
                                              food_insecure_persons = "# of Food Insecure Persons in 2018",
                                              child_food_insecurity_rate = "2018 Child food insecurity rate",
                                              food_insecure_children = "# of Food Insecure Children in 2018")

food_insecurity$county <- gsub(" County, Illinois", "", food_insecurity$county)

# Shiny App

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    
    tags$style(
      ".block {
      margin: 10px;
      min-height: 200px;
    }
    
    span, a {
    }
    "
    ),
    
    shinyjs::useShinyjs(),
    fluidRow(
      tabBox(
        title = NULL,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "850px", width = 12, 
        tabPanel("SNAP-Ed Community Networks Map", 
                 fluidRow(
                   column(10, 
                          leafletOutput("map", height = 800),
                   ),
                   column(2,
                          h3("Community Network Filters"),
                          pickerInput("regionInput", "SNAP-Ed Region",
                                      choices = levels(as.factor(snap_ed_networks_sf$region)),
                                      selected = NULL,#levels(as.factor(snap_ed_networks_sf$Region)),
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE)), 
                          pickerInput("unitInput", "Extension Unit",
                                      choices =  str_sort(levels(factor(snap_ed_networks_sf$unit)), numeric = TRUE),
                                      selected = NULL,#levels(as.factor(snap_ed_networks_sf$Unit)),
                                      multiple = TRUE,
                                      options = list(
                                        `live-search` = TRUE,
                                        `actions-box` = TRUE)),
                          pickerInput("networkInput", "SNAP-Ed Community Network",
                                      choices = levels(as.factor(snap_ed_networks_sf$network_name)),
                                      selected = NULL,#levels(as.factor(snap_ed_networks_sf$community_network_name)),
                                      multiple = TRUE,
                                      options = list(
                                        `live-search` = TRUE,
                                        `actions-box` = TRUE)),
                          h3("Site Filters"),
                          pickerInput("programInput", "Programming Intervention Type",
                                      choices = c("Program Activities", "Indirect Activities", "PSE Site Activities", "Coalitions", "Partnerships"),
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE)),
                          pickerInput("eligiblesiteInput", "Eligible Site Type",
                                      choices = levels(as.factor(eligible_sites$site_type)),
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = list(
                                        `actions-box` = TRUE)),
                          HTML(markerLegendHTML(IconSet = IconSet)),
                   )
                 )
        ),
        tabPanel("Community Profile",
                 fluidRow(
                   column(10,
                          fluidRow(
                            column(3,
                                   h4("SNAP-Ed Eligibility"),
                                   htmlOutput("text1")
                            ),
                            column(4,
                                   h4("Food Insecurity"),
                                   fluidRow(
                                     column(6, htmlOutput("text4")),
                                     column(6, htmlOutput("text5"))
                                   )
                            ),
                            column(2,
                                   h4("Adult Obesity"),
                                   htmlOutput("text3")
                            ),
                            
                            column(3,
                                   h4("Limited English Proficiency"),
                                   htmlOutput("text2")
                            ),
                          ),
                          fluidRow(
                            fluidRow(
                              column(4, plotlyOutput("plot3")),
                              column(8, plotlyOutput("plot2")),
                            )
                          ),
                          fluidRow(
                            column(7, plotlyOutput("plot5")),
                            column(5, plotlyOutput("plot7")),
                          ),
                          fluidRow(
                            column(5, plotlyOutput("plot1")),
                            column(3, plotlyOutput("plot6")),                            
                            column(4, plotlyOutput("plot4")),
                          ),
                   ),
                   column(2,
                          h3("Community Profile Filters"),
                          pickerInput("unit2Input", "Extension Unit",
                                      choices = str_sort(levels(as.factor(snap_recipient_households_demo_tracts$unit)), numeric = TRUE),
                                      selected = NULL, #show groupby(state) by default
                                      multiple = TRUE,
                                      options = pickerOptions(liveSearch=T, maxOptions = 1)),  #select all > NULL
                          
                          pickerInput("countyInput", "County",
                                      choices = levels(as.factor(snap_recipient_households_demo_tracts$county)),
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = pickerOptions(liveSearch=T, maxOptions = 1)), #select all > NULL
                          
                          pickerInput("network2Input", "SNAP-Ed Community Network",
                                      choices = levels(as.factor(c(snap_recipient_households_demo_cities$network_name,
                                                                   snap_recipient_households_demo_tracts$network_name))),
                                      selected = NULL, #show groupby(state) by default
                                      multiple = TRUE,
                                      options = pickerOptions(liveSearch=T, maxOptions = 1)),  #select all > NULL
                          
                          pickerInput("cityInput", "City",
                                      choices = levels(as.factor(snap_recipient_households_demo_cities$geographic_area_name)),
                                      selected = NULL,
                                      multiple = TRUE,
                                      options = pickerOptions(liveSearch=T, maxOptions = 1)),
                          
                          actionButton("reset_input", "Clear All Filters"),
                          h5("Metrics for Illinois displayed by default."), 
                   ),
                   
                 ),
        ),
        
        tabPanel("External Links & Sources",
                 div(
                   class = "block",
                   HTML('
                               <h4><b>External Links</b></h4>
                               
                               <i>The data sources included in this list are provided for easy access and are considered reliable sources of information
                               for use in SNAP-Ed. Using other sources of data is encouraged, provided that they are reliable, trustworthy sources.
                               <br>Please reach out to the <a href = "mailto: uie-fcsevaluation@illinois.edu">SNAP-Ed Evaluation Team </a> or your
                               SNAP-Ed Regional Educator if you have questions about appropriate use cases for any of the data sources listed.</i><br>
                               
                               <li> <a href=https://snaped.engagementnetwork.org/assessment/>SNAP-Ed Assessment</a></li>
                               <li> <a href=http://www.countyhealthrankings.org/>County Health Rankings</a></li>
                               <li> <a href=https://www.ers.usda.gov/data-products/food-access-research-atlas/go-to-the-atlas/>Food Access Research Atlas</a></li>
                               <li> <a href=https://www.ers.usda.gov/data-products/food-environment-atlas/go-to-the-atlas/>Food Environment Atlas</a></li>
                               <li> <a href=https://engagementnetwork.org/map-room/?action=tool_map&tool=footprint>CARES Engagement Network (Vulnerable populations footprint)</a></li>
                               <li> <a href=https://www.communitycommons.org/>Community Commons</a></li>
                               <li> <a href=https://insight.livestories.com/s/v2/win-measures/2fda874f-6683-49bd-adb2-22f6f3c5a718/>Well Being in the Nation Measurement Framework</a></li>
                               <li> <a href=https://map.feedingamerica.org/>Feeding America Map the Meal Gap</a></li>
                               <li> <a href=https://iys.cprd.illinois.edu/results/county>Illinois Youth Survey</a></li>
                               <li> <a href=https://eat-move-save.extension.illinois.edu/>Find Food IL Community Food Map</a></li>
                               <li> <a href=https://go.illinois.edu/UIECountyProfilesLIVE>Illinois Extension County Profiles</a></li>
                               
                               <h4><b>Sources</b></h4>
                               <h5><b>SNAP-Ed Eligibility</b></h5>
                               <p>Figures for SNAP-Ed eligibility (individuals with income at or below 185% of the Federal Poverty Level) are provided by the 2020 American Community Survey 5-year estimates.
                               <br><a href=https://www.census.gov/programs-surveys/acs/about.html>About the American Community Survey</a></br></p>
                               
                               <h5><b>Programming Sites</b></h5>
                               <p>Programming Sites for Illinois Extension are sourced from PEARS and typically updated on a weekly basis.
                               <br><a href=https://www.k-state.edu/oeie/pears/>Office of Educational Innovation and Evaluation (OEIE) - PEARS</a></br></p>
                               
                               <h5><b>Eligible Sites</b></h5>
                               <p>The following sources are utilized for eligible sites:<br>
                               
                               <li> Family Community Resource Centers and Women, Infants, and Children (WIC) Offices - <a href=https://www.dhs.state.il.us/page.aspx?module=12>IDHS Office Locator</a>
                               <li> Federally Qualified Health Centers - <a href=https://data.hrsa.gov/geo>HRSA Data by Geography</a>
                               <li> Head Start Centers - <a href=https://eclkc.ohs.acf.hhs.gov/center-locator>Head Start Center Locator</a>
                               <li> Schools - <a href=https://www.isbe.net/Pages/Nutrition-Data-Analytics-Maps.aspx>ISBE Free and Reduced-Price Meal Eligibility Data</a>
                               <li> Homeless Shelters - <a href=http://www.illinoisfoodbanks.org/sites.asp>Illinois Pantries, Soup Kitchens and Homeless Shelters</a>
                               ')
                 )
        )
      )
    ),
  ),
)



server <- function(input, output, session) {
  
  observeEvent(
    # define pickerinputs to be observed
    c(
      input$regionInput,
      input$unitInput
    ),
    {
      ## filter the data based on the pickerinputs
      # include an ifelse condition first to check wheter at least one value is choosen in all of the filters.
      
      if (!is.null(input$regionInput) &
          is.null(input$unitInput)) {
        snap_ed_networks_sf2 <- snap_ed_networks_sf %>%
          filter(region %in% input$regionInput)
        
        updatePickerInput(
          session,
          "unitInput",
          choices =  str_sort(levels(factor(snap_ed_networks_sf2$unit)), numeric = TRUE),
          selected = NULL
        )
        
        updatePickerInput(
          session,
          "networkInput",
          choices = levels(factor(snap_ed_networks_sf2$network_name)),
          selected = NULL
        )
        
      }
      else if (!is.null(input$unitInput)){
        snap_ed_networks_sf2 <- snap_ed_networks_sf %>%
          filter(unit %in% input$unitInput)
        
        updatePickerInput(
          session,
          "networkInput",
          choices = levels(factor(snap_ed_networks_sf2$network_name)),
          selected = NULL
        )
      } else{
        
        updatePickerInput(
          session,
          "unitInput",
          choices = str_sort(levels(factor(snap_ed_networks_sf$unit)), numeric = TRUE),
          selected = NULL
        )
        
        updatePickerInput(
          session,
          "networkInput",
          choices = levels(factor(snap_ed_networks_sf$network_name)),
          selected = NULL
        )
      }
      
    },
    ignoreInit = TRUE,
    ignoreNULL = F
  )
  
  
  
  snap_ed_networks_sf.reactive <-
    reactive({
      if (!is.null(input$regionInput) & is.null(input$unitInput) & is.null(input$networkInput)) #fix this?
        # one condition should be enough.
      {
        snap_ed_networks_sf %>% # filters
          filter(
            region %in% input$regionInput
          )
      } else if (!is.null(input$unitInput) & is.null(input$networkInput)){
        snap_ed_networks_sf %>% # filters
          filter(
            unit %in% input$unitInput
          )
      } else if (!is.null(input$networkInput)){
        snap_ed_networks_sf %>% # filters
          filter(
            network_name %in% input$networkInput
          )
      } else
      {
        snap_ed_networks_sf
      }
    })
  
  site_programming.reactive <-
    reactive({
      if (!is.null(input$programInput))
      {
        input_cols <- mgsub(input$programInput, c("Program Activities", "Indirect Activities", "PSE Site Activities", "Coalitions", "Partnerships"),
                            c("program_activities", "indirect_activities", "PSE_site_activities", "coalitions", "partnerships"))
        
        ind_filters = c()
        
        for (i in 1:length(input_cols)){
          ind_filters[i] = paste0("!is.na(", input_cols[i], ")")
        }
        
        filters = paste(ind_filters, collapse = ' | ') 
        
        site_programming <- filter(site_programming, eval(parse(text = filters)))
      } else
      {
        site_programming
      }
      
    })
  
  eligible_sites.reactive <-
    reactive({
      if (!is.null(input$eligiblesiteInput))
      {
        
        eligible_sites %>% filter(site_type %in% input$eligiblesiteInput)
        
      } else
      {
        eligible_sites
      }
      
    })   
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles("CartoDB.Voyager", options = providerTileOptions(
        updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
        updateWhenIdle = FALSE)) %>%
      setView(-89.241943359375, 40.17047886718109, zoom = 7) %>% 
      addLayersControl(
        overlayGroups = c("SNAP-Ed Community Networks", "Eligible Individuals", "Eligibility %", "Programming Sites", "Eligible Sites"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE)) %>%
      addPolygons(data = IL_tracts_sf_merged, 
                  fillColor = ~pal1(IL_tracts_sf_merged$snap_eligibility_percent), 
                  fillOpacity = 0.5, 
                  weight = 0.9, 
                  smoothFactor = 0.2, 
                  stroke=TRUE,
                  color="white",
                  popup = ~popup_sb1,
                  group = "Eligibility %") %>% 
      addPolygons(data = IL_tracts_sf_merged, 
                  fillColor = ~pal2(IL_tracts_sf_merged$individuals_income_below_185_percent_poverty_level), 
                  fillOpacity = 0.5, 
                  weight = 0.9, 
                  smoothFactor = 0.2, 
                  stroke=TRUE,
                  color="white",
                  popup = ~popup_sb1,
                  group = "Eligible Individuals") %>%
      addLegend(pal = pal1, 
                values = IL_tracts_sf_merged$snap_eligibility_percent, 
                position = "bottomright", 
                title = 'SNAP-Ed Eligibility %',
                group = "Eligibility %") %>%
      addLegend(pal = pal2, 
                values = IL_tracts_sf_merged$individuals_income_below_185_percent_poverty_level, 
                position = "bottomright", 
                title = 'SNAP-Ed Eligible Individuals',
                group = "Eligible Individuals")
    
    
  })
  
  
  observe({
    
    popup2 <- paste0("Community Network: \n", as.character(snap_ed_networks_sf.reactive()$network_name),
                     "</br/> # of Eligible Individuals: \n", as.character(snap_ed_networks_sf.reactive()$individuals_income_below_185_percent_poverty_level),
                     "</br/> % of Individuals Below 185% of poverty level (SNAP-Ed Eligibility): \n", as.character(snap_ed_networks_sf.reactive()$snap_eligibility_percent))
    
    leafletProxy("map", data = snap_ed_networks_sf.reactive()) %>%
      clearGroup(group = "SNAP-Ed Community Networks") %>%
      addPolygons(
        fillOpacity = 0,
        
        highlight = highlightOptions(
          weight = 3,
          fillOpacity = 0,
          color = "black",
          opacity = 1.0,
          bringToFront = TRUE,
          sendToBack = FALSE),
        
        weight = 2,
        smoothFactor = 0.2,
        stroke=TRUE,
        color="black",
        popup = ~popup2,
        group = "SNAP-Ed Community Networks",
        
        label = paste0("", as.character(snap_ed_networks_sf.reactive()$network_name)), #will crash app without paste0("", add ~, req? https://stackoverflow.com/questions/57570990/observeleafletproxy-is-causing-shinyapp-to-crash
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      )
  })
  
  observe({
    
    popup3 <- paste0("SNAP-Ed Site: \n", as.character(site_programming.reactive()$site_name),
                     "</br/> Program Activities: \n", as.character(site_programming.reactive()$program_activities), #if null, skip line
                     "</br/> Indirect Activities: \n", as.character(site_programming.reactive()$indirect_activities),
                     "</br/> PSE Site Activities: \n", as.character(site_programming.reactive()$PSE_site_activities),
                     "</br/> Coalitions: \n", as.character(site_programming.reactive()$coalitions),
                     "</br/> Partnerships: \n", as.character(site_programming.reactive()$partnerships))
    
    leafletProxy("map", data = site_programming.reactive()) %>%
      clearGroup(group = "Programming Sites") %>%
      addAwesomeMarkers(lng=~longitude,
                        lat=~latitude,
                        icon = ~IconSet["Programming"],
                        popup=~popup3,
                        label =  paste0("", as.character(site_programming.reactive()$site_name)),
                        group = "Programming Sites",
                        clusterOptions = markerClusterOptions(disableClusteringAtZoom=14, spiderfyOnMaxZoom=FALSE, showCoverageOnHover=FALSE))
    
  })
  
  observe({
    
    #enclose this all in loop: for (i in 1:length(input$eligiblesiteInput)){}
    #list of lists of c(df, icon)
    
    fcrc <- eligible_sites.reactive()[eligible_sites.reactive()$site_type == "Family Community Resource Center",]
    fqhc <- eligible_sites.reactive()[eligible_sites.reactive()$site_type == "Federally Qualified Health Center",]
    wic <- eligible_sites.reactive()[eligible_sites.reactive()$site_type == "Women, Infants, and Children (WIC)",]
    schools <- eligible_sites.reactive()[eligible_sites.reactive()$site_type == "Schools",]
    head_starts <- eligible_sites.reactive()[eligible_sites.reactive()$site_type == "Head Start Centers",]
    homeless_shelters <- eligible_sites.reactive()[eligible_sites.reactive()$site_type == "Homeless Shelters",]  
    
    leafletProxy("map") %>%
      clearGroup(group = "Eligible Sites")
    
    if (nrow(fcrc)!=0)
      leafletProxy("map") %>%
      addAwesomeMarkers(data=fcrc,
                        lng=~longitude,
                        lat=~latitude,
                        icon = ~IconSet["FCRC"],
                        popup=~paste0(site_name,
                                      "</br/>", site_address,
                                      "</br/>", site_city, ", ", site_state, " ", site_zip),
                        label = ~as.character(site_name),
                        group = "Eligible Sites",
                        clusterOptions = markerClusterOptions(disableClusteringAtZoom=14, spiderfyOnMaxZoom=FALSE, showCoverageOnHover=FALSE))
    
    if (nrow(fqhc)!=0)
      leafletProxy("map") %>%
      addAwesomeMarkers(data=fqhc,
                        lng=~longitude,
                        lat =~latitude,
                        icon = ~IconSet["FQHC"],
                        popup =~paste0(site_name,
                                       "</br/>", site_address,
                                       "</br/>", site_city, ", ", site_state, " ", site_zip),
                        label = ~as.character(site_name),
                        group = "Eligible Sites",
                        clusterOptions = markerClusterOptions(disableClusteringAtZoom=14, spiderfyOnMaxZoom=FALSE, showCoverageOnHover=FALSE))
    
    if (nrow(wic)!=0)
      leafletProxy("map") %>%
      addAwesomeMarkers(data=wic,
                        lng =~longitude,
                        lat =~latitude,
                        icon = ~IconSet["WIC"],
                        popup =~paste0(site_name,
                                       "</br/>", site_address,
                                       "</br/>", site_city, ", ", site_state, " ", site_zip),
                        label = ~as.character(site_name),
                        group = "Eligible Sites",
                        clusterOptions = markerClusterOptions(disableClusteringAtZoom=14, spiderfyOnMaxZoom=FALSE, showCoverageOnHover=FALSE))
    
    if (nrow(schools)!=0)
      leafletProxy("map") %>%
      addAwesomeMarkers(data=schools,
                        lng =~longitude,
                        lat =~latitude,
                        icon = ~IconSet["Schools"],
                        popup =~paste0(site_name,
                                       "</br/>", site_address,
                                       "</br/>", site_city, ", ", site_state, " ", site_zip),
                        label = ~as.character(site_name),
                        group = "Eligible Sites",
                        clusterOptions = markerClusterOptions(disableClusteringAtZoom=14, spiderfyOnMaxZoom=FALSE, showCoverageOnHover=FALSE))
    
    if (nrow(head_starts)!=0)
      leafletProxy("map") %>%
      addAwesomeMarkers(data=head_starts,
                        lng =~longitude,
                        lat =~latitude,
                        icon = ~IconSet["Head Start Centers"],
                        popup =~paste0(site_name,
                                       "</br/>", site_address,
                                       "</br/>", site_city, ", ", site_state, " ", site_zip),
                        label = ~as.character(site_name),
                        group = "Eligible Sites",
                        clusterOptions = markerClusterOptions(disableClusteringAtZoom=14, spiderfyOnMaxZoom=FALSE, showCoverageOnHover=FALSE))
    
    if (nrow(homeless_shelters)!=0)
      leafletProxy("map") %>%
      addAwesomeMarkers(data=homeless_shelters,
                        lng =~longitude,
                        lat =~latitude,
                        icon = ~IconSet["Homeless Shelters"],
                        popup =~paste0(site_name,
                                       "</br/>", site_address,
                                       "</br/>", site_city, ", ", site_state, " ", site_zip),
                        label = ~as.character(site_name),
                        group = "Eligible Sites",
                        clusterOptions = markerClusterOptions(disableClusteringAtZoom=14, spiderfyOnMaxZoom=FALSE, showCoverageOnHover=FALSE))
  })        
  
  #Community Profile
  
  observeEvent(
    # define pickerinputs to be observed
    c(
      input$unit2Input,
      input$network2Input
    ),
    {
      if (!is.null(input$unit2Input) & is.null(input$network2Input)) {
        
        snap_recipient_households_demo_tracts2 <- snap_recipient_households_demo_tracts %>%
          filter(unit %in% input$unit2Input)
        
        snap_recipient_households_demo_cities2 <- snap_recipient_households_demo_cities %>%
          filter(unit %in% input$unit2Input)
        
        updatePickerInput(
          session,
          "countyInput",
          choices = levels(factor(snap_recipient_households_demo_tracts2$county)),
          selected = NULL
        )
        
        updatePickerInput(
          session,
          "network2Input",
          choices = levels(as.factor(c(snap_recipient_households_demo_cities$network_name,
                                       snap_recipient_households_demo_tracts$network_name))),
          selected = NULL
        )
      } else if (!is.null(input$network2Input)) {
        
        snap_recipient_households_demo_cities2 <- snap_recipient_households_demo_cities %>%
          filter(network_name %in% input$network2Input)
        
        updatePickerInput(
          session,
          "cityInput",
          choices = levels(factor(snap_recipient_households_demo_cities2$geographic_area_name)),
          selected = NULL
        )
        
        
      } else{
        updatePickerInput(
          session,
          "countyInput",
          choices = levels(factor(snap_recipient_households_demo_tracts$county)),
          selected = NULL
        )
        updatePickerInput(
          session,
          "network2Input",
          choices = levels(as.factor(c(snap_recipient_households_demo_cities$network_name,
                                       snap_recipient_households_demo_tracts$network_name))),
          selected = NULL
        )
        updatePickerInput(
          session,
          "cityInput",
          choices = levels(factor(snap_recipient_households_demo_cities$geographic_area_name)),
          selected = NULL
        )
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = F
  )
  
  snap_recipient_households_demo.reactive <-
    reactive({
      
      if (!is.null(input$unit2Input) & is.null(input$countyInput) & is.null(input$network2Input)) {  
        
        snap_recipient_households_demo_tracts %>%
          group_by(unit, demo) %>%
          summarise(snap_recipient_households = sum(snap_recipient_households),
                    total_snap_recipient_households = sum(total_snap_recipient_households)) %>%
          ungroup() %>% filter(unit %in% input$unit2Input)
        
      } else if (!is.null(input$countyInput)) {
        
        snap_recipient_households_demo_tracts %>%
          group_by(county, demo) %>%
          summarise(snap_recipient_households = sum(snap_recipient_households),
                    total_snap_recipient_households = sum(total_snap_recipient_households)) %>%
          ungroup() %>% filter(county %in% input$countyInput)
        
      } else if (!is.null(input$network2Input) & is.null(input$cityInput)) {
        if (input$network2Input %in% snap_recipient_households_demo_cities$network_name) {
          
          snap_recipient_households_demo_cities %>%
            group_by(network_name, demo) %>%
            summarise(snap_recipient_households = sum(snap_recipient_households),
                      total_snap_recipient_households = sum(total_snap_recipient_households)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        } else {
          snap_recipient_households_demo_tracts %>%
            group_by(network_name, demo) %>%
            summarise(snap_recipient_households = sum(snap_recipient_households),
                      total_snap_recipient_households = sum(total_snap_recipient_households)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        }
        
        
      } else if (!is.null(input$cityInput)) {
        
        snap_recipient_households_demo_cities %>% filter(geographic_area_name %in% input$cityInput)
        
      } else {
        
        snap_recipient_households_demo_tracts %>%
          group_by(state, demo) %>%
          summarise(snap_recipient_households = sum(snap_recipient_households),
                    total_snap_recipient_households = sum(total_snap_recipient_households)) %>%
          ungroup()
        
      }
      
    })
  
  poverty_individuals_age.reactive <-
    reactive({
      
      if (!is.null(input$unit2Input) & is.null(input$countyInput) & is.null(input$network2Input)) {  
        
        poverty_individuals_age_tracts %>%
          group_by(unit, age, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population), 
                    individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)) %>%
          ungroup() %>% filter(unit %in% input$unit2Input)
        
      } else if (!is.null(input$countyInput)) {
        
        poverty_individuals_age_tracts %>%
          group_by(county, age, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population), 
                    individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)) %>%
          ungroup() %>% filter(county %in% input$countyInput)
        
      } else if (!is.null(input$network2Input) & is.null(input$cityInput)) {
        
        if (input$network2Input %in% poverty_individuals_age_cities$network_name) {
          
          poverty_individuals_age_cities %>%
            group_by(network_name, age, poverty_status) %>%
            summarise(individuals = sum(individuals),
                      total_population = sum(total_population),
                      individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        } else {
          poverty_individuals_age_tracts %>%
            group_by(network_name, age, poverty_status) %>%
            summarise(individuals = sum(individuals),
                      total_population = sum(total_population),
                      individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        }
        
      } else if (!is.null(input$cityInput)) {
        
        poverty_individuals_age_cities %>% filter(geographic_area_name %in% input$cityInput)
        
      } else {
        
        poverty_individuals_age_tracts %>%
          group_by(state, age, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population), 
                    individuals_income_below_185_percent_poverty_level = sum(individuals_income_below_185_percent_poverty_level)) %>%
          ungroup()
        
      }
      
    })
  
  poverty_individuals_sex.reactive <-
    reactive({
      
      if (!is.null(input$unit2Input) & is.null(input$countyInput) & is.null(input$network2Input)) {  
        
        poverty_individuals_sex_tracts %>%
          group_by(unit, sex, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population)) %>%
          ungroup() %>% filter(unit %in% input$unit2Input)
        
      } else if (!is.null(input$countyInput)) {
        
        poverty_individuals_sex_tracts %>%
          group_by(county, sex, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population)) %>%
          ungroup() %>% filter(county %in% input$countyInput)
        
      } else if (!is.null(input$network2Input) & is.null(input$cityInput)) {
        
        if (input$network2Input %in% poverty_individuals_sex_cities$network_name) {
          
          poverty_individuals_sex_cities %>%
            group_by(network_name, sex, poverty_status) %>%
            summarise(individuals = sum(individuals),
                      total_population = sum(total_population)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        } else {
          poverty_individuals_sex_tracts %>%
            group_by(network_name, sex, poverty_status) %>%
            summarise(individuals = sum(individuals),
                      total_population = sum(total_population)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        }
        
      } else if (!is.null(input$cityInput)) {
        
        poverty_individuals_sex_cities %>% filter(geographic_area_name %in% input$cityInput)
        
      } else {
        
        poverty_individuals_sex_tracts %>%
          group_by(state, sex, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population)) %>%
          ungroup()
      }
      
    })
  
  lep_households.reactive <-
    reactive({
      
      if (!is.null(input$unit2Input) & is.null(input$countyInput) & is.null(input$network2Input)) {  
        
        lep_households_tracts %>%
          group_by(unit, language) %>%
          summarise(households = sum(households),
                    total_lep_households = sum(total_lep_households),
                    total_households = sum(total_households)) %>%
          ungroup() %>% filter(unit %in% input$unit2Input)
        
      } else if (!is.null(input$countyInput)) {
        
        lep_households_tracts %>%
          group_by(county, language) %>%
          summarise(households = sum(households),
                    total_lep_households = sum(total_lep_households),
                    total_households = sum(total_households)) %>%
          ungroup() %>% filter(county %in% input$countyInput)
        
      } else if (!is.null(input$network2Input) & is.null(input$cityInput)) {
        
        if (input$network2Input %in% lep_households_cities$network_name) {
          
          lep_households_cities %>%
            group_by(network_name, language) %>%
            summarise(households = sum(households),
                      total_lep_households = sum(total_lep_households),
                      total_households = sum(total_households)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        } else {
          lep_households_tracts %>%
            group_by(network_name, language) %>%
            summarise(households = sum(households),
                      total_lep_households = sum(total_lep_households),
                      total_households = sum(total_households)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        }
        
      } else if (!is.null(input$cityInput)) {
        
        lep_households_cities %>% filter(geographic_area_name %in% input$cityInput)
        
      } else {
        
        lep_households_tracts %>%
          group_by(state, language) %>%
          summarise(households = sum(households),
                    total_lep_households = sum(total_lep_households),
                    total_households = sum(total_households)) %>%
          ungroup()
      }
      
    })
  
  
  observe({
    output$plot1 <- renderPlotly({
      
      snap_recipient_households_race <- snap_recipient_households_demo.reactive() %>%
        filter(!demo %in% c("Hispanic Or Latino Origin", "No Hispanic Or Latino Origin"))
      
      percent_eligible = paste0(as.character(round((100 * snap_recipient_households_race$snap_recipient_households / snap_recipient_households_race$total_snap_recipient_households), digits = 1)), "%")
      
      ggplotly(ggplot(snap_recipient_households_race, aes(x=demo, y=snap_recipient_households, fill=demo)) +
                 geom_col(show.legend = FALSE) +
                 geom_text(aes(label=percent_eligible, y = snap_recipient_households+0.02*max(snap_recipient_households)), position = position_dodge(width = 0.6)) +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1),
                       axis.title.x = element_blank(), axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5)) +
                 ggtitle("SNAP Households by Race") +
                 scale_x_discrete(labels = c("American Indian And Alaska Native" = "American Indian\nAnd Alaska Native",
                                             "Black Or African American" = "Black Or\nAfrican American",
                                             "Native Hawaiian And Other Pacific Islander" = "Native Hawaiian\nAnd Other\nPacific Islander",
                                             "Some Other Race" = "Some Other\nRace",
                                             "Two Or More Races" = "Two Or More\nRaces"
                 )) +
                 scale_y_continuous(labels = scales::comma), tooltip = c("y")) %>%
        layout(showlegend = FALSE) %>%
        style(hoverinfo = "none", traces = c(8, 9, 10, 11, 12, 13, 14)) #https://stackoverflow.com/questions/51004936/ggplotly-only-return-tooltip-hover-text-on-certain-geom-objects
    })
  })
  
  
  observe({
    snap_recipient_households_ethnicity <- snap_recipient_households_demo.reactive() %>%
      filter(demo %in% c("Hispanic Or Latino Origin", "No Hispanic Or Latino Origin"))
    
    output$plot6 <- renderPlotly({
      
      percent_eligible = paste0(as.character(round((100 * snap_recipient_households_ethnicity$snap_recipient_households / snap_recipient_households_ethnicity$total_snap_recipient_households), digits = 1)), "%")
      
      ggplotly(ggplot(snap_recipient_households_ethnicity, aes(x=demo, y=snap_recipient_households, fill=demo)) +
                 geom_col(show.legend = FALSE) +
                 geom_text(aes(label=percent_eligible, y = snap_recipient_households+0.02*max(snap_recipient_households)), position = position_dodge(width = 0.6)) +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1),
                       axis.title.x = element_blank(), axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5)) +
                 ggtitle("SNAP Households by Ethnicity") +
                 scale_x_discrete(labels = c("Hispanic Or Latino Origin" = "Hispanic Or\nLatino Origin",
                                             "No Hispanic Or Latino Origin" = "No Hispanic Or\nLatino Origin")) +
                 scale_y_continuous(labels = scales::comma), tooltip = c("y")) %>%
        layout(showlegend = FALSE) %>%
        style(hoverinfo = "none", traces = c(3, 4)) #https://stackoverflow.com/questions/51004936/ggplotly-only-return-tooltip-hover-text-on-certain-geom-objects
    })
  })
  
  observe({
    output$plot2 <- renderPlotly({
      percent_total_population = paste0(as.character(round((100 * poverty_individuals_age.reactive()$individuals / poverty_individuals_age.reactive()$total_population), digits = 1)), "%")
      
      age_level_order <- factor(poverty_individuals_age.reactive()$age, level = c("Under 5", "5 to 17", "18 to 34", "35 to 64", "65 and Over"))
      
      ggplotly(ggplot(poverty_individuals_age.reactive(), aes(x=age_level_order, y=individuals, fill=factor(poverty_status))) +
                 geom_col(position="dodge") +
                 geom_text(aes(label=percent_total_population, y = individuals+0.02*max(individuals)), position = position_dodge(width = 0.9)) +
                 theme(#axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.title.x = element_blank(), axis.title.y = element_blank(),
                   plot.title = element_text(hjust = 0.5)) +
                 ggtitle("Poverty Status of Individuals by Age") +
                 scale_fill_discrete(name = "Poverty Status") +
                 scale_y_continuous(labels = scales::comma), tooltip = c("y"))  %>%
        style(hoverinfo = "none", traces = c(3, 4))
    })
    
    output$text1 <- renderUI({
      HTML(
        paste("<b>Individuals Earning Below 185% Poverty Level:</b>",
              as.character(format(unique(poverty_individuals_age.reactive()$individuals_income_below_185_percent_poverty_level), big.mark=",")),
              "<b>Percentage of Eligible Individuals:</b>",
              paste0(as.character(unique(round(100 * poverty_individuals_age.reactive()$individuals_income_below_185_percent_poverty_level/
                                                 poverty_individuals_age.reactive()$total_population, digits = 1))), "%"), sep = "<br/>")
      )
    })
    
  })
  
  observe({
    output$plot3 <- renderPlotly({
      percent_total_population = paste0(as.character(round((100 * poverty_individuals_sex.reactive()$individuals / poverty_individuals_sex.reactive()$total_population), digits = 1)), "%")
      
      ggplotly(ggplot(poverty_individuals_sex.reactive(), aes(x=sex, y=individuals, fill=factor(poverty_status))) +
                 geom_col(position="dodge") +
                 geom_text(aes(label=percent_total_population, y = individuals+0.02*max(individuals)), position = position_dodge(width = 0.9)) +
                 theme(#axis.text.x = element_text(angle = 45, hjust = 1),
                   axis.title.x = element_blank(), axis.title.y = element_blank(),
                   plot.title = element_text(hjust = 0.5)) +
                 ggtitle("Poverty Status of Individuals by Sex") +
                 scale_y_continuous(labels = scales::comma), tooltip = c("y"))  %>%
        style(hoverinfo = "none", traces = c(3, 4)) %>%
        layout(showlegend = FALSE)
    })
  })
  
  observe({
    output$plot4 <- renderPlotly({
      percent_lep = paste0(as.character(round((100 * lep_households.reactive()$households / lep_households.reactive()$total_lep_households), digits = 1)), "%")
      
      ggplotly(ggplot(lep_households.reactive(), aes(x=language, y=households, fill=language)) +
                 geom_col(show.legend = FALSE) +
                 geom_text(aes(label=percent_lep, y = households+0.02*max(households)), position = position_dodge(width = 0.6)) +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1),
                       axis.title.x = element_blank(), axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5)) +
                 ggtitle("LEP Households by Language") +
                 scale_x_discrete(labels = c("Asian And Pacific Island Languages" = "Asian And\nPacific Island\nLanguages",
                                             "Other Indo European Languages" = "Other\nIndo European\nLanguages")) +
                 scale_y_continuous(labels = scales::comma),
               tooltip = c("y")
      ) %>%
        layout(showlegend = FALSE) %>%
        style(hoverinfo = "none", traces = c(5, 6, 7, 8))
    })
    
    output$text2 <- renderUI({
      HTML(
        paste("<b>LEP Households:</b>",
              as.character(format(unique(lep_households.reactive()$total_lep_households), big.mark=",")),
              "<b>Percentage of LEP Households:</b>",
              paste0(as.character(unique(round(100 * lep_households.reactive()$total_lep_households/
                                                 lep_households.reactive()$total_households, digits = 1))), "%"), sep = "<br/>")
      )
    })
    
  })
  
  
  adult_obesity.reactive <-
    reactive({
      
      if (!is.null(input$countyInput)) {
        
        adult_obesity %>% filter(county %in% input$countyInput)
        
      } else {
        
        adult_obesity %>% filter(county == "")
        
      }
      
    })
  
  
  observe({
    
    if ((!is.null(input$countyInput) & is.null(input$network2Input) & is.null(input$cityInput)) |
        (is.null(input$unit2Input) & is.null(input$network2Input) & is.null(input$cityInput))) {
      
      output$text3 <- renderUI({
        HTML(
          paste("<b>Adult Obesity Rate:</b>",
                paste0(as.character(adult_obesity.reactive()$percent_adults_obesity), "%"), sep = "<br/>")
        )
      })
      
    } else {
      output$text3 <- renderUI({
        HTML("This metric is only available by County/State.")
      })
    }
    
  })
  
  food_insecurity.reactive <-
    reactive({
      
      if (!is.null(input$countyInput)) {
        
        food_insecurity %>% filter(county %in% input$countyInput)
        
      } else {
        
        food_insecurity %>% filter(county == "Illinois")
        
      }
      
    })
  
  
  observe({
    
    if ((!is.null(input$countyInput) & is.null(input$network2Input) & is.null(input$cityInput)) |
        (is.null(input$unit2Input) & is.null(input$network2Input) & is.null(input$cityInput))) {
      
      output$text4 <- renderUI({
        HTML(
          paste("<b>2018 Food Insecurity Rate:</b>",
                as.character(food_insecurity.reactive()$food_insecurity_rate),
                "<b>Food Insecure Persons in 2018:</b>",
                as.character(food_insecurity.reactive()$food_insecure_persons), sep = "<br/>")
          
          
        )
      })
      
      output$text5 <- renderUI({
        HTML(
          paste("<b>2018 Child Food Insecurity Rate:</b>",
                as.character(food_insecurity.reactive()$child_food_insecurity_rate),
                "<b>Food Insecure Children in 2018:</b>",
                as.character(food_insecurity.reactive()$food_insecure_children), sep = "<br/>")
          
          
        )
      })
      
    } else {
      output$text4 <- renderUI({
        HTML("This metric is only available by County/State.")
      })
      output$text5 <- renderUI({
        HTML("This metric is only available by County/State.")
      })
    }
    
  })
  
  observeEvent(input$reset_input, {
    #ids > vector > if notnull() > reset()
    shinyjs::reset("unit2Input")
    shinyjs::reset("countyInput")
    shinyjs::reset("network2Input")
    shinyjs::reset("cityInput")
  })
  
  poverty_individuals_demo.reactive <-
    reactive({
      
      if (!is.null(input$unit2Input) & is.null(input$countyInput) & is.null(input$network2Input)) {  
        
        poverty_individuals_demo_tracts %>%
          group_by(unit, demo, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population)) %>%
          ungroup() %>% filter(unit %in% input$unit2Input)
        
      } else if (!is.null(input$countyInput)) {
        
        poverty_individuals_demo_tracts %>%
          group_by(county, demo, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population)) %>%
          ungroup() %>% filter(county %in% input$countyInput)
        
      } else if (!is.null(input$network2Input) & is.null(input$cityInput)) {
        
        if (input$network2Input %in% poverty_individuals_demo_cities$network_name) {
          
          poverty_individuals_demo_cities %>%
            group_by(network_name, demo, poverty_status) %>%
            summarise(individuals = sum(individuals),
                      total_population = sum(total_population)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        } else {
          poverty_individuals_demo_tracts %>%
            group_by(network_name, demo, poverty_status) %>%
            summarise(individuals = sum(individuals),
                      total_population = sum(total_population)) %>%
            ungroup() %>% filter(network_name %in% input$network2Input)
        }
        
      } else if (!is.null(input$cityInput)) {
        
        poverty_individuals_demo_cities %>% filter(geographic_area_name %in% input$cityInput)
        
      } else {
        
        poverty_individuals_demo_tracts %>%
          group_by(state, demo, poverty_status) %>%
          summarise(individuals = sum(individuals),
                    total_population = sum(total_population)) %>%
          ungroup()
        
      }
      
    })
  
  observe({
    output$plot5 <- renderPlotly({
      
      poverty_individuals_race <- poverty_individuals_demo.reactive() %>%
        filter(!demo %in% c("Hispanic Or Latino Origin", "No Hispanic Or Latino Origin"))
      
      percent_total_population = paste0(as.character(round((100 * poverty_individuals_race$individuals / poverty_individuals_race$total_population), digits = 1)), "%")
      
      #demo_level_order <- factor(poverty_individuals_demo.reactive()$demo, level = c("American Indian And Alaska Native", "Asian", "Black Or African American", "Native Hawaiian And Other Pacific Islander", "Some Other Race", "Two Or More Races", "White", "Hispanic Or Latino Origin"))
      
      ggplotly(ggplot(poverty_individuals_race, aes(x=demo, y=individuals, fill=factor(poverty_status))) +
                 geom_col(position="dodge") +
                 geom_text(aes(label=percent_total_population, y = individuals+0.02*max(individuals)), position = position_dodge(width = 0.9)) +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1),
                       axis.title.x = element_blank(), axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5)) +
                 ggtitle("Poverty Status of Individuals by Race") +
                 scale_x_discrete(labels = c("American Indian And Alaska Native" = "American Indian\nAnd Alaska Native",
                                             "Black Or African American" = "Black Or\nAfrican American",
                                             "Native Hawaiian And Other Pacific Islander" = "Native Hawaiian\nAnd Other\nPacific Islander"
                                             #    "Some Other Race" = "Some Other\nRace",
                                             #    "Two Or More Races" = "Two Or More\nRaces",
                 )) +
                 scale_fill_discrete(name = "Poverty Status") +
                 scale_y_continuous(labels = scales::comma), tooltip = c("y")) %>%
        style(hoverinfo = "none", traces = c(3, 4)) %>%
        layout(showlegend = FALSE)
    })
  })
  
  observe({
    output$plot7 <- renderPlotly({
      
      poverty_individuals_ethnicity <- poverty_individuals_demo.reactive() %>%
        filter(demo %in% c("Hispanic Or Latino Origin", "No Hispanic Or Latino Origin"))
      
      percent_total_population = paste0(as.character(round((100 * poverty_individuals_ethnicity$individuals / poverty_individuals_ethnicity$total_population), digits = 1)), "%")
      
      #demo_level_order <- factor(poverty_individuals_demo.reactive()$demo, level = c("American Indian And Alaska Native", "Asian", "Black Or African American", "Native Hawaiian And Other Pacific Islander", "Some Other Race", "Two Or More Races", "White", "Hispanic Or Latino Origin"))
      
      ggplotly(ggplot(poverty_individuals_ethnicity, aes(x=demo, y=individuals, fill=factor(poverty_status))) +
                 geom_col(position="dodge") +
                 geom_text(aes(label=percent_total_population, y = individuals+0.02*max(individuals)), position = position_dodge(width = 0.9)) +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1),
                       axis.title.x = element_blank(), axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5)) +
                 ggtitle("Poverty Status of Individuals by Ethnicity") +
                 scale_x_discrete(labels = c("Hispanic Or Latino Origin" = "Hispanic Or\nLatino Origin",
                                             "No Hispanic Or Latino Origin" = "No Hispanic Or\nLatino Origin")) +
                 scale_fill_discrete(name = "Poverty Status") +
                 scale_y_continuous(labels = scales::comma), tooltip = c("y")) %>%
        style(hoverinfo = "none", traces = c(3, 4))
    })
  })
  
}


shinyApp(ui, server)
# SNAP-Ed Community Networks Dashboard

## Description

The SNAP-Ed Community Networks Dashboard serves as an internal strategic planning resource for [Illinois SNAP-Ed](https://inep.extension.illinois.edu/who-we-are/our-story/snap-ed) staff. 
Illinois Extension defines a SNAP-Ed community network as a geographical area where SNAP-Ed eligible adults live, work, shop, and access community resources.
Field staff can use the SNAP-Ed Community Networks Map tab to explore local SNAP-Ed programming and public assistance sites.
The Community Profile tab visualizes demographic characteristics throughout Illinois by unit, county, community network, or city. 
Video tutorials for utilizing the SNAP-Ed Community Networks Dashboard are available at the following links: 

- [SNAP-Ed Community Networks Dashboard Part 1: Map](https://mediaspace.illinois.edu/media/t/1_gpbd7xiz)
- [SNAP-Ed Community Networks Dashboard Part 2: Profile](https://mediaspace.illinois.edu/media/t/1_gcy8heuq)

## Application

The dashboard is accessible via [this link](https://ilsnap-ed.shinyapps.io/community_networks_dashboard_fy_22/).

## Data Sources

### INEP Program Sites

INEP Program Sites for the 2022 report year are sourced from PEARS. PEARS module exports included as example files are generated using the [Faker](https://faker.readthedocs.io/en/master/) 
Python package and do not represent actual program evaluation data.
[Office of Educational Innovation and Evaluation (OEIE) - PEARS](https://www.k-state.edu/oeie/pears/)

### Community Sites

The following sources are utilized for community sites:

- Family Community Resource Centers (FCRC) and Women, Infants, and Children (WIC) Offices - [IDHS Office Locator](https://www.dhs.state.il.us/page.aspx?module=12)
- Federally Qualified Health Centers (FQHC) - [HRSA Data by Geography](https://data.hrsa.gov/geo)
- Head Start Centers - [Head Start Center Locator]https://eclkc.ohs.acf.hhs.gov/center-locator)
- Eligible Schools - [ISBE Free and Reduced-Price Meal Eligibility Data](https://www.isbe.net/Pages/Nutrition-Data-Analytics-Maps.aspx)
- Emergency Shelters - [Illinois Pantries, Soup Kitchens and Emergency Shelters](http://www.illinoisfoodbanks.org/sites.asp)

### Food Insecurity
Feeding America\'s *Map the Meal Gap* study provides annual estimates of Food Insecurity rates.
[Feeding America | Food Insecurity Report Briefs](https://www.feedingamerica.org/research/map-the-meal-gap/overall-executive-summary)

### Adult Obesity
Adult Obesity Rate is calculated by County Health Rankings and based on responses to the Behavioral Risk Factor Surveillance Survey (BRFSS). 
[County Health Rankings | Adult Obesity](https://www.countyhealthrankings.org/app/illinois/2022/measure/factors/11/description)

### Demographics

The following demographic measures are provided by the 2020 American Community Survey 5-year estimates.
[About the American Community Survey](https://www.census.gov/programs-surveys/acs/about.html)
- Poverty Status: [S1701 POVERTY STATUS IN THE PAST 12 MONTHS](https://data.census.gov/cedsci/table?q=185%20federal%20poverty%20level&tid=ACSST5Y2020.S1701)
- Limited English Proficiency: [S1602 LIMITED ENGLISH SPEAKING HOUSEHOLDS](https://data.census.gov/cedsci/table?q=S1602&tid=ACSST5Y2020.S1602)
- SNAP Recipient Households: [S2201 FOOD STAMPS/SUPPLEMENTAL NUTRITION ASSISTANCE PROGRAM (SNAP)](https://data.census.gov/cedsci/table?q=S2201&tid=ACSST5Y2020.S2201)

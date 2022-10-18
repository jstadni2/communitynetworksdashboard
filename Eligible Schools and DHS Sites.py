import os
import pandas as pd
import numpy as np

# Calculate the path to the root directory of this script
ROOT_DIR = os.path.realpath(os.path.join(os.path.dirname(__file__), '.'))
os.chdir(ROOT_DIR)

## Eligible Schools

#download xlsx of site list from https://www.isbe.net/Pages/Nutrition-Data-Analytics-Maps.aspx

ffr_schools = pd.read_excel(r"Free and Reduced-Price Lunch Eligibility List (2020).xlsx", header=3)

eligible_schools = ffr_schools.loc[ffr_schools['Eligibility Percent'] >= 50, ['Site', 'Site Address', 'Site City', 'Site County', 'Site Zip']].rename(
    columns={'Site' : 'site_name', 'Site Address' : 'site_address', 'Site City' : 'site_city', 'Site County' : 'site_county', 'Site Zip' : 'site_zip'})
eligible_schools['site_state'] = 'IL'

eligible_schools.to_csv('eligible_schools.csv', index=False)

## WIC Sites

#download text of site list from https://www.dhs.state.il.us/page.aspx?module=12&officetype=&county

foo = open('WIC_Sites.txt')
lines = foo.readlines()
foo.close

for index, line in enumerate(lines):
    lines[index] = line.strip()
    
a = np.array(lines)

wic_indices = []

for i in range(len(lines)):
    if lines[i] == 'Women, Infants, and Children':
        wic_indices.append(i)

site_indices = [x - 1 for x in wic_indices]
site_name = a[site_indices]

add_indices = [x + 1 for x in wic_indices]
site_address = a[add_indices]

city_state_zip_indices = [x + 2 for x in wic_indices]
site_city_state_zip = a[city_state_zip_indices ]

phone_indices = [x + 4 for x in wic_indices]
site_phone = a[phone_indices ]

fax_indices = [x + 6 for x in wic_indices]
site_fax = a[fax_indices ]

d = {'site_name' : site_name, 'site_address' : site_address, 'site_city_state_zip' : site_city_state_zip, 'site_phone' : site_phone, 'site_fax' : site_fax}

wic_sites = pd.DataFrame(d)

wic_sites['site_city_state_zip2'] = wic_sites['site_city_state_zip'].str.split(pat=',')
wic_sites['site_city'] = wic_sites['site_city_state_zip2'].str[0]
wic_sites['site_state'] = 'IL'
wic_sites['site_zip'] = wic_sites['site_city_state_zip2'].str[-1].str.replace('IL ', '')

wic_sites['site_phone'] = wic_sites['site_phone'].str.replace('Phone: ', '')
wic_sites['site_fax'] = wic_sites['site_fax'].str.replace('Fax: ', '')

wic_sites = wic_sites.loc[~wic_sites.duplicated(subset=['site_name', 'site_address'], keep='first')]

wic_sites.drop(columns=['site_city_state_zip', 'site_city_state_zip2']).to_csv('wic_sites.csv', index=False)


## Family Community Resource Centers

#download text of site list from https://www.dhs.state.il.us/page.aspx?module=12&officetype=&county

foo = open('FCRC_Sites.txt')
lines = foo.readlines()
foo.close

for index, line in enumerate(lines):
    lines[index] = line.strip()
    
a = np.array(lines)

fcrc_indices = []

for i in range(len(lines)):
    if lines[i] == 'Family Community Resource Center':
        fcrc_indices.append(i)

site_indices = [x - 1 for x in fcrc_indices]
site_name = a[site_indices]

add_indices = [x + 1 for x in fcrc_indices]
site_address = a[add_indices]

city_state_zip_indices = [x + 2 for x in fcrc_indices]
site_city_state_zip = a[city_state_zip_indices ]

phone_indices = [x + 4 for x in fcrc_indices]
site_phone = a[phone_indices ]

fax_indices = [x + 6 for x in fcrc_indices]
site_fax = a[fax_indices ]

d = {'site_name' : site_name, 'site_address' : site_address, 'site_city_state_zip' : site_city_state_zip, 'site_phone' : site_phone, 'site_fax' : site_fax}

fcrc_sites = pd.DataFrame(d)

fcrc_sites['site_city_state_zip2'] = fcrc_sites['site_city_state_zip'].str.split(pat=',')
fcrc_sites['site_city'] = fcrc_sites['site_city_state_zip2'].str[0]
fcrc_sites['site_state'] = 'IL'
fcrc_sites['site_zip'] = fcrc_sites['site_city_state_zip2'].str[-1].str.replace('IL ', '')

fcrc_sites['site_phone'] = fcrc_sites['site_phone'].str.replace('Phone: ', '')
fcrc_sites['site_fax'] = fcrc_sites['site_fax'].str.replace('Fax: ', '')

fcrc_sites = fcrc_sites.loc[~fcrc_sites.duplicated(subset=['site_name', 'site_address'], keep='first')]

fcrc_sites.drop(columns=['site_city_state_zip', 'site_city_state_zip2']).to_csv('fcrc_sites.csv', index=False)


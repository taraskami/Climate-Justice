# Install necessary libraries for data manipulation
install.packages("dplyr")
install.packages("devtools")
devtools::install_github("mkuhn/dict")
library(dplyr)
library(dict)

# Read in per Capita C02 Emissions data
emissions = read.csv(file='https://raw.githubusercontent.com/taraskami/Climate-Justice/master/Data_/raw/co-emissions-per-capita.csv')
colnames(emissions) = c('Country','Code','Year','PC_Emissions')
emissions = emissions[which(emissions$Year >= 1970),]

# Read in Country Land Area data  (in square kilometers)
land_area = read.csv('https://raw.githubusercontent.com/taraskami/Climate-Justice/master/Data_/raw/API_AG.LND.TOTL.K2_DS2_en_csv_v2_1221051.csv')
land_area = land_area[c('Country.Name','Country.Code','X2018')]

# Read in Temperature Change data
temperature = read.csv(file='https://raw.githubusercontent.com/taraskami/Climate-Justice/master/Data_/raw/FAOSTAT_data_7-15-2020.csv')
temperature = temperature[c('Area','Year','Value')]
colnames(temperature) = c('Country','Year','Temp_Change')

# Read in Population data
pop = read.csv(file='https://raw.githubusercontent.com/taraskami/Climate-Justice/master/Data_/raw/FAOSTAT_data_7-15-2020_pop.csv')
pop = pop[c('Area','Year','Value')]
colnames(pop) = c('Country','Year','Population')
pop['Population'] = pop['Population']*1000

# Read in data to create chloropleth map
map_data = read.csv('https://raw.githubusercontent.com/taraskami/Climate-Justice/master/Data_/raw/AnyConv.com__TM_WORLD_BORDERS_SIMPL-0.3.csv')
map_data['order_col'] = 1:nrow(map_data)

# Join population and temperature data on Country and Name
pop_temp = merge(pop,temperature,by=c('Country','Year'))

# Standardize Country names so that join between dataset with population and temperature data
# and dataset with C02 emissions data can be performed. 

# Create dictionary to perform the country name mappings
country_map = dict()
country_map['Bolivia (Plurinational State of)'] = 'Bolivia'
country_map['Brunei Darussalam'] = 'Brunei'
country_map['Cabo Verde'] = 'Cape Verde'
country_map["Côte d'Ivoire"] = "Cote d'Ivoire"
country_map['Czechia'] = 'Czech Republic'
country_map['Democratic Republic of the Congo'] = 'Democratic Republic of Congo'
country_map['China, Hong Kong SAR'] = 'Hong Kong'
country_map['Iran (Islamic Republic of)'] = 'Iran'
country_map["Lao People's Democratic Republic"] = 'Laos'
country_map['North Macedonia"'] = 'Macedonia'
country_map['Micronesia (Federated States of)'] = 'Micronesia'
country_map['Republic of Moldova'] = 'Moldova'
country_map["Democratic People's Republic of Korea"] = 'North Korea'
country_map['Russia'] = 'Russian Federation'
country_map['Republic of Korea'] = 'South Korea'
country_map['Eswatini'] = 'Swaziland'
country_map['Syrian Arab Republic'] = 'Syria'
country_map['China, Taiwan Province of'] = 'Taiwan'
country_map['United Republic of Tanzania'] = 'Tanzania'
country_map['Timor-Leste'] = 'Timor'
country_map['United Kingdom of Great Britain and Northern Ireland'] = 'United Kingdom'
country_map['United States of America'] = 'United States'
country_map['Venezuela (Bolivarian Republic of)'] = 'Venezuela'
country_map['Viet Nam'] = 'Vietnam'

# Create function which performs the country name mappings
map_names = function(country){
  if(country %in% names(country_map)){
    return(country_map[country])
  }
  return(country)
}

# Perform country name mappings
pop_temp['Country'] = as.vector(unlist(sapply(pop_temp$Country,FUN = map_names)))

# Join population/temperature dataset with emissions dataset on Country and Year
ptc_df = merge(pop_temp,emissions,by=c('Country','Year'))

# create total C02 emissions for each country for each year from 1970-2017
ptc_df['Total_Emissions'] = ptc_df$Population * ptc_df$PC_Emissions

# Join population/temperature/emissions dataset with the country land area dataset
ptcl_df = na.omit(merge(ptc_df,land_area,by.x='Code',by.y = 'Country.Code',all.x=TRUE))
ptcl_df = ptcl_df[c('Country','Code','Year','Temp_Change','PC_Emissions','Population','X2018')]
colnames(ptcl_df)[length(ptcl_df)] = 'Land_Area'

# Create country density feature (in people per square kilometer)
ptcl_df['Country_Density'] = ptcl_df$Population / ptcl_df$Land_Area

# Join map data with population/temperature/emissions/land area dataset with mapping information dataset
# to produce final dataset
final_df = merge(ptcl_df,map_data,by.x = 'Code',by.y = 'ISO3.C.3')
colnames(final_df)[1] = 'ISO3.C.3'
final_df = final_df[order(final_df$order_col),]
final_df = final_df[,-c(2,19)]

# Write to processed dataset to csv
write.csv(final_df,'processed_map_data.csv',row.names=FALSE)

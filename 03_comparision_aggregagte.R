## Meta-Information ######------######------######------
## Author: Hardika Dayalani (hardika.dayalani@gmail.com)
## Creation: Namibia Small Area Poverty Estimates 
## Description: Aggregate Harmonized Data

## Environment Setup ######------######------######------
rm(list = ls())
## Set Working Directory
getwd()
setwd("C:/Users/dayalani/Documents/02_Work/WB/Namibia")

## Load Libraries 
library(data.table)

## Census Data ######------######------######------

## Load Harmonized Census Data
cen_df = fread("namibia_census_2011_harmonized.csv")

## Population  
agg_df = cen_df[, .(pop = sum(wta_hh*hhsize)), by = region_name]

## Population by urbanicity
temp_df = cen_df[rururb == "rural", .(rural_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

temp_df = cen_df[rururb == "urban", .(urban_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

## Population by access to toilet 
temp_df = cen_df[is.na(toiletshared), .(no_toilet_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

temp_df = cen_df[toiletshared == 0, .(shared_toilet_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

temp_df = cen_df[toiletshared == 1, .(private_toilet_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

## Population dependent on agriculture 
temp_df = cen_df[industrycat10_year == "Agriculture, Hunting, Fishing, etc.", 
                 .(agg_dep_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

## Population with access to radio
temp_df = cen_df[radio == 1, .(radio_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

## Population with access to television
temp_df = cen_df[television == 1, .(television_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

## Population with access to computer
temp_df = cen_df[computer == 1, .(computer_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

## Population with access to cellphone
temp_df = cen_df[cellphone == 1, .(cellphone_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

## Population with access to landphone
temp_df = cen_df[landphone == 1, .(landphone_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

## Population with access to internet
temp_df = cen_df[internet == 1, .(internet_pop = sum(wta_hh*hhsize)), by = region_name]
agg_df = merge(agg_df, temp_df, by = "region_name")

write.csv(agg_df, "namibia_census_aggregate.csv", row.names = FALSE)

## Survey Data ######------######------######------

## Load Harmonized Survey Data
sur_df = fread("namibia_nhies_2015_hamonized_data.csv")

## Population  
agg_df = sur_df[, .(pop = sum(wta_hh*hhsize)), by = region_prev_name]

## Population by urbanicity
temp_df = sur_df[rururb == "rural", .(rural_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

temp_df = sur_df[rururb == "urban", .(urban_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

## Population by access to toilet 
temp_df = sur_df[is.na(toiletshared), .(no_toilet_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

temp_df = sur_df[toiletshared == 0, .(shared_toilet_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

temp_df = sur_df[toiletshared == 1, .(private_toilet_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

## Population dependent on agriculture 
temp_df = sur_df[industrycat10_year == "Agriculture, Hunting, Fishing, etc.", 
                 .(agg_dep_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

## Population with access to radio
temp_df = sur_df[radio == 1, .(radio_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

## Population with access to television
temp_df = sur_df[television == 1, .(television_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

## Population with access to computer
temp_df = sur_df[computer == 1, .(computer_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

## Population with access to cellphone
temp_df = sur_df[cellphone == 1, .(cellphone_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

## Population with access to landphone
temp_df = sur_df[landphone == 1, .(landphone_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

## Population with access to internet
temp_df = sur_df[internet == 1, .(internet_pop = sum(wta_hh*hhsize)), by = region_prev_name]
agg_df = merge(agg_df, temp_df, by = "region_prev_name")

write.csv(agg_df, "namibia_nhies_aggregate.csv", row.names = FALSE)

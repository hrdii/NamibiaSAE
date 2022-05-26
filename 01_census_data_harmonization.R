## Meta-Information ######------######------######------
## Author: Hardika Dayalani (hardika.dayalani@gmail.com)
## Creation: Namibia Small Area Poverty Estimates 
## Description: Clean Census 2011 data for harmonization

## Environment Setup ######------######------######------

rm(list = ls())

## Set Working Directory
getwd()
setwd("C:/Users/dayalani/Documents/02_Work/WB/Namibia")

## Load Libraries 
library(haven)
library(data.table)

## Geographic Data ######------######------######------

## Import Constituency Data
const_code_df = read.csv("Namibia_Constituency_Code.txt", header=FALSE)

## Clean Constituency Data
names(const_code_df) = c("const_code", "region", "constituency")
const_code_df$const_code = sprintf("%04.f", const_code_df$const_code)
const_code_df$region_code = substr(const_code_df$const_code, 1, 2)

## Region Code 
region_code_df = const_code_df[, c("region", "region_code")]
region_code_df = unique(region_code_df)

## Housing Data ######------######------######------

## Import Data
house_df = read_sav("NAM_census_2011/HOUSING RECORDS.sav")
names(house_df) = tolower(names(house_df))

## Subset relevant variables
temp = c("region", "constituency", "urban_rural", "hh_type", 
         "household_serial_number", "h1", "h4", "h5", "h6", "h7", "h8a", 
         "h8b", "h8c", "h9", "h10", "h14", "hwgt")

house_df = house_df[, temp] 
## Dropped 11 variables from 28 to 17

## Subset to conventional households
house_df = house_df[house_df$hh_type == 100, ]
## Dropped 1127 observations from 94774 to 93647

## Keeping weight = 1, don't want to throw out households  
# The 20% sampled households have a weight of 5 for each record type, 
# and the large households with 50 or more people and
# the households in areas with less than 250 households all have a weight of 1.

## region names
house_df$region_code = sprintf("%02.f", house_df$region)
temp = match(house_df$region_code, region_code_df$region_code)
house_df$region_name = region_code_df$region[temp]

## constituency
house_df$const_code = paste0(house_df$region_code, sprintf("%02.f", house_df$constituency))
temp = match(house_df$const_code, const_code_df$const_code)
house_df$constituency_name = const_code_df$constituency[temp]

## Unique Household ID
house_df$hid = paste0(house_df$const_code, 
                      sprintf("%04.f", house_df$household_serial_number))

## urban_rural
house_df$rururb = NA
house_df$rururb[house_df$urban_rural == 1] = "1"
house_df$rururb[house_df$urban_rural == 2] = "0"
house_df$rururb = factor(house_df$rururb, labels = c("0" = "rural",
                                                     "1" = "urban"))
## Household Weight
house_df$wta_hh = house_df$hwgt

## dweltyp (Type of Dwelling)
## H1. Housing Type
house_df$dweltyp = NA
house_df$dweltyp[house_df$h1 == 1] = "1"
house_df$dweltyp[house_df$h1 == 2] = "6"
house_df$dweltyp[house_df$h1 %in% c(3, 4)] = "3"
house_df$dweltyp[house_df$h1 == 5] = "7"
house_df$dweltyp[house_df$h1 %in% c(6, 7, 10)] = "9"
house_df$dweltyp[house_df$h1 %in% c(8, 9)] = "8"

house_df$dweltyp = factor(house_df$dweltyp, 
                          levels = 1:9, 
                          labels = c("1" = "Detached house", 
                                     "2" = "Multi-family house",
                                     "3" = "Separate apartment",
                                     "4" = "Communal apartment", 
                                     "5" = "Room in a larger dwelling",
                                     "6" = "Several buildings connected",
                                     "7" = "Several separate buildings",
                                     "8" = "Improvised housing unit",
                                     "9" = "Other"))

## rooms (Number of habitable rooms)
## H4. Sleeping Rooms
house_df$rooms = house_df$h4

## roof (Main material used for roof)
## H6. Roof
house_df$roof = NA
house_df$roof[house_df$h6 == 1] = "12"
house_df$roof[house_df$h6 == 2] = "9"
house_df$roof[house_df$h6 == 3] = "7"
house_df$roof[house_df$h6 == 4] = "11"
house_df$roof[house_df$h6 == 5] = "1"
house_df$roof[house_df$h6 %in% c(6, 7, 8, 10)] = "15"
house_df$roof[house_df$h6 == 8] = "3"

house_df$roof = factor(house_df$roof, 
                       levels = 1:15, 
                       labels = c("1" = "Natural - Thatch/palm leaf", 
                                  "2" = "Natural - Sod",
                                  "3" = "Natural - Other", 
                                  "4" = "Rudimentary - Rustic mat",
                                  "5" = "Rudimentary - Palm/bamboo",
                                  "6" = "Rudimentary - Wood planks",
                                  "7" = "Rudimentary - Other", 
                                  "8" = "Finished - Roofing",
                                  "9" = "Finished - Asbestos", 
                                  "10" = "Finished - Tile",
                                  "11" = "Finished - Concrete", 
                                  "12" = "Finished - Metal tile", 
                                  "13" = "Finished - Roofing shingles",
                                  "14" = "Finished - Other",
                                  "15" = "Other - Specific")) 

## wall (Main material used for external walls)
## H5. Outer Walls
house_df$wall = NA
house_df$wall[house_df$h5 == 1] = "13"
house_df$wall[house_df$h5 %in% c(2, 8, 9)] = "19"
house_df$wall[house_df$h5 == 3] = "3"
house_df$wall[house_df$h5 == 4] = "18"
house_df$wall[house_df$h5 == 5] = "17"
house_df$wall[house_df$h5 %in% c(6, 7)] = "10"

house_df$wall = factor(house_df$wall, 
                       levels = 1:19,
                       labels = c("1" = "Natural - Cane/palm/trunks",
                                  "2" = "Natural - Dirt",
                                  "3" = "Natural - Other", 
                                  "4" = "Rudimentary - Bamboo with mud",
                                  "5" = "Rudimentary - Stone with mud",
                                  "6" = "Rudimentary - Uncovered adobe",
                                  "7" = "Rudimentary - Plywood",
                                  "8" = "Rudimentary - Cardboard",
                                  "9" = "Rudimentary - Reused wood",
                                  "10" = "Rudimentary - Other", 
                                  "11" = "Finished - Woven Bamboo", 
                                  "12" = "Finished - Stone with lime/cement",
                                  "13" = "Finished - Cement blocks", 
                                  "14" = "Finished - Covered adobe",
                                  "15" = "Finished - Wood planks/shingles",
                                  "16" = "Finished - Plaster wire",
                                  "17" = "Finished - GRC/Gypsum/Asbestos", 
                                  "18" = "Finished - Other", 
                                  "19" = "Other")) 

## floor (Main material used for floor)
## H7. Floor
house_df$floor = NA
house_df$floor[house_df$h7 == 1] = "1"
house_df$floor[house_df$h7 %in% c(2, 5)] = "11"
house_df$floor[house_df$h7 == 3] = "3"
house_df$floor[house_df$h7 == 4] = "4"
house_df$floor[house_df$h7 == 6] = "10"
house_df$floor[house_df$h7 == 7] = "14"

house_df$floor = factor(house_df$floor,
                        levels = 1:14,
                        labels = c("1" = "Natural - Earth/sand", 
                                   "2" = "Natural - Dung",
                                   "3" = "Natural - Other", 
                                   "4" = "Rudimentary - Wood planks", 
                                   "5" = "Rudimentary - Palm/bamboo",
                                   "6" = "Rudimentary - Other",
                                   "7" = "Finished - Parquet or polished wood",
                                   "8" = "Finished - Vinyl or asphalt strips",
                                   "9" = "Finished - Ceramic/marble/granite",
                                   "10" = "Finished - Floor tiles/teraso", 
                                   "11" = "Finished - Cement/red bricks", 
                                   "12" = "Finished - Carpet",
                                   "13" = "Finished - Other",
                                   "14" = "Other - Specific")) 

## water14 (Sources of drinking water (14 categories))
## H9. Drinking Water Source
house_df$water14 = NA
house_df$water14[house_df$h9 == 1] = "1"
house_df$water14[house_df$h9 == 2] = "2"
house_df$water14[house_df$h9 == 3] = "3"
house_df$water14[house_df$h9 %in% c(4, 5)] = "4"
house_df$water14[house_df$h9 %in% c(6, 7)] = "13"
house_df$water14[house_df$h9 == 8] = "5"
house_df$water14[house_df$h9 == 9] = "10"
house_df$water14[house_df$h9 == 10] = "14"

house_df$water14 = factor(house_df$water14,
                          levels = 1:14, 
                          labels = c("1" = "Piped water into dwelling",
                                     "2" = "Piped water to yard/plot", 
                                     "3" = "Public tap or standpipe",
                                     "4" = "Tube well or borehole",
                                     "5" = "Protected dug well",
                                     "6" = "Protected spring",
                                     "7" = "Bottled water",
                                     "8" = "Rainwater",
                                     "9" = "Unprotected spring",
                                     "10" = "Unprotected dug well",
                                     "11" = "Cart with small tank/drum",
                                     "12" = "Tanker-truck",
                                     "13" = "Surface water",
                                     "14" = "Other"))

## toilet14 (Main sanitation facility (14 categories))
## H10. Toilet Facility
house_df$toilet14 = NA
house_df$toilet14[house_df$h10 %in% c(1, 2)] = "2"
house_df$toilet14[house_df$h10 %in% c(3, 4)] = "3"
house_df$toilet14[house_df$h10 == 5] = "5"
house_df$toilet14[house_df$h10 == 6] = "6"
house_df$toilet14[house_df$h10 == 7] = "10"
house_df$toilet14[house_df$h10 == 8] = "11"
house_df$toilet14[house_df$h10 == 9] = "13"
house_df$toilet14[house_df$h10 == 10] = "14"

house_df$toilet14 = factor(house_df$toilet14, 
                           levels = 1:14,
                           labels = c("1" = "A flush toilet",
                                      "2" = "A piped sewer system",
                                      "3" = "A septic tank",
                                      "4" = "Pit latrine",
                                      "5" = "Ventilated improved pit latrine (VIP)",
                                      "6" = "Pit latrine with slab",
                                      "7" = "Composting toilet",
                                      "8" = "Special case",
                                      "9" = "A flush/pour flush to elsewhere",
                                      "10" = "A pit latrine without slab",
                                      "11" = "Bucket",
                                      "12" = "Hanging toilet or hanging latrine",
                                      "13" = "No facilities or bush or field",
                                      "14" = "Other"))

## toiletshared (Is toilet facility shared with other households?)
house_df$toiletshared = NA
house_df$toiletshared[house_df$h10 %in% c(2, 4)] = 1
house_df$toiletshared[house_df$h10 %in% c(1, 3, 5:8)] = 0

## fuelcook (Main cooking fuel)
## H8a: Cooking
house_df$fuelcook = NA
house_df$fuelcook[house_df$h8a %in% c(1, 2)] = "4"
house_df$fuelcook[house_df$h8a == 3] = "5"
house_df$fuelcook[house_df$h8a == 4] = "2"
house_df$fuelcook[house_df$h8a == 5] = "1"
house_df$fuelcook[house_df$h8a == 6] = "3"
house_df$fuelcook[house_df$h8a %in% c(8, 9, 11)] = "9"

house_df$fuelcook = factor(house_df$fuelcook, 
                           levels = c(1:5, 9, 10),
                           labels = c("1" = "Firewood",
                                      "2" = "Kerosene",
                                      "3" = "Charcoal",
                                      "4" = "Electricity",
                                      "5" = "Gas",
                                      "9" = "Other",
                                      "10" = "None"))

## fuelligh (Main source of lighting) 
## H8b: Lighting
house_df$fuelligh = NA
house_df$fuelligh[house_df$h8b %in% c(1, 2)] = "1"
house_df$fuelligh[house_df$h8b == 3] = "4"
house_df$fuelligh[house_df$h8b == 4] = "2"
house_df$fuelligh[house_df$h8b %in% c(5, 6, 8, 9, 11)] = "9"
house_df$fuelligh[house_df$h8b == 7] = "3"
house_df$fuelligh[house_df$h8b == 11] = "10"

house_df$fuelligh = factor(house_df$fuelligh, 
                           levels = c(1:4, 9, 10),
                           labels = c("1" = "Electricity",
                                      "2" = "Kerosene",
                                      "3" = "Candles",
                                      "4" = "Gas",
                                      "9" = "Other",
                                      "10" = "None"))

## heatsource (Main source of heating)
## H8c: Heating
house_df$heatsource = NA
house_df$heatsource[house_df$h8c %in% c(1, 2)] = "4"
house_df$heatsource[house_df$h8c == 3] = "5"
house_df$heatsource[house_df$h8c == 5] = "1"
house_df$heatsource[house_df$h8c == 6] = "3" 
house_df$heatsource[house_df$h8c %in% c(8, 9, 11)] = "9"
house_df$heatsource[house_df$h8c == 10] = "10" 

house_df$heatsource = factor(house_df$heatsource, 
                             levels = c(1:6, 9, 10),
                             labels = c("1" = "Firewood",
                                        "2" = "Kerosene",
                                        "3" = "Charcoal",
                                        "4" = "Electricity",
                                        "5" = "Gas",
                                        "6" = "Central", 
                                        "9" = "Other",
                                        "10" = "No heating"))

## Subset to Harmonized Variables 
temp = c("region_code", "region_name", "const_code", "constituency_name", "hid",
         "wta_hh", "rururb", "dweltyp", "rooms", "roof", "wall", "floor", "water14", 
         "toilet14", "toiletshared", "fuelcook", "fuelligh", "heatsource")

house_df = house_df[, temp] 

## Person Data ######------######------######------

## Import Data
per_df = read_sav("NAM_census_2011/PERSONS RECORDS.sav")
names(per_df) = tolower(names(per_df))
per_df = setDT(per_df)

## Subset to conventional households
per_df = per_df[per_df$hh_type == 100, ]
## Dropped 25992 observations 

## Subset to relevant variables 
temp = c("region", "constituency", "household_serial_number", "b3", "b4", "b5",
         "b8", "b19b", "b19c", "b19d", "b19e", "b19f", "b19i", "d1", "d2", "d3",
         "e1", "e2", "e3", "e4", "pwgt")

per_df = per_df[, .SD, .SDcols = temp]
## Dropped 30 variables 

## Unique Household ID
per_df$hid = paste0(sprintf("%02.f", per_df$region),
                    sprintf("%02.f", per_df$constituency),
                    sprintf("%04.f", per_df$household_serial_number))

## Weights
per_df$wta_pop = per_df$pwgt

## agecat (Age Group)
## B5: Age
temp_df = data.frame(code = as.character(c(0:15)), 
                     label = c("0",
                               "01-04",
                               "05-09",
                               "10-14",
                               "15-19",
                               "20-24",
                               "25-29",
                               "30-34",
                               "35-39",
                               "40-44",
                               "45-49",
                               "50-54",
                               "55-59",
                               "60-64",
                               "65-74",
                               "75+"))
per_df$b5 = as.character(per_df$b5)
temp = match(per_df$b5, temp_df$code)
per_df$agecat = temp_df$label[temp]

## B4: Sex ## c(`0.Female` = 0, `1. Male` = 1)
per_df$sex = per_df$b4 - 1
per_df$sex = factor(per_df$sex, labels = c("0" = "Female",
                                           "1" = "Male"))

## relathh9 (Relationship to head of household harmonized across all regions)
## B3. Relationship
per_df$relathh9 = NA
per_df$relathh9[per_df$b3 == 1] = "1"
per_df$relathh9[per_df$b3 == 2] = "2"
per_df$relathh9[per_df$b3 == 3] = "3"
per_df$relathh9[per_df$b3 == 4] = "6"
per_df$relathh9[per_df$b3 == 5] = "5"
per_df$relathh9[per_df$b3 == 6] = "4"
per_df$relathh9[per_df$b3 == 7] = "7"
per_df$relathh9[per_df$b3 == 8] = "8"
per_df$relathh9[per_df$b3 == 9] = "9"

per_df$relathh9 = factor(per_df$relathh9, 
                         levels = 1:9,
                         labels = c("1" = "Head",
                                    "2" = "Spouse",
                                    "3" = "Child",
                                    "4" = "Parents/parents-in-law",
                                    "5" = "Grandchild",
                                    "6" = "Son-in-law/daughter-in-law",
                                    "7" = "Other relative",
                                    "8" = "Domestic help/boarder",
                                    "9" = "Non-relative"))

## marital5 (Marital status)
## B8. Marital Status
per_df$marital5 = NA
per_df$marital5[per_df$b8 == 1] = "2"
per_df$marital5[per_df$b8 %in% c(2, 3)] = "1"
per_df$marital5[per_df$b8 == 4] = "3"
per_df$marital5[per_df$b8 %in% c(5, 7)] = "4"
per_df$marital5[per_df$b8 == 6] = "5"

per_df$marital5 = factor(per_df$marital5, 
                         levels = 1:5,
                         labels = c("1" = "Married",
                                    "2" = "Never married", 
                                    "3" = "Living together",
                                    "4" = "Divorced/Separated",
                                    "5" = "Widowed"))

## literacy (Individual can read and write)
## D1. Literacy
per_df$literacy = NA
per_df$literacy[per_df$d1 == 1] = "1"
per_df$literacy[per_df$d1 == 2] = "0"

per_df$literacy = factor(per_df$literacy, labels = c("1" = "yes",
                                                     "0" = "no"))

## everattd (Ever attended school)
## D2. Ever Attended School
per_df$everattd = NA
per_df$everattd[per_df$d2 == 1] = "0"
per_df$everattd[per_df$d2 %in% 2:5] = "1"

per_df$everattd = factor(per_df$everattd, labels = c("1" = "yes",
                                                     "0" = "no"))

## educat5 (Highest level of education completed (5 categories))
## D3. Highest Grade Completed
per_df$educat5 = per_df$d3 + 1
per_df$educat5[per_df$educat5 > 5] = NA

per_df$educat5 = factor(per_df$educat5, labels = c("1" = "No education",
                                                   "2" = "Primary incomplete",
                                                   "3" = "Primary complete but Secondary incomplete",
                                                   "4" = "Secondary complete",
                                                   "5" = "Tertiary (completed or incomplete)"))

## lstatus (Labor status (7-day ref period))
## E1. Work
per_df$lstatus = factor(per_df$e1, labels = c("1" = "Employed",
                                              "2" = "Unemployed",
                                              "3" = "Not in labor force"))

## industrycat10_year (1 digit industry classification, primary job (12-mon ref period))
## E3. Industry
per_df$industrycat10_year = NA
per_df$industrycat10_year[per_df$e3 == 1] = "1"
per_df$industrycat10_year[per_df$e3 == 2] = "2"
per_df$industrycat10_year[per_df$e3 == 3] = "3"
per_df$industrycat10_year[per_df$e3 %in% c(4, 5)] = "4"
per_df$industrycat10_year[per_df$e3 == 6] = "5"
per_df$industrycat10_year[per_df$e3 == 7] = "6"
per_df$industrycat10_year[per_df$e3 %in% c(8, 10)] = "7"
per_df$industrycat10_year[per_df$e3 %in% c(9, 16:21)] = "10"
per_df$industrycat10_year[per_df$e3 %in% c(11:14)] = "8"
per_df$industrycat10_year[per_df$e3 == 15] = "9"

per_df$industrycat10_year = factor(per_df$industrycat10_year,
                                   levels = 1:10,
                                   labels = c("1" = "Agriculture, Hunting, Fishing, etc.",
                                              "2" = "Mining",
                                              "3" = "Manufacturing",
                                              "4" = "Public Utility Services",
                                              "5" = "Construction",
                                              "6" = "Commerce",
                                              "7" = "Transport and Communications",
                                              "8" = "Financial and Business Services",
                                              "9" = "Public Administration",
                                              "10" = "Others Services, Unspecified"))

## occup_year (1 digit occupational classification, primary job)
## E2. Occupation 
per_df$occup_year = per_df$e2 - 1
per_df$occup_year[per_df$occup_year == 0] = 10
per_df$occup_year[per_df$occup_year == 98] = 99

per_df$occup_year = factor(per_df$occup_year, 
                           levels = c(1:10, 99),
                           labels = c("1" = "Managers",
                                      "2" = "Professionals",
                                      "3" = "Technicians and associate professionals",
                                      "4" = "Clerical support workers",
                                      "5" = "Service and sales workers",
                                      "6" = "Skilled agricultural, forestry and fishery workers",
                                      "7" = "Craft and related trades workers",
                                      "8" = "Plant and machine operators, and assemblers",
                                      "9" = "Elementary occupations",
                                      "10" = "Armed forces occupations",
                                      "99" = "Other/unspecified")) 

## B19B. ICT: Radio
per_df$radio = per_df$b19b
per_df$radio[per_df$radio == 2] = 0

temp_df = per_df[, sum(radio), by = hid]
temp_df$V1[temp_df$V1 > 0] = 1
temp = match(per_df$hid, temp_df$hid)
per_df$radio = temp_df$V1[temp]

## B19C. ICT: TV
per_df$television = per_df$b19c
per_df$television[per_df$television == 2] = 0

temp_df = per_df[, sum(television), by = hid]
temp_df$V1[temp_df$V1 > 0] = 1
temp = match(per_df$hid, temp_df$hid)
per_df$television = temp_df$V1[temp]

## B19D. ICT: Computer
per_df$computer = per_df$b19d
per_df$computer[per_df$computer == 2] = 0

temp_df = per_df[, sum(computer), by = hid]
temp_df$V1[temp_df$V1 > 0] = 1
temp = match(per_df$hid, temp_df$hid)
per_df$computer = temp_df$V1[temp]

## B19E. ICT: Cell Phone
per_df$cellphone = per_df$b19e
per_df$cellphone[per_df$cellphone == 2] = 0

temp_df = per_df[, sum(cellphone), by = hid]
temp_df$V1[temp_df$V1 > 0] = 1
temp = match(per_df$hid, temp_df$hid)
per_df$cellphone = temp_df$V1[temp]

## B19F. ICT: Telephone (Fixed)
per_df$landphone = per_df$b19f
per_df$landphone[per_df$landphone == 2] = 0

temp_df = per_df[, sum(landphone), by = hid]
temp_df$V1[temp_df$V1 > 0] = 1
temp = match(per_df$hid, temp_df$hid)
per_df$landphone = temp_df$V1[temp]

## B19I. ICT: Internet
per_df$internet = per_df$b19i
per_df$internet[per_df$internet == 2] = 0

temp_df = per_df[, sum(internet), by = hid]
temp_df$V1[temp_df$V1 > 0] = 1
temp = match(per_df$hid, temp_df$hid)
per_df$internet = temp_df$V1[temp]

## Subset to Harmonized Variables 
temp = c("hid", "wta_pop", "agecat", "sex", "relathh9", "marital5", "literacy", 
         "everattd", "educat5", "lstatus", "industrycat10_year", "occup_year",
         "radio", "television", "computer", "cellphone", "landphone", "internet")

per_df = per_df[, .SD, .SDcols = temp] 

## Merge Housing & Person Data ######------######------######------

## Calculate Household Size
temp_df = per_df[, .N, by = hid]

## Add Household Size
temp = match(house_df$hid, temp_df$hid)
house_df$hhsize = temp_df$N[temp]

## Subset to Head of household
per_df = per_df[per_df$relathh9 == "Head", ]

## Merge data
house_df = merge(house_df, per_df, by = "hid", all = TRUE)

## Save Data
write.csv(house_df, "namibia_census_2011_harmonized.csv", row.names = FALSE)

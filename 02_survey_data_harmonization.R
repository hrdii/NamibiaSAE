## Meta-Information ######------######------######------
## Author: Hardika Dayalani (hardika.dayalani@gmail.com)
## Creation: Namibia Small Area Poverty Estimates 
## Description: Clean NHIES 2015 data for harmonization

## Environment Setup ######------######------######------
rm(list = ls())

## Set Working Directory
getwd()
setwd("C:/Users/dayalani/Documents/02_Work/WB/Namibia")

## Load Libraries 
library(haven)
library(labelled)

# df <- read_dta("2015 NHIES/NHIES_2015_16_household_level.dta")

## Household Level Data ######------######------######------

## Household File
survey_hhdf <- read_dta("2015 NHIES/NAM_2015_NHIES_v01_M_v06_A_SSAPOV_H.dta")

## Subset to nonempty columns
temp = sapply(survey_hhdf, FUN = function(x) sum(is.na(x)) == 10090)
sum(temp) ## 85 of 169 columns are empty
survey_hhdf = survey_hhdf[, !temp]

## Generate Data dictionary
survey_hhdict = look_for(survey_hhdf)

## Subset to Relevant Variables
temp = c("hid", "wta_hh", "dweltyp", "rooms", "roof", "wall", "floor", "water14",
         "toilet14", "toiletshared", "fuelcook", "fuelligh", "heatsource", 
         "radio", "television", "computer", "cellphone", "landphone",  "internet")

survey_hhdf = survey_hhdf[, temp]

survey_hhdf$dweltyp = factor(survey_hhdf$dweltyp, 
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

survey_hhdf$roof = factor(survey_hhdf$roof, 
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

survey_hhdf$wall = factor(survey_hhdf$wall, 
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

survey_hhdf$floor = factor(survey_hhdf$floor,
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

survey_hhdf$water14 = factor(survey_hhdf$water14,
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

survey_hhdf$toilet14 = factor(survey_hhdf$toilet14, 
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

survey_hhdf$fuelcook = factor(survey_hhdf$fuelcook, 
                              levels = c(1:5, 9, 10),
                              labels = c("1" = "Firewood",
                                         "2" = "Kerosene",
                                         "3" = "Charcoal",
                                         "4" = "Electricity",
                                         "5" = "Gas",
                                         "9" = "Other",
                                         "10" = "None"))

survey_hhdf$fuelligh = factor(survey_hhdf$fuelligh, 
                              levels = c(1:4, 9, 10),
                              labels = c("1" = "Electricity",
                                         "2" = "Kerosene",
                                         "3" = "Candles",
                                         "4" = "Gas",
                                         "9" = "Other",
                                         "10" = "None"))

survey_hhdf$heatsource = factor(survey_hhdf$heatsource, 
                                levels = c(1:6, 9, 10),
                                labels = c("1" = "Firewood",
                                           "2" = "Kerosene",
                                           "3" = "Charcoal",
                                           "4" = "Electricity",
                                           "5" = "Gas",
                                           "6" = "Central", 
                                           "9" = "Other",
                                           "10" = "No heating"))

## Household-level Poverty / Expenditure File ######------######------

## Constituency 2014
const_code_df = read.csv("Namibia_Constituency_Code_post_2013.txt", header=FALSE)
names(const_code_df) = c("const_code", "region", "constituency", "constituency_prev")
const_code_df$const_code = sprintf("%04.f", const_code_df$const_code)

## Poverty / Expenditure File
survey_pdf <- read_dta("2015 NHIES/NAM_2015_NHIES_v01_M_v06_A_SSAPOV_P.dta")

## Subset to nonempty columns
temp = sapply(survey_pdf, FUN = function(x) sum(is.na(x)) == 10090)
sum(temp) ## 25 of 49 columns are empty
survey_pdf = survey_pdf[, !temp]

## Generate Data dictionary
survey_pdict = look_for(survey_pdf)

## 2014 Region Code
survey_pdf$region_code = sapply(survey_pdf$region1, FUN = function(x) strsplit(x, "-")[[1]][1])
survey_pdf$region_code = sprintf("%02.f", as.numeric(survey_pdf$region_code))

## 2014 Region Name
survey_pdf$region_name = sapply(survey_pdf$region1, FUN = function(x) strsplit(x, "-")[[1]][2])

## 2011 Region Code
survey_pdf$region1_prev = gsub(" ", "", survey_pdf$region1_prev)
survey_pdf$region1_prev = tolower(survey_pdf$region1_prev)
survey_pdf$region_prev_code = sapply(survey_pdf$region1_prev, FUN = function(x) strsplit(x, "-")[[1]][1])
survey_pdf$region_prev_code = sprintf("%02.f", as.numeric(survey_pdf$region_prev_code))

## 2011 Region Name
survey_pdf$region_prev_name = sapply(survey_pdf$region1_prev, FUN = function(x) strsplit(x, "-")[[1]][2])

## 2014 Constituency Code
survey_pdf$const_code = substr(survey_pdf$hid, 1, 4)

## 2014 Constituency Name
temp = match(survey_pdf$const_code, const_code_df$const_code)
survey_pdf$contituency_name = const_code_df$constituency[temp]

## 2011 Constituency Name
survey_pdf$contituency_prev_name = const_code_df$constituency_prev[temp]


## Subset to Relevant Variables
temp = c("region_code", "region_name", "region_prev_code", "region_prev_name", 
         "const_code", "contituency_name", "contituency_prev_name", "strata", 
         "rururb", "hid", "hhsize", "wta_pop", "wta_cadq", "welfaretype", 
         "wel_abs", "pl_abs", "pl_fd", "pl_ext", "poor_abs", "poor_fd", 
         "poor_ext", "wel_PPPnom", "wel_shpr", "wel_shprtype", "wel_othtype", 
         "wel_PPP")

survey_pdf = survey_pdf[, temp]

survey_pdf$rururb = factor(survey_pdf$rururb, 
                           levels = 0:1, 
                           labels = c("0" = "rural",
                                      "1" = "urban"))

survey_pdf$poor_abs = factor(survey_pdf$poor_abs, 
                             levels = 0:1, 
                             labels = c("0" = "Non-poor",
                                        "1" = "Poor"))

survey_pdf$poor_fd = factor(survey_pdf$poor_fd, 
                            levels = 0:1, 
                            labels = c("0" = "Non-poor",
                                       "1" = "Poor"))

survey_pdf$poor_ext = factor(survey_pdf$poor_ext, 
                             levels = 0:1, 
                             labels = c("0" = "Non-poor",
                                        "1" = "Poor"))

## Individual Level Data ######------######------######------

## Individual File
survey_indf <- read_dta("2015 NHIES/NAM_2015_NHIES_v01_M_v06_A_SSAPOV_I.dta")

## Subset to nonempty columns
temp = sapply(survey_indf, FUN = function(x) sum(is.na(x)) == 41581)
sum(temp) ## 4 of 45 columns are empty
survey_indf = survey_indf[, !temp]

## Generate Data dictionary
survey_indict = look_for(survey_indf)

## Age Group
survey_indf$agecat = cut(survey_indf$ageyrs, include.lowest = TRUE, right = FALSE,
                         breaks = c(0, 1, seq(from = 5, to = 65, by = 5), 75, Inf),
                         labels = c("0", "01-04", "05-09", "10-14", "15-19", 
                                    "20-24", "25-29", "30-34", "35-39", "40-44",
                                    "45-49", "50-54", "55-59", "60-64", "65-74",
                                    "75+"))

## Subset to Relevant Variables
temp = c("hid", "pid", "agecat", "sex", "relathh9", "marital5", "literacy", 
         "everattd", "educat5")
survey_indf = survey_indf[ ,temp]

survey_indf$sex = factor(survey_indf$sex, labels = c("0" = "Female",
                                                     "1" = "Male"))

survey_indf$relathh9 = factor(survey_indf$relathh9, 
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

survey_indf$marital5 = factor(survey_indf$marital5, 
                              levels = 1:5,
                              labels = c("1" = "Married",
                                         "2" = "Never married", 
                                         "3" = "Living together",
                                         "4" = "Divorced/Separated",
                                         "5" = "Widowed"))

survey_indf$literacy = factor(survey_indf$literacy, labels = c("1" = "yes",
                                                               "0" = "no"))

survey_indf$everattd = factor(survey_indf$everattd, labels = c("1" = "yes",
                                                               "0" = "no"))

survey_indf$educat5 = factor(survey_indf$educat5, labels = c("1" = "No education",
                                                             "2" = "Primary incomplete",
                                                             "3" = "Primary complete but Secondary incomplete",
                                                             "4" = "Secondary complete",
                                                             "5" = "Tertiary (completed or incomplete)"))

## Individual Level Data ######------######------######------

## Individual-level Labor File
survey_ldf <- read_dta("2015 NHIES/NAM_2015_NHIES_v01_M_v06_A_SSAPOV_L.dta")

## Subset to nonempty columns
temp = sapply(survey_ldf, FUN = function(x) sum(is.na(x)) == 41581)
sum(temp) ## 49 of 97 columns are empty
survey_ldf = survey_ldf[, !temp]

## Generate Data dictionary
survey_ldict = look_for(survey_ldf)

## Subset to Relevant Variables
temp = c("hid", "pid", "lstatus", "industrycat10_year", "occup_year")
survey_ldf = survey_ldf[, temp]

survey_ldf$lstatus = factor(survey_ldf$lstatus, labels = c("1" = "Employed",
                                              "2" = "Unemployed",
                                              "3" = "Not in labor force"))

survey_ldf$industrycat10_year = factor(survey_ldf$industrycat10_year,
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

survey_ldf$occup_year = factor(survey_ldf$occup_year, 
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

## Merge Data ######------######------######------

## Individual Level Data
survey_indf = merge(survey_indf, survey_ldf, by = c("hid", "pid"), all = TRUE)

## Subset to Head of household
survey_indf = survey_indf[survey_indf$relathh9 == "Head", ]

## Household Data
survey_hhdf = merge(survey_hhdf, survey_pdf, by = "hid", all = TRUE)

## Household + Individual Data
survey_hhdf = merge(survey_hhdf, survey_indf, by = "hid", all = TRUE)

temp = c(
  
  ## Identifiers
  
  "hid", ## Household ID
  "pid", ## Person ID
  "region_code", ## 2014 Region Code
  "region_name", ## 2014 Region Name
  "region_prev_code", ## 2011 Region Code
  "region_prev_name", ## 2011 Region Name
  "const_code", ## 2014 Constituency Code
  "contituency_name", ## 2014 Constituency Name
  "contituency_prev_name", ## 2011 Constituency Name  
  "rururb", ## Rural / Urban
  "strata", ## Strata
  
  ## Weights
  
  "wta_hh", ## Household Weight
  "wta_pop", ## Population Weight
  "wta_cadq", ## Adult equivalent Weights
  
  ## Household Characteristics 
  
  "hhsize", ## Household Size
  
  ## House Characteristics
  
  "dweltyp", ## Type of Dwelling
  "rooms", ## Number of Habitable Rooms
  "roof", ## Roof Material
  "wall", ## External Wall Material
  "floor", ## Floor Material
  
  ## Sanitation
  
  "water14", ## Main drinking water source
  "toilet14", ## Toilet Facility
  "toiletshared", ## Is the toilet shared
  
  ## Energy
  
  "fuelcook", ## Cooking Fuel
  "fuelligh", ## Lighting Fuel
  "heatsource", ## Heat Source

  ## Access to Information
  
  "radio", "television", "computer", "cellphone", "landphone", "internet",
  
  ## Welfare / Expenditure
  
  "welfaretype", "wel_abs", "pl_abs", "pl_fd", "pl_ext", "poor_abs",
  "poor_fd", "poor_ext", "wel_PPPnom", "wel_shpr", "wel_shprtype", 
  "wel_othtype", "wel_PPP",
  
  ## Head of Household Demographic Characteristics 
  
  "agecat", ## Age group
  "sex", ## Sex
  "relathh9", ## Relationship
  "marital5", ## Marital Statue
  "literacy", ## Can read and write
  "everattd", ## Ever Attended School
  "educat5", ## Highest Grade Completed
  
  ## Head of Household Labor Characteristics 
  "lstatus", ## Employment Status
  "industrycat10_year", ## Industry
  "occup_year" ## Occupation 
)

survey_hhdf = survey_hhdf[, temp]

write.csv(survey_hhdf, "namibia_nhies_2015_hamonized_data.csv", row.names = FALSE)

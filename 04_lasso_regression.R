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
library(data.table)
library(glmnet)
library(corrplot)

## Fit LASSO Regression #####------#####------#####------

## Load Data
df = fread("namibia_nhies_2015_hamonized_data.csv")

## Reposnse Variable 
y = df$wel_abs

## Weights
wt = df$wta_hh

## Predictor Matrix
temp = c(
  
  "region_name",
  
  ## Household Characteristics 
  
  "hhsize", ## Household Size
  "rururb", ## Rural / Urban
  
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
  
  ## Head of Household Demographic Characteristics 
  
  "agecat", ## Age group
  "sex", ## Sex
  "marital5", ## Marital Statue
  "literacy", ## Can read and write
  "everattd", ## Ever Attended School
  "educat5", ## Highest Grade Completed
  
  ## Head of Household Labor Characteristics 
  "lstatus", ## Employment Status
  "industrycat10_year", ## Industry
  "occup_year" ## Occupation 
)

x = data.matrix(df[, .SD, .SDcols = temp])

a = 1 ## Alpha parameter to pic between lasso, ridge and elastic net regularization path

## K-fold cross validation for optimal lambda 
cv_model = cv.glmnet(x, y, weights = wt, alpha = a, 
                     family = "multinomial", 
                     penalty.factor = c(0, rep(1, {length(temp) - 1})))

## Optimal Lambda
cv_model$lambda.min

## Best Fit Model #####------#####------#####------
bf_model = glmnet(x, y, weights = wt, alpha = a, lambda = cv_model$lambda.min,
                  family = "multinomial", 
                  penalty.factor = c(0, rep(1, {length(temp) - 1})))

## Examine Model Coefficients 
vars = as.matrix(coef(bf_model))
vars = data.frame(var_names = rownames(vars), s0 = vars)
vars = vars[vars$s0 != 0, "var_names"]

## Linear Model
f = as.formula(paste("wel_abs ~", paste(vars[2:length(vars)], collapse = "+")))
lm_model = lm(formula = f, data = df, weights = df$wta_hh)
summary(lm_model)


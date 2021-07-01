##ScotPHO - website updates
##Epilepsy incidence
##https://www.scotpho.org.uk/health-wellbeing-and-disease/epilepsy/data/secondary-care/
##Code to prepare data before running app.R

############################.
## Data preparation ----
############################.
############################.

#Packages 
library(dplyr) #data manipulation
library(readr) #read csvs
library(tidyr) #preparing data - not needed unless new data coming through

#File paths - to be updated as necessary
data_folder <- "//PHI_conf/ScotPHO/Website/Topics/"
filepath <- "Epilepsy/202103_update/"

#Read in data
epilepsy <- read_csv(paste0(data_folder, filepath, "Epilepsy_incidence_age_sex.csv")) %>%
  mutate(measure = round(measure, 1)) #round numbers (one decimal place)

#Save data to shiny_app folder
saveRDS(epilepsy, file="shiny_app/data/epilepsy_incidence.rds")

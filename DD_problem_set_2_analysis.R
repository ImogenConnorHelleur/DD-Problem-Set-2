#library in packages
library(tidyverse)
library(readxl)

#set options to avoid scientific notation
options(scipen=999)

#Data prep######################################################################
#read in data
setagaya_ward_data <- read_excel("13112_20221_20224_e.xlsx", 
                                 col_types = c("numeric", "text", "text", 
                                               "numeric", "text", "text", "text", 
                                               "text", "text", "numeric", "text", 
                                               "text", "numeric", "text", "text", 
                                               "text", "numeric", "text", "text", 
                                               "text", "text", "text", "numeric", 
                                               "text", "numeric", "numeric", "text", 
                                               "text", "text"))

shibuya_ward_data <- read_excel("13113_20221_20224_e.xlsx", 
                                col_types = c("numeric", "text", "text", 
                                              "numeric", "text", "text", "text", 
                                              "text", "text", "numeric", "text", 
                                              "text", "numeric", "text", "text", 
                                              "text", "numeric", "text", "text", 
                                              "text", "text", "text", "numeric", 
                                              "text", "numeric", "numeric", "text", 
                                              "text", "text"))

chuo_ward_data <- read_excel("13102_20221_20224_e.xlsx", 
                             col_types = c("numeric", "text", "text", 
                                           "numeric", "text", "text", "text", 
                                           "text", "text", "numeric", "text", 
                                           "text", "numeric", "text", "text", 
                                           "text", "numeric", "text", "text", 
                                           "text", "text", "text", "numeric", 
                                           "text", "numeric", "numeric", "text", 
                                           "text", "text"))

minato_ward_data <- read_excel("13103_20221_20224_e.xlsx", 
                               col_types = c("numeric", "text", "text", 
                                             "numeric", "text", "text", "text", 
                                             "text", "text", "numeric", "text", 
                                             "text", "numeric", "text", "text", 
                                             "text", "numeric", "text", "text", 
                                             "text", "text", "text", "numeric", 
                                             "text", "numeric", "numeric", "text", 
                                             "text", "text"))

#bind datasets together
data <- bind_rows(setagaya_ward_data, 
                  shibuya_ward_data, 
                  chuo_ward_data, 
                  minato_ward_data)

#clean up column names
names(data) <- make.names(names(data), unique=TRUE)
names(data) <- gsub("\\.", "_", names(data))
names(data) <- gsub("åf", "_", names(data))
names(data) <- tolower(names(data))

#fix area m^2 column to only be numeric
#if area is stated at 2000+, then work out area from other columns
data <- data %>%
  mutate(area_m_2_numeric = if_else(area_m_2_ == "2,000 m^2 or greater.", 
                                    transaction_price_total_ / transaction_price_unit_price_m_2_, 
                                    as.numeric(area_m_2_)))

#Analysis#######################################################################
#Question 1

question1_data <- data %>%
  filter(type == "Pre-owned Condominiums, etc.") %>%
  mutate(price_per_m2 = transaction_price_total_ / area_m_2_numeric) %>%
  group_by(city_town_ward_village) %>%
  summarise(mean_price_per_m2 = mean(price_per_m2))

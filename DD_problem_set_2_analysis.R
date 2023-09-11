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


#Question 2#####################################################################

question2_data <- data %>%
  group_by(city_town_ward_village) %>%
  sample_n(150)

#Question 3#####################################################################

#Get a column for price per m2
question3_data <- question2_data %>%
  mutate(price_per_m2 = transaction_price_total_ / area_m_2_numeric) 

#get summary table of sd and mean for each ward in sample populations
question3_data_summary <- question3_data %>%
  group_by(city_town_ward_village) %>%
  summarise(mean_price_per_m2 = mean(price_per_m2),
            sd_price_per_m2 = sd(price_per_m2))

#set up two sample t-tests for each pair of wards https://www.datacamp.com/tutorial/t-tests-r-tutorial
#Chuo and Minato
t_test_data_chuo_minato <- question3_data %>%
  filter(city_town_ward_village %in% c("Chuo Ward", "Minato Ward"))

t_test_result_chuo_minato <- t.test(price_per_m2 ~ city_town_ward_village,
                                    data = t_test_data_chuo_minato)

#Chuo and Setagaya
t_test_data_chuo_setagaya <- question3_data %>%
  filter(city_town_ward_village %in% c("Chuo Ward", "Setagaya Ward"))

t_test_result_chuo_setagaya <- t.test(price_per_m2 ~ city_town_ward_village,
                                      data = t_test_data_chuo_setagaya)

#Chuo and Shibuya
t_test_data_chuo_shibuya <- question3_data %>%
  filter(city_town_ward_village %in% c("Chuo Ward", "Shibuya Ward"))

t_test_result_chuo_shibuya <- t.test(price_per_m2 ~ city_town_ward_village,
                                      data = t_test_data_chuo_shibuya)

#Minato and Setagaya
t_test_data_minato_setagaya <- question3_data %>%
  filter(city_town_ward_village %in% c("Minato Ward", "Setagaya Ward"))

t_test_result_minato_setagaya <- t.test(price_per_m2 ~ city_town_ward_village,
                                        data = t_test_data_minato_setagaya)

#Minato and Shibuya
t_test_data_minato_shibuya<- question3_data %>%
  filter(city_town_ward_village %in% c("Minato Ward", "Shibuya Ward"))

t_test_result_minato_shibuya <- t.test(price_per_m2 ~ city_town_ward_village,
                                       data = t_test_data_minato_shibuya)

#Setagaya and Shibuya
t_test_data_setagaya_shibuya <- question3_data %>%
  filter(city_town_ward_village %in% c("Setagaya Ward", "Shibuya Ward"))

t_test_result_setagaya_shibuya <- t.test(price_per_m2 ~ city_town_ward_village,
                                         data = t_test_data_setagaya_shibuya)

#create boxplots of the four ward for visual comparison
ggplot(question3_data) +
  geom_boxplot(aes(x = city_town_ward_village,
                   y = price_per_m2),
               outlier.shape = NA) +
  labs(title = "Boxplot comparisions of sample distributions for each ward (n = 150)",
       x = "Ward",
       y = "Price per m^2",
       caption = "Not showing outliers.") +
  scale_y_continuous(limits = quantile(question3_data$price_per_m2, c(0.1, 0.9)))
  theme_bw()



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
            sd_price_per_m2 = sd(price_per_m2),
            n = n())

#create boxplots of the four ward for visual comparison
ggplot(question3_data) +
  geom_boxplot(aes(x = city_town_ward_village,
                   y = price_per_m2),
               outlier.shape = NA) +
  labs(title = "Boxplot comparisions of sample distributions for each ward (n = 150)",
       x = "Ward",
       y = "Price per m^2",
       caption = "Not showing outliers.") +
  scale_y_continuous(limits = quantile(question3_data$price_per_m2, c(0.1, 0.9))) +
  theme_bw() #+
  # geom_errorbar(ymin = 1270825.6, ymax = 1750965.1118313, width = 0.2)

#set up two sample t-tests for each pair of wards https://www.datacamp.com/tutorial/t-tests-r-tutorial
#Chuo and Minato
t_test_result_chuo_minato <- t.test(price_per_m2 ~ city_town_ward_village,
                                    data = question3_data,
                                    subset = question3_data$city_town_ward_village %in% c("Chuo Ward", 
                                                                                          "Minato Ward"),
                                    alternative = "two.sided",
                                    mu = 0, paired = FALSE, var.equal = TRUE,
                                    conf.level = 0.95)

#Chuo and Setagaya
t_test_result_chuo_setagaya <- t.test(price_per_m2 ~ city_town_ward_village,
                                      data = question3_data,
                                      subset = question3_data$city_town_ward_village %in% c("Chuo Ward", 
                                                                                            "Setagaya Ward"),
                                      alternative = "two.sided",
                                      mu = 0, paired = FALSE, var.equal = TRUE,
                                      conf.level = 0.95)

#Chuo and Shibuya
t_test_result_chuo_shibuya <- t.test(price_per_m2 ~ city_town_ward_village,
                                     data = question3_data,
                                     subset = question3_data$city_town_ward_village %in% c("Chuo Ward", 
                                                                                           "Shibuya Ward"),
                                     alternative = "two.sided",
                                     mu = 0, paired = FALSE, var.equal = TRUE,
                                     conf.level = 0.95)

#Minato and Setagaya
t_test_result_minato_setagaya <- t.test(price_per_m2 ~ city_town_ward_village,
                                     data = question3_data,
                                     subset = question3_data$city_town_ward_village %in% c("Minato Ward", 
                                                                                           "Setagaya Ward"),
                                     alternative = "two.sided",
                                     mu = 0, paired = FALSE, var.equal = TRUE,
                                     conf.level = 0.95)

#Minato and Shibuya
t_test_result_minato_shibuya <- t.test(price_per_m2 ~ city_town_ward_village,
                                        data = question3_data,
                                        subset = question3_data$city_town_ward_village %in% c("Minato Ward", 
                                                                                              "Shibuya Ward"),
                                       alternative = "two.sided",
                                       mu = 0, paired = FALSE, var.equal = TRUE,
                                       conf.level = 0.95)

#Setagaya and Shibuya
t_test_result_setagaya_shibuya <- t.test(price_per_m2 ~ city_town_ward_village,
                                         data = question3_data,
                                         subset = question3_data$city_town_ward_village %in% c("Setagaya Ward", 
                                                                                               "Shibuya Ward"),
                                         alternative = "two.sided",
                                         mu = 0, paired = FALSE, var.equal = TRUE,
                                         conf.level = 0.95)


#Question 4#####################################################################
#get confidence interval for each mean
question4_data <- question3_data_summary %>%
  mutate(T_stat = qt(p = 0.975, df = 298)) %>% #for alpha/2 = 0.025
  mutate(standard_error = sd_price_per_m2 / sqrt(n)) %>%
  mutate(confidence_interval_start = mean_price_per_m2 - T_stat * standard_error) %>%
  mutate(confidence_interval_end = mean_price_per_m2 + T_stat * standard_error)

#plot means with confidence intervals
ggplot(question4_data) + 
  geom_col(aes(x = city_town_ward_village, 
               y = mean_price_per_m2),
           fill = "steelblue")  +
  geom_errorbar(aes(x = city_town_ward_village,
                    ymin = confidence_interval_start, 
                    ymax = confidence_interval_end), 
                width = 0.25) +
  labs(title = "Mean price per m^2 for each ward with 95% confidence intervals",
       x = "Ward",
       y = "Mean price per m^2")
  

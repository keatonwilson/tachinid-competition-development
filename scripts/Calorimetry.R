#Keaton Wilson
#2018-05-22
#keatonwilson@me.com
#Checking the calorimetry measurements
#

#Packages
library(tidyverse)
#install.packages("googlesheets")
library(googlesheets)

#Reading in the data from google sheets
gs_ls() #listing all my sheets and logging in
calories = gs_title("Tachinid Calorimetry") #registering the sheet I want
calories_df = gs_read(calories) #turning it into a dataframe/tibble

#removing the bogus values
calories_filtered = calories_df %>%
  filter(sample_cal > 0 ) %>%
  filter(sample_cal < 100 | tissue_ID == "Thorax") 

#plotting

calories_filtered %>%
  ggplot(aes(x = tissue_weight, y = sample_cal, color = factor(sex))) +
  geom_point() +
  facet_wrap(~ tissue_ID, scales = "free") +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1))

#linear models
lm.ab = calories_filtered %>%
  filter(tissue_ID == "Abdomen") %>%
lm(sample_cal ~ 0 + tissue_weight, data = .)

lm.head = calories_filtered %>%
  filter(tissue_ID == "Head") %>%
  lm(sample_cal ~ 0 + tissue_weight, data = .)

lm.thorax = calories_filtered %>%
  filter(tissue_ID == "Thorax") %>%
  lm(sample_cal ~ 0 + tissue_weight, data = .)

summary(lm.ab)
summary(lm.head)
summary(lm.thorax)


calories_filtered %>%
  filter(tissue_ID == "Head") %>%
  ggplot(aes(x = tissue_weight, y = sample_cal, color = factor(sex))) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1))

#Extracting linear model features for later use

library(broom)
lm_ab_summ = tidy(lm.ab)
lm_head_summ = tidy(lm.head)
lm_thorax_summ = tidy(lm.thorax)

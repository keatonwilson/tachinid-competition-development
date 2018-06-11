#Calorimeter bomb fly data
#keatonwilson@me.com
#2018-05-14
#

#Reading in the data
library(tidyverse)

tach_cal = read_csv("tach_cal.csv")


#Plotting

ggplot(tach_cal, aes(x = tissue_weight, y = sample_cal)) +
  geom_point() +
  facet_wrap(~factor(tissue_ID)) +
  theme_bw()

#Bad values for heads - let's throw them out for now. 
#

tach_cal %>%
  mutate(tissue_ID = factor(tissue_ID)) %>%
  filter((sample_cal < 100)) %>%
  ggplot(aes(x = tissue_weight, y = sample_cal)) +
  geom_point() +
  facet_wrap(~factor(tissue_ID)) +
  theme_bw() +
  geom_smooth(method = "lm")

#Splitting by tissue

tach_ab = tach_cal %>%
  filter(tissue_ID == "Abdomen") 

tach_head = tach_cal %>%
  filter(tissue_ID == "Head") %>%
  filter(sample_cal < 100)

tach_thorax = tach_cal %>%
  filter(tissue_ID == "Thorax")

lm.ab = lm(sample_cal ~ tissue_weight, data = tach_ab)
lm.head = lm(sample_cal ~ tissue_weight, data = tach_head)
lm.thorax = lm(sample_cal ~ tissue_weight, data = tach_thorax)


ggplot(tach_ab, aes(x = tissue_weight, y = sample_cal, color = factor(sex))) +
  geom_point(size = 5) +
    theme_bw()

ggplot(tach_thorax, aes(x = tissue_weight, y = sample_cal, color = factor(sex))) +
  geom_point(size = 5) +
  theme_bw()

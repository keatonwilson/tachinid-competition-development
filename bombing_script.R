#Bombing Script
#Keaton Wilson
#keatonwilson@me.com
#2019-03-18

#libraries
library(tidyverse)
library(stringr)

#importing data
skin_bomb = read_csv("./Data/skin_bomb.csv", 
                     col_names = c("sample_id", "hc_width", 
                                  "frozen_weight", "tissue", 
                                  "sample_weight", "spike_weight",
                                  "gross_heat", "cal", "notes"), skip = 1)

gut_bomb = read_csv("./Data/gut_bomb.csv",
                    col_names = c("sample_id", "hc_width", 
                                  "frozen_weight", "tissue", 
                                  "sample_weight", "spike_weight",
                                  "gross_heat", "cal", "notes"), skip = 1)

#exploring
ggplot(gut_bomb, aes(x = hc_width, y = cal)) +
  geom_point() +
  theme_classic()

ggplot(skin_bomb, aes(x = hc_width, y = cal)) +
  geom_point() +
  theme_classic()

#Not strong correlations between either, let's remove the points Lennie suggested were bad runs

gut_bomb$sample_id
skin_bomb$sample_id

skin_bomb_clean = skin_bomb[-c(7,11, 22, 23, 24),]
gut_bomb_clean = gut_bomb[-c(23,24),]

ggplot(gut_bomb_clean, aes(x = hc_width, y = cal)) +
  geom_point() +
  theme_classic()

ggplot(skin_bomb_clean, aes(x = hc_width, y = cal)) +
  geom_point() +
  theme_classic()

lm_1 = lm(cal ~ hc_width + frozen_weight, data = skin_bomb_clean)
lm_2 = lm(cal ~ hc_width, data = skin_bomb_clean)
lm_3 = lm(cal ~ frozen_weight, data = skin_bomb_clean)

lm_1_gut = lm(cal ~ hc_width + frozen_weight, data = gut_bomb_clean)
lm_2_gut = lm(cal ~ hc_width, data = gut_bomb_clean)
lm_3_gut = lm(cal ~ frozen_weight, data = gut_bomb_clean)

ggplot(gut_bomb_clean, aes(x = frozen_weight, y = hc_width)) +
  geom_point() +
  theme_classic()

gut_bomb_clean = gut_bomb_clean %>%
mutate(sample_id = as.factor(str_replace(gut_bomb_clean$sample_id, "\\(.*\\)", "")))

skin_bomb_clean = skin_bomb_clean %>%
  mutate(sample_id = as.factor(sample_id))

cal_master = left_join(gut_bomb_clean, skin_bomb_clean, by = c("sample_id", "hc_width", "frozen_weight")) %>%
  select(-c("tissue.x", "tissue.y", "sample_weight.x", "sample_weight.y", 
            "spike_weight.x", "spike_weight.y", "notes.x", "notes.y", "gross_heat.y",
            "gross_heat.x")) %>%
  rename(gut_cal = cal.x, skin_cal = cal.y) %>%
  filter(hc_width > 5.50)

lm_full = lm(gut_cal ~ hc_width, data = cal_master)  

ggplot(cal_master, aes(x = skin_cal, y = gut_cal)) +
  geom_point() +
  theme_classic()

lm_4 = lm(frozen_weight ~ hc_width, data = cal_master)

cal_master %>%
ggplot(aes(x = hc_width, y = gut_cal)) +
  geom_point() +
  theme_classic()

cal_master %>%
  filter(!is.na(gut_cal) & !is.na(skin_cal)) %>%
  mutate(total_cal = gut_cal + skin_cal) %>%
  ggplot(aes(x = hc_width, y = total_cal)) +
  geom_point() +
  theme_classic()

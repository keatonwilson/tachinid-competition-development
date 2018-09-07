#Energy Tradeoffs, allometry and comparisons
#Keaton Wilson
#keatonwilson@me.com
#2018-09-06

#Packages
library(stringr)
library(tidyverse)
library(ggextra)

#You will need access to the linear summaries of the models used in the calorimetry script they output at the end of the script into the working environment. You'll also need the tach_master_impute_long dataframe generated in the cohort_size_host_quality script and used in the previous allometry_plots_analyses. 

lm_head_summ
lm_thorax_summ
lm_ab_summ

#Cleaning up the strings
tach_master_impute_long$organ = str_replace(tach_master_impute_long$organ, "Weight", "")

#Calculating the calories per segement weight
tach_master_impute_long = tach_master_impute_long %>%
  mutate(calories = ifelse(organ == "Head", organ_weight*lm_head_summ[[2]],
                           ifelse(organ == "Ab", organ_weight*lm_ab_summ[[2]],
                                  ifelse(organ == "Thorax", organ_weight*lm_thorax_summ[[2]], NA))))
#Summarizing and checking the data
tach_master_impute_long %>%
  group_by(organ, Sex) %>%
  summarize(mean_calories = mean(calories))


#Just a quick plot
ggplot(tach_master_impute_long, aes(x = log(FlyWeight), y = log(calories), color = Sex, pch = organ)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

#The data in another structure that might be useful later on
tach_master_cal = tach_master %>%
  select(UniqueFlyID, Sex, FlyWeight, HeadWeight, ThoraxWeight, AbWeight) %>%
  mutate(head_cal = (HeadWeight*lm_head_summ[[2]]),
         thorax_cal = ThoraxWeight*lm_thorax_summ[[2]], 
         ab_cal = AbWeight*lm_ab_summ[[2]])


##The interesting body parts
Fig.10 = tach_master_impute_long %>%
  filter(organ == "Ab" | organ == "Thorax") %>%
  mutate(imp = ifelse(ThoraxWeight_imp == TRUE, TRUE, 
                      ifelse(AbWeight_imp == TRUE, TRUE, 
                             ifelse(FlyWeight_imp == TRUE, TRUE, FALSE)))) %>%
  ggplot(aes(x = log(FlyWeight), y = log(calories), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_point(data = tach_master_impute_long %>%
               filter(organ == "Ab" | organ == "Thorax" | organ == "Head") %>%
               mutate(imp = ifelse(ThoraxWeight_imp == TRUE, TRUE, 
                                   ifelse(AbWeight_imp == TRUE, TRUE, 
                                          ifelse(HeadWeight_imp == TRUE, TRUE,
                                                ifelse(FlyWeight_imp == TRUE, TRUE, FALSE))))) %>%
               filter(imp == TRUE), size = 3) +
  geom_smooth(method = "lm", aes(lty = Sex)) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 3) +
  coord_cartesian(xlim = c(0.5, 3), ylim = c(-2,4)) +
  geom_point(data = tach_master_impute_long %>%
               filter(organ == "Head"),
             size = 2, alpha = 0.5) +
  geom_smooth(data = tach_master_impute_long %>%
                filter(organ == "Head"), method = "lm", aes(group = 1)) +
  scale_color_discrete(guide = FALSE) +
  scale_shape_discrete(labels = c("Female", "Male")) +
  scale_linetype_discrete(guide = FALSE) +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 3.55, label = "Thorax",
                                      size = 9), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 2.7, label = "Abdomen",
                                      size = 9), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 1, label = "Head",
                                      size = 9), color = "black") +
  ylab("log calories") +
  xlab("log10 Body Weight (mg)")

#ggsave(Fig.10, file = "./output/Fig.10.pdf", device = "pdf", width = 10, height = 8, units = "in")


Fig_7_cal = ggplot(tach_master_cal, aes(x = thorax_cal, y = ab_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2) +
  theme_classic() +
  xlab("Thorax Calories") +
  ylab("Abdomen Calories") +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_size_continuous(name = "Fly Weight (mg)", range = c(1,5)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

#Stats
lm_cal_1 = lm(ab_cal ~ thorax_cal*Sex, data = tach_master_cal)
summary(lm_cal_1)

Fig_8_cal = ggplot(tach_master_cal, aes(x = head_cal, y = ab_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2) +
  theme_classic() +
  xlab("Head Calories") +
  ylab("Abdomen Calories") +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_size_continuous(name = "Fly Weight (mg)", range = c(1,5)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

#Stats
lm_cal_2 = lm(ab_cal ~ head_cal + Sex, data = tach_master_cal)
summary(lm_cal_2)


Fig_9_cal = ggplot(tach_master_cal, aes(x = head_cal, y = thorax_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2) +
  theme_classic() +
  xlab("Head Calories") +
  ylab("Thorax Calories") +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_size_continuous(name = "Fly Weight (mg)", range = c(1,5)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

#Stats
lm_cal_3 = lm(thorax_cal ~ head_cal + Sex, data = tach_master_cal) 

#Have to plot manually because it's additive
preds_fig_9 = as.tibble(predict(lm_cal_3, tach_master_cal, se.fit = TRUE, interval = "confidence", level = 0.95)$fit)
class(preds_fig_9[[1]])

Fig_9_cal = bind_cols(tach_master_cal, preds_fig_9) %>%
  ggplot(aes(x = head_cal, y = thorax_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.6) +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymax = upr, ymin = lwr, group = Sex), alpha = 0.2, color = NA) +
  theme_classic() +
  xlab("Head Calories") +
  ylab("Thorax Calories") +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_size_continuous(name = "Fly Weight (mg)", range = c(1,5)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))


#Normalized Energy Tradeoffs

#Just looking at the energy densities
tach_master_cal %>%
  mutate(head_energy_dens = head_cal/HeadWeight,
         thorax_energy_dens = thorax_cal/ThoraxWeight,
         ab_energy_dens = ab_cal/AbWeight) %>%
  select(Sex, FlyWeight, head_energy_dens, thorax_energy_dens, ab_energy_dens)


#Making a new dataframe with summaries
tach_cal = tach_master_impute_long %>%
  mutate(cal_dens = calories/organ_weight) %>%
  group_by(UniqueFlyID) %>%
  mutate(total_cal_dens = sum(cal_dens, na.rm = TRUE),
         total_cals = sum(calories, na.rm = TRUE)) %>%
  select(-(WingIntact:LegsWeight_imp)) %>%
  arrange(UniqueFlyID)

#Is this how Goggy is getting his plots? I'm not sure how to intrepret the y-axis here. 
ggplot(data = tach_cal, aes(x = organ_weight, y = cal_dens/total_cals, color = as.factor(organ))) +
  geom_point(aes(shape = Sex)) +
  theme_classic()

#This plot demonstrates the calorie trade-off between abdomens and thoraces beautifully
test = tach_master_cal %>%
  mutate(total_cal = head_cal + thorax_cal + ab_cal) %>%
  ggplot(aes(x = thorax_cal/total_cal, y = ab_cal/total_cal, color = Sex, group = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.7) +
  theme_classic() +
  scale_size_continuous("Fly Weight (mg)", range = c(2,7)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  xlab("Normalized Thorax Calories") +
  ylab("Normalized Abdomen Calories")

test = ggExtra::ggMarginal(test, type = "density", groupColour = TRUE, groupFill = TRUE)

ggsave(test, "./output/Fig10a.eps", width = 10, height = 8, units = "in", device = "eps")

#stats
stats_data = tach_master_cal %>%
  mutate(total_cal = head_cal + thorax_cal + ab_cal,
         rel_ab = ab_cal/total_cal, 
         rel_thorax = thorax_cal/total_cal,
         rel_head = head_cal/total_cal)

lm_tradeoff_1 = lm(rel_ab ~ rel_thorax, data = stats_data)

test2 =  tach_master_cal %>%
  mutate(total_cal = head_cal + thorax_cal + ab_cal) %>%
  ggplot(aes(x = thorax_cal/total_cal, y = head_cal/total_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.7) +
  theme_classic() +
  scale_size_continuous("Fly Weight (mg)") +
  scale_color_discrete(labels = c("Female", "Male")) +
  xlab("Normalized Thorax Calories") +
  ylab("Normalized Head Calories")

test2 = ggExtra::ggMarginal(test2, type = "density", groupColour = TRUE, groupFill = TRUE)

  

test3 = tach_master_cal %>%
  mutate(total_cal = head_cal + thorax_cal + ab_cal) %>%
  ggplot(aes(x = ab_cal/total_cal, y = head_cal/total_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.7) +
  theme_classic() +
  scale_size_continuous("Fly Weight (mg)") +
  scale_color_discrete(labels = c("Female", "Male")) +
  xlab("Normalized Abdomen Calories") +
  ylab("Normalized Head Calories")


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
tach_master_long$organ = str_replace(tach_master_long$organ, "Weight", "")

#Calculating the calories per segement weight
tach_master_impute_long = tach_master_impute_long %>%
  mutate(calories = ifelse(organ == "Head", organ_weight*lm_head_summ[[2]],
                           ifelse(organ == "Ab", organ_weight*lm_ab_summ[[2]],
                                  ifelse(organ == "Thorax", organ_weight*lm_thorax_summ[[2]], NA))))

tach_master_long = tach_master_long %>%
  mutate(calories = ifelse(organ == "Head", organ_weight*lm_head_summ[[2]],
                           ifelse(organ == "Ab", organ_weight*lm_ab_summ[[2]],
                                  ifelse(organ == "Thorax", organ_weight*lm_thorax_summ[[2]], NA))))
#Summarizing and checking the data
tach_master_impute_long %>%
  group_by(organ, Sex) %>%
  summarize(mean_calories = mean(calories))

tach_master_long %>%
  group_by(organ, Sex) %>%
  summarize(mean_calories = mean(calories, na.rm = TRUE))

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
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.7) +
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
  geom_line(aes(y = fit), size = 1.25) +
  geom_ribbon(aes(ymax = upr, ymin = lwr, group = Sex), alpha = 0.15, color = NA) +
  theme_classic() +
  xlab("Head Calories") +
  ylab("Thorax Calories") +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_size_continuous(name = "Fly Weight (mg)", range = c(1,5)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

cal_cal = ggarrange(Fig_7_cal, Fig_8_cal, Fig_9_cal, common.legend = TRUE, labels = "auto", label.x = 0.85)
ggsave(plot = cal_cal, filename = "./output/Figure6paneled.pdf", device = "pdf")

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
  theme_classic(base_size = 20) +
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


#Recreating Figure 3, but with energy
#The data in another structure that might be useful later on
tach_master_sib_cal = tach_master %>%
  select(UniqueFlyID, Sex, FlyWeight, HeadWeight, ThoraxWeight, AbWeight, sib_number, CaterpillarID) %>%
  mutate(head_cal = (HeadWeight*lm_head_summ[[2]]),
         thorax_cal = ThoraxWeight*lm_thorax_summ[[2]], 
         ab_cal = AbWeight*lm_ab_summ[[2]], 
         total_cal = head_cal + thorax_cal + ab_cal) 

ggplot(data = tach_master_sib_cal, aes(y = total_cal, x = FlyWeight, color = Sex)) +
  geom_point() +
  theme_classic()

ggplot(data = tach_master_sib_cal, aes(x = sib_number, y = , color = Sex)) +
  geom_point()

ggplot(data = tach_master_sib_cal, aes(x = sib_number, y = (ab_cal/FlyWeight), color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

ggplot(data = tach_master_sib_cal, aes(x = sib_number, y = (thorax_cal/FlyWeight), color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()


#Goggy meeting (2018-09-24) suggestion of Percent Calories/Fly Weight
glimpse(tach_master_sib_cal)

tach_master_sib_cal %>%
  mutate(percent_head = head_cal/total_cal,
         percent_thorax = thorax_cal/total_cal,
         percent_ab = ab_cal/total_cal) %>%
  select(Sex, FlyWeight, percent_head, percent_thorax, percent_ab) %>%
  gather(value = percent, key = segment, percent_head:percent_ab) %>%
ggplot(aes(x = FlyWeight, y = percent, color = segment, shape = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  #facet_wrap(~ segment) +
  geom_smooth(method = "lm", aes(lty = Sex)) +
  xlab("Fly Weight (mg)") +
  ylab("Percent Calories")

#A few modifications
#1. Males Thoraces look polynomial to me. 
#2. One line for heads

tach_master_percent = tach_master_sib_cal %>%
  mutate(percent_head = head_cal/total_cal,
         percent_thorax = thorax_cal/total_cal,
         percent_ab = ab_cal/total_cal) %>%
  select(Sex, FlyWeight, percent_head, percent_thorax, percent_ab, CaterpillarID) %>%
  gather(value = percent, key = segment, percent_head:percent_ab)

p1 = tach_master_percent %>%
ggplot(aes(x = FlyWeight, y = percent, shape = Sex, color = segment)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(data = tach_master_percent %>%
                filter(segment == "percent_thorax"),
              method = "lm",
              formula = y ~ poly(x, 2),
              color = "#00BFC4",
              aes(lty = Sex)) +

  geom_smooth(data = tach_master_percent %>%
                filter(segment == "percent_head"),
              aes(group = 1),
              method = "lm",
              color = "#7CAE00") +
  geom_smooth(data = tach_master_percent %>%
                filter(segment == "percent_ab"),
              method = "lm",
              formula = y ~ poly(x, 2),
              color = "#F8766d",
              aes(lty = Sex)) +
  theme_classic(base_size = 20) +
  xlab("Fly Weight (mg)") +
  ylab("Percent Calories") +
  scale_color_discrete(name = "Body Segment",
                       labels = c("Abdomen", "Head", "Thorax"))
  
g1 = ggarrange(p1, test, nrow = 2, labels = "auto", label.x = 0.65)
ggsave("/Users/KeatonWilson/Documents/Writing/Tachinid Development/Figures/Nature Figures/no_impute/Figure4Panel.pdf", height = 11, width = 8.5, units = "in")

hist = ggplot(tach_master_impute, aes(x = FlyWeight, fill = Sex)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 0.75) +
 geom_density(aes(group = 1), alpha = 0.5) +
  theme_classic(base_size = 20) + 
    xlab("Fly Weight (mg)") +
    ylab("Density") +
  scale_fill_discrete(name = "Sex", 
                      labels = c("Female", "Male")) 

#Hist without imputations
hist = ggplot(tach_master, aes(x = FlyWeight, fill = Sex)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 0.75) +
  geom_density(aes(group = 1), alpha = 0.5) +
  theme_classic(base_size = 20) + 
  xlab("Fly Weight (mg)") +
  ylab("Density") +
  scale_fill_discrete(name = "Sex", 
                      labels = c("Female", "Male")) 

ggsave("./output/hist.pdf", hist)
  
library(diptest)
library(modes)

#without imputations
weights = tach_master %>%
  filter(!is.na(FlyWeight)) %>%
 # filter(FlyWeight_imp == "FALSE") %>%
  select(FlyWeight) %>%
  pull()

weights2 = tach_master %>%
  filter(!is.na(FlyWeight)) %>%
  #filter(FlyWeight_imp == "FALSE") %>%
  select(FlyWeight) %>%
  pull()

weights_male = tach_master %>%
  filter(!is.na(FlyWeight)) %>%
 # filter(FlyWeight_imp == "FALSE") %>%
  filter(Sex == "M") %>%
  select(FlyWeight) %>%
  pull()

weights_female = tach_master %>%
  filter(!is.na(FlyWeight)) %>%
 # filter(FlyWeight_imp == "FALSE") %>%
  filter(Sex == "F") %>%
  select(FlyWeight) %>%
  pull()





dip.test(weights)
dip.test(weights_male)
dip.test(weights_female)


bimodality_amplitude(weights_male, fig = TRUE)
bimodality_amplitude(weights_female, fig = TRUE)
bimodality_amplitude(weights2, fig = TRUE)
bimodality_coefficient(weights2, TRUE)
bimodality_coefficient(weights_male)
bimodality_coefficient(weights_female)

    
weights
plot(density(weights))

shapiro.test(weights)


tach_master_impute

ggplot(tach_master_impute, aes(x = AbWeight/FlyWeight, fill = Sex)) +
  geom_density() +
  theme_classic()



test = tach_master_cal %>%
  mutate(total_cal = head_cal + thorax_cal + ab_cal,
         rel_head_cal = head_cal/total_cal,
         rel_thorax_cal = thorax_cal/total_cal,
         rel_abdomen_cal = ab_cal/total_cal)

lmhead = lm(rel_head_cal ~ rel_thorax_cal, data = test)
lmhead2 = lm(rel_head_cal ~ rel_abdomen_cal, data = test)
  
ggplot(tach_master_impute, aes(x = FlyWeight)) +
  geom_histogram(binwidth = 0.5) +
  theme_classic() +
  facet_wrap(~ CaterpillarID)

f1 = ggplot(tach_master_impute, aes(x = FlyWeight, y = sib_number)) +
  geom_label(size = 3, alpha = 0.6, aes(label = CaterpillarID)) +
  theme_classic(base_size = 20)

p1 = p1 + geom_vline(xintercept = 10, lty = 2)
f1 = f1 + geom_vline(xintercept = 10, lty = 2)
hist = hist + geom_vline(xintercept = 10, lty = 2)

ggarrange(p1, f1, hist, nrow = 3, align = "h", legend = "none")

tach_master_impute %>%
  group_by(CaterpillarID, sib_number) %>%
  summarize(mean_weight = mean(FlyWeight, na.rm = TRUE)) %>%
  ggplot(aes(x = mean_weight, y = sib_number)) +
  geom_point(size = 3) +
  theme_classic() +
  geom_vline(xintercept = 8.5) +
  geom_jitter(data = tach_master_impute, aes(x = FlyWeight, y = sib_number), alpha = 0.2)


ggplot(tach_master_percent, aes(x = FlyWeight, y = percent, color = segment)) +
  geom_point(aes(shape = Sex)) +
  theme_classic() +
  facet_wrap(~ CaterpillarID)

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

rm(DMSO_timecourse)

DMSO_timecourse <- read_csv("~/student documents/UBC/Research/chaoborus imaging/in vivo, methylcellulose/Endothelium vs. haem pH with DMSO, standard pH curve (outlined)/DMSO timecourse.csv")
View(DMSO_timecourse)

pH_standards <- read_csv("~/student documents/UBC/Research/chaoborus imaging/in vivo, methylcellulose/Endothelium vs. haem pH with DMSO, standard pH curve (outlined)/pH standards.csv")
View(pH_standards)

#make single time variable
DMSOlong <-
  pivot_longer(
    DMSO_timecourse,
    cols = c(`pre.inj`, `1h`, `2h`, `3h`, `4h`, `5h`),
    names_to = "h", values_to = "area"
  ) %>%
  select(ant.post, larva, slope, intercept, h, area) %>%
  mutate_at("h", str_replace, "pre.inj", "0") %>% ## pre.inj to 0
  mutate_at("h", str_replace, "1h", "1") %>%
  mutate_at("h", str_replace, "2h", "2") %>%
  mutate_at("h", str_replace, "3h", "3") %>%
  mutate_at("h", str_replace, "4h", "4") %>%
  mutate_at("h", str_replace, "5h", "5") %>%
  mutate(larva = as_factor(larva)) %>%
  mutate(h = as_factor(h)) %>%
  mutate(pH_calc = ((area - intercept)/slope))

print(DMSOlong, n=72)

## get rid of in haem values where we dont have pH standard values
DMSOlong$area[is.na(DMSOlong$pH_calc) == T] <- NA

# make means and sd
meansd <- DMSOlong %>%
  group_by(ant.post, h) %>% 
  dplyr::summarise(
    area_mean = mean(area, na.rm = T), 
    area_sd = sd(area, na.rm = T),
    pHmean = mean(pH_calc, na.rm = T),
    pHsd = sd(pH_calc, na.rm = T)) 

print(meansd, n= 25)

#plot means/ sd
ggplot(data = meansd, aes(x= h)) +
  geom_point(aes(y= area_mean, 
                 colour= ant.post)) +
  geom_line(aes(y= area_mean, 
                group= ant.post, 
                colour= ant.post)) +
  geom_errorbar(aes(x= h, 
                    group= ant.post, 
                    colour= ant.post,
                    ymin= area_mean - area_sd, 
                    ymax= area_mean + area_sd), 
                group= "ant.post",
                width= 0.1) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  labs(x = "h", 
       y = "Area (mm^2)") +
  ggtitle("area means") +
  theme_classic()

#plot pHmeans/ pHsd
ggplot(data = meansd, aes(x= h)) +
  geom_point(aes(y= pHmean, 
                 colour= ant.post)) +
  geom_line(aes(y= pHmean, 
                group= ant.post, 
                colour= ant.post)) +
  geom_errorbar(aes(x= h, 
                    group= ant.post, 
                    colour= ant.post,
                    ymin= pHmean - pHsd, 
                    ymax= pHmean + pHsd), 
                group= "ant.post",
                width= 0.1) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  labs(x = "h", 
       y = "pH") +
  ggtitle("pH means") +
  theme_classic()

#plot area by pH
ggplot(data = meansd, aes(x= pHmean)) +
  geom_point(aes(y= area_mean, 
                 colour= ant.post)) +
  geom_line(aes(y= area_mean, 
                group= ant.post, 
                colour= ant.post)) +
  geom_errorbar(aes(x= pHmean, 
                    group= ant.post, 
                    colour= ant.post,
                    ymin= area_mean - area_sd, 
                    ymax= area_mean + area_sd), 
                group= "ant.post",
                width= 0.1) +
  geom_errorbarh(aes(colour = ant.post,
                     y = area_mean,
                     xmax = pHmean + pHsd, 
                     xmin = pHmean - pHsd)) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  labs(x = "pH", 
       y = "Area (mm^2)") +
  #ggtitle("") +
  theme_classic() +
  theme(legend.position = "none")

##############  stats for in haem  ################

# make data frame of just pre.inj and 5h for direct comparison with MCMC
start_end <- DMSOlong %>%
  filter(h %in% c(0, 5))

print(start_end, n= 24)

prior <- list(
  R = list(V = 1, nu = 0.002), 
  G = list(G = list(V = 2, nu = 0.2)))

# make ant only
start_end.ant <- start_end %>%
  filter(ant.post == 'ant')
print(start_end.ant)

mcmod.sup.start_end.ant <-
  MCMCglmm::MCMCglmm(
    pH_calc ~ h-1, random = ~larva,
    data = start_end.ant, scale = FALSE, prior = prior,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.sup.start_end.ant)

# make post only
start_end.post <- start_end %>%
  filter(ant.post == 'post')
print(start_end.post)

mcmod.sup.start_end.post <-
  MCMCglmm::MCMCglmm(
    pH_calc ~ h-1, random = ~larva,
    data = start_end.post, scale = FALSE, prior = prior,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.sup.start_end.post)


# both together
mcmod.sup.start_end <-
  MCMCglmm::MCMCglmm(
    pH_calc ~ h + ant.post, random = ~larva,
    data = start_end.ant, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.sup.start_end)


mcmod.sup.DMSOlong <-
  MCMCglmm::MCMCglmm(
    pH_calc ~ h + ant.post, random = ~larva,
    data = DMSOlong, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.sup.DMSOlong)


install.packages("MCMCglmm")
library(MCMCglmm)

mcmod.sup <-
  MCMCglmm::MCMCglmm(
    area ~ h-1, random = ~larva,
    data = DMSOlong, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.sup)

library(lmerTest)
z <- lmer(pH_calc ~ h + (1|larva), data = DMSOlong)
summary(z)
confint(z)
plot(z)
anova(z, type = 1)
summary(e)
TukeyHSD(e)

install.packages("emmeans")
library(emmeans)
f <- emmeans(z, pairwise ~ area | h)

###############      arranging data for pH standards      ############### 

standards.long <- 
  pivot_longer(
    pH_standards,
    cols = c(`6`, `7`, `8`),
    names_to = "pH", 
    values_to = "area")

print(standards.long, n= 36)

#making % change column
standards.long.pct <- standards.long %>%
  group_by(ant.post, larva) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  as_tibble(DMSOlong.pct) %>%
  ungroup() %>%
  mutate(larva = as_factor(larva))

print(standards.long.pct, n= 50)

# make means and sd

std_stats <-
  standards.long.pct %>%
  select(-larva) %>% ## exclude larva
  group_by(pH, ant.post) %>% 
  ## now compute mean and sd:
  summarize(across(everything(), na.rm= T,
                   tibble::lst(mean = mean, sd = sd)))

print(std_stats)


## plotting mean standards
ggplot(data = std_stats, 
       aes(x= pH)) +
  geom_point(aes(y= area_mean, 
                 colour= ant.post)) +
  geom_line(aes(y= area_mean, 
                group= ant.post, 
                colour= ant.post)) +
  geom_errorbar(aes(x= pH, 
                    group= ant.post, 
                    colour= ant.post,
                    ymin= area_mean - area_sd, 
                    ymax= area_mean + area_sd), 
                group= "ant.post",
                width= 0.2) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  #ylim(0.3, 1) +
  labs(x = "pH", 
       y = "Airsac area (mm^2)") +
  theme_classic()


## plotting individual standards
ggplot(data = standards.long.pct, 
       aes(x= pH)) +
  geom_point(aes(y= area, 
                 colour= larva,
                 group = ant.post)) +
  geom_line(aes(y= area,
                 group = larva))

+
  geom_line(aes(y= area_mean, 
                group= ant.post, 
                colour= ant.post)) +
  geom_errorbar(aes(x= pH, 
                    group= ant.post, 
                    colour= ant.post,
                    ymin= area_mean - area_sd, 
                    ymax= area_mean + area_sd), 
                group= "ant.post",
                width= 0.2) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  #ylim(0.3, 1) +
  labs(x = "pH", 
       y = "Airsac area (mm^2)") +
  theme_classic()

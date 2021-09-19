install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

DMSO_timecourse <- read_csv("~/student documents/UBC/Research/chaoborus imaging/in vivo, methylcellulose/Endothelium vs. haem pH with DMSO, standard pH curve (outlined)/DMSO timecourse.csv")
View(DMSO_timecourse)

pH_standards <- read_csv("~/student documents/UBC/Research/chaoborus imaging/in vivo, methylcellulose/Endothelium vs. haem pH with DMSO, standard pH curve (outlined)/pH standards.csv")
View(pH_standards)

#make single time variable
DMSOlong <-
  pivot_longer(
    DMSO_timecourse,
    cols = c(`pre.inj`, `1h`, `2h`, `3h`, `4h`, `5h`),
    names_to = "h", 
    values_to = "area") %>%
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

print(DMSOlong)

#making % change column
DMSOlong.pct <- DMSOlong %>%
  group_by(ant.post, larva) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  mutate(
    pH.pct.change = ((pH_calc - pH_calc[1]) / pH_calc[1]
    )*100) %>%
  as_tibble(DMSOlong.pct) %>%
  ungroup()

print(DMSOlong.pct, n= 50)

# make means and sd
pct.meansd <- DMSOlong.pct %>%
  group_by(ant.post, h) %>% 
  dplyr::summarise(
    pct.mean = mean(area.pct.change), 
    pct.sd = sd(area.pct.change),
    pct.pHmean = mean(pH.pct.change),
    pct.pHsd = sd(pH.pct.change)) 

print(pct.meansd, n= 25)

#plot means/ sd
ggplot(data = pct.meansd, 
       aes(x= h)) +
  geom_point(aes(y= pct.mean, 
                 colour= ant.post)) +
  geom_line(aes(y= pct.mean, 
                group= ant.post, 
                colour= ant.post)) +
  geom_errorbar(aes(x= h, 
                    group= ant.post, 
                    colour= ant.post,
                    ymin= pct.mean - pct.sd, 
                    ymax= pct.mean + pct.sd), 
                group= "ant.post",
                width= 0.2) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  labs(x = "h", y = "% change") +
  theme_classic()

#plot pH means/ pH sd
ggplot(data = pct.meansd, 
       aes(x= h)) +
  geom_point(aes(y= pct.pHmean, 
                 colour= ant.post)) +
  geom_line(aes(y= pct.pHmean, 
                group= ant.post, 
                colour= ant.post)) +
  geom_errorbar(aes(x= h, 
                    group= ant.post, 
                    colour= ant.post,
                    ymin= pct.pHmean - pct.pHsd, 
                    ymax= pct.pHmean + pct.pHsd), 
                group= "ant.post",
                width= 0.2) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
 # ylim(-1, 60) +
  labs(x = "h", y = "% change") +
  theme_classic()

########################    stats    #########################

start_end.pct <- DMSOlong.pct %>%
  filter(h %in% c(0, 5))

print(start_end.pct)

mcmod.sup.start_end.pct <-
  MCMCglmm::MCMCglmm(
    area.pct.change ~ h, random = ~larva,
    data = start_end.pct, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.sup.start_end.pct)

# only ~1.22% of variation is caused by larval identity
mean(mcmod.sup.start_end.pct$VCV[,1]/(mcmod.sup.start_end.pct$VCV[,1] + mcmod.sup.start_end.pct$VCV[,2]))


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


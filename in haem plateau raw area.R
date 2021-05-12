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
    names_to = "h", values_to = "area"
  ) %>%
  select(ant.post, larva, h, area) %>%
  mutate_at("h", str_replace, "pre.inj", "0") %>% ## pre.inj to 0
  mutate_at("h", str_replace, "1h", "1") %>%
  mutate_at("h", str_replace, "2h", "2") %>%
  mutate_at("h", str_replace, "3h", "3") %>%
  mutate_at("h", str_replace, "4h", "4") %>%
  mutate_at("h", str_replace, "5h", "5")

DMSOlong$h <- as.numeric(DMSOlong$h)
DMSOlong$h <- as_factor(DMSOlong$h)

print(DMSOlong)

# make means and sd
meansd <- DMSOlong %>%
  group_by(ant.post, h) %>% 
  dplyr::summarise(
    mean = mean(area), 
    sd = sd(area)) 

print(meansd, n= 25)

#plot means/ sd
ggplot(data = meansd, aes(x= h)) +
  geom_point(aes(y= mean, 
                 colour= ant.post)) +
  geom_line(aes(y= mean, 
                group= ant.post, 
                colour= ant.post)) +
  geom_errorbar(aes(x= h, 
                    group= ant.post, 
                    colour= ant.post,
                    ymin= mean - sd, 
                    ymax= mean + sd), 
                group= "ant.post",
                width= 0.1) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  labs(x = "h", 
       y = "Area (mm^2)") +
  theme_classic()

##############  stats for in haem  ################


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
  ungroup()

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


## plotting standards
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
       y = "Area change (%)") +
  theme_classic()

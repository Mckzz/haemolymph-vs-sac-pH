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

#making % change column
DMSOlong.pct <- DMSOlong %>%
  group_by(ant.post, larva) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  as_tibble(DMSOlong.pct) %>%
  ungroup()

print(DMSOlong.pct, n= 50)

# make means and sd
pct.meansd <- DMSOlong.pct %>%
  group_by(ant.post, h) %>% 
  dplyr::summarise(
    pct.mean = mean(area.pct.change), 
    pct.sd = sd(area.pct.change)) 

print(pct.meansd, n= 25)

#plot means/ sd
ggplot(data = pct.meansd, aes(x= h)) +
  geom_point(aes(y= pct.mean, colour= ant.post)) +
  geom_line(aes(y= pct.mean, group= ant.post, colour= ant.post)) +
  geom_errorbar(aes(x= h, group= ant.post, colour= ant.post,
                    ymin= pct.mean - pct.sd, 
                    ymax= pct.mean + pct.sd), 
                group= "ant.post",
                width= 0.2) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  labs(x = "h", y = "% change") +
  theme_classic()


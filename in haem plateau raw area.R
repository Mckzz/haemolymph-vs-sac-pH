install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
install.packages("Hmisc")

rm(DMSO_timecourse)
head(DMSO_timecourse)

is.data.frame(DMSO_timecourse)

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

# makes separate anterior/ posterior dataframes
anterior <- subset(DMSOlong, ant.post == "ant", 
                   select = c(larva, h, area))

posterior <- subset(DMSOlong, ant.post == "post", 
                    select = c(larva, h, area))

print(anterior, n=30)

#making means
ant.means <- anterior %>% 
  group_by(h) %>% 
  dplyr::summarise(
    ant = mean(area))

print(ant.means, n= 36)

post.means <- posterior %>% 
  group_by(h) %>% 
  summarise(
    post = mean(area))

print(post.means, n= 36)

#combining means
means <- ant.means

means$post <- post.means$post
print(means, n= 36)

#make sd values
ant.stdv <- anterior %>%
  group_by(h) %>%
  dplyr::summarize(
    ant = sd(area))

print(ant.stdv)

post.stdv <- posterior %>%
  group_by(h) %>%
  dplyr::summarize(
    post = sd(area))

print(post.stdv)

#combine sd values
stdv <- post.stdv
stdv$ant <- ant.stdv$ant
print(stdv)

#combine %change means and sdtvs using left join after pivoting long
mean.long <- means %>% 
  pivot_longer(
    cols = c(`ant`, `post`),
    names_to = "antpost", values_to = "area")

print(mean.long)

stdv.long <- stdv %>%
  pivot_longer(
    cols = c(`ant`, `post`),
    names_to = "antpost", values_to = "stdv")

print(stdv.long)

mean.sd <-
  left_join(mean.long, stdv.long, by = c("h", "antpost"))

print(mean.sd)

######## plotting ########

#plot means/ sd
ggplot(data = mean.sd, aes(x= h)) +
  geom_point(aes(y= area, colour= antpost)) +
  geom_line(aes(x= h, y= area, group= antpost, colour= antpost)) +
  geom_errorbar(aes(x= h, group= antpost, colour= antpost,
                    ymin= area - stdv, 
                    ymax= area + stdv), 
                width= 0.2) +
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  labs(x = "h", y = "area (mm^2)") +
  theme_classic()

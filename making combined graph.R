install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#########################################    still not working, just used illustrator for now      ###############################


fit <- pH_standards %>%
  pivot_longer(
    cols = c(`6`, `7`, `8`),
    names_to = "pH", values_to = "area"
  )

print(fit, n= 36)

#make data frame conssting of the sac sizes at h= 0
size.pre <- subset(DMSOlong.pct, h == "0", 
                       select = c(ant.post, larva, area))
print(size.pre)

size.pre$area.pre <- size.pre$area

#join the h= 0 sizes to fit and rename variables
fit.pre <- left_join(fit, size.pre, by = c("larva", "ant.post")) %>%
  dplyr::rename(area = area.x)%>%
  dplyr::rename(area.pre = area.y)

print(fit.pre, n= 36)

#making % change column based on area in pH standard vs initial area measured in intact larva
fit.pct <- fit.pre %>%
  group_by(ant.post, larva) %>%
  mutate(
    area.pct.change = ((area - area.pre) / area.pre
    )*100) %>%
  as_tibble(fit.pct) %>%
  ungroup()

print(fit.pct, n= 50)

#calculate means for each pH 

fitmeans <-
  fit.pct %>%
  select(-larva) %>% ## exclude larva
  group_by(pH) %>% ## group by pH
  ## now compute mean and sd:
  summarize(across(everything(), na.rm = T,
                   tibble::lst(mean = mean, sd = sd)))

print(fitmeans)

##### plotting #####
ggplot(data = fit, aes(x= pH)) +
  geom_point(aes(y= area, colour= antpost)) +
  geom_line(aes(y= area, group= antpost, colour= antpost))



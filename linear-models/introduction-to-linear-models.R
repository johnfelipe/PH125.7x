#### Confounding ####

library(Lahman)
library(tidyverse)
?Lahman
data("Teams")
head(Teams)

Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H-HR-X2B-X3B)/G, BB=BB/G, HR=HR/G) %>%
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))

#### Stratification and Multivariate Regression ####

library(Lahman)
library(tidyverse)
?Lahman
data("Teams")
head(Teams)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_per_game = R/G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)
head(dat)

dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)

dat %>%
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game) * sd(R_per_game) / sd(BB_per_game))

#

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_per_game = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)
head(dat)

dat %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_strata)

dat %>%
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game) * sd(R_per_game) / sd(HR_per_game))


### Linear Models ###
library(dslabs)
library(tidyverse)
library(HistData)
?HistData
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
head(galton_heights)

galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

lm(son ~ father, data = galton_heights)

#

library(dslabs)
library(tidyverse)
library(HistData)
?HistData
data("GaltonFamilies")

galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))

lm(formula = son ~ father_centered, data = galton_heights)

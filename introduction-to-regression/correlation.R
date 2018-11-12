#### Correlation ####

### Correlation ###
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

galton_heights %>% ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)


### Correlation Coefficient ###
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

galton_heights %>% summarize(cor(father, son))


### Sample Correlation is a Random Variable ###
library(dslabs)
library(tidyverse)
library(HistData)
?HistData
data("GaltonFamilies")

set.seed(0)
R <- sample_n(galton_heights, 23, replace = TRUE) %>%
  summarize(cor(father, son))
R

B <- 1000
N <-25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
  summarize(r = cor(father, son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color = "black")

mean(R)
sd(R)  
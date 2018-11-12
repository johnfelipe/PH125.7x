#### Advanced dplyr: Tibbles ####

library(Lahman)
library(tidyverse)
data("Teams")

head(Teams)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2) 
head(dat)

dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

#### Tibbles: Differences from Data Frames ####
library(Lahman)
library(tidyverse)
data("Teams")

Teams
as_tibble(Teams)


#### do ####
library(Lahman)
library(tidyverse)
data("Teams")

head(Teams)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2) 
head(dat)

get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficients[2,2])
}

dat %>%
  group_by(HR) %>%
  do(get_slope(.))

#

get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))
 

### Question 2 ###
# 1 point possible (graded)
# You want to take the tibble dat, which we’ve been using in this video, and run the linear model R ~ BB for each strata of HR. Then you want to add three new columns to your grouped tibble: the coefficient, standard error, and p-value for the BB term in the model.
# 
# You’ve already written the function get_slope, shown below.

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

# What additional code could you write to accomplish your goal?
  
# a
dat %>% 
  group_by(HR) %>% 
  do(get_slope)

# b
dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))

# c
dat %>% 
  group_by(HR) %>% 
  do(slope = get_slope(.))

# d
dat %>% 
  do(get_slope(.))


#### broom ####
library(Lahman)
library(tidyverse)
library(broom)
data("Teams")
head(Teams)


### Question 2 ###
# Question 2
# 1 point possible (graded)
# You want to know whether the relationship between home runs and runs per game varies by baseball league. You create the following dataset:
  
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         BB = BB/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

head(dat)

# What code would help you quickly answer this question?

# a  
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

# b
dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

# c 
dat %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

# d
dat %>% 
  group_by(lgID) %>% 
  do(mod = lm(R ~ HR, data = .))
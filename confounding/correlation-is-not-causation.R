library(tidyverse)
library(broom)

N <- 25
G <- 1000000
sim_data <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))
sim_data

res <- sim_data %>%
  group_by(group) %>%
  summarize(r = cor(X, Y)) %>%
  arrange(desc(r))
res

sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(X, Y)) +
  geom_point() + 
  geom_smooth(method = "lm")

res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~ X, data = .)))

### Outliers ###
set.seed(1)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

  
tibble(x,y) %>% ggplot(aes(x,y)) + geom_point(alpha = 0.5)

cor(x,y)

cor(x[-23], y[-23])

tibble(x,y) %>% 
  ggplot(aes(rank(x),rank(y))) + 
  geom_point(alpha = 0.5)

cor(rank(x), rank(y))

cor(x, y, method = "spearman")

### 
library(dslabs)
data(admissions)
?admissions
admissions

admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted/100*applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

admissions %>% 
  group_by(major) %>%  
  summarize(major_selectivity = sum(admitted*applicants)/sum(applicants),
            percent_women_applicants = sum(applicants*(gender=="women")/sum(applicants))*100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

admissions %>% 
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()

admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))

?admissions

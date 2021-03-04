library(tidyverse)
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(bayestestR)


study_s1 <- read_csv("data/study_1/study.csv")
study_s2 <- read_csv("data/study_2/study.csv")

study_s1 <- study_s1 %>% 
  mutate(
    in_retest = case_when(partid %in% study_s2$partid ~ 1,
                          TRUE ~ 0)
  )

# compare subsample demographics
age_mod <- stan_glm(age ~ in_retest, chains = 4, iter = 10000,
                    data = study_s1, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
describe_posterior(age_mod, ci = .95, test = c())

edu_mod <- stan_glm(yearsofedu ~ in_retest, chains = 4, iter = 10000,
                    data = study_s1, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
describe_posterior(edu_mod, ci = .95, test = c())

sex_mod <- stan_glm(sex ~ in_retest, chains = 4, iter = 10000,
                    data = study_s1 %>%
                      mutate(sex = case_when(sex == "male" ~ 0,
                                             TRUE ~ 1)),
                    family = binomial(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
describe_posterior(sex_mod, ci = .95, test = c())

# compare subsample study parameters
rat_mod <- stan_glm(rating ~ in_retest, chains = 4, iter = 10000,
                    data = study_s1, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
describe_posterior(rat_mod, ci = .95, test = c())

ris_mod <- stan_glm(m_risk ~ in_retest, chains = 4, iter = 10000,
                    data = study_s1, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
describe_posterior(ris_mod, ci = .95, test = c())

asp_mod <- stan_glm(n_aspects ~ in_retest, chains = 4, iter = 10000,
                    data = study_s1, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
describe_posterior(asp_mod, ci = .95, test = c())

sen_mod <- stan_glm(mean_sentiment ~ in_retest, chains = 4, iter = 10000,
                    data = study_s1, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
describe_posterior(sen_mod, ci = .95, test = c())


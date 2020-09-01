library(tidyverse)
library(BayesianFirstAid)
library(patchwork)
library(DescTools)
library(rstanarm)
library(bayesplot)
library(psych)
library(data.table)
library(caret)
library(Rcpp)
library(rstanarm)
library(beanplot)
options(mc.cores = parallel::detectCores())
sourceCpp("cpp/vum_functions.cpp")
source("r/study_1/helper.R")

# load data
study_joint <- read_csv("data/study_2/study_joint.csv")
sim_dat <- readRDS("data/similarities/similarity_ratings.RDS")
study_long_s1 <- read_csv("data/study_1/study_long.csv")
study_long_s2 <- read_csv("data/study_2/study_long.csv")
study_s1 <- read_csv("data/study_1/study.csv")
study_s2 <- read_csv("data/study_2/study.csv")
ratings_s1 <- study_joint %>%
  select(partid, rating_s1)
ratings_s2 <- study_joint %>%
  select(partid, rating_s2)


# select only participants who participated in both studies and add ratings of s1 to s2 and vice versa


study_long_s1_betw <- study_long_s1 %>%
  filter(partid %in% unique(study_long_s2$partid)) %>%
  select(-rating) %>%
  left_join(ratings_s2, by = "partid") %>%
  rename(rating = rating_s2)
study_s1_betw <- study_s1 %>%
  filter(partid %in% unique(study_long_s2$partid)) %>%
  select(-rating) %>%
  left_join(ratings_s2, by = "partid") %>%
  rename(rating = rating_s2)

study_long_s2_betw <- study_long_s2 %>%
  select(-rating) %>%
  left_join(ratings_s1, by = "partid") %>%
  rename(rating = rating_s1)
study_s2_betw <- study_s2 %>%
  select(-rating) %>%
  left_join(ratings_s1, by = "partid") %>%
  rename(rating = rating_s1)


### RQ1: Modeling self-reported risk-taking propensity =========================

### Cross validation using five folds predicting ratings of s2 with aspects of s1

# set seed for replicability of results
set.seed(1)

# test within occasions
cv_within_s1 <- cross_val2(study_long_s1, study_s1)
cv_within_s2 <- cross_val2(study_long_s2, study_s2)

# use these parameters to get predictions across occasions

soe_mod_ws1 <- stan_polr(rating_f ~ m_risk,
                     data = study_s1 %>%
                       mutate(rating_f = factor(rating, levels = 0:10,
                                                ordered = TRUE)), 
                     prior = rstanarm::R2(.25, what = "median"),
                     prior_counts = dirichlet(rep(1, 11)),
                     iter = 5000, chains = 4)
soe_mod_ws2 <- stan_polr(rating_f ~ m_risk,
                         data = study_s2 %>%
                           mutate(rating_f = factor(rating, levels = 0:10,
                                                    ordered = TRUE)), 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 5000, chains = 4)
woe_mod_ws1 <- stan_polr(rating_f ~ m_aspects,
                         data = study_s1 %>%
                           mutate(rating_f = factor(rating, levels = 0:10,
                                                    ordered = TRUE)), 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 5000, chains = 4)
woe_mod_ws2 <- stan_polr(rating_f ~ m_aspects,
                         data = study_s2 %>%
                           mutate(rating_f = factor(rating, levels = 0:10,
                                                    ordered = TRUE)), 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 5000, chains = 4)



pred_between_s1 <- pred_acc(study_long_s1_betw, study_s1_betw,
                            mean(cv_within_s1$parameters$phi),
                            mean(cv_within_s1$parameters$QT_Intercept),
                            mean(cv_within_s1$parameters$QT_ns),
                            mean(cv_within_s1$parameters$QT_na),
                            mean(cv_within_s1$parameters$QT_SMRD),
                            soe_mod_ws1, woe_mod_ws1)
pred_between_s2 <- pred_acc(study_long_s2_betw, study_s2_betw,
                            mean(cv_within_s2$parameters$phi),
                            mean(cv_within_s2$parameters$QT_Intercept),
                            mean(cv_within_s2$parameters$QT_ns),
                            mean(cv_within_s2$parameters$QT_na),
                            mean(cv_within_s2$parameters$QT_SMRD),
                            soe_mod_ws2, woe_mod_ws2)



### Rsquared comparison plot ---------------------------------------------------

rsq_dat <- tibble(
  rsqs_within_s1 = cv_within_s1$m_rsq,
  rsqs_within_s2 = cv_within_s2$m_rsq,
  rsqs_between_s1 = pred_between_s1$rsqs,
  rsqs_between_s2 = pred_between_s2$rsqs,
  model = names(cv_within_s1$m_rsq))

rsq_range <- tibble(
  rsqs_within_s1_min = apply(cv_within_s1$rsq_df, 2, min),
  rsqs_within_s2_min = apply(cv_within_s2$rsq_df, 2, min),
  rsqs_within_s1_max = apply(cv_within_s1$rsq_df, 2, max),
  rsqs_within_s2_max = apply(cv_within_s2$rsq_df, 2, max),
  model = names(cv_within_s1$m_rsq))

rsq_dat$model <- c("QT", "EXT", "FIRST", "LAST", "SUM", "VUM", "SoE", "WoE")
rsq_dat <- rsq_dat[c(1, 5, 2, 3, 4, 6, 7, 8), ]
rsq_range$model <- c("QT", "EXT", "FIRST", "LAST", "SUM", "VUM", "SoE", "WoE")
rsq_range <- rsq_range[c(1, 5, 2, 3, 4, 6, 7, 8), ]


# baseline model with demographic variables as predictors
mod_age <- list()
mod_sex <- list()
mod_all <- list()
dat <- list("study_s1" = study_s1 %>%
              mutate(rating_f = factor(rating, levels = 0:10,
                                       ordered = TRUE)),
            "study_s2" = study_s2 %>%
              mutate(rating_f = factor(rating, levels = 0:10,
                                                ordered = TRUE)))

r2_age <- c()
r2_sex <- c()
r2_all <- c()
  
for (i in 1:2) {
  mod_age_temp <- stan_polr(rating_f ~ age,
                            data = dat[[i]], 
                            prior = rstanarm::R2(.25, what = "median"),
                            prior_counts = dirichlet(rep(1, 11)),
                            iter = 5000, chains = 4)
  pred_age_temp <- mod_age_temp %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  r2_age[i] <- cor(pred_age_temp, dat[[i]]$rating, method = "spearman")
  
  mod_sex_temp <- stan_polr(rating_f ~ sex,
                            data = dat[[i]], 
                            prior = rstanarm::R2(.25, what = "median"),
                            prior_counts = dirichlet(rep(1, 11)),
                            iter = 5000, chains = 4)
  
  pred_sex_temp <- mod_sex_temp %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  r2_sex[i] <- cor(pred_sex_temp, dat[[i]]$rating, method = "spearman")
  
  mod_all_temp <- stan_polr(rating_f ~ age + sex + yearsofedu + income + job,
                            data = dat[[i]], 
                            prior = rstanarm::R2(.25, what = "median"),
                            prior_counts = dirichlet(rep(1, 11)),
                            iter = 5000, chains = 4)
  
  pred_all_temp <- mod_all_temp %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  r2_all[i] <- cor(pred_all_temp, dat[[i]]$rating, method = "spearman")
}



r2_age <- mean(r2_age, na.rm = TRUE)
r2_sex <- mean(r2_sex, na.rm = TRUE)
r2_all <- mean(r2_all, na.rm = TRUE)

save(rsq_dat, rsq_range, r2_age, r2_sex, r2_all,
     file = "data/study_1/cross_study_analysis.RData")
load("data/study_1/cross_study_analysis.RData")

### Plot ---
pdf("plots/study_1/rsq_diff_occasions.pdf", height = 5, width = 11)

layout(matrix(c(1, 2), 1), widths = c(1.55, 1))

r2col <- "#c41449"

# barplot with rsquared values ---
par(mar = c(3.5, 4.3, .8, 0.5))

barplot(cbind(rsqs_within_s1, rsqs_within_s2, rsqs_between_s1, rsqs_between_s2) ~
          model, data = rsq_dat,
        beside = TRUE, ylim = c(0, 1), las = 1, cex.names = 1.1,
        axes = FALSE, xlab = "", border = NA,
        col = c("white"))
points(seq(1.5, 36.5, by = 5), rsq_dat$rsqs_within_s1, col = "#440154FF",
       pch = 16, cex = 1.5)
points(seq(1.5, 36.5, by = 5) + 1, rsq_dat$rsqs_within_s2, col = "#39558CFF",
       pch = 16, cex = 1.5)
points(seq(1.5, 36.5, by = 5) + 2, rsq_dat$rsqs_between_s1, col = "#3CBC75FF",
       pch = 16, cex = 1.5)
points(seq(1.5, 36.5, by = 5) + 3, rsq_dat$rsqs_between_s2, col = "#DCE318FF",
       pch = 16, cex = 1.5)
axis(1, c(-1, seq(3, 38, 5), 55), lwd = 2, labels = FALSE)
axis(2, c(seq(0, 1, .2), 1.05), labels = TRUE, lwd = 2, las = 1, cex.axis = 1.1)
mtext("Models", side = 1, line = 3, cex = 1.5, font = 2, padj = -.4)
mtext(expression(italic(r[s])), side = 2, line = 3, cex = 1.5, font = 2, padj = .2)
# add line between initial and regression models
#abline(v = 30.5, lty = 2, col = "gray", lwd = 1.5)
lines(seq(0, 55, 2) + .2, rep(r2_age, length(seq(0, 55, 2))), lwd = 2, lty = 1,
      pch = 16, type = "b", col = r2col)
lines(seq(-1, 55, 2) + .2, rep(r2_sex, length(seq(-1, 55, 2))), lwd = 2, lty = 1,
      pch = 17, type = "b", col = r2col)
lines(seq(-1, 55, 2) + .4, rep(r2_all, length(seq(-1, 55, 2))), lwd = 2, lty = 1,
      pch = 4, type = "b", col = r2col)

# add maximum 
segments(c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))),
         c(rbind(rsq_dat$rsqs_within_s1, rsq_dat$rsqs_within_s2)),
         c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))),
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         lwd = 2, col = rep(c("#440154FF", "#39558CFF"), 6))
segments(c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))) - .1,
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))) + .1,
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         lwd = 2, col = rep(c("#440154FF", "#39558CFF"), 6))

# add minimum 
segments(c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))),
         c(rbind(rsq_dat$rsqs_within_s1, rsq_dat$rsqs_within_s2)),
         c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))),
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         lwd = 2, col = rep(c("#440154FF", "#39558CFF"), 6))
segments(c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))) - .1,
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))) + .1,
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         lwd = 2, col = rep(c("#440154FF", "#39558CFF"), 6))



# barplot legend ---

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0.5, 1))

hadj_l1 <- -.09
vadj_l1 <- -.01

points(rep(.18, 4) + hadj_l1, c(.925 , .88, .835, .79) + vadj_l1, pch = 16,
       col = c("#440154FF", "#39558CFF", "#3CBC75FF", "#DCE318FF"), cex = 2.5)
text(.1 + hadj_l1, .98 + vadj_l1, "Prediction", adj = 0, cex = 1.6)
text(rep(.27, 4) + hadj_l1, c(.925 , .88, .835, .79) + vadj_l1,
     c("Within S1 (Cross-validation)", "Within S2 (Cross-validation)",
       "From S1 to S2 (Prediction)", "From S2 to S1 (Post-diction)"),
     adj = 0, cex = 1.3)

hadj_l2 <- -.09
vadj_l2 <- .3

# reference model legend
lines(c(.17, .19) + hadj_l2, c(.345 , .345) + vadj_l2, lwd = 1.75, lty = 1,
      pch = 1, type = "l", col = r2col)
points(c(.15, .21) + hadj_l2, c(.345, .345) + vadj_l2, pch = 16,
       col = r2col, cex = 1.25)

lines(c(.17, .19) + hadj_l2, c(.3 , .3) + vadj_l2, lwd = 1.75, lty = 1,
      pch = 1, type = "l", col = r2col)
points(c(.15, .21) + hadj_l2, c(.3, .3) + vadj_l2, pch = 17,
       col = r2col, cex = 1.25)

lines(c(.17, .19) + hadj_l2, c(.255, .255) + vadj_l2, lwd = 1.75, lty = 1,
      pch = 1, type = "l", col = r2col)
points(c(.15, .21) + hadj_l2, c(.255, .255) + vadj_l2, pch = 4,
       col = r2col, cex = 1.25, lwd = 1.75)

text(.1 + hadj_l2, .4 + vadj_l2, "Reference Models", adj = 0, cex = 1.6)
text(rep(.27, 4) + hadj_l2, c(.345, .3, .2425) + vadj_l2,
     c("Age", "Sex", "Five sociodemographic\npredictors"), adj = 0, cex = 1.3)

dev.off()


### Robustness check only with participants who listed more than one aspect ====


# select only participants who participated in both studies and add ratings of s1 to s2 and vice versa


study_long_s1_betw <- study_long_s1 %>%
  filter(partid %in% unique(study_long_s2$partid) & n_aspects > 1) %>%
  select(-rating) %>%
  left_join(ratings_s2, by = "partid") %>%
  rename(rating = rating_s2)
study_s1_betw <- study_s1 %>%
  filter(partid %in% unique(study_long_s2$partid) & n_aspects > 1) %>%
  select(-rating) %>%
  left_join(ratings_s2, by = "partid") %>%
  rename(rating = rating_s2)

study_long_s2_betw <- study_long_s2 %>%
  filter(n_aspects > 1) %>% 
  select(-rating) %>%
  left_join(ratings_s1, by = "partid") %>%
  rename(rating = rating_s1)
study_s2_betw <- study_s2 %>%
  filter(n_aspects > 1) %>% 
  select(-rating) %>%
  left_join(ratings_s1, by = "partid") %>%
  rename(rating = rating_s1)


### RQ1: Modeling self-reported risk-taking propensity =========================

### Cross validation using five folds predicting ratings of s2 with aspects of s1

# set seed for replicability of results
set.seed(1)

# test within occasions
# test within occasions
cv_within_s1 <- cross_val2(study_long_s1 %>% filter(n_aspects > 1),
                           study_s1 %>% filter(n_aspects > 1))
cv_within_s2 <- cross_val2(study_long_s2 %>% filter(n_aspects > 1),
                           study_s2 %>% filter(n_aspects > 1))

# use these parameters to get predictions across occasions

soe_mod_ws1 <- stan_polr(rating_f ~ m_risk,
                         data = study_s1 %>%
                           mutate(rating_f = factor(rating, levels = 0:10,
                                                    ordered = TRUE)) %>% 
                           filter(n_aspects > 1), 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 5000, chains = 4)
soe_mod_ws2 <- stan_polr(rating_f ~ m_risk,
                         data = study_s2 %>%
                           mutate(rating_f = factor(rating, levels = 0:10,
                                                    ordered = TRUE)) %>% 
                           filter(n_aspects > 1), 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 5000, chains = 4)
woe_mod_ws1 <- stan_polr(rating_f ~ m_aspects,
                         data = study_s1 %>%
                           mutate(rating_f = factor(rating, levels = 0:10,
                                                    ordered = TRUE)) %>% 
                           filter(n_aspects > 1), 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 5000, chains = 4)
woe_mod_ws2 <- stan_polr(rating_f ~ m_aspects,
                         data = study_s2 %>%
                           mutate(rating_f = factor(rating, levels = 0:10,
                                                    ordered = TRUE)) %>% 
                           filter(n_aspects > 1), 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 5000, chains = 4)



pred_between_s1 <- pred_acc(study_long_s1_betw %>% filter(n_aspects > 1),
                            study_s1_betw %>% filter(n_aspects > 1),
                            mean(cv_within_s1$parameters$phi),
                            mean(cv_within_s1$parameters$QT_Intercept),
                            mean(cv_within_s1$parameters$QT_ns),
                            mean(cv_within_s1$parameters$QT_na),
                            mean(cv_within_s1$parameters$QT_SMRD),
                            soe_mod_ws1, woe_mod_ws1)
pred_between_s2 <- pred_acc(study_long_s2_betw %>% filter(n_aspects > 1),
                            study_s2_betw %>% filter(n_aspects > 1),
                            mean(cv_within_s2$parameters$phi),
                            mean(cv_within_s2$parameters$QT_Intercept),
                            mean(cv_within_s2$parameters$QT_ns),
                            mean(cv_within_s2$parameters$QT_na),
                            mean(cv_within_s2$parameters$QT_SMRD),
                            soe_mod_ws2, woe_mod_ws2)



### Rsquared comparison plot ---------------------------------------------------

rsq_dat <- tibble(
  rsqs_within_s1 = cv_within_s1$m_rsq,
  rsqs_within_s2 = cv_within_s2$m_rsq,
  rsqs_between_s1 = pred_between_s1$rsqs,
  rsqs_between_s2 = pred_between_s2$rsqs,
  model = names(cv_within_s1$m_rsq))

rsq_range <- tibble(
  rsqs_within_s1_min = apply(cv_within_s1$rsq_df, 2, min),
  rsqs_within_s2_min = apply(cv_within_s2$rsq_df, 2, min),
  rsqs_within_s1_max = apply(cv_within_s1$rsq_df, 2, max),
  rsqs_within_s2_max = apply(cv_within_s2$rsq_df, 2, max),
  model = names(cv_within_s1$m_rsq))

rsq_dat$model <- c("QT", "EXT", "FIRST", "LAST", "SUM", "VUM", "SoE", "WoE")
rsq_dat <- rsq_dat[c(1, 5, 2, 3, 4, 6, 7, 8), ]
rsq_range$model <- c("QT", "EXT", "FIRST", "LAST", "SUM", "VUM", "SoE", "WoE")
rsq_range <- rsq_range[c(1, 5, 2, 3, 4, 6, 7, 8), ]


# baseline model with demographic variables as predictors
mod_age <- list()
mod_sex <- list()
mod_all <- list()
dat <- list("study_s1" = study_s1 %>%
              mutate(rating_f = factor(rating, levels = 0:10,
                                       ordered = TRUE)) %>% 
              filter(n_aspects > 1),
            "study_s2" = study_s2 %>%
              mutate(rating_f = factor(rating, levels = 0:10,
                                       ordered = TRUE)) %>% 
              filter(n_aspects > 1))

r2_age <- c()
r2_sex <- c()
r2_all <- c()

for (i in 1:2) {
  mod_age_temp <- stan_polr(rating_f ~ age,
                            data = dat[[i]], 
                            prior = rstanarm::R2(.25, what = "median"),
                            prior_counts = dirichlet(rep(1, 11)),
                            iter = 5000, chains = 4)
  pred_age_temp <- mod_age_temp %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  r2_age[i] <- cor(pred_age_temp, dat[[i]]$rating, method = "spearman")
  
  mod_sex_temp <- stan_polr(rating_f ~ sex,
                            data = dat[[i]], 
                            prior = rstanarm::R2(.25, what = "median"),
                            prior_counts = dirichlet(rep(1, 11)),
                            iter = 5000, chains = 4)
  
  pred_sex_temp <- mod_sex_temp %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  r2_sex[i] <- cor(pred_sex_temp, dat[[i]]$rating, method = "spearman")
  
  mod_all_temp <- stan_polr(rating_f ~ age + sex + yearsofedu + income + job,
                            data = dat[[i]], 
                            prior = rstanarm::R2(.25, what = "median"),
                            prior_counts = dirichlet(rep(1, 11)),
                            iter = 5000, chains = 4)
  
  pred_all_temp <- mod_all_temp %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  r2_all[i] <- cor(pred_all_temp, dat[[i]]$rating, method = "spearman")
}



r2_age <- mean(r2_age, na.rm = TRUE)
r2_sex <- mean(r2_sex, na.rm = TRUE)
r2_all <- mean(r2_all, na.rm = TRUE)

save(rsq_dat, rsq_range, r2_age, r2_sex, r2_all,
     file = "data/study_1/cross_study_analysis_mult_asp.RData")
load("data/study_1/cross_study_analysis_mult_asp.RData")
### Plot ---
pdf("plots/study_1/rsq_diff_occasions_mult_asp.pdf", height = 5, width = 11)

layout(matrix(c(1, 2), 1), widths = c(1.55, 1))

r2col <- "#c41449"

# barplot with rsquared values ---
par(mar = c(3.5, 4.3, .8, 0.5))

barplot(cbind(rsqs_within_s1, rsqs_within_s2, rsqs_between_s1, rsqs_between_s2) ~
          model, data = rsq_dat,
        beside = TRUE, ylim = c(0, 1), las = 1, cex.names = 1.1,
        axes = FALSE, xlab = "", border = NA,
        col = c("white"))
points(seq(1.5, 36.5, by = 5), rsq_dat$rsqs_within_s1, col = "#440154FF",
       pch = 16, cex = 1.5)
points(seq(1.5, 36.5, by = 5) + 1, rsq_dat$rsqs_within_s2, col = "#39558CFF",
       pch = 16, cex = 1.5)
points(seq(1.5, 36.5, by = 5) + 2, rsq_dat$rsqs_between_s1, col = "#3CBC75FF",
       pch = 16, cex = 1.5)
points(seq(1.5, 36.5, by = 5) + 3, rsq_dat$rsqs_between_s2, col = "#DCE318FF",
       pch = 16, cex = 1.5)
axis(1, c(-1, seq(3, 38, 5), 55), lwd = 2, labels = FALSE)
axis(2, c(seq(0, 1, .2), 1.05), labels = TRUE, lwd = 2, las = 1, cex.axis = 1.1)
mtext("Models", side = 1, line = 3, cex = 1.5, font = 2, padj = -.4)
mtext(expression(italic(r[s])), side = 2, line = 3, cex = 1.5, font = 2, padj = .2)
# add line between initial and regression models
#abline(v = 30.5, lty = 2, col = "gray", lwd = 1.5)
lines(seq(0, 55, 2) + .2, rep(r2_age, length(seq(0, 55, 2))), lwd = 2, lty = 1,
      pch = 16, type = "b", col = r2col)
lines(seq(-1, 55, 2) + .2, rep(r2_sex, length(seq(-1, 55, 2))), lwd = 2, lty = 1,
      pch = 17, type = "b", col = r2col)
lines(seq(-1, 55, 2) + .4, rep(r2_all, length(seq(-1, 55, 2))), lwd = 2, lty = 1,
      pch = 4, type = "b", col = r2col)

# add maximum 
segments(c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))),
         c(rbind(rsq_dat$rsqs_within_s1, rsq_dat$rsqs_within_s2)),
         c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))),
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         lwd = 2, col = rep(c("#440154FF", "#39558CFF"), 6))
segments(c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))) - .1,
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))) + .1,
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         lwd = 2, col = rep(c("#440154FF", "#39558CFF"), 6))

# add minimum 
segments(c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))),
         c(rbind(rsq_dat$rsqs_within_s1, rsq_dat$rsqs_within_s2)),
         c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))),
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         lwd = 2, col = rep(c("#440154FF", "#39558CFF"), 6))
segments(c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))) - .1,
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         c(rbind(1.5 + seq(0, 35, 5), 2.5 + seq(0, 35, 5))) + .1,
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         lwd = 2, col = rep(c("#440154FF", "#39558CFF"), 6))



# barplot legend ---

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0.5, 1))

hadj_l1 <- -.09
vadj_l1 <- -.01

points(rep(.18, 4) + hadj_l1, c(.925 , .88, .835, .79) + vadj_l1, pch = 16,
       col = c("#440154FF", "#39558CFF", "#3CBC75FF", "#DCE318FF"), cex = 2.5)
text(.1 + hadj_l1, .98 + vadj_l1, "Prediction", adj = 0, cex = 1.6)
text(rep(.27, 4) + hadj_l1, c(.925 , .88, .835, .79) + vadj_l1,
     c("Within S1 (Cross-validation)", "Within S2 (Cross-validation)",
       "From S1 to S2 (Prediction)", "From S2 to S1 (Post-diction)"),
     adj = 0, cex = 1.3)

hadj_l2 <- -.09
vadj_l2 <- .3

# reference model legend
lines(c(.17, .19) + hadj_l2, c(.345 , .345) + vadj_l2, lwd = 1.75, lty = 1,
      pch = 1, type = "l", col = r2col)
points(c(.15, .21) + hadj_l2, c(.345, .345) + vadj_l2, pch = 16,
       col = r2col, cex = 1.25)

lines(c(.17, .19) + hadj_l2, c(.3 , .3) + vadj_l2, lwd = 1.75, lty = 1,
      pch = 1, type = "l", col = r2col)
points(c(.15, .21) + hadj_l2, c(.3, .3) + vadj_l2, pch = 17,
       col = r2col, cex = 1.25)

lines(c(.17, .19) + hadj_l2, c(.255, .255) + vadj_l2, lwd = 1.75, lty = 1,
      pch = 1, type = "l", col = r2col)
points(c(.15, .21) + hadj_l2, c(.255, .255) + vadj_l2, pch = 4,
       col = r2col, cex = 1.25, lwd = 1.75)

text(.1 + hadj_l2, .4 + vadj_l2, "Reference Models", adj = 0, cex = 1.6)
text(rep(.27, 4) + hadj_l2, c(.345, .3, .2425) + vadj_l2,
     c("Age", "Sex", "Five sociodemographic\npredictors"), adj = 0, cex = 1.3)
dev.off()



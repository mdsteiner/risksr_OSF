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
library(brms)
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
cv_within_s1 <- cross_val(study_long_s1, study_s1)
cv_within_s2 <- cross_val(study_long_s2, study_s2)

# use these parameters to get predictions across occasions
pred_between_s1 <- pred_acc(study_long_s1_betw, study_s1_betw,
                            mean(cv_within_s1$parameters$phi),
                            mean(cv_within_s1$parameters$QT_Intercept),
                            mean(cv_within_s1$parameters$QT_ns),
                            mean(cv_within_s1$parameters$QT_na),
                            mean(cv_within_s1$parameters$QT_SMRD))
pred_between_s2 <- pred_acc(study_long_s2_betw, study_s2_betw,
                            mean(cv_within_s2$parameters$phi),
                            mean(cv_within_s2$parameters$QT_Intercept),
                            mean(cv_within_s2$parameters$QT_ns),
                            mean(cv_within_s2$parameters$QT_na),
                            mean(cv_within_s2$parameters$QT_SMRD))



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

rsq_dat$model[rsq_dat$model == "EXTREME"] <- "EXT"
rsq_dat <- rsq_dat[c(1, 5, 2, 3, 4, 6), ]
rsq_range$model[rsq_range$model == "EXTREME"] <- "EXT"
rsq_range <- rsq_range[c(1, 5, 2, 3, 4, 6), ]


# baseline model with demographic variables as predictors
mod_age <- list()
mod_sex <- list()
mod_all <- list()
dat <- list(study_s1, study_s2)

for (i in 1:2) {
  mod_age[[i]] <- stan_glm(rating ~ age, data = dat[[i]],
                      chains = 4, iter = 8000, family = gaussian(),
                      prior_intercept = normal(location = 0, scale = 10),
                      prior = normal(location = 0, scale = 2.5))
  mod_sex[[i]] <- stan_glm(rating ~ sex, data = dat[[i]],
                      chains = 4, iter = 8000, family = gaussian(),
                      prior_intercept = normal(location = 0, scale = 10),
                      prior = normal(location = 0, scale = 2.5))
  mod_all[[i]] <- stan_glm(rating ~ age + sex + yearsofedu + income + job, data = dat[[i]],
                      chains = 4, iter = 8000, family = gaussian(),
                      prior_intercept = normal(location = 0, scale = 10),
                      prior = normal(location = 0, scale = 2.5))
}


r2_age <- mean(sapply(mod_age, function(x) median(bayes_R2(x))))
r2_sex <- mean(sapply(mod_sex, function(x) median(bayes_R2(x))))
r2_all <- mean(sapply(mod_all, function(x) median(bayes_R2(x))))

### Plot ---
pdf("plots/study_1/rsq_diff_occasions.pdf", height = 5, width = 9)

layout(matrix(c(1, 2), 1), widths = c(1.4, 1))

r2col <- "#c41449"

# barplot with rsquared values ---
par(mar = c(3.5, 4.3, .8, 0.5))
barplot(cbind(rsqs_within_s1, rsqs_within_s2, rsqs_between_s1, rsqs_between_s2) ~
          model, data = rsq_dat,
        beside = TRUE, ylim = c(0, 1), las = 1, cex.names = 1.1,
        axes = FALSE, xlab = "", border = NA,
        col = c("#440154FF", "#39558CFF", "#3CBC75FF", "#DCE318FF"))
axis(1, c(-1, seq(3, 28, 5), 35), lwd = 2, labels = FALSE)
axis(2, c(seq(0, 1, .2), 1.05), labels = TRUE, lwd = 2, las = 1, cex.axis = 1.1)
mtext("Models", side = 1, line = 3, cex = 1.5, font = 2, padj = -.4)
mtext(expression(R^2), side = 2, line = 3, cex = 1.5, font = 2, padj = .2)
lines(seq(0, 35, 2) + .2, rep(r2_age, length(seq(0, 35, 2))), lwd = 2, lty = 1,
      pch = 16, type = "b", col = r2col)
lines(seq(-1, 35, 2) + .2, rep(r2_sex, length(seq(-1, 35, 2))), lwd = 2, lty = 1,
      pch = 17, type = "b", col = r2col)
lines(seq(-1, 35, 2) + .4, rep(r2_all, length(seq(-1, 35, 2))), lwd = 2, lty = 1,
      pch = 4, type = "b", col = r2col)

# add maximum 
segments(c(rbind(1.5 + seq(0, 25, 5), 2.5 + seq(0, 25, 5))),
         c(rbind(rsq_dat$rsqs_within_s1, rsq_dat$rsqs_within_s2)),
         c(rbind(1.5 + seq(0, 25, 5), 2.5 + seq(0, 25, 5))),
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         lwd = 2)
segments(c(rbind(1.5 + seq(0, 25, 5), 2.5 + seq(0, 25, 5))) - .1,
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         c(rbind(1.5 + seq(0, 25, 5), 2.5 + seq(0, 25, 5))) + .1,
         c(rbind(rsq_range$rsqs_within_s1_max, rsq_range$rsqs_within_s2_max)),
         lwd = 2)

# add minimum 
segments(c(rbind(1.5 + seq(0, 25, 5), 2.5 + seq(0, 25, 5))),
         c(rbind(rsq_dat$rsqs_within_s1, rsq_dat$rsqs_within_s2)),
         c(rbind(1.5 + seq(0, 25, 5), 2.5 + seq(0, 25, 5))),
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         lwd = 2, col = "white")
segments(c(rbind(1.5 + seq(0, 25, 5), 2.5 + seq(0, 25, 5))) - .1,
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         c(rbind(1.5 + seq(0, 25, 5), 2.5 + seq(0, 25, 5))) + .1,
         c(rbind(rsq_range$rsqs_within_s1_min, rsq_range$rsqs_within_s2_min)),
         lwd = 2, col = "white")



# barplot legend ---

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0.5, 1))

hadj_l1 <- -.09
vadj_l1 <- -.01

points(rep(.18, 4) + hadj_l1, c(.925 , .88, .835, .79) + vadj_l1, pch = 15,
       col = c("#440154FF", "#39558CFF", "#3CBC75FF", "#DCE318FF"), cex = 3)
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


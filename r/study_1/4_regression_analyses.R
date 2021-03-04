# Anlysis script Study 1 of "Mapping the Cognitive 
library(tidyverse)
library(data.table)
library(rstanarm)
library(patchwork)
library(ggrepel)

### Load Data ==================================================================

study_s1 <- read_csv("data/study_1/study.csv") %>% 
  mutate(rating_f = factor(rating, levels = 0:10, ordered = TRUE))
study_s2 <- read_csv("data/study_2/study.csv") %>% 
  mutate(rating_f = factor(rating, levels = 0:10, ordered = TRUE))
s1_to_s2 <- study_s1 %>% 
  select(partid, m_risk, s_risk, m_aspects, s_aspects, age, sex, yearsofedu,
         job, income) %>% 
  right_join(study_s2 %>% select(partid, rating, rating_f), by = "partid")
s2_to_s1 <- study_s2 %>% 
  select(partid, m_risk, m_aspects, s_aspects, s_risk, age, sex, yearsofedu,
         job, income) %>% 
  left_join(study_s1 %>% select(partid, rating, rating_f), by = "partid")

### Functions ==================================================================
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)][1]
}

c_kappa <- function(x) (x - (1/11)) / (1 - (1/11))

get_same_preds <- function(x) {
  
  out <- matrix(NA, nrow = ncol(x), ncol = ncol(x))
  
  for (col_i in 1:ncol(out)) {
    for (row_i in 1:ncol(out)) {
      out[row_i, col_i] <- mean(x[, row_i] == x[, col_i])
    }
  }
  
  colnames(out) <- colnames(x)
  rownames(out) <- colnames(x)
  
  out
}

get_same_preds_cor <- function(x, crit, corr = TRUE) {
  
  out <- matrix(NA, nrow = ncol(x), ncol = ncol(x))
  
  if (isTRUE(corr)) {
    for (col_i in 1:ncol(out)) {
      for (row_i in 1:ncol(out)) {
        out[row_i, col_i] <- mean(x[, row_i] == x[, col_i] &
                                    x[, row_i] == crit)
      }
    }
  } else {
    for (col_i in 1:ncol(out)) {
      for (row_i in 1:ncol(out)) {
        out[row_i, col_i] <- mean(x[, row_i] == x[, col_i] &
                                    x[, row_i] != crit)
      }
    }
  }
  
  colnames(out) <- colnames(x)
  rownames(out) <- colnames(x)
  
  out
}

eval_fit <- function(WoE_mod, SoE_mod, age_mod, sex_mod, demo_mod, dat,
                     plot = TRUE){
  
  
  WoE_fit <- WoE_mod %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  Soe_fit <- SoE_mod %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  
  age_fit <- age_mod %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  sex_fit <- sex_mod %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  demo_fit <- demo_mod %>% 
    posterior_predict() %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  if (isTRUE(plot)) {
    p1 <- tibble(WoE = WoE_fit,
                 criterion = dat$rating_f) %>% 
      ggplot(aes(criterion, WoE)) +
      geom_violin() +
      stat_summary(fun=median, geom="point", size=1.5, color="red") + 
      coord_cartesian(ylim = c(0, 10)) +
      theme_light()
    
    p2 <- tibble(SoE = Soe_fit,
                 criterion = dat$rating_f) %>% 
      ggplot(aes(criterion, SoE)) +
      geom_violin() +
      stat_summary(fun=median, geom="point", size=1.5, color="red") + 
      coord_cartesian(ylim = c(0, 10)) +
      theme_light()
    
    p1 + p2
  }
  
  list(
    # overall same predictions
    same_pred = get_same_preds(data.frame("SoE" = Soe_fit,
                                          "WoE" = WoE_fit,
                                          "age" = age_fit,
                                          "sex" = sex_fit,
                                          "all" = demo_fit)),
    # of same predictions, how many are correct
    same_pred_cor = get_same_preds_cor(data.frame("SoE" = Soe_fit,
                                                  "WoE" = WoE_fit,
                                                  "age" = age_fit,
                                                  "sex" = sex_fit,
                                                  "all" = demo_fit),
                                       dat$rating),
    # of same predictions, how many are incorrect
    same_pred_incor = get_same_preds_cor(data.frame("SoE" = Soe_fit,
                                                    "WoE" = WoE_fit,
                                                    "age" = age_fit,
                                                    "sex" = sex_fit,
                                                    "all" = demo_fit),
                                         dat$rating, FALSE),
    # overall accuracy
    acc = c(WoE = mean(WoE_fit == dat$rating),
            SoE = mean(Soe_fit == dat$rating),
            age = mean(age_fit == dat$rating),
            sex = mean(sex_fit == dat$rating),
            demo = mean(demo_fit == dat$rating)),
    # chance corrected accuracy
    acc_cor = c(WoE = c_kappa(mean(WoE_fit == dat$rating)),
                SoE = c_kappa(mean(Soe_fit == dat$rating)),
                age = c_kappa(mean(age_fit == dat$rating)),
                sex = c_kappa(mean(sex_fit == dat$rating)),
                demo = c_kappa(mean(demo_fit == dat$rating))),
    # n distinct predictions:
    n_distinct = c(WoE = length(unique(WoE_fit)),
                   SoE = length(unique(Soe_fit)),
                   age = length(unique(age_fit)),
                   sex = length(unique(sex_fit)),
                   demo = length(unique(demo_fit))),
    loo_comp = loo_compare(loo(WoE_mod), loo(SoE_mod), loo(age_mod),
                           loo(sex_mod), loo(demo_mod))
    
  )
  
} 


eval_pred <- function(WoE_mod, SoE_mod, age_mod, sex_mod, demo_mod, dat) {
  
  WoE_pred <- WoE_mod %>% 
    posterior_predict(newdata = dat) %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  Soe_pred <- SoE_mod %>% 
    posterior_predict(newdata = dat) %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  
  age_pred <- age_mod %>% 
    posterior_predict(newdata = dat) %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  sex_pred <- sex_mod %>% 
    posterior_predict(newdata = dat) %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  demo_pred <- demo_mod %>% 
    posterior_predict(newdata = dat) %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  
  list(
    # overall same predictions
    same_pred = get_same_preds(data.frame("SoE" = Soe_pred,
                                          "WoE" = WoE_pred,
                                          "age" = age_pred,
                                          "sex" = sex_pred,
                                          "all" = demo_pred)),
    # of same predictions, how many are correct
    same_pred_cor = get_same_preds_cor(data.frame("SoE" = Soe_pred,
                                                  "WoE" = WoE_pred,
                                                  "age" = age_pred,
                                                  "sex" = sex_pred,
                                                  "all" = demo_pred),
                                       dat$rating),
    # of same predictions, how many are incorrect
    same_pred_incor = get_same_preds_cor(data.frame("SoE" = Soe_pred,
                                                    "WoE" = WoE_pred,
                                                    "age" = age_pred,
                                                    "sex" = sex_pred,
                                                    "all" = demo_pred),
                                         dat$rating, FALSE),
    # overall accuracy
    acc = c(WoE = mean(WoE_pred == dat$rating),
            SoE = mean(Soe_pred == dat$rating),
            age = mean(age_pred == dat$rating),
            sex = mean(sex_pred == dat$rating),
            demo = mean(demo_pred == dat$rating)),
    # chance corrected accuracy
    acc_cor = c(WoE = c_kappa(mean(WoE_pred == dat$rating)),
                SoE = c_kappa(mean(Soe_pred == dat$rating)),
                age = c_kappa(mean(age_pred == dat$rating)),
                sex = c_kappa(mean(sex_pred == dat$rating)),
                demo = c_kappa(mean(demo_pred == dat$rating))),
    # n distinct predictions:
    n_distinct = c(WoE = length(unique(WoE_pred)),
                   SoE = length(unique(Soe_pred)),
                   age = length(unique(age_pred)),
                   sex = length(unique(sex_pred)),
                   demo = length(unique(demo_pred)))
  )
}

create_mds <- function(dat, lbls, xlim = c(-.4, .4), ylim = c(-.2, .2),
                       col_vec = NULL, pal = "black") {

  mds_cor <- (1 - dat) %>%
    cmdscale() %>%
    as_tibble() %>% 
    mutate(Categories = col_vec)
  colnames(mds_cor)[1:2] <- c("d1", "d2")
  
  p <- ggplot(mds_cor, aes(d1, d2)) +
    geom_point() +
    geom_label_repel(label = lbls) + 
    labs(x = "", y = "") +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    ggpubr::theme_pubr() +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16,face = "bold")
    )
  
  p
}

### Analyses based on the mean aspects and mean strengths ================

### Study 1 to Study 2 --------------------------------------------------
# fit WoE mod
WoE_mod_s1 <- stan_polr(rating_f ~ m_aspects,
                        data = study_s1, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)

# fit SoE mod
SoE_mod_s1 <- stan_polr(rating_f ~ m_risk,
                        data = study_s1, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)

# fit age mod
age_mod_s1 <- stan_polr(rating_f ~ age,
                        data = study_s1, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)

# fit sex mod
sex_mod_s1 <- stan_polr(rating_f ~ sex,
                        data = study_s1, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)

# fit demo mod
demo_mod_s1 <- stan_polr(rating_f ~ age + sex + yearsofedu + income + job,
                        data = study_s1, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)


### evaluate the fitting performance and similarities of models
res_fit_s1 <- eval_fit(WoE_mod_s1, SoE_mod_s1, age_mod_s1, sex_mod_s1,
                       demo_mod_s1, study_s1)

### evaluate the prediction performance and similarities of models
res_pred_s2 <- eval_pred(WoE_mod_s1, SoE_mod_s1, age_mod_s1, sex_mod_s1,
                         demo_mod_s1, study_s2)

### evaluate the prediction performance and similarities of models
res_pred_s1_to_s2 <- eval_pred(WoE_mod_s1, SoE_mod_s1, age_mod_s1, sex_mod_s1,
                               demo_mod_s1, s1_to_s2)

### Study 2 to Study 1 --------------------------------------------------
# fit WoE mod
WoE_mod_s2 <- stan_polr(rating_f ~ m_aspects,
                        data = study_s2, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)

# fit SoE mod
SoE_mod_s2 <- stan_polr(rating_f ~ m_risk,
                        data = study_s2, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)

# fit age mod
age_mod_s2 <- stan_polr(rating_f ~ age,
                        data = study_s2, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)

# fit sex mod
sex_mod_s2 <- stan_polr(rating_f ~ sex,
                        data = study_s2, 
                        prior = R2(.25, what = "median"),
                        prior_counts = dirichlet(rep(1, 11)),
                        iter = 10000, chains = 4)

# fit demo mod
demo_mod_s2 <- stan_polr(rating_f ~ age + sex + yearsofedu + income + job,
                         data = study_s2, 
                         prior = R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 10000, chains = 4)

### evaluate the fitting performance and similarities of models
res_fit_s2 <- eval_fit(WoE_mod_s2, SoE_mod_s2, age_mod_s2, sex_mod_s2,
                       demo_mod_s2, study_s2)

### evaluate the prediction performance and similarities of models
res_pred_s1 <- eval_pred(WoE_mod_s2, SoE_mod_s2, age_mod_s2, sex_mod_s2,
                         demo_mod_s2, study_s1)

### evaluate the prediction performance and similarities of models
res_pred_s2_to_s1 <- eval_pred(WoE_mod_s2, SoE_mod_s2, age_mod_s2, sex_mod_s2,
                               demo_mod_s2, s2_to_s1)

### save everything ============================================================

save(res_fit_s1, res_fit_s2, res_pred_s1, res_pred_s2, res_pred_s1_to_s2,
     res_pred_s2_to_s1, file = "data/study_1/res_regressions.RData")

### analyse results ============================================================

load("data/study_1/res_regressions.RData")

# fit s1
res_fit_s1
res_fit_s2


tab_s1 <- matrix("-", nrow = 5, ncol = 5)
tab_s1[lower.tri(tab_s1)] <- round(res_fit_s1$same_pred, 2)[lower.tri(res_fit_s1$same_pred)]
tab_s1[upper.tri(tab_s1)] <- round(res_fit_s1$same_pred_cor, 2)[upper.tri(res_fit_s1$same_pred_cor)]

tab_s1 <- cbind(c("SoE", "WoE", "Age", "Sex", "5 soc. dem. pred."),
                tab_s1)
cat(paste(apply(tab_s1, 1, paste, collapse = " & "),
          collapse = "\\\\ \n"))


tab_s2 <- matrix("-", nrow = 5, ncol = 5)
tab_s2[lower.tri(tab_s2)] <- round(res_fit_s2$same_pred, 2)[lower.tri(res_fit_s2$same_pred)]
tab_s2[upper.tri(tab_s2)] <- round(res_fit_s2$same_pred_cor, 2)[upper.tri(res_fit_s2$same_pred_cor)]

tab_s2 <- cbind(c("SoE", "WoE", "Age", "Sex", "5 soc. dem. pred."),
                tab_s2)
cat(paste(apply(tab_s2, 1, paste, collapse = " & "),
          collapse = "\\\\ \n"))


# predictions of s1 based on fit of s2
res_pred_s1
# predictions of s2 based on fit of s1
res_pred_s2

# cross_study analyses

# predictions from s1 to s2 base on fit of s1
res_pred_s1_to_s2
# predictions from s2 to s1 base on fit of s2
res_pred_s2_to_s1

tab_cross_s1 <- matrix("-", nrow = 5, ncol = 5)
tab_cross_s1[lower.tri(tab_cross_s1)] <- round(res_pred_s1_to_s2$same_pred, 2)[lower.tri(res_pred_s1_to_s2$same_pred)]
tab_cross_s1[upper.tri(tab_cross_s1)] <- round(res_pred_s1_to_s2$same_pred_cor, 2)[upper.tri(res_pred_s1_to_s2$same_pred_cor)]

tab_cross_s1 <- cbind(c("SoE", "WoE", "Age", "Sex", "5 soc. dem. pred."),
                      tab_cross_s1)
cat(paste(apply(tab_cross_s1, 1, paste, collapse = " & "),
          collapse = "\\\\ \n"))


tab_cross_s2 <- matrix("-", nrow = 5, ncol = 5)
tab_cross_s2[lower.tri(tab_cross_s2)] <- round(res_pred_s2_to_s1$same_pred, 2)[lower.tri(res_pred_s2_to_s1$same_pred)]
tab_cross_s2[upper.tri(tab_cross_s2)] <- round(res_pred_s2_to_s1$same_pred_cor, 2)[upper.tri(res_pred_s2_to_s1$same_pred_cor)]

tab_cross_s2 <- cbind(c("SoE", "WoE", "Age", "Sex", "5 soc. dem. pred."),
                      tab_cross_s2)
cat(paste(apply(tab_cross_s2, 1, paste, collapse = " & "),
          collapse = "\\\\ \n"))

# mds plot
pdf("plots/study_1/mds_ord_models.pdf", width = 13, height = 4)
p1 <- create_mds(res_fit_s1$same_pred,
           c("SoE", "WoE", "Age", "Sex", "5 soc. dem. pred.")) 
p2 <- create_mds(res_pred_s1_to_s2$same_pred,
             c("SoE", "WoE", "Age", "Sex", "5 soc. dem. pred.")) 
p3 <- create_mds(res_pred_s2_to_s1$same_pred,
             c("SoE", "WoE", "Age", "Sex", "5 soc. dem. pred."))
p1 + p2 + p3 +
  plot_annotation(tag_levels =  "a", tag_suffix = ")")
dev.off()


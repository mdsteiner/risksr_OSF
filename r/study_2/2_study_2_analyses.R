# Analysis script for study 2 - Aspect and Evidence Stability, and Risk Preferences
library(tidyverse)
library(BayesianFirstAid)
library(patchwork)
library(DescTools)
library(rstanarm)
library(bayesplot)
options(mc.cores = parallel::detectCores())

# load data
study_joint <- read_csv("data/study_2/study_joint.csv")
sim_dat <- readRDS("data/similarities/similarity_ratings.RDS")
study_long_s1 <- read_csv("data/study_1/study_long.csv")
study_long_s2 <- read_csv("data/study_2/study_long.csv")

# Stability ====================================================================

# test-retest correlation of risk preference ratings of study 1 and 2
cor(study_joint$rating_s1, study_joint$rating_s2, method = "spearman")

# test-retest correlation of average strength of evidence of study 1 and 2
cor(study_joint$m_risk_s1, study_joint$m_risk_s2, method = "spearman")

# correlation of number of aspects listed in study 1 and 2
cor(study_joint$n_aspects_s1, study_joint$n_aspects_s2, method = "spearman")

# compare distributions of the ratings of the two studies
p1 <- study_joint %>%
  ggplot(aes(factor(rating_s1))) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2", stat = "count") +
  labs(x = "Risk Preference",
       y = "Frequency",
       subtitle = "Study 1") +
  theme_bw()+
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 16,face = "bold", hjust = 1.3),
    axis.title.y = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey"),
    plot.subtitle = element_text(size = 14)
  )

p2 <- study_joint %>%
  ggplot(aes(factor(rating_s2))) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2", stat = "count") +
  labs(x = "",
       y = "",
       subtitle = "Study 2") +
  theme_bw()+
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey"),
    plot.subtitle = element_text(size = 14)
  )

pdf("plots/study_2/SOEP_distribution_comparison.pdf",
       width = 12, height = 6)
p1 + p2
dev.off() 

# RQ 3a: Gauging aspect stability ==============================================

# compute kendall's coefficient of concordance for the similarity ratings
cod <- c()
cod_list <- list()
cod_mat <- matrix(NA, nrow = 21, ncol = 3)
mdn_ratings <- c()
mean_up <- c()

n_aspects <- 4286

# loop through the 21 groups and compute Kendall's W
for (i in 1:21) {
  ind_low <- ceiling(n_aspects / 21 * (i - 1)) + 1
  ind_upp <- ceiling(n_aspects / 21 * i)
  
   temp_dat <- sim_dat %>%
    select(starts_with("rating_")) %>%
    slice(ind_low:ind_upp)
   
  temp_dat <- temp_dat[,-which(is.na(temp_dat[1,]))]
   
  # For the triplets of raters
  cod_list[[i]] <- KendallW(temp_dat, correct = TRUE, test = TRUE)
  cod[i] <- cod_list[[i]]$estimate
  
  # For every combination of the two raters; this is used for a robustness check
  rat_comb <- matrix(c(1, 2, 1, 3, 2, 3), ncol = 2, byrow = TRUE)
  cod_mat[i, 1] <- KendallW(temp_dat[, rat_comb[1,]], correct = TRUE,
                            test = TRUE)$estimate
  cod_mat[i, 2] <- KendallW(temp_dat[, rat_comb[2,]], correct = TRUE,
                            test = TRUE)$estimate
  cod_mat[i, 3] <- KendallW(temp_dat[, rat_comb[3,]], correct = TRUE,
                            test = TRUE)$estimate
  
  # Save median ratings to then use as a robustness check
  mdn_ratings <- c(mdn_ratings, apply(temp_dat, 1, median, na.rm = TRUE))
  
  # Save mean ratings only of the two raters with the highest agreement 
  max_cod <- which.max(cod_mat[i,])[1]
  mean_up <- c(mean_up, rowMeans(temp_dat[, rat_comb[max_cod,]], na.rm = TRUE))

}

# add median and mean ratings of the raters with the highest agreement to dataframe
sim_dat$mdn_similarity <- mdn_ratings
sim_dat$m_similarity_up <- mean_up

# aggregate to participant level
agg_sim <- sim_dat %>%
    mutate(overlap_up = case_when(m_similarity_up >= 5 ~ 1,
                               TRUE ~ 0),
           overlap_mdn = case_when(mdn_similarity >= 5 ~ 1,
                                   TRUE ~ 0),
           aspects = case_when(substr(aspect_id_1, 1, 2) == "s1" &
                                 substr(aspect_id_2, 1, 2) == "s1" ~ "within_s1",
                               substr(aspect_id_1, 1, 2) == "s2" &
                                 substr(aspect_id_2, 1, 2) == "s2" ~ "within_s2",
                               TRUE ~ "between")) %>%
    select(partid, aspects, overlap_up, overlap_mdn, m_similarity_up,
           mdn_similarity) %>%
    group_by(partid, aspects) %>%
    summarise(p_overlap_up = mean(overlap_up, na.rm = TRUE),
              p_overlap_mdn = mean(overlap_mdn, na.rm = TRUE),
              m_mdn_similarity = mean(mdn_similarity, na.rm = TRUE),
              m_m_up_similarity = mean(m_similarity_up, na.rm = TRUE)) %>%
    gather(variable, value, p_overlap_up, p_overlap_mdn, m_mdn_similarity,
           m_m_up_similarity) %>%
    unite(temp, variable, aspects) %>%
    spread(temp, value)

# Add to study_joint data
study_joint <- study_joint %>%
  left_join(agg_sim, by = "partid")

# Distribution of Kendall's W of rater triplets
hist(cod)
mean(cod)
range(cod)
quantile(cod, c(.25, .5, .75))

# Distribution of Kendall's W of all rater pairs
hist(rowMeans(cod_mat))

# Distribution of Kendall's W of rater pairs with highest W
hist(apply(cod_mat, 1, function(x) max(x)[1]))

### Average similarity ratings within and between studies
# with average similarity across rater triplets as preregistered
mean(study_joint$m_similarity_between, na.rm = TRUE)
mean(study_joint$m_similarity_within_s1, na.rm = TRUE)
mean(study_joint$m_similarity_within_s2, na.rm = TRUE)

# same with mdn similarity for robustness
mean(study_joint$m_mdn_similarity_between, na.rm = TRUE)
mean(study_joint$m_mdn_similarity_within_s1, na.rm = TRUE)
mean(study_joint$m_mdn_similarity_within_s2, na.rm = TRUE)

# same with mean over two raters with highest agreement for robustness
mean(study_joint$m_m_up_similarity_between, na.rm = TRUE)
mean(study_joint$m_m_up_similarity_within_s1, na.rm = TRUE)
mean(study_joint$m_m_up_similarity_within_s2, na.rm = TRUE)


### proportion of stable aspects: overlap defined as mean similarity rating of >=4
# with average similarity across rater triplets as preregistered
mean(study_joint$p_overlap_between, na.rm = TRUE)
mean(study_joint$p_overlap_within_s1, na.rm = TRUE)
mean(study_joint$p_overlap_within_s2, na.rm = TRUE)

# same with mdn similarity for robustness
mean(study_joint$p_overlap_mdn_between, na.rm = TRUE)
mean(study_joint$p_overlap_mdn_within_s1, na.rm = TRUE)
mean(study_joint$p_overlap_mdn_within_s2, na.rm = TRUE)

# same with mean over two raters with highest agreement for robustness
mean(study_joint$p_overlap_up_between, na.rm = TRUE)
mean(study_joint$p_overlap_up_within_s1, na.rm = TRUE)
mean(study_joint$p_overlap_up_within_s2, na.rm = TRUE)

# look at distribution of proportion of overlap
p_over_betw <- study_joint %>%
  ggplot(aes(p_overlap_between)) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2") +
  labs(x = "Proportion of Overlap Between",
       y = "Frequency") +
  geom_vline(xintercept = mean(study_joint$p_overlap_between,
                               na.rm = TRUE), lty = 2, col = "#d20537",
             size = 1.2, alpha = .8) +
  theme_bw()+
  xlim(-.05, 1.05) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

p_over_ws1 <- study_joint %>%
  ggplot(aes(p_overlap_within_s1)) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2") +
  labs(x = "Proportion of Overlap Within S1",
       y = "Frequency") +
  geom_vline(xintercept = mean(study_joint$p_overlap_within_s1,
                               na.rm = TRUE), lty = 2, col = "#d20537",
             size = 1.2, alpha = .8) +
  theme_bw()+
  xlim(-.05, 1.05) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

p_over_ws2 <- study_joint %>%
  ggplot(aes(p_overlap_within_s2)) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2") +
  labs(x = "Proportion of Overlap Within S2",
       y = "Frequency") +
  geom_vline(xintercept = mean(study_joint$p_overlap_within_s2,
                               na.rm = TRUE), lty = 2, col = "#d20537",
             size = 1.2, alpha = .8) +
  theme_bw()+
  xlim(-.05, 1.05) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

# look at distribution of similarity between 
p_sim_bet <- study_joint %>%
  ggplot(aes(m_similarity_between)) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2") +
  labs(x = "M Similarity Between",
       y = "Frequency") +
  geom_vline(xintercept = mean(study_joint$m_similarity_between,
                               na.rm = TRUE), lty = 2, col = "#d20537",
             size = 1.2, alpha = .8) +
  theme_bw()+
  xlim(-.01, 5.01) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

# look at distribution of similarity between 
p_sim_withs1 <- study_joint %>%
  ggplot(aes(m_similarity_within_s1)) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2") +
  labs(x = "M Similarity Within S1",
       y = "Frequency") +
  geom_vline(xintercept = mean(study_joint$m_similarity_within_s1,
                               na.rm = TRUE), lty = 2, col = "#d20537",
             size = 1.2, alpha = .8) +
  theme_bw()+
  xlim(-.01, 5.01) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

# look at distribution of similarity between 
p_sim_withs2 <- study_joint %>%
  ggplot(aes(m_similarity_within_s2)) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2") +
  labs(x = "M Similarity Within S2",
       y = "Frequency") +
  geom_vline(xintercept = mean(study_joint$m_similarity_within_s2,
                               na.rm = TRUE), lty = 2, col = "#d20537",
             size = 1.2, alpha = .8) +
  theme_bw()+
  xlim(-.01, 5.01) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

pdf("plots/study_2/distributions_similarities_overlaps.pdf", height = 8,
    width = 12)
p_over_betw + p_over_ws1 + p_over_ws2 + p_sim_bet + p_sim_withs1 + p_sim_withs2 +
  plot_layout(ncol = 2, byrow = FALSE)
dev.off()

# correlation between participants' own similarity ratings and the average 
# similarity ratings

# with average over all three raters
cor(study_joint$similarity, study_joint$m_similarity_between,
    method = "spearman")
# with median over all three raters
cor(study_joint$similarity, study_joint$m_mdn_similarity_between,
    method = "spearman")
# with average over two raters with highest agreement
cor(study_joint$similarity, study_joint$m_m_up_similarity_between, 
    method = "spearman")

# RQ 3b: Gauging evidence stability ============================================

# Paired t-test with the average evidence stability (obtained with VUM)
#   of the two time-points
tt <- bayes.t.test(study_joint$pred_VUM_s1, study_joint$pred_VUM_s2, paired = TRUE)
summary(tt)

# Correlation between strength of evidence as aggregated using VUM
cor(study_joint$pred_VUM_s1, study_joint$pred_VUM_s2, method = "spearman")

# RQ 4a: The relation between aspect stability and the stability of RTP ========

# stan_lm with absolute difference in SOEP from s1 and s2 as DV, and proportion
# of overlap as predictor. (This is the preregistered model)

mod_4a <- stan_glm(delta_rating ~ p_overlap_between, data = study_joint,
                   chains = 4, iter = 15000, family = gaussian(),
                   prior_intercept = normal(location = 0, scale = 10),
                   prior = normal(location = 0, scale = 2.5))

summary(mod_4a)

# residual plot
plot(fitted(mod_4a), resid(mod_4a))

mcmc_areas(
  as.array(mod_4a),
  pars = "p_overlap_between",
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

posterior <- as.data.frame(mod_4a)
mean(posterior$p_overlap_between)
quantile(posterior$p_overlap_between, c(.025, .975))

# use gamma distribution with log link; the + .0001 to ensure positive values
mod_4a2 <- stan_glm(I(delta_rating + .0001) ~ p_overlap_between, data = study_joint,
                   chains = 4, iter = 15000, family = Gamma(link = "log"),
                   prior_intercept = normal(location = 0, scale = 10),
                   prior = normal(location = 0, scale = 2.5))

summary(mod_4a2)

mcmc_areas(
  as.array(mod_4a2),
  pars = "p_overlap_between",
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

posterior <- as.data.frame(mod_4a2)
mean(posterior$p_overlap_between)
quantile(posterior$p_overlap_between, c(.025, .975))

cor(study_joint$delta_rating, study_joint$p_overlap_between,
    method = "spearman")

### Check how model looks if median coefficient of concordance is used (directly
### with gamma distribution and log-link)
mod_4a3 <- stan_glm(I(delta_rating + .0001) ~ p_overlap_mdn_between, data = study_joint,
                    chains = 4, iter = 15000, family = Gamma(link = "log"),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))

summary(mod_4a3)

mcmc_areas(
  as.array(mod_4a3),
  pars = "p_overlap_mdn_between",
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

posterior <- as.data.frame(mod_4a3)
mean(posterior$p_overlap_mdn_between)
quantile(posterior$p_overlap_mdn_between, c(.025, .975))

### Check how model looks if only the ratings of the two raters with the highest
### coefficient of concordance are used

mod_4a4 <- stan_glm(I(delta_rating + .0001) ~ p_overlap_up_between, data = study_joint,
                    chains = 4, iter = 15000, family = Gamma(link = "log"),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))

summary(mod_4a4)

mcmc_areas(
  as.array(mod_4a4),
  pars = "p_overlap_up_between",
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

posterior <- as.data.frame(mod_4a4)
mean(posterior$p_overlap_up_between)
quantile(posterior$p_overlap_up_between, c(.025, .975))

### Check how the relation looks when we use the average similarity as predictor
# stan_lm with absolute difference in SOEP from s1 and s2 as DV, and mean
# similarity as predictor.

mod_4a5 <- stan_glm(delta_rating ~ m_similarity_between, data = study_joint,
                   chains = 4, iter = 15000, family = gaussian(),
                   prior_intercept = normal(location = 0, scale = 10),
                   prior = normal(location = 0, scale = 2.5))

summary(mod_4a5)

mcmc_areas(
  as.array(mod_4a5),
  pars = "m_similarity_between",
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

posterior <- as.data.frame(mod_4a5)
mean(posterior$m_similarity_between)
quantile(posterior$m_similarity_between, c(.025, .975))

# use gamma distribution with log link; the + .0001 to ensure positive values
mod_4a6 <- stan_glm(I(delta_rating + .0001) ~ m_similarity_between, data = study_joint,
                    chains = 4, iter = 15000, family = Gamma(link = "log"),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))

summary(mod_4a6)

mcmc_areas(
  as.array(mod_4a6),
  pars = "m_similarity_between",
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

posterior <- as.data.frame(mod_4a6)
mean(posterior$m_similarity_between)
quantile(posterior$m_similarity_between, c(.025, .975))

# correlation between the risk preference and average similatiry
cor(study_joint$delta_rating, study_joint$m_similarity_between, method = "spearman")

# RQ 4b: The relation between evidence stability and the stability of RTP ======

# stan_glm with absolute difference in SOEP from s1 and s2 as DV, and absolute
# difference in VUM as predictor.
mod_4b <- stan_glm(delta_rating ~ delta_VUM, data = study_joint,
                   chains = 4, iter = 15000,
                   prior_intercept = normal(location = 0, scale = 10),
                   prior = normal(location = 0, scale = 2.5),
                   adapt_delta = .96)

summary(mod_4b)

mcmc_areas(
  as.array(mod_4b),
  pars = "delta_VUM",
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

posterior <- as.data.frame(mod_4b)
mean(posterior$delta_VUM)
quantile(posterior$delta_VUM, c(.025, .975))

cor(study_joint$delta_rating, study_joint$delta_VUM, method = "spearman")
plot(study_joint$delta_VUM, study_joint$delta_rating)

# Rerun analysis with directional differences (i.e., without taking the absolute)
mod_4b2 <- stan_glm(delta_rating ~ delta_pred_VUM,
                   data = study_joint %>% 
                     mutate(delta_pred_VUM = scale(pred_VUM_s2 - pred_VUM_s1),
                            delta_rating = scale(rating_s2 - rating_s1)),
                   chains = 4, iter = 15000, family = gaussian(),
                   prior_intercept = normal(location = 0, scale = 10),
                   prior = normal(location = 0, scale = 2.5))

summary(mod_4b2)


mcmc_areas(
  as.array(mod_4b2),
  pars = "delta_pred_VUM",
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)

posterior <- as.data.frame(mod_4b2)
mean(posterior$delta_pred_VUM)
quantile(posterior$delta_pred_VUM, c(.025, .975))

study_joint %>% 
  mutate(delta_pred_VUM = pred_VUM_s2 - pred_VUM_s1,
         delta_rating = rating_s2 - rating_s1) %>%
  select(delta_pred_VUM, delta_rating) %>%
  cor(method = "spearman")

#### Plot stability of risk preference ratings, strength of evidence, and the
#### relation of the two

# risk preferences
pc_soep <- study_joint %>%
  select(rating_s1, rating_s2, partid) %>%
  gather(Timepoint, SRRTP, rating_s1, rating_s2) %>%
  mutate(Timepoint = case_when(Timepoint == "rating_s1" ~ "Study 1",
                               TRUE ~ "Study 2"),
         SRRTP = SRRTP + rnorm(2 * nrow(study_joint), 0, 0.1)) %>%
  ggplot(aes(Timepoint, SRRTP, group = partid)) +
  geom_point(alpha = .6) +
  geom_line(col = "#646464", alpha = .3) +
  labs(y = "Risk Preference",
       x = "") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14,face = "bold")
  )

# strength of evidence
pc_soe <- study_joint %>%
  select(m_risk_s1, m_risk_s2, partid) %>%
  gather(Timepoint, SoE, m_risk_s1, m_risk_s2) %>%
  mutate(Timepoint = case_when(Timepoint == "m_risk_s1" ~ "Study 1",
                               TRUE ~ "Study 2")) %>%
  ggplot(aes(Timepoint, SoE, group = partid)) +
  geom_point() +
  geom_line(col = "#646464", alpha = .3) +
  labs(y = "Aggr. Strength of Evidence",
       x = "") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14,face = "bold")
  )

# relation of differences in risk preferences and differences in strength of
# between s1 and s2
ps <- study_joint %>%
  mutate(delta_m_risk = m_risk_s2 - m_risk_s1,
         delta_rating = rating_s2 - rating_s1) %>%
  ggplot(aes(delta_m_risk, delta_rating)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, col = "#d20537") +
  labs(x = expression(bold(Delta) ~ bold(Aggr.) ~ bold(Strength) ~ bold(of) ~ bold(Evidence)),
       y = expression(bold(Delta) ~ bold(Risk) ~ bold(Preference))) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14,face = "bold")
  )

pdf("plots/study_2/RQ4b_stability.pdf", height = 4, width = 12)
pc_soep + pc_soe + ps
dev.off()

# plot stability with VUM aggregated ratings -----------------------------------

pc_soep <- study_joint %>%
  select(rating_s1, rating_s2, partid) %>%
  gather(Timepoint, SRRTP, rating_s1, rating_s2) %>%
  mutate(Timepoint = case_when(Timepoint == "rating_s1" ~ "Study 1",
                               TRUE ~ "Study 2"),
         SRRTP = SRRTP + rnorm(2 * nrow(study_joint), 0, 0.1)) %>%
  ggplot(aes(Timepoint, SRRTP, group = partid)) +
  geom_point(alpha = .6) +
  geom_line(col = "#646464", alpha = .3) +
  labs(y = "Risk Preference",
       x = "") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14,face = "bold")
  )

pc_soe <- study_joint %>%
  select(pred_VUM_s1, pred_VUM_s2, partid) %>%
  gather(Timepoint, SoE, pred_VUM_s1, pred_VUM_s2) %>%
  mutate(Timepoint = case_when(Timepoint == "pred_VUM_s1" ~ "Study 1",
                               TRUE ~ "Study 2")) %>%
  ggplot(aes(Timepoint, SoE, group = partid)) +
  geom_point() +
  geom_line(col = "#646464", alpha = .3) +
  labs(y = "Aggr. Strength of Evidence",
       x = "") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, colour = "black"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14,face = "bold")
  )

ps <- study_joint %>%
  mutate(delta_VUM = pred_VUM_s2 - pred_VUM_s1,
         delta_rating = rating_s2 - rating_s1) %>%
  ggplot(aes(delta_VUM, delta_rating)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = FALSE, col = "#d20537") +
  labs(x = expression(bold(Delta) ~ bold(Aggr.) ~ bold(Strength) ~ bold(of) ~ bold(Evidence)),
       y = expression(bold(Delta) ~ bold(Risk) ~ bold(Preference))) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 14,face = "bold")
  )

pdf("plots/study_2/RQ4b_stability_VUM.pdf", height = 4, width = 12)
pc_soep + pc_soe + ps
dev.off()
 
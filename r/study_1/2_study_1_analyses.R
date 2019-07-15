# Anlysis script Study 1 of "Mapping the Cognitive 
library(tidyverse)
library(patchwork)
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

### Load Data ==================================================================

study <- read_csv("data/study_1/study.csv")
study_long <- read_csv("data/study_1/study_long.csv")

### Descriptive statistics and preliminary analyses ============================

### demographics ---------------------------------------------------------------

# sex
mean(study$sex == "female")
sum(study$sex == "female")

# age
mean(study$age)
range(study$age)

# years of education
mean(study$yearsofedu)
range(study$yearsofedu)

# income
Mode(study$income)

# employment status
Mode(study$job)

# completion time
mean(study$duration / 60)

### Number of aspects listed ---------------------------------------------------

# Mean and range of number of aspects listed
mean(study$n_aspects)
range(study$n_aspects)

# Proportion and number of people who listed between one and four aspects
mean(study$n_aspects <= 4)
sum(study$n_aspects <= 4)

# Proportion of contra-aspects
mean(study_long$r_risk < 0)

# Ratio of pro-aspects within persons
study$pro_ratio <- study$n_seeking / (study$n_seeking + study$n_avoiding)
mean(study$pro_ratio)

# Participants who listed only pro- or only contra-aspects
mean(study$pro_ratio %in% c(1, 0))
sum(study$pro_ratio %in% c(1, 0))

# Ratio of pro aspects overall
sum(study_long$r_risk_binary == "Pro") / length(study_long$r_risk_binary)

# Plot distribution of number of aspects
ggplot(study, aes(factor(n_aspects))) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2", stat = "count") +
  theme_bw() +
  labs(x = "Number of Aspects Listed",
       y = "Frequency") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold")
  )

ggsave("plots/study_1/n_aspects_distribution.pdf",
       width = 8, height = 6)

# Plot distribution of ratio of pro aspects
ggplot(study, aes(x = pro_ratio)) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Rate of Pro Aspects Per Participant",
       y = "Frequency") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

ggsave("plots/study_1/pro_ratio_distribution.pdf",
       width = 8, height = 6)


# Plot distribution of number of pro- and contra-aspects
tibble(
  x = paste0(ifelse(study$n_seeking < 10, paste0("0", study$n_seeking),
                    study$n_seeking),"\n",
             ifelse(study$n_avoiding < 10, paste0("0", study$n_avoiding),
                    study$n_avoiding))
  ) %>%
  ggplot(aes(x)) +
  geom_bar(fill = c(rep("#a5d7d2", 8), rep("#1ea5a5", 6), rep("#2d373c", 6),
                   rep("#8c9196", 4), rep("#d20537", 3), rep("#eb829b", 3),
                   "#006e6e", "#bec3c8", "#006e6e", "#000000")) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  theme_bw()+
  labs(x = "N Pro (Top) and N Contra (Bottom)",
       y = "Count") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 16,face = "bold")
  )

ggsave("plots/study_1/pro_contra_distribution.pdf",
       width = 8, height = 6)

### Strength of evidence -------------------------------------------------------

# Mean average (within person) strength of evidence
mean(study$m_risk)
range(study$m_risk)

# correlation of strength of evidence and age
cor.test(study$m_risk, study$age, method = "spearman")

# Plot distribution of participants' average strength of evidence
ggplot(study, aes(x = m_risk)) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Strength of Evidence",
       y = "Frequency") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

ggsave("plots/study_1/soe_distribution.pdf",
       width = 8, height = 6)

# Risk Preferences (SOEP general risk item) ------------------------------------

# Average risk preference rating
mean(study$rating)

# Risk averse participants
mean(study$rating < 5)
sum(study$rating < 5)

# Distribution of risk preference ratings
study %>%
  ggplot(aes(factor(rating))) +
  geom_histogram(col = "#a5d7d2", fill = "#a5d7d2", stat = "count") +
  labs(x = "Risk Preference",
       y = "Frequency") +
  theme_bw()+
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="lightgrey")
  )

ggsave("plots/study_1/SOEP_distribution.pdf",
       width = 8, height = 6)

# Correlation of age and reported risk preferences
cor.test(study$rating, study$age, method = "spearman")

### Strength of evidence and risk preferences ----------------------------------

# overall correlation between the two
cor(study$rating, study$m_risk, use = "pairwise", method = "spearman")

# Correlations of strength of evidence and risk preferences as function of aspect
# position
cors <- study_long %>%
  group_by(aspect_ind_cat) %>%
  summarise(
    cors = cor(r_risk, rating, use = "pairwise", method = "spearman"),
    N = n()
  ) %>%
  ungroup() %>%
  drop_na()

# Plot correlation as function of aspect position
barplot(cors ~ aspect_ind_cat, data = cors, ylim = c(0, 1), ylab = "Correlation",
        xlab = "Aspect Position")

# Plot relationship between strength of evidence and risk preferences as
# function of aspect position
study_long %>%
  select(r_risk, rating, aspect_ind_cat) %>%
  drop_na() %>%
  ggplot(aes(x = r_risk, y = rating)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_label(data = cors, aes(label = paste("rho =", round(cors, 2), "; N =", N)), 
             x = -Inf, y = Inf, hjust = -0.2, vjust = 1.2)+
  facet_wrap(~ aspect_ind_cat, ncol = 3) +
  theme_bw() +
  labs(
    x = "Aspect's strength of evidence",
    y = "Risk Preference"
  ) + 
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )
ggsave("plots/study_1/evidencestrength_propensity_correlations.pdf",
       width = 9, height = 7)


### Correlations of strength of evidence and risk preferences as function of 
### aspect position AND number of aspects listed

# add categorical variable showing number of aspects listed
study_long <- study_long %>%
  mutate(n_aspects_cat = case_when(n_aspects == 1 ~ "1 Aspect",
                                   n_aspects == 2 ~ "2 Aspects",
                                   n_aspects == 3 ~ "3 Aspects",
                                   n_aspects == 4 ~ "4 Aspects",
                                   n_aspects == 5 ~ "5 Aspects",
                                   n_aspects == 6 ~ "6 Aspects",
                                   TRUE ~ NA_character_))

# correlation dataset with correlations as a function of aspect position
cors <- study_long %>%
  group_by(aspect_ind_cat, n_aspects_cat) %>%
  summarise(
    cors = cor(r_risk, rating, use = "pairwise", method = "spearman"),
    N = n()
  ) %>%
  ungroup() %>%
  drop_na()

# show n participants per row
cors %>% group_by(n_aspects_cat) %>% summarise(N = N[1])

study_long %>% 
  select(r_risk, rating, n_aspects_cat, aspect_ind_cat) %>%
  drop_na() %>%
  ggplot(aes(x = r_risk, y = rating)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm", se = FALSE, col = "#d20537") +
  geom_label(data = cors,
             aes(label = paste0("list(r[s] == ", round(cors, 2), ", N == ", N, ")")), 
             x = -Inf, y = 10, parse = TRUE, hjust = -.015) +
  facet_grid(n_aspects_cat ~ aspect_ind_cat) +
  theme_bw() +
  labs(
    x = "Aspect's Strength of Evidence",
    y = "Risk-Taking Propensity"
  ) + 
  ylim(-.1, 11) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )

ggsave("plots/study_1/evidencestrength_propensity_corrs_grid.pdf",
       width = 14, height = 8)


### Variation in strength of evidence ratings over aspects ---------------------

# gauge variation in strength of evidence ratings using ICCs 
icc_mod <- stan_lmer(r_risk ~ 1 + (1|partid), data = study_long, chains = 4,
                     iter = 8000)

# get ICC measure
icc_mod_df <- as.data.frame(icc_mod)
Vind <- icc_mod_df$`Sigma[partid:(Intercept),(Intercept)]`
R.MAS <- Vind / (Vind + icc_mod_df$sigma^2)
median(R.MAS)

# Plot the stability of the strength of evidence and 
p_risk <- study_long %>% 
  filter(n_aspects_cat != "1 Aspect") %>%
  mutate(aspect_ind_cat = substr(aspect_ind_cat, 8, 9)) %>%
  ggplot(aes(x = aspect_ind_cat, y = r_risk, group = partid)) +
  geom_point(alpha = .2) + 
  geom_line(alpha = .2) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, group = 1,
               col = "#d20537") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", fun.args = list(mult=1),
               group = 1, col = "#d20537") +
  facet_wrap( ~ n_aspects_cat, ncol = 5, scales = "free_x") +
  theme_bw() +
  labs(
    x = "",
    y = "SoE"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill="#a5d7d2")
  )

pdf("plots/study_1/ratings_variation.pdf", width = 8, height = 6)
p_risk 
dev.off()

### RQ1: Modeling self-reported risk-taking propensity =========================

### Cross validation using five folds

# set seed for replicability of results
set.seed(1)
cv_all <- cross_val(study_long, study)
cv_mult_asp <- cross_val(study_long %>% filter(n_aspects > 1),
                         study %>% filter(n_aspects > 1))

# Modeling analysis for all participants ---------------------------------------

# plot predictions and criteria
rsq_dat <- tibble(
  rsqs = cv_all$m_rsq,
  model = names(cv_all$m_rsq)
)

cv_all$predictions %>%
  gather("model", "prediction", -criterion, -partid) %>%
  mutate(model = gsub("pred_", "", model)) %>%
  ggplot(aes(prediction, criterion)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm", col = "#d20537") +
  geom_label(data = rsq_dat,
             aes(label = paste0("R^2 == ", round(rsqs, 2))),
             x = -Inf, y = 9.5, hjust = -.15, parse = TRUE) +
  facet_wrap(~ factor(model, levels = c("QT", "SUM", "EXTREME", "FIRST",
                                        "LAST", "VUM"),
                      labels = c("QT", "SUM", "EXTREME", "FIRST", "LAST", "VUM")),
             scales = "free_x") +
  ylim(0, 10) +
  labs(
    x = "Model Prediction",
    y = "Risk Preferences"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill = "#a5d7d2")
  )

ggsave("plots/study_1/predictions_models.pdf",
       width = 8, height = 6)

# Parameter estimates
mean(cv_all$parameters$phi)

# ..._lower and ..._upper are the upper and lower bounds of the 95% HDI
apply(cv_all$parameters[, c("QT_SMRD", "QT_SMRD_lower", "QT_SMRD_upper")],
      2, mean)

apply(cv_all$parameters[, c("QT_na", "QT_na_lower", "QT_na_upper")],
      2, mean)

apply(cv_all$parameters[, c("QT_ns", "QT_ns_lower", "QT_ns_upper")],
      2, mean)

# Modeling analysis for participants who listed more than 1 aspect -------------

# plot predictions and criteria
rsq_dat <- tibble(
  rsqs = cv_mult_asp$m_rsq,
  model = names(cv_mult_asp$m_rsq)
)

cv_mult_asp$predictions %>%
  gather("model", "prediction", -criterion, -partid) %>%
  mutate(model = gsub("pred_", "", model)) %>%
  ggplot(aes(prediction, criterion)) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm", col = "#d20537") +
  geom_label(data = rsq_dat,
             aes(label = paste0("R^2 == ", round(rsqs, 2))),
             x = -Inf, y = 9.5, hjust = -.15, parse = TRUE) +
  facet_wrap(~ factor(model, levels = c("QT", "SUM", "EXTREME", "FIRST",
                                        "LAST", "VUM"),
                      labels = c("QT", "SUM", "EXTREME", "FIRST", "LAST", "VUM")),
             scales = "free_x") +
  ylim(0, 10) +
  labs(
    x = "Model Prediction",
    y = "Risk Preferences"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold"),
    strip.background = element_rect(fill = "#a5d7d2")
  )

ggsave("plots/study_1/predictions_models_mult_asp.pdf",
       width = 8, height = 6)

# Parameter estimates
mean(cv_mult_asp$parameters$phi)

apply(cv_mult_asp$parameters[, c("QT_SMRD", "QT_SMRD_lower", "QT_SMRD_upper")],
      2, mean)

apply(cv_mult_asp$parameters[, c("QT_na", "QT_na_lower", "QT_na_upper")],
      2, mean)

apply(cv_mult_asp$parameters[, c("QT_ns", "QT_ns_lower", "QT_ns_upper")],
      2, mean)


### Rsquared and parameters plot -----------------------------------------------

rsq_dat <- tibble(
  rsqs_all = cv_all$m_rsq,
  rsqs_mult_asp = cv_mult_asp$m_rsq,
  model = names(cv_all$m_rsq))

rsq_dat$model[rsq_dat$model == "EXTREME"] <- "EXT"
rsq_dat <- rsq_dat[c(1, 5, 2, 3, 4, 6), ]


# model with demographic variables as predictors
mod_age <- stan_glm(rating ~ age, data = study,
                    chains = 4, iter = 8000, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
mod_sex <- stan_glm(rating ~ sex, data = study,
                    chains = 4, iter = 8000, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))
mod_all <- stan_glm(rating ~ age + sex + yearsofedu + income + job, data = study,
                    chains = 4, iter = 8000, family = gaussian(),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 2.5))

r2_age <- median(bayes_R2(mod_age))
r2_sex <- median(bayes_R2(mod_sex))
r2_all <- median(bayes_R2(mod_all))

### Plot ---
pdf("plots/study_1/rsq_phi.pdf", height = 4, width = 12)

layout(matrix(c(1, 1, 1, 2, 3, 3, 3, 3), 1, byrow = TRUE))


# barplot with rsquared values ---
par(mar = c(5, 6, .8, 0))
barplot(cbind(rsqs_all, rsqs_mult_asp) ~ model, data = rsq_dat,
        beside = TRUE, ylim = c(0, 1), las = 1, cex.names = 1.5,
        axes = FALSE, xlab = "")
axis(1, c(-1, seq(2, 17, 3), 20), lwd = 2, labels = FALSE)
axis(2, c(seq(0, 1, .2), 1.05), labels = TRUE, lwd = 2, las = 1, cex.axis = 1.5)
mtext("Models", side = 1, line = 4, cex = 1.4, font = 2, padj = -.4)
mtext(expression(R^2), side = 2, line = 4, cex = 1.4, font = 2, padj = .2)
lines(seq(0, 20, 2) + .2, rep(r2_age, length(seq(0, 20, 2))), lwd = 1.75, lty = 2,
      pch = 16, type = "b", col = "brown1")
lines(seq(-1, 20, 2) + .2, rep(r2_sex, length(seq(-1, 20, 2))), lwd = 1.75, lty = 2,
      pch = 17, type = "b", col = "brown1")
lines(seq(-1, 20, 2) + .4, rep(r2_all, length(seq(-1, 20, 2))), lwd = 1.75, lty = 2,
      pch = 4, type = "b", col = "brown1")

# barplot legend ---

par(mar = c(0, 0, 0, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

hadj <- -.09
vadj <- -.1

points(c(.25, .25) + hadj, c(.72 , .66) + vadj, pch = 15,
       col = c("#4d4d4d", "#e6e6e6"), cex = 4)
text(.1 + hadj, .78 + vadj, "Sample", adj = 0, cex = 1.8)
text(c(.4, .4) + hadj, c(.72, .66) + vadj, c("All", "> 1 Aspect"),
     adj = 0, cex = 1.5)

# reference model legend
lines(c(.23, .27) + hadj, c(.5 , .5) + vadj, lwd = 1.75, lty = 2,
      pch = 1, type = "l", col = "brown1")
points(c(.19, .31) + hadj, c(.5, .5) + vadj, pch = 16,
       col = "brown1", cex = 1.25)

lines(c(.23, .27) + hadj, c(.43 , .43) + vadj, lwd = 1.75, lty = 2,
      pch = 1, type = "l", col = "brown1")
points(c(.19, .31) + hadj, c(.43, .43) + vadj, pch = 17,
       col = "brown1", cex = 1.25)

lines(c(.23, .27) + hadj, c(.36, .36) + vadj, lwd = 1.75, lty = 2,
      pch = 1, type = "l", col = "brown1")
points(c(.19, .31) + hadj, c(.36, .36) + vadj, pch = 4,
       col = "brown1", cex = 1.25)

text(.1 + hadj, .56 + vadj, "Ref Model", adj = 0, cex = 1.8)
text(c(.4, .4 , .4) + hadj, c(.5, .43, .36) + vadj,
     c("Age", "Sex", "Demo"), adj = 0, cex = 1.5)

# Violin plot for parameter distribution ---
par(mar = c(5, 6, 0, 0))
plot.new()
plot.window(ylim = c(0.5, 1.5), xlim = c(.5, 4))
abline(h = 1, lty = 2, col = "#646464", lwd = 3)
beanplot(cv_all$parameters$phi, cv_mult_asp$parameters$phi, xlim = c(0, 4),
         ylim = c(0.5, 1.5), axes = FALSE, col = "white", what=c(0,1,0,0),
         cut = 1, add = TRUE)
points(1 + rnorm(length(cv_all$parameters$phi), 0, .05), cv_all$parameters$phi,
       pch = 16)
points(2 + rnorm(length(cv_mult_asp$parameters$phi), 0, .05),
       cv_mult_asp$parameters$phi, pch = 16)
lines(c(.92, 1.08), rep(mean(cv_all$parameters$phi), 2), col = "red",
      lwd = 5)
lines(c(1.92, 2.08), rep(mean(cv_mult_asp$parameters$phi), 2), col = "red",
      lwd = 5)

axis(1, c(-1, 1, 2, 20), labels = c("", "All", "> 1 Aspect", ""), lwd = 2,
     cex.axis = 1.5)
axis(2, c(-1, .5, .75, 1, 1.25, 1.5), labels = TRUE, lwd = 2, las = 1,
     cex.axis = 1.5)
mtext("Models", side = 1, line = 4, cex = 1.4, font = 2, padj = -.4)
mtext(expression(phi), side = 2, line = 4, cex = 1.4, font = 2, padj = .2)

arrows(2.6, .95, 2.6,.6, lwd = 4)
arrows(2.6, 1.05, 2.6, 1.4, lwd = 4)

# text(3.4, 1.05, "Unit Weight", font = 3, cex = 1.8)
text(2.85, 1.25, "Primacy Effect", font = 3, cex = 1.8, srt = 90)
text(2.85, .755, "Recency Effect", font = 3, cex = 1.8, srt = 90)

dev.off()

### RQ2: Exploring the contents of evidence ====================================

# Plot the distributions of responses
study %>%
  select(p_social, p_situation, med_frequency, p_active, p_control,
         mean_sentiment) %>%
  pairs.panels(method = "spearman")

# Violin plots of the distribution
pl_social <- ggplot(study, aes(p_social)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Proportion of Social Comparisons",
       y = "Proportion") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pl_situation <- ggplot(study, aes(p_situation)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Proportion of Personal Experiences",
       y = "") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pl_frequency <- ggplot(study, aes(med_frequency)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Median Frequency",
       y = "") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )


pl_active <- ggplot(study, aes(p_active)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Proportion of Active Choices",
       y = "Proportion") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pl_control <- ggplot(study, aes(p_control)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Proportion of Controllable Choices",
       y = "") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pl_sentiment <- ggplot(study, aes(mean_sentiment)) +
  geom_histogram(aes(y=..count../sum(..count..)),
                 col = "#a5d7d2", fill = "#a5d7d2") +
  theme_bw() +
  labs(x = "Mean Sentiment",
       y = "") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pdf("plots/study_1/contents_distributions.pdf",
    width = 12, height = 6)

pl_social + pl_situation + pl_frequency + pl_active + pl_control + pl_sentiment +
  plot_layout(ncol = 3)

dev.off()

# Gauge number of "Not Applicable" responses
study_long %>%
  select(r_social, r_situation, rank_frequency, r_active, r_control,
         sentiment, partid) %>%
  group_by(partid) %>%
  summarise(
    na_social = mean(is.na(r_social)),
    na_situation = mean(is.na(r_situation)),
    na_frequency = mean(is.na(rank_frequency)),
    na_active = mean(is.na(r_active)),
    na_control = mean(is.na(r_control)),
    na_sentiment = mean(is.na(sentiment)),
    N = n()
  ) %>%
  ungroup() %>%
  select(-partid) %>%
  pairs.panels()


# Compare distributions of pro vs contra aspects
pc_dat <- study_long %>%
  select(r_social, r_situation, rank_frequency, r_active, r_control,
         sentiment, partid, r_risk_binary) %>%
  group_by(partid, r_risk_binary) %>%
  summarise(
         p_social = mean(r_social, na.rm = TRUE) * 100,
         p_situation = mean(r_situation, na.rm = TRUE) * 100,
         p_control = mean(r_control == "controllable") * 100,
         p_active = mean(r_active == "active", na.rm = TRUE) * 100,
         med_frequency = median(rank_frequency, na.rm = TRUE),
         mean_sentiment = mean(sentiment, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(r_risk_binary != "neutral")

pl_social <- ggplot(pc_dat %>% mutate(
  r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))),
  aes(r_risk_binary, p_social)) +
  geom_violin() +
  geom_jitter(alpha = .3, width = .1, height = .01) +
  stat_summary(fun.data = mean_se, color = "#d20537", na.rm = TRUE,
               width = .1, fun.args = list(mult=2), geom = "errorbar",
               size = 1.2) +
  stat_summary(fun.data = mean_sdl, color = "#d20537", na.rm = TRUE,
               geom = "point", size = 3) +
  theme_bw() +
  scale_y_continuous(limits = c(-5,105), expand = c(0, 0)) +
  labs(y = "% Social Comparisons",
       x = "Aspect") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pl_situation <- ggplot(pc_dat %>% mutate(
  r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))),
  aes(r_risk_binary, p_situation)) +
  geom_violin() +
  geom_jitter(alpha = .3, width = .1, height = .01) +
  stat_summary(fun.data = mean_se, color = "#d20537", na.rm = TRUE,
               width = .1, fun.args = list(mult=2), geom = "errorbar",
               size = 1.2) +
  stat_summary(fun.data = mean_sdl, color = "#d20537", na.rm = TRUE,
               geom = "point", size = 3) +
  theme_bw() +
  scale_y_continuous(limits = c(-5,105), expand = c(0, 0)) +
  labs(y = "% Pers Experiences",
       x = "Aspect") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pl_frequency <- ggplot(pc_dat %>% mutate(
  r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))),
  aes(r_risk_binary, med_frequency)) +
  geom_violin() +
  geom_jitter(alpha = .3, width = .1, height = .01) +
  stat_summary(fun.data = mean_se, color = "#d20537", na.rm = TRUE,
               width = .1, fun.args = list(mult=2), geom = "errorbar",
               size = 1.2) +
  stat_summary(fun.data = mean_sdl, color = "#d20537", na.rm = TRUE,
               geom = "point", size = 3) +
  theme_bw() +
  scale_y_continuous(limits = c(.9, 6.1), expand = c(0, 0)) +
  labs(y = "Mdn Frequency",
       x = "Aspect") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )


pl_active <- ggplot(pc_dat %>% mutate(
  r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))),
  aes(r_risk_binary, p_active)) +
  geom_violin() +
  geom_jitter(alpha = .3, width = .1, height = .01) +
  stat_summary(fun.data = mean_se, color = "#d20537", na.rm = TRUE,
               width = .1, fun.args = list(mult=2), geom = "errorbar",
               size = 1.2) +
  stat_summary(fun.data = mean_sdl, color = "#d20537", na.rm = TRUE,
               geom = "point", size = 3) +
  scale_y_continuous(limits = c(-5, 105), expand = c(0, 0)) +
  theme_bw() +
  labs(y = "% Active Choices",
       x = "Aspect") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pl_control <- ggplot(pc_dat %>% mutate(
  r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))),
  aes(r_risk_binary, p_control)) +
  geom_violin() +
  geom_jitter(alpha = .3, width = .1, height = .01) +
  stat_summary(fun.data = mean_se, color = "#d20537", na.rm = TRUE,
               width = .1, fun.args = list(mult=2), geom = "errorbar",
               size = 1.2) +
  stat_summary(fun.data = mean_sdl, color = "#d20537", na.rm = TRUE,
               geom = "point", size = 3) +
  theme_bw() +
  scale_y_continuous(limits = c(-5, 105), expand = c(0, 0)) +
  labs(y = "% Controllable",
       x = "Aspect") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pl_sentiment <- ggplot(pc_dat %>% mutate(
  r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))),
  aes(r_risk_binary, mean_sentiment)) +
  geom_violin() +
  geom_jitter(alpha = .3, width = .1, height = .01) +
  stat_summary(fun.data = mean_se, color = "#d20537", na.rm = TRUE,
               width = .1, fun.args = list(mult=2), geom = "errorbar",
               size = 1.2) +
  stat_summary(fun.data = mean_sdl, color = "#d20537", na.rm = TRUE,
               geom = "point", size = 3) +
  theme_bw() +
  labs(y = "M Sentiment",
       x = "Aspect") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

pdf("plots/study_1/contents_distributions_pro_vs_contra.pdf",
    width = 12, height = 6)

pl_social + pl_situation + pl_frequency + pl_active + pl_control + pl_sentiment +
  plot_layout(ncol = 3)

dev.off()

# Stacked barplots of the different categories
sb_social <- study_long %>%
  filter(r_risk_binary != "neutral") %>%
  group_by(r_risk_binary) %>%
  summarise(
    Yes = mean(r_social == 1, na.rm = TRUE),
    No = mean(r_social == 0, na.rm = TRUE)
  ) %>%
  gather(Answer, percent, -r_risk_binary) %>%
  mutate(r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))) %>%
  ggplot(aes(fill = Answer, x = r_risk_binary, y = percent)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(x = r_risk_binary, y = rep(c(.1, .5), each = 2),
                label = paste0(round(percent *100),"%")), size = 5,
            fontface = "bold") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Aspects",
       y = "Response Ratio",
       title = "Social Comparison") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13,face = "bold"),
    title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_manual(values = c("#a5d7d2", "#1ea5a5"))

sb_situation <- study_long %>%
  filter(r_risk_binary != "neutral") %>%
  group_by(r_risk_binary) %>%
  summarise(
    Yes = mean(r_situation == 1, na.rm = TRUE),
    No = mean(r_situation == 0, na.rm = TRUE)
  ) %>%
  gather(Answer, percent, -r_risk_binary) %>%
  mutate(r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))) %>%
  ggplot(aes(fill = Answer, x = r_risk_binary, y = percent)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(x = r_risk_binary, y = rep(c(.3, .8), each = 2),
                label = paste0(round(percent *100),"%")), size = 5,
            fontface = "bold") +
  theme_bw() +
  labs(x = "Aspects",
       y = "Response Ratio",
       title = "Pers Experiences") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13,face = "bold"),
    title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_manual(values = c("#a5d7d2", "#1ea5a5"))

sb_control <- study_long %>%
  filter(r_risk_binary != "neutral") %>%
  group_by(r_risk_binary) %>%
  summarise(
    Contr. = mean(r_control == "controllable", na.rm = TRUE),
    Uncontr. = mean(r_control == "uncontrollable", na.rm = TRUE)
  ) %>%
  gather(Answer, percent, -r_risk_binary) %>%
  mutate(r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))) %>%
  ggplot(aes(fill = Answer, x = r_risk_binary, y = percent)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(x = r_risk_binary, y = rep(c(.5, .18), each = 2),
                label = paste0(round(percent *100),"%")), size = 5,
            fontface = "bold") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Aspects",
       y = "Response Ratio",
       title = "Controllable") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13,face = "bold"),
    title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_manual(values = c("#a5d7d2", "#1ea5a5"))

sb_active <- study_long %>%
  filter(r_risk_binary != "neutral") %>%
  group_by(r_risk_binary) %>%
  summarise(
    Active = mean(r_active == "active", na.rm = TRUE),
    Passive = mean(r_active == "passive", na.rm = TRUE)
  ) %>%
  gather(Answer, percent, -r_risk_binary) %>%
  mutate(r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))) %>%
  ggplot(aes(fill = Answer, x = r_risk_binary, y = percent)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(x = r_risk_binary, y = rep(c(.5, .06), each = 2),
                label = paste0(round(percent *100),"%")), size = 5,
            fontface = "bold") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Aspects",
       y = "Response Ratio",
       title = "Active Choice") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13,face = "bold"),
    title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_manual(values = c("#a5d7d2", "#1ea5a5"))

sb_frequency <- study_long %>%
  filter(r_risk_binary != "neutral") %>%
  group_by(r_risk_binary) %>%
  summarise(
    Day = mean(r_frequency == "day", na.rm = TRUE),
    Week = mean(r_frequency == "week", na.rm = TRUE),
    Month = mean(r_frequency == "month", na.rm = TRUE),
    Year = mean(r_frequency == "year", na.rm = TRUE),
    Less = mean(r_frequency == "less_reg", na.rm = TRUE),
    Once = mean(r_frequency == "less_once", na.rm = TRUE)
  ) %>%
  gather(Answer, percent, -r_risk_binary) %>%
  mutate(Answer = factor(Answer, levels = c("Day", "Week", "Month", "Year",
                                            "Less", "Once"),
                         labels = c("1/Day", "1/Week", "1/Month", "1/Year",
                                    "< 1/Year", "Once")),
         r_risk_binary = factor(r_risk_binary, levels = c("Pro", "Contra"))) %>%
  ggplot(aes(fill = Answer, x = r_risk_binary, y = percent)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(x = r_risk_binary,
                y = c(.9, .9, .625, .625, .375, .375, .21, .13, .14, .055, .05,
                      0.015),
                label = paste0(round(percent *100),"%")), size = 5,
            fontface = "bold", col = c(rep("black", 4), rep("white", 6),
                                       rep("black", 2))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "Aspects",
       y = "Response Ratio",
       title = "Frequency") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13,face = "bold"),
    title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_manual(values = c("#a5d7d2", "#1ea5a5", "#2d373c", "#8c9196",
                               "#d20537", "#eb829b"))

pdf("plots/study_1/contents_stacked_barplots.pdf",
    width = 12, height = 8)

{{sb_social + sb_situation + sb_control + sb_active + plot_layout(ncol = 2)} |
    sb_frequency} + plot_layout(widths = c(1,.4))

dev.off()

### Quantification of differences between pro- and contra-aspects --------------
# Test whether these sources differ for pro vs contra aspects (takes long to run
# so the outputs were saved and are just loaded in below)

# # mixed effects model predicting social comparison using pro and contra as predictor
# soc_mod <- stan_glmer(r_social ~ r_risk_binary + (r_risk_binary | partid),
#                       data = study_long %>% filter(r_risk_binary != "neutral"),
#                       family = binomial(), chains = 4, iter = 8000)
# 
# # mixed effects model predicting personal experience using pro and contra as predictor
# sit_mod <- stan_glmer(r_situation ~ r_risk_binary + (r_risk_binary | partid),
#                       data = study_long %>% filter(r_risk_binary != "neutral"),
#                       family = binomial(), chains = 4, iter = 8000)
# 
# # mixed effects model predicting active vs passive using pro and contra as predictor
# act_mod <- stan_glmer(r_active ~ r_risk_binary + (r_risk_binary | partid),
#                       data = study_long %>% filter(r_risk_binary != "neutral") %>%
#                         mutate(r_active = case_when(r_active == "active" ~ 1L,
#                                                     r_active == "passive" ~ 0L,
#                                                     TRUE ~ NA_integer_)),
#                       family = binomial(), chains = 4, iter = 8000)
# 
# # mixed effects model predicting social comparison using pro and contra as predictor
# cnt_mod <- stan_glmer(r_control ~ r_risk_binary + (r_risk_binary | partid),
#                       data = study_long %>% filter(r_risk_binary != "neutral") %>%
#                         mutate(r_control = case_when(r_control == "controllable" ~ 1L,
#                                                      r_control == "uncontrollable" ~ 0L,
#                                                     TRUE ~ NA_integer_)),
#                       family = binomial(), chains = 4, iter = 8000)
# 
# # mixed effects model predicting sentiment using pro and contra as predictor
# snt_mod <- stan_lmer(sentiment ~ r_risk_binary + (r_risk_binary | partid),
#                       data = study_long %>% filter(r_risk_binary != "neutral"),
#                      chains = 4, iter = 8000)
# ordinal mixed effects model predicting frequency using pro and contra as predictor
# freq_mod <- brm(r_frequency ~ r_risk_binary + (r_risk_binary|partid),
#                 data = study_long %>% filter(r_risk_binary != "neutral") %>%
#                   mutate(r_frequency = factor(r_frequency, ordered = TRUE,
#                                               levels = c("day", "week", "month",
#                                                          "year", "less_reg",
#                                                          "less_once"))),
#                 family=cumulative("logit"), chains = 4, iter = 8000)
# 
# # save model outputs so they don't have to be rerun again
# save(soc_mod, sit_mod, act_mod, cnt_mod, snt_mod, freq_mod,
#      file = "data/study_1/contents_models.RData")
# 
# load model outputs
load("data/study_1/contents_models.RData")

# Inspect model outputs

# social comparison
fixef(soc_mod)
posterior_interval(soc_mod, prob = .95)[1:2,]
psycho::analyze(soc_mod)
# estimated probabilities for contra- and pro-aspects
exp(c(fixef(soc_mod)[1], sum(fixef(soc_mod)))) /
  (1 + exp( c(fixef(soc_mod)[1], sum(fixef(soc_mod)))))

# personal experience
fixef(sit_mod)
posterior_interval(sit_mod, prob = .95)[1:2,]
psycho::analyze(sit_mod)
# estimated probabilities for contra- and pro-aspects
exp(c(fixef(sit_mod)[1], sum(fixef(sit_mod)))) /
  (1 + exp( c(fixef(sit_mod)[1], sum(fixef(sit_mod)))))

# active vs passive
fixef(act_mod)
posterior_interval(act_mod, prob = .95)[1:2,]
psycho::analyze(act_mod)
# estimated probabilities for contra- and pro-aspects
exp(c(fixef(act_mod)[1], sum(fixef(act_mod)))) /
  (1 + exp( c(fixef(act_mod)[1], sum(fixef(act_mod)))))

# controllable vs uncontrollable
fixef(cnt_mod)
posterior_interval(cnt_mod, prob = .95)[1:2,]
psycho::analyze(cnt_mod)
# estimated probabilities for contra- and pro-aspects
exp(c(fixef(cnt_mod)[1], sum(fixef(cnt_mod)))) /
  (1 + exp( c(fixef(cnt_mod)[1], sum(fixef(cnt_mod)))))

# sentiment
fixef(snt_mod)
posterior_interval(snt_mod, prob = .95)[1:2,]
psycho::analyze(snt_mod)
# estimated means for contra- and pro-aspects
c(fixef(snt_mod)[1], sum(fixef(snt_mod)))
tapply(study_long$sentiment, study_long$r_risk_binary, mean, na.rm = TRUE)

# frequency
summary(freq_mod)
fixef(freq_mod)

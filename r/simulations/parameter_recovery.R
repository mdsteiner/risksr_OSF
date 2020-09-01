library(tidyverse)
library(data.table)
library(rstanarm)
library(patchwork)
library(Rcpp)
sourceCpp("cpp/vum_functions.cpp")

### Load Data ==================================================================

study_long <- read_csv("data/study_1/study_long.csv") %>% 
  select(partid, r_risk)
study <- read_csv("data/study_1/study.csv") %>% 
  select(partid, n_seeking, n_avoiding, SMRD)

### parameter recovery VUM =====================================================

phi <- seq(.1, 3, by = .025)
phi_recov <- vector("double", length(phi))

partid <- study_long %>%
  arrange(partid) %>%
  select(partid) %>%
  pull()

r_risk <- study_long %>%
  arrange(partid) %>%
  select(r_risk) %>%
  pull()

fitruns <- 10

error <- vector("double", length(phi))
rat_cors <- vector("double", length(phi))

for (rr in seq_along(phi)) {
  
  # get and save predictions for VUM
  rating <- study_long %>%
    group_by(partid) %>%
    summarize(pred_rating = vum(r_risk, phi[rr])) %>%
    ungroup() %>%
    arrange(partid) %>%
    select(pred_rating) %>%
    pull()
  
  
  ll <- Inf
  for (i in 1:fitruns) {
    fit_i <- nlminb(start = abs(rnorm(1, mean = 1, sd = 0.4)), objective = loglik,
                    lower = 0.001, upper = 10, partid = partid,
                    r_risk = r_risk, rating = rating)
    if (fit_i$objective < ll) {
      phi_i <- fit_i$par
    }
    
  }
  
  phi_recov[rr] <- phi_i
  
  rating_recov <- study_long %>%
    group_by(partid) %>%
    summarize(pred_rating = vum(r_risk, phi_i)) %>%
    ungroup() %>%
    arrange(partid) %>%
    select(pred_rating) %>%
    pull()
  
  error[rr] <- mean(abs(rating - rating_recov))
  rat_cors[rr] <- cor(rating, rating_recov)
  
}

pdf("plots/simulations/recovery_vum.pdf", height = 4, width = 5)
ggplot(data.frame(phi, phi_recov), aes(phi, phi_recov)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, color = "gray") +
  geom_point() +
  coord_cartesian(xlim = c(0,3), ylim = c(0, 3)) +
  labs(x = expression(phi), y = expression(phi~recovered)) +
  ggpubr::theme_pubr() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )
dev.off()

hist(error)
hist(rat_cors)
mean(rat_cors)

### parameter recovery QT =====================================================

b_seeking <- seq(-3, 3, by = .4)
b_avoiding <- seq(-3, 3, by = .4)
b_SMRD <- seq(-3, 3, by = .4)
b_0 <- seq(-3, 3, by = .4)
bs <- expand.grid(b_0, b_seeking, b_avoiding, b_SMRD)
bs_recov <- matrix(0, nrow = nrow(bs), ncol = 4)

error_qt <- vector("double", length(phi))
rat_cors_qt <- vector("double", length(phi))

for (rr in seq_len(nrow(bs))) {
  
  # get and save predictions for VUM
  study <- study %>%
    mutate(rating = bs[rr, 1] + bs[rr, 2] * n_seeking + bs[rr, 3] * n_avoiding +
             bs[rr, 4] * SMRD)
  
  mod_i <- lm(rating ~ n_seeking + n_avoiding + SMRD, data = study)
  
  bs_recov[rr,] <- coef(mod_i)
  
  error_qt[rr] <- mean(abs(study$rating - fitted(mod_i)))
  rat_cors_qt[rr] <- cor(study$rating, fitted(mod_i))
}

qt_recov <- bs %>% 
  as.data.frame() %>% 
  cbind(bs_recov)

names(qt_recov) <- c("b_0", "b_seeking", "b_avoiding", "b_SMRD", "b_0_recov",
                     "b_seeking_recov", "b_avoiding_recov", "b_SMRD_recov")

p1 <- ggplot(qt_recov, aes(b_0, b_0_recov)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, color = "gray") +
  geom_point() +
  coord_cartesian(xlim = c(-3,3), ylim = c(-3, 3)) +
  labs(x = expression(b[0]), y = expression(b[0]~recovered)) +
  ggpubr::theme_pubr() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

p2 <- ggplot(qt_recov, aes(b_seeking, b_seeking_recov)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, color = "gray") +
  geom_point() +
  coord_cartesian(xlim = c(-3,3), ylim = c(-3, 3)) +
  labs(x = expression(b[seeking]), y = expression(b[seeking]~recovered)) +
  ggpubr::theme_pubr() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

p3 <- ggplot(qt_recov, aes(b_avoiding, b_avoiding_recov)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, color = "gray") +
  geom_point() +
  coord_cartesian(xlim = c(-3,3), ylim = c(-3, 3)) +
  labs(x = expression(b[avoiding]), y = expression(b[avoiding]~recovered)) +
  ggpubr::theme_pubr() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )

p4 <- ggplot(qt_recov, aes(b_SMRD, b_SMRD_recov)) +
  geom_abline(intercept = 0, slope = 1, lty = 2, color = "gray") +
  geom_point() +
  coord_cartesian(xlim = c(-3,3), ylim = c(-3, 3)) +
  labs(x = expression(b[SMRD]), y = expression(b[SMRD]~recovered)) +
  ggpubr::theme_pubr() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16,face = "bold")
  )


pdf("plots/simulations/recovery_qt.pdf", width = 14, height = 4)
p1 + p2 + p3 + p4 + plot_layout(nrow = 1)
dev.off()

hist(error_qt)
hist(rat_cors_qt)
mean(rat_cors_qt)

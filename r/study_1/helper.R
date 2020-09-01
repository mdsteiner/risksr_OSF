Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)][1]
}


cross_val <- function(data_long, data, fitruns = 10, k = 5, r2 = "cor") {
  
  
  # Prepare objects for data analysis
  indices <- createFolds(data$rating, k = k)
  
  pred_list <- list()
  pars <- tibble("QT_Intercept" = NA, "QT_ns" = NA, "QT_na" = NA, "QT_SMRD" = NA,
                 "QT_Intercept_lower" = NA, "QT_ns_lower" = NA, "QT_na_lower" = NA,
                 "QT_SMRD_lower" = NA, "QT_Intercept_upper" = NA, "QT_ns_upper" = NA,  
                 "QT_na_upper" = NA, "QT_SMRD_upper" = NA, "phi" = NA)
  QT_par_names <- names(pars)[-13]
  
  rsq_df <- tibble("QT" = NA, "EXTREME" = NA, "FIRST" = NA, "LAST" = NA, "SUM" = NA,
                   "VUM" = NA)
  
  # loop through test sets
  for (ind in 1:length(indices)) {
    # vector with test set participant ids
    test_part <- data %>% slice(indices[[ind]]) %>% select(partid) %>% pull()
    
    # create test and training sets
    training_long <- data_long %>% filter(!(partid %in% test_part))
    training <- data %>% filter(!(partid %in% test_part))
    test_long <- data_long %>% filter(partid %in% test_part)
    test <- data %>% filter(partid %in% test_part)
    
    # get predictions for heuristic models without free parameters
    pred_df <- test_long %>%
      group_by(partid) %>%
      summarise(
        pred_EXTREME = if_else(length(r_risk[which.max(abs(r_risk))]) >= 1 &&
                                 abs(sum(sign(r_risk[which.max(abs(r_risk))]))) !=
                                 length(r_risk[which.max(abs(r_risk))]), 0,
                               r_risk[which.max(abs(r_risk))][1]),
        pred_FIRST = r_risk[1],
        pred_LAST = tail(r_risk, 1),
        pred_SUM = sum(r_risk)
      ) %>%
      ungroup() %>%
      arrange(partid)
    
    # fit QT
    qt_mod <- stan_glm(rating ~ n_seeking + n_avoiding + SMRD,
                       chains = 4,
                       iter = 5000,
                       data = training,
                       family = gaussian(),
                       prior_intercept = normal(location = 0, scale = 10),
                       prior = normal(location = 0, scale = 2.5))
    
    # store QT parameter values
    pars[ind, QT_par_names] <- c(qt_mod$coefficients,
                                 qt_mod$stan_summary[1:4, c("2.5%", "97.5%")])
    
    # get and store predictions for QT
    pred_df$pred_QT <- pars[["QT_Intercept"]][ind] +
      pars[["QT_ns"]][ind] * test$n_seeking[order(test$partid)] +
      pars[["QT_na"]][ind] * test$n_avoiding[order(test$partid)] +
      pars[["QT_SMRD"]][ind] * test$SMRD[order(test$partid)]
    
    # fit VUM
    ll <- Inf
    for (i in 1:fitruns) {
      fit_i <- nlminb(start = abs(rnorm(1, mean = 1, sd = 0.4)), objective = loglik,
                      lower = 0, upper = 10, partid = training_long$partid,
                      r_risk = training_long$r_risk, rating = training_long$rating)
      if (fit_i$objective < ll) {
        phi <- fit_i$par
      }
      
    }
    
    pars[ind, "phi"] <- phi
    
    # get and save predictions for VUM
    pred_df$pred_VUM <- test_long %>%
      group_by(partid) %>%
      summarize(pred_VUM = vum(r_risk, phi)) %>%
      ungroup() %>%
      arrange(partid) %>%
      select(pred_VUM) %>%
      pull()
    
    pred_df$criterion <- test$rating[order(test$partid)]
    
    # compute Rsquared
    rsq_df[ind,] <- apply(pred_df[, c("pred_QT", "pred_EXTREME", "pred_FIRST",
                                      "pred_LAST", "pred_SUM", "pred_VUM")],
                          2, function(x, crit, r2){
                            if (r2 == "var") {
                              1 - sum((crit - x) ** 2) / sum((crit - mean(crit)) ** 2)
                            } else if (r2 == "cor") {
                              cor(x, crit) ** 2
                            }
                          }, crit = pred_df$criterion, r2 = r2)
    
    
    
    pred_list[[ind]] <- pred_df
  }
  
  # check correlations of predictions
  predictions <- do.call(rbind, pred_list)
  predictions %>% select(-partid) %>% pairs.panels()
  
  # get mean Rsquared per model
  m_rsq <- colMeans(rsq_df)
  
  # get adjusted mean Rsquared
  n_par <- c(3, rep(0, 4), 1)
  adj_rsqs <- 1 - (1 - m_rsq) * (nrow(data) - 1) / (nrow(data) - n_par - 1)
  
  out <- list(
    parameters = pars,
    predictions = predictions,
    m_rsq = m_rsq,
    adj_rsqs = adj_rsqs,
    rsq_df = rsq_df
  )
  
}

cross_val2 <- function(data_long, data, fitruns = 10, k = 5, r2 = "cor") {
  
  # data_long = study_long_s1
  # data = study_s1
  # fitruns = 10
  # k = 5
  # r2 = "cor"
  # Prepare objects for data analysis
  indices <- createFolds(data$rating, k = k)
  
  data <- data %>% 
    mutate(rating_f = factor(rating, levels = 0:10, ordered = TRUE))
  
  pred_list <- list()
  pars <- data.frame("QT_Intercept" = NA, "QT_ns" = NA, "QT_na" = NA, "QT_SMRD" = NA,
                 "QT_Intercept_lower" = NA, "QT_ns_lower" = NA, "QT_na_lower" = NA,
                 "QT_SMRD_lower" = NA, "QT_Intercept_upper" = NA, "QT_ns_upper" = NA,  
                 "QT_na_upper" = NA, "QT_SMRD_upper" = NA, "phi" = NA)
  QT_par_names <- names(pars)[-13]
  
  rsq_df <- data.frame("QT" = NA, "EXTREME" = NA, "FIRST" = NA, "LAST" = NA, "SUM" = NA,
                   "VUM" = NA, "LM_SoE" = NA, "LM_WoE" = NA)
  
  # loop through test sets
  for (ind in 1:length(indices)) {
    # vector with test set participant ids
    test_part <- data %>% slice(indices[[ind]]) %>% select(partid) %>% pull()
    
    # create test and training sets
    training_long <- data_long %>% filter(!(partid %in% test_part))
    training <- data %>% filter(!(partid %in% test_part))
    test_long <- data_long %>% filter(partid %in% test_part)
    test <- data %>% filter(partid %in% test_part)
    
    # get predictions for heuristic models without free parameters
    pred_df <- test_long %>%
      group_by(partid) %>%
      summarise(
        pred_EXTREME = if_else(length(r_risk[which.max(abs(r_risk))]) >= 1 &&
                                 abs(sum(sign(r_risk[which.max(abs(r_risk))]))) !=
                                 length(r_risk[which.max(abs(r_risk))]), 0,
                               r_risk[which.max(abs(r_risk))][1]),
        pred_FIRST = r_risk[1],
        pred_LAST = tail(r_risk, 1),
        pred_SUM = sum(r_risk)
      ) %>%
      ungroup() %>%
      arrange(partid) %>% 
      as.data.frame()
    
    # fit QT
    qt_mod <- stan_glm(rating ~ n_seeking + n_avoiding + SMRD,
                       chains = 4,
                       iter = 5000,
                       data = training,
                       family = gaussian(),
                       prior_intercept = normal(location = 0, scale = 10),
                       prior = normal(location = 0, scale = 2.5))
    
    # store QT parameter values
    pars[ind, QT_par_names] <- c(qt_mod$coefficients,
                                 qt_mod$stan_summary[1:4, c("2.5%", "97.5%")])
    
    # get and store predictions for QT
    pred_df$pred_QT <- pars[["QT_Intercept"]][ind] +
      pars[["QT_ns"]][ind] * test$n_seeking[order(test$partid)] +
      pars[["QT_na"]][ind] * test$n_avoiding[order(test$partid)] +
      pars[["QT_SMRD"]][ind] * test$SMRD[order(test$partid)]
    
    
    # fit soe model
    soe_mod <- stan_polr(rating_f ~ m_risk,
                         data = training, 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 5000, chains = 4)
    
    soe_pred <- soe_mod %>% 
      posterior_predict(newdata = test) %>% 
      apply(2, Mode) %>% 
      unlist() %>% 
      as.numeric()
    
    # get and store predictions for soe model
    pred_df$pred_LM_soe <- soe_pred[order(test$partid)]
    
    # fit woe model
    woe_mod <- stan_polr(rating_f ~ m_aspects,
                         data = training, 
                         prior = rstanarm::R2(.25, what = "median"),
                         prior_counts = dirichlet(rep(1, 11)),
                         iter = 2000, chains = 4)
    
    woe_pred <- woe_mod %>% 
      posterior_predict(newdata = test) %>% 
      apply(2, Mode) %>% 
      unlist() %>% 
      as.numeric()
    
    # get and store predictions for woe model
    pred_df$pred_LM_woe <- woe_pred[order(test$partid)]
    
    # fit VUM
    ll <- Inf
    for (i in 1:fitruns) {
      fit_i <- nlminb(start = abs(rnorm(1, mean = 1, sd = 0.4)), objective = loglik,
                      lower = 0, upper = 10, partid = training_long$partid,
                      r_risk = training_long$r_risk, rating = training_long$rating)
      if (fit_i$objective < ll) {
        phi <- fit_i$par
      }
      
    }
    
    pars[ind, "phi"] <- phi
    
    # get and save predictions for VUM
    pred_df$pred_VUM <- test_long %>%
      group_by(partid) %>%
      summarize(pred_VUM = vum(r_risk, phi)) %>%
      ungroup() %>%
      arrange(partid) %>%
      select(pred_VUM) %>%
      pull()
    
    pred_df$criterion <- test$rating[order(test$partid)]
    
    # compute Rsquared
    rsq_df[ind,] <- apply(pred_df[, c("pred_QT", "pred_EXTREME", "pred_FIRST",
                                      "pred_LAST", "pred_SUM", "pred_VUM",
                                      "pred_LM_soe", "pred_LM_woe")],
                          2, function(x, crit, r2){
                            if (r2 == "var") {
                              1 - sum((crit - x) ** 2) / sum((crit - mean(crit)) ** 2)
                            } else if (r2 == "cor") {
                              cor(x, crit, method = "spearman")
                            }
                          }, crit = pred_df$criterion, r2 = r2)
    
    
    
    pred_list[[ind]] <- pred_df
  }
  
  # check correlations of predictions
  predictions <- do.call(rbind, pred_list)
  predictions %>% select(-partid) %>% pairs.panels()
  
  # get mean Rsquared per model
  m_rsq <- colMeans(rsq_df)
  
  out <- list(
    parameters = pars,
    predictions = predictions,
    m_rsq = m_rsq,
    rsq_df = rsq_df
  )
  
}

# function for robustness test of findings of study 1
rob_s1 <- function(data_long_s1, data_s1, data_long_s2, data_s2, fitruns = 10,
                   r2 = "cor") {
  
  # Prepare objects for data analysis
  pred_list <- list()
  pars <- tibble("QT_Intercept" = NA, "QT_ns" = NA, "QT_na" = NA, "QT_SMRD" = NA,
                 "QT_Intercept_lower" = NA, "QT_ns_lower" = NA, "QT_na_lower" = NA,
                 "QT_SMRD_lower" = NA, "QT_Intercept_upper" = NA, "QT_ns_upper" = NA,  
                 "QT_na_upper" = NA, "QT_SMRD_upper" = NA, "phi" = NA)
  QT_par_names <- names(pars)[-13]
  
  
  # get predictions for heuristic models without free parameters
  pred_df <- data_long_s2 %>%
    group_by(partid) %>%
    summarise(
      pred_EXTREME = if_else(length(r_risk[which.max(abs(r_risk))]) >= 1 &&
                               abs(sum(sign(r_risk[which.max(abs(r_risk))]))) !=
                               length(r_risk[which.max(abs(r_risk))]), 0,
                             r_risk[which.max(abs(r_risk))][1]),
      pred_FIRST = r_risk[1],
      pred_LAST = tail(r_risk, 1),
      pred_SUM = sum(r_risk)
    ) %>%
    ungroup() %>%
    arrange(partid)
  
  # fit QT
  qt_mod <- stan_glm(rating ~ n_seeking + n_avoiding + SMRD,
                     chains = 4,
                     iter = 5000,
                     data = data_s1,
                     family = gaussian(),
                     prior_intercept = normal(location = 0, scale = 10),
                     prior = normal(location = 0, scale = 2.5))
  
  # store QT parameter values
  pars[1, QT_par_names] <- c(qt_mod$coefficients,
                             qt_mod$stan_summary[1:4, c("2.5%", "97.5%")])
  
  # get and store predictions for QT
  pred_df$pred_QT <- pars[["QT_Intercept"]] +
    pars[["QT_ns"]] * data_s2$n_seeking[order(data_s2$partid)] +
    pars[["QT_na"]] * data_s2$n_avoiding[order(data_s2$partid)] +
    pars[["QT_SMRD"]] * data_s2$SMRD[order(data_s2$partid)]
  
  # fit VUM
  ll <- Inf
  for (i in 1:fitruns) {
    fit_i <- nlminb(start = abs(rnorm(1, mean = 1, sd = 0.4)), objective = loglik,
                    lower = 0, upper = 10, partid = data_long_s1$partid,
                    r_risk = data_long_s1$r_risk, rating = data_long_s1$rating)
    if (fit_i$objective < ll) {
      phi <- fit_i$par
    }
  }
  
  pars[1, "phi"] <- phi
  
  # get and save predictions for VUM
  pred_df$pred_VUM <- data_long_s2 %>%
    group_by(partid) %>%
    summarize(pred_VUM = vum(r_risk, phi)) %>%
    ungroup() %>%
    arrange(partid) %>%
    select(pred_VUM) %>%
    pull()
  
  pred_df$criterion <- data_s2$rating[order(data_s2$partid)]
  
  # compute Rsquared
  rsqs <- apply(pred_df[, c("pred_QT", "pred_EXTREME", "pred_FIRST",
                            "pred_LAST", "pred_SUM", "pred_VUM")],
                2, function(x, crit, r2){
                  if (r2 == "var") {
                    1 - sum((crit - x) ** 2) / sum((crit - mean(crit)) ** 2)
                  } else if (r2 == "cor") {
                    cor(x, crit) ** 2
                  }
                }, crit = pred_df$criterion, r2 = r2)
  
  
  # check correlations of predictions
  predictions <- pred_df
  predictions %>% select(-partid) %>% pairs.panels()
  
  # get adjusted mean Rsquared
  n_par <- c(3, rep(0, 4), 1)
  adj_rsqs <- 1 - (1 - rsqs) * (nrow(data_s2) - 1) / (nrow(data_s2) - n_par - 1)
  
  out <- list(
    parameters = pars,
    predictions = predictions,
    adj_rsqs = adj_rsqs,
    rsqs = rsqs
  )
  
}

# get predicionts
pred_acc <- function(data_long, data, phi, QT_Intercept, QT_ns, QT_na, QT_SMRD,
                     soe_mod, woe_mod, r2 = "cor") {
  
  data <- data %>% 
    mutate(rating_f = factor(rating, levels = 0:10, ordered = TRUE))
  
  # get predictions for heuristic models without free parameters
  pred_df <- data_long %>%
    group_by(partid) %>%
    summarise(
      pred_EXTREME = if_else(length(r_risk[which.max(abs(r_risk))]) >= 1 &&
                               abs(sum(sign(r_risk[which.max(abs(r_risk))]))) !=
                               length(r_risk[which.max(abs(r_risk))]), 0,
                             r_risk[which.max(abs(r_risk))][1]),
      pred_FIRST = r_risk[1],
      pred_LAST = tail(r_risk, 1),
      pred_SUM = sum(r_risk)
    ) %>%
    ungroup() %>%
    arrange(partid)
  
  # get and store predictions for QT
  pred_df$pred_QT <- QT_Intercept + QT_ns * data$n_seeking[order(data$partid)] +
    QT_na * data$n_avoiding[order(data$partid)] + QT_SMRD * data$SMRD[order(data$partid)]
  
  
  # get and store predictions for soe mod
  woe_pred <- woe_mod %>% 
    posterior_predict(newdata = data) %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  # get and store predictions for woe model
  pred_df$pred_LM_woe <- woe_pred[order(data$partid)]
  
  
  # get and store predictions for soe mod
  soe_pred <- soe_mod %>% 
    posterior_predict(newdata = data) %>% 
    apply(2, Mode) %>% 
    unlist() %>% 
    as.numeric()
  
  # get and store predictions for soe model
  pred_df$pred_LM_soe <- soe_pred[order(data$partid)]
  
  # get and save predictions for VUM
  pred_df$pred_VUM <- data_long %>%
    group_by(partid) %>%
    summarize(pred_VUM = vum(r_risk, phi)) %>%
    ungroup() %>%
    arrange(partid) %>%
    select(pred_VUM) %>%
    pull()
  
  pred_df$criterion <- data$rating[order(data$partid)]
  
  # compute Rsquared
  rsqs <- apply(pred_df[, c("pred_QT", "pred_EXTREME", "pred_FIRST",
                            "pred_LAST", "pred_SUM", "pred_VUM",
                            "pred_LM_soe", "pred_LM_woe")],
                2, function(x, crit, r2){
                  if (r2 == "var") {
                    1 - sum((crit - x) ** 2) / sum((crit - mean(crit)) ** 2)
                  } else if (r2 == "cor") {
                    cor(x, crit, method = "spearman")
                  }
                }, crit = pred_df$criterion, r2 = r2)
  
  
  # check correlations of predictions
  predictions <- pred_df
  
  
  out <- list(
    predictions = predictions,
    rsqs = rsqs
  )
  
}


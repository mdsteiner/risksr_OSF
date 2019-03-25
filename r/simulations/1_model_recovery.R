rm(list=ls(all=TRUE)) # Empty workspace
options(warn=-1) # dont show warnings

library(parallel)
library(itertools)
library(foreach)
library(utils)
library(data.table)
library(rstanarm)
library(truncnorm)
library(Rcpp)

source("r/simulations/models_risksr.R")
sourceCPP("cpp/vum_functions.cpp")
source("r/simulations/rescale.R")
source("r/simulations/loglik_fun.R")
source("r/simulations/calculate_SMRD.R")

## Setup parallel processing: assign cpu cores
nc = detectCores()

## Platform switch (linux/windows/mac)
if (.Platform$OS.type == "windows") {
  library(doParallel)
  # outfile = "" -> worker output is printed to the main console
  cl = makeCluster(nc, outfile = "outfile.txt") 
  registerDoParallel(cl)
  cat("\n\nRegistered ", nc, " Cores on windows","\n\n")
} else { 
  if (.Platform$OS.type %in% c("mac","unix")) {
    library(doMC)
    # outfile = "" -> worker output is printed to the main
    cl = makeCluster(nc, outfile = "")  console
    registerDoMC(cores=nc)
    if(getDoParWorkers()!=nc) { stop("doMC does not work with all cores") }
    cat("\n\nRegistered ", nc, " Cores on", .Platform$OS.typ,"\n\n")
  }
}

## pass functions, to clusters
clusterEvalQ(cl, {
  library(utils)
  library(data.table)
  library(rstanarm)
  library(truncnorm)
  library(Rcpp)
  # Add all of our custom functions here
  source("r/simulations/models_risksr.R")
  sourceCpp("cpp/vum_functions.cpp")
  source("r/simulations/rescale.R")
  source("r/simulations/loglik_fun.R")
  source("r/simulations/calculate_SMRD.R")
})

## set seed
clusterSetRNGStream(cl = cl, iseed = 924)

## Define variables here
# Define typical number of aspects and their riskiness based on pilot
aspects <- fread("data/pilot/aspects.csv",
                 select = c("partid", "aspect_ind", "r_risk"))
aspects[, r_risk := as.numeric(r_risk)]
r_risk_prob <- aspects[, prop.table(table(r_risk))]

aspects <- aspects[, .(n_aspects = length(aspect_ind)), by = partid]
aspects <- aspects[, prop.table(table(n_aspects))]

# Define how many simulations, sample sizes, and model_space
n_sim <- 1000
Ns <- c(150, 200, 250)
model_space <- fitting_model_space <- c("qt", "vu", "mostext", "takethefirst",
                                        "takethelast", "tally", "sumofevid",
                                        "meanofevid")

# Error sd's
sds <- 0:3

# Simulation: split simulations vector to all nc cores
foreach(sims = ihasNext(isplitVector(1:n_sim, chunks=nc)),
        .combine = rbind,
        # Include all variables that we want to clusters to know
        .export = c("aspects", "Ns", "model_space", "fitting_model_space"),
        # Include all packages that the clusters need to know
        .packages = c("data.table", "parallel", "itertools", "utils", "rstanarm")) %dopar% {
          
          # Write result template for model recovery
          write.table(x = data.frame(i.sim = NA,
                                     N = NA,
                                     simulating.model = NA,
                                     fitting.model = NA,
                                     test.predictions = NA,
                                     test.ratings = NA,
                                     error_sd = NA,
                                     r2 = NA,
                                     loglik = NA),
                      sep = ";", 
                      row.names = FALSE,
                      col.names = TRUE,
                      file = "data/simulations/model_recovery/model_recov.csv",
#                      file = paste0("../Data/results/model_recov_", paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), ".csv"),
                      append = FALSE)

          # Write result template for parameter recovery
          write.table(x = data.frame(i.sim = NA,
                                     N = NA,
                                     simulating.model = NA,
                                     fitting.model = NA,
                                     error_sd = NA,
                                     true_phi = NA,
                                     true_beta_n_seeking = NA,
                                     true_beta_n_avoiding = NA,
                                     true_beta_SMRD = NA,
                                     fitted_phi = NA,
                                     fitted_beta_n_seeking = NA,
                                     fitted_beta_n_avoiding = NA,
                                     fitted_beta_SMRD = NA),
                      sep = ";", 
                      row.names = FALSE,
                      col.names = TRUE,
                      file = "data/simulations/parameter_recovery/parameter_recov.csv",
#                      file = paste0("../Data/results/parameter_recov_", paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), ".csv"),
                      append = FALSE)
          
          # Write table for power simulation
          write.table(x = data.frame(predictor = NA,
                                     HDI_low = NA,
                                     median_posterior = NA,
                                     HDI_high = NA,
                                     beta_pos_aspects = NA,
                                     beta_neg_aspects = NA,
                                     beta_SMRD = NA,
                                     med_r2 = NA,
                                     error_sd = NA,
                                     sim = NA),
                      sep = ";",
                      row.names = FALSE,
                      col.names = TRUE,
                      file = "data/simulations/power_simulations/power_sim.csv",
                      # file = paste0("../Data/results/power_sim_", paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), ".csv"),
                      append = FALSE)
          
          # Write table for learningset predictions and ratings
          write.table(x = data.frame(i.sim = NA,
                                     N = NA,
                                     simulating.model = NA,
                                     pred_raw = NA,
                                     pred_noise = NA,
                                     pred_noise_rescale = NA,
                                     error_sd = NA),
                      sep = ";", 
                      row.names = FALSE,
                      col.names = TRUE,
                      file = "data/simulations/learningset_predictions/predictions.csv",
                      # file = paste0("../Data/results/predictions_", paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), ".csv"),
                      append = FALSE)
          
          # Loop over all simulations
          ll.pred <- lapply(sims, FUN = function(i.sim) {
            
            # Loop over all sample sizes
            ll.pred <- lapply(Ns, FUN = function(N) {

              # Define how many aspects each participant listed based on pilot data
              n_aspects <- sample(as.numeric(names(aspects)), N, replace = TRUE, 
                                  prob = aspects)
              
              # Create data: partid, aspect_index, r_risk (riskiness of aspect),
              # r_risk_binary, and SMRD
              data <- data.table(partid = rep(1:N, times = n_aspects),
                                 r_risk = sample(as.numeric(names(r_risk_prob)),
                                                 sum(n_aspects), replace = TRUE,
                                                 prob = r_risk_prob),
                                 aspect_ind = unlist(lapply(n_aspects, function(i) 1:i)))
              
              data[, r_risk_binary := cut(r_risk, breaks = c(0, 49, 50, 100),
                                          labels = c("avoiding", "neutral", "seeking"),
                                          include.lowest = TRUE)]
              data <- merge(data, calculate_SMRD(data), by = "partid")
              
              # Loop over model_space
              ll.pred <- lapply(model_space, FUN = function(simulating.model) {
                
                phi <- b_seeking <- b_avoiding <- b_SMRD <- NA
                # Define phi if model is vu
                if(simulating.model == "vu") {
                  phi <- runif(n = 1, min = 0.1, max = 1.9)
                }
                if(simulating.model == "qt") {
                  b_seeking = runif(n = 1, min = 0, max = 3) 
                  b_avoiding = runif(n = 1, min = -3, max = 0)
                  b_SMRD = runif(n = 1, min = 0, max = 3)
                }
                
                ll.pred <- lapply(sds, FUN = function(i.sd) {
                  # Make predictions
                  predictions <- models_risksr(data, model = simulating.model,
                                               phi = phi, b_seeking = b_seeking,
                                               b_avoiding = b_avoiding,
                                               b_SMRD = b_SMRD, n = 1)
                  predictions$prediction.noise <- rnorm(n = nrow(predictions),
                                                        mean = predictions$prediction,
                                                        sd = i.sd)
                  predictions$prediction.noise.rescale <- round(rescale(predictions$prediction.noise,
                                                                        from = 0, to = 10))
                  
                  write.table(x = data.frame(i.sim = i.sim,
                                             N = N,
                                             simulating.model = simulating.model,
                                             pred_raw = predictions$prediction,
                                             pred_noise = predictions$prediction.noise,
                                             pred_noise_rescale = predictions$prediction.noise.rescale,
                                             error_sd = i.sd),
                              sep = ";", 
                              row.names = FALSE,
                              col.names = FALSE,
                              file = "data/simulations/learningset_predictions/predictions.csv",
                              # file = paste0("../Data/results/predictions_", paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), ".csv"),
                              append = TRUE)

                  predictions <- predictions[, c("partid", "prediction.noise.rescale")]
                  colnames(predictions) <- c("partid", "rating")

                  data[, rating := NULL]
                  data <- merge(data, predictions, by = "partid") 
                  
                  # Split into learning and testset
                  learningset <- data[partid %in% 1:(N/2), ]
                  testset <- data[!learningset, ]
                  
                  # Loop over fitting_model_space
                  out <- lapply(fitting_model_space, FUN = function(fitting.model) {
                    # Create opt pars vector which is to be filled if fitting model is vu or qt
                    opt_pars <- c("phi" = NA, "n_seeking" = NA,
                                  "n_avoiding" = NA, "SMRD" = NA)
                    
                    # Fitting procedure 
                    if(fitting.model == "vu") {
                      fit <- nlminb(start = rnorm(1, mean = 1, sd = 0.4),
                                    objective = loglik, lower = 0, upper = 10,
                                    partid = learningset$partid,
                                    r_risk = learningset$r_risk,
                                    rating = learningset$rating)
                      opt_pars["phi"] <- fit$par
                    }
                    if(fitting.model == "qt") {
                      qt_data <- learningset[, .(rating = unique(rating),
                                                 n_seeking = sum(r_risk_binary == "seeking"),
                                                 n_avoiding = sum(r_risk_binary == "avoiding"),
                                                 SMRD = unique(SMRD)), by = partid]
                      # opt_par <- qt_data[, lm(rating ~ n_seeking + n_avoiding + SMRD)]$coefficients
                      mod_rstan <- stan_glm(
                        chains = 3,
                        iter = 5000,
                        formula = "rating ~ n_seeking + n_avoiding + SMRD",
                        data = as.data.frame(qt_data),
                        family = gaussian(),
                        prior_intercept = normal(location = 0, scale = 10),
                        prior = normal(location = 0, scale = 2.5))
                      opt_pars[c("n_seeking", "n_avoiding", "SMRD")] <- mod_rstan$coefficients[c("n_seeking", "n_avoiding", "SMRD")]
                      
                      # R-squared
                      med_r2 <- median(bayes_R2(mod_rstan))
                      
                      if(simulating.model == "qt") {
                        # Coefficients
                        tmp_coefs <- as.data.table(mod_rstan$stan_summary[
                          c("n_seeking", "n_avoiding", "SMRD"),
                          c("2.5%", "50%", "97.5%")],
                          keep.rownames = "predictor")
                        
                        write.table(x = data.frame(predictor = tmp_coefs[, 1],
                                                   HDI_low = tmp_coefs[, 2],
                                                   median_posterior = tmp_coefs[, 3],
                                                   HDI_high = tmp_coefs[, 4],
                                                   beta_pos_aspects = b_seeking,
                                                   beta_neg_aspects = b_avoiding,
                                                   beta_SMRD = b_SMRD,
                                                   med_r2 = med_r2,
                                                   error_sd = i.sd,
                                                   sim = i.sim),
                                    sep = ";",
                                    row.names = FALSE,
                                    col.names = FALSE,
                                    file = "data/simulations/power_simulations/power_sim.csv",
                                    # file = paste0("../Data/results/power_sim_", paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), ".csv"),
                                    append = TRUE)
                      }
                      
                    }
                    
                    # Predict testset with fitted parameters
                    test.predictions <- models_risksr(testset,
                                                      model = fitting.model,
                                                      b_seeking = opt_pars["n_seeking"],
                                                      b_avoiding = opt_pars["n_avoiding"],
                                                      b_SMRD = opt_pars["SMRD"],
                                                      phi = opt_pars["phi"], n = 1)
                    test.ratings <- testset[, .(rating = unique(rating)), by = partid]
                    
                    # Calculate correlation between rating and prediction
                    r2 <- cor(test.ratings$rating, test.predictions$prediction)^2
                    
                    # Calculate log likelihood
                    log.likelihood <- -2 * sum(dnorm(x = test.ratings$rating,
                                                     mean = rescale(test.predictions$prediction,
                                                                    from = 0,
                                                                    to = 10),
                                                     sd = 1, log = TRUE))
                    
                    write.table(x = data.frame(i.sim = i.sim,
                                               N = N,
                                               simulating.model = simulating.model,
                                               fitting.model = fitting.model,
                                               test.predictions = test.predictions$prediction,
                                               test.ratings = test.ratings$rating,
                                               error_sd = i.sd,
                                               r2 = r2,
                                               loglik = log.likelihood),
                                sep = ";", 
                                row.names = FALSE,
                                col.names = FALSE,
                                file = "data/simulations/model_recovery/model_recov.csv",
                                # file = paste0("../Data/results/model_recov_", paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), ".csv"),
                                append = TRUE)
                    
                    if(all(c(simulating.model, fitting.model) %in% c("qt", "vu"))) {
                      write.table(x = data.frame(i.sim = i.sim,
                                                 N = N,
                                                 simulating.model = simulating.model,
                                                 fitting.model = fitting.model,
                                                 error_sd = i.sd,
                                                 true_phi = phi,
                                                 true_beta_n_seeking = b_seeking,
                                                 true_beta_n_avoiding = b_avoiding,
                                                 true_beta_SMRD = b_SMRD,
                                                 fitted_phi = opt_pars["phi"],
                                                 fitted_beta_n_seeking = opt_pars["n_seeking"],
                                                 fitted_beta_n_avoiding = opt_pars["n_avoiding"],
                                                 fitted_beta_SMRD = opt_pars["SMRD"]),
                                  sep = ";", 
                                  row.names = FALSE,
                                  col.names = FALSE,
                                  file = "data/simulations/parameter_recovery/parameter_recov.csv",
                                  # file = paste0("../Data/results/parameter_recov_", paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), ".csv"),
                                  append = TRUE)
                    }
                  })
                  return(out)
                })
              })
            })
          })
        }

## stop the cluster 
stopCluster(cl)


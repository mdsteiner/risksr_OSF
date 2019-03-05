Rcpp::sourceCpp("cpp/cpp_functions.cpp")
library(truncnorm)

models_risksr <- function(data, model, b_seeking = 0.5, b_avoiding = -0.5,
                          b_SMRD = 1.5, phi = 1, n = 0) {
  # Error
  if (!(model %in% c("qt", "vu", "mostext", "takethefirst", "takethelast", "tally",
                  "sumofevid", "meanofevid"))) {
    stop("Model must be one of qt, vu, mostext, takethefirst, tally, sumofevid, meanofevid.")
  }
  if (any(!(c("partid", "r_risk", "r_risk_binary", "aspect_ind") %in% colnames(data)))) {
    stop("Data must contain columns r_risk, r_risk_binary, aspect_ind, and partid")
  }

  # Predictions
  if (model == "qt") {
    data[, .(prediction = 5 + b_seeking * sum(r_risk_binary == "seeking") +
               b_avoiding * sum(r_risk_binary == "avoiding") + b_SMRD *
               unique(SMRD)), by = partid]
  } else if (model == "vu") {
    data[, .(prediction = vum(r_risk, phi)),
         by = partid]
  } else if (model == "mostext") {
    data[, .(prediction = r_risk[which.max(abs(r_risk - mean(r_risk)))]),
         by = partid]
  } else if (model == "takethefirst") {
    data[, .(prediction = r_risk[1]), by = partid]
  } else if (model == "takethelast") {
    data[, .(prediction = tail(r_risk, 1)), by = partid]
  } else if (model == "tally") {
    data[, .(prediction = sum(r_risk_binary == "seeking") -
               sum(r_risk_binary == "avoiding")), by = partid]
  } else if (model == "sumofevid") {
    data[, .(prediction = sum(r_risk)), by = partid]
  } else if (model == "meanofevid") {
    data[, .(prediction = mean(r_risk)), by = partid]
  } 

}

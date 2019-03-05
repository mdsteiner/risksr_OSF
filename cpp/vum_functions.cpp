#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double vum(NumericVector x, double phi) {
  double vu = 0;
  double w = 1;
  
  for (double t = 0; t < x.size(); t++) {
    w = std::pow(1 / (t + 1), phi);
    vu += w * (x[t] - vu);
  }
  return vu;
}


double vum(NumericVector x, double phi);

// [[Rcpp::export]]
double loglik(double phi, StringVector partid, NumericVector r_risk,
              NumericVector rating) {
  StringVector parts = unique(partid);
  NumericVector vus(parts.size());
  for (int ind = 0; ind < parts.size(); ind++) {
    LogicalVector r(partid.size());
    for( int i = 0; i < partid.size(); i++){
      r[i] = (partid[i] == parts[ind]);
    }
    NumericVector curr_dat = r_risk[r];
    vus[ind] = vum(curr_dat, phi);
  }
  double m_rating = mean(rating);
  double sd_rating = sd(rating);
  double loglik = -2.0 * sum(dnorm(vus, m_rating, sd_rating, true));
  return loglik;
}
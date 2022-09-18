// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <math.h>
#include <stdlib.h>
using namespace Rcpp;

//[[Rcpp::export]]
double do_rgig1(double lambda, double chi, double psi) {

  if (chi == 0){
    chi = DBL_MIN; // DOUBLE_XMIN 
  }

  if ( !(R_FINITE(lambda) && R_FINITE(chi) && R_FINITE(psi)) ||
       (chi <  0. || psi < 0)      ||
       (chi == 0. && lambda <= 0.) ||
       (psi == 0. && lambda >= 0.) ) {
    throw std::bad_function_call();
  }

  double res;

  // circumvent GIGrvg in these cases
  if (chi < 11 * DBL_EPSILON) { // DOUBLE_EPS 
    /* special cases which are basically Gamma and Inverse Gamma distribution */
    if (lambda > 0.0) {
      res = R::rgamma(lambda, 2.0/psi);
    }
    else {
      res = 1.0/R::rgamma(-lambda, 2.0/chi); // fixed
    }
  }

  else if (psi < 11 * DBL_EPSILON) { // DOUBLE_EPS 
    /* special cases which are basically Gamma and Inverse Gamma distribution */
    if (lambda > 0.0) {
      res = R::rgamma(lambda, 2.0/psi);  // fixed
    }
    else {
      res = 1.0/R::rgamma(-lambda, 2.0/chi); // fixed
    }

  } else {
    SEXP (*fun)(int, double, double, double) = NULL;
    if (!fun) fun = (SEXP(*)(int, double, double, double)) R_GetCCallable("GIGrvg", "do_rgig");

    res = as<double>(fun(1, lambda, chi, psi));
  }

  return res;
}

void res_protector(double& x){
  if (std::abs(x) < DBL_MIN * std::pow(10, 10)){
    double sign = std::copysign(1, x);
    x = DBL_MIN * std::pow(10, 10) * sign;
  }
}

/*---------------------------------------------------------------------------*/
/*                                                                           */
/* Original implementation in R code (R package "Bessel" v. 0.5-3) by        */
/*   Martin Maechler, Date: 23 Nov 2009, 13:39                               */
/*                                                                           */
/* Translated into C code by Kemal Dingic, Oct. 2011.                        */
/*                                                                           */
/* Modified by Josef Leydold on Tue Nov  1 13:22:09 CET 2011                 */
/*                                                                           */
/* Translated into C++ code by Peter Knaus, Mar. 2019.                       */
/*                                                                           */
/*---------------------------------------------------------------------------*/
double unur_bessel_k_nuasympt(double x, double nu, bool islog, bool expon_scaled){
  double M_LNPI = 1.14472988584940017414342735135;
  
  double z;
  double sz, t, t2, eta;
  double d, u1t,u2t,u3t,u4t;
  double res;
  
  
  z = x / nu;
  
  sz = hypot(1,z);
  t = 1. / sz;
  t2 = t*t;
  
  if (expon_scaled){
    eta = (1./(z + sz));
  } else {
    eta = sz;
  }
  
  eta += log(z) - log1p(sz);
  u1t = (t * (3. - 5.*t2))/24.;
  u2t = t2 * (81. + t2*(-462. + t2 * 385.))/1152.;
  u3t = t*t2 * (30375. + t2 * (-369603. + t2 * (765765. - t2 * 425425.)))/414720.;
  u4t = t2*t2 * (4465125.
                   + t2 * (-94121676.
                   + t2 * (349922430.
                   + t2 * (-446185740.
                   + t2 * 185910725.)))) / 39813120.;
                   d = (-u1t + (u2t + (-u3t + u4t/nu)/nu)/nu)/nu;
                   
                   res = log(1.+d) - nu*eta - 0.5*(log(2.*nu*sz) - M_LNPI);
                   
                   if (islog){
                     return res;
                   } else {
                     return exp(res);
                   }
}

double log_ratio_value_marginalBFS(int d, double proposal, double old_val, double scale_par, arma::vec param_vec, double b1, double b2) {
  arma::vec besselKvalue_partA(d, arma::fill::none);
  arma::vec besselKvalue_partB(d, arma::fill::none);
  long double par1A = std::abs(proposal - 0.5);
  long double par1B = std::abs(old_val - 0.5);
  for (int j = 0; j < d; j++){
    double par2A = std::exp(0.5*std::log(proposal) + 0.5*std::log(scale_par) + std::log(std::abs(param_vec(j))));
    double par2B = std::exp(0.5*std::log(old_val) + 0.5*std::log(scale_par) + std::log(std::abs(param_vec(j))));
    
    
    if(par1A < 50 and par2A < 50){
      besselKvalue_partA(j) = std::log(R::bessel_k(par2A, par1A, true)) - par2A;
    }else{
      besselKvalue_partA(j) = unur_bessel_k_nuasympt(par2A, par1A, true, false);
    }
    
    if(par1B < 50 and par2B < 50){
      besselKvalue_partB(j) = std::log(R::bessel_k(par2B, par1B, true)) - par2B;
    }else{
      besselKvalue_partB(j) = unur_bessel_k_nuasympt(par2B, par1B, true, false);
    }
    
  }
  
  //gamma prior
  double partA = (b1 - 1 + 1 + d/4.0)*(std::log(proposal) - std::log(old_val));
  double partB = (d/2.0*std::log(scale_par) - d*std::log(2) -
                  b2 + arma::as_scalar(arma::sum(arma::log(arma::abs(param_vec)))))*(proposal - old_val);
  double partC = d/2.0*(std::log(proposal)*proposal - std::log(old_val)*old_val);
  double partD =  - d*(std::lgamma(proposal + 1) - log(proposal) - std::lgamma(old_val +1) + log(old_val));
  double partE = arma::as_scalar(arma::sum(besselKvalue_partA -besselKvalue_partB));
  double res = partA + partB + partC +partD + partE;
  
  return res;
}

//' @name MH_step
//' @noRd
// [[Rcpp::export]]
double MH_step(double current_val, double c_tuning_par, int d, double scale_par, arma::vec param_vec, double b, double nu,
               double hyp1, double hyp2){
  
  double b1 = nu;
  double b2 = nu * b;
  
  double old_value = current_val;
  double log_prop = R::rnorm(std::log(old_value), c_tuning_par);
  double proposal = std::exp(log_prop);
  
  double unif = R::runif(0, 1);
  
  double log_R = log_ratio_value_marginalBFS(d, proposal, old_value, scale_par, param_vec, b1, b2);
  
  double res;
  if (std::log(unif) < log_R){
    res = proposal;
  } else {
    res = old_value;
  }
  
  res_protector(res);
  
  return res;
}

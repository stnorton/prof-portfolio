//This stan code fits a model with lags done ahead of time, rather than in the 
// Stan code
//lags should be calculated manually, and the first observation in each time 
//series should be removed before data is given to stan

functions{
  
  real partial_sum(int[] n_slice,
                   int start, int end,
                   vector[] Y,
                   matrix lagvars,
                   vector[] alpha, matrix[] beta,
                   matrix[] Omega, vector[] tau,
                   int[] country)
  {
    for(i in n_slice){
      return multi_normal_cholesky_lpdf(Y[i]| alpha[country[i]] + to_matrix(beta[country[i]])*lagvars[i]',
      quad_form_diag(to_matrix(Omega[country[i]]), tau[country[i]]));
    }                 
  }
                   
  
  
}


data{
  
  int N; //# observations
  int K; //# dimensions of Y
  int C; //# of countries
  int R; //# of regions
  
  int<lower = 1, upper=C> country[N]; //country id for each obs
  int<lower = 1, upper=N> n[N]; //observation id for each obs

  vector[K] Y[N]; //the outcome array - each variable's time series stacked by region
  matrix[N,K] L; //the lagged variables
  
}

parameters{
  
  
  //individual level
  vector<lower = 0>[K] tau[C]; //scale for residuals
  matrix[K, K] z_beta[C]; //untransformed betas 
  vector[K] z_alpha[C]; //untransformed intercepts
  
  //hierarchical parameters
  cholesky_factor_corr[K] Omega[C]; //country level correlation matrix
  matrix[K, K] bhat_location; //mean for prior on beta
  matrix<lower = 0>[K, K] bhat_scale; //scale for prior on beta
  vector[K] ahat_location; //means for prior on intercepts
  vector<lower = 0>[K] ahat_scale; //variance for intercept prior
  
  
}

transformed parameters{
  
  matrix[K, K] beta[C]; //VAR(1) coefficients, country specific
  vector[K] alpha[C]; //country specific intercepts

  
  for(c in 1:C){
    //recentering random effects
    alpha[c] = ahat_location + ahat_scale .*z_alpha[c];
    beta[c] = bhat_location + bhat_scale*z_beta[c];
  }
 
}

model{
  
  int grainsize = 100;
  
  //hyperpriors
  ahat_location ~ normal(0,1);
  ahat_scale ~ cauchy(0, 1); 
  to_vector(bhat_location) ~ normal(0, 0.5);
  to_vector(bhat_scale) ~ cauchy(0, 0.5);

  
  //hierarchical priors
  for(c in 1:C){
    //non-centered paramaterization to avoid convergence issues
    z_alpha[c] ~ normal(0, 1);
    to_vector(z_beta[c]) ~ normal(0, 1);
    tau[c] ~ cauchy(0, 2.5);
    Omega[c] ~ lkj_corr_cholesky(2);
  }
  
  target+= reduce_sum_static(partial_sum, n, grainsize, Y, L,
                      alpha, beta, Omega, tau, 
                      country);
  
  
}



data {
  
  int<lower=0> N;                        // number of poll results (one separately for each poll and party)
  int<lower=0> K;                        // number of unique parties
  int<lower=0> R;                        // number of unique elections
  int<lower=0> L;                        // number of unique institutes
  int<lower=0> P;                        // number of unique polls
  int<lower=0> n_B;                      // number of rows of B-splines
  int<lower=0> KR;                       // number of unique party-election combinations

  vector<lower=0, upper=1>[N] poll;      // poll estimate for each poll and party
  vector<lower=0, upper=1>[KR] vote;     // election results for each party

  vector<lower=0>[P] n;                  // sample size

  matrix[n_B, P] B;                      // B-spline matrix
  
  int<lower=1, upper=KR> kr_id[N];       // party-election identifier
  int<lower=1, upper=R> r_id[P];         // election identifier
  int<lower=1, upper=L> l_id[P];         // institute identifier
  int<lower=1, upper=K> k_id[N];         // party identifier
  int<lower=1, upper=P> p_id[N];         // poll identifier
  
  vector[K-1] zero_r[R];                 // zero mean matrix for mvn distribution
  vector[K-1] zero_l[L];                 // zero mean matrix for mvn distribution
  vector[K] zero_rK[R];                  // zero mean matrix for mvn distribution

}

transformed data {
  
  // transform election outcome to log scale
  vector[KR] log_vote;
  log_vote = log(vote);
  
}

parameters {
  
  // party-election bias
  vector[K-1] alpha_raw[R]; // alpha for K-1 parties due to redundant parametrisation
  cholesky_factor_corr[K-1] lkj_alpha_corr; // cholesky factor of corr. matrix 
  real<lower=0> sd_tau_alpha; // sd for non-centerred common party sd
  vector<lower=0>[K-1] tau_alpha_sc; // scale for non-centerred common party sd
  
  // party-institute bias
  vector[K-1] alpha2_raw[L]; // alpha2 for K-1 parties due to redundant parametrisation
  cholesky_factor_corr[K-1] lkj_alpha2_corr; // cholesky factor of corr. matrix 
  real<lower=0> sd_tau_alpha2; // sd for non-centerred common party sd
  vector<lower=0>[K-1] tau_alpha2_sc; // scale for non-centerred common party sd 
  
  // poll variation over time
  vector[K-1] beta_raw[R,n_B]; // beta for K-1 parties due to redundant parametrisation
  cholesky_factor_corr[K-1] lkj_beta_corr; // cholesky factor of corr. matrix 
  real<lower=0> sd_tau_beta; // sd for non-centerred common party sd
  vector<lower=0>[K-1] tau_beta_sc; // scale for non-centerred common party sd
  
  // party-election variance
  vector[K] phi[R];
  cholesky_factor_corr[K] lkj_phi_corr; // cholesky factor of corr. matrix 
  real<lower=0> sd_tau_phi; // sd for non-centerred common party sd
  vector<lower=0>[K] tau_phi_sc; // scale for non-centerred common party sd
  
}

transformed parameters{
  
  // redundant parameters for all parties
  vector[K] alpha[R];
  vector[K] alpha2[L];
  vector[K] beta[R,n_B];
  
  // party specific sd for mvn distribution
  vector<lower=0>[K-1] tau_alpha; 
  vector<lower=0>[K-1] tau_alpha2; 
  vector<lower=0>[K-1] tau_beta;
  vector<lower=0>[K] tau_phi; 
  
  // matrix with b-spline coefficiants for each party and election
  matrix[KR, n_B] splines_matrix;
  
  // non-centered parametrisation for party specific sd
  tau_alpha = tau_alpha_sc * sd_tau_alpha;
  tau_alpha2 = tau_alpha2_sc * sd_tau_alpha2;
  tau_beta = tau_beta_sc * sd_tau_beta;
  tau_phi = tau_phi_sc * sd_tau_phi;
  
  // redundant parametrisation
  for(i in 1:R) {
    alpha[i] = append_row(alpha_raw[i],0);
    for(j in 1:n_B){
      beta[i,j] = append_row(beta_raw[i,j], 0);
    }
  }  
  
  for(i in 1:L) {
    alpha2[i] = append_row(alpha2_raw[i],0);
  }
    
  // reshape beta for mean computation
  for(i in 1:n_B) {
    for(j in 1:R) {
      splines_matrix[((j-1)*K+1):(j*K),i] = beta[j,i];
    }
  }
  
}

model {
  
  vector[N] log_p; // unscaled mean on log scale
  vector[N] p; // scaled mean 
  vector[N] log_sigma; // variance on log scale
  vector[N] sigma; // variance

  // hyper-priors
  sd_tau_alpha ~ normal(0, 0.2)T[0,];
  sd_tau_alpha2 ~ normal(0, 0.2)T[0,];
  sd_tau_beta ~ normal(0, 0.2)T[0,];
  sd_tau_phi ~ normal(0, 0.2)T[0,];
  
  for(i in 1:(K-1)){
    tau_alpha_sc[i] ~ normal(0, 1)T[0,];
    tau_alpha2_sc[i] ~ normal(0, 1)T[0,];
    tau_beta_sc[i] ~ normal(0, 1)T[0,];
  }
  
  for(i in 1:K){
    tau_phi_sc[i] ~ normal(0, 1)T[0,];
  }  

  // cholesky factor of correlation matrix
  lkj_alpha_corr ~ lkj_corr_cholesky(2);
  lkj_alpha2_corr ~ lkj_corr_cholesky(2);
  lkj_beta_corr ~ lkj_corr_cholesky(2);
  lkj_phi_corr ~ lkj_corr_cholesky(2);
  
  // multivariate normal priors for parameter estimates
  alpha_raw ~ multi_normal_cholesky(zero_r, diag_pre_multiply(tau_alpha, lkj_alpha_corr));
  alpha2_raw ~ multi_normal_cholesky(zero_l, diag_pre_multiply(tau_alpha2, lkj_alpha2_corr));
  for(i in 1:n_B){
    beta_raw[,i] ~ multi_normal_cholesky(zero_r, diag_pre_multiply(tau_beta, lkj_beta_corr));
  }
  phi ~ multi_normal_cholesky(zero_rK, diag_pre_multiply(tau_phi, lkj_phi_corr));

  // mean model
  for(i in 1:N){
    log_p[i] = log_vote[kr_id[i]] + 
      alpha[r_id[p_id[i]],k_id[i]] +
      alpha2[l_id[p_id[i]],k_id[i]] + 
      splines_matrix[kr_id[i],]*B[,p_id[i]];
  }
  
  for(i in 1:P){
    p[((i-1)*K+1):(i*K)] = softmax(log_p[((i-1)*K+1):(i*K)]); // rescale p to sum to 1
  }
  
  // variance model
  for(i in 1:N) {
    log_sigma[i] = log(p[i]*(1-p[i])/n[p_id[i]]) + 
    phi[r_id[p_id[i]], k_id[i]];
  }
  
  sigma = exp(log_sigma); // transform to normal scale
  
  // poll estimates  
  poll ~ normal(p, sqrt(sigma));
    
}

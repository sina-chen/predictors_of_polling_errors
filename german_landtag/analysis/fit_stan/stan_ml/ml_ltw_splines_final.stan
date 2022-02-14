data {
  
  int<lower=0> N;                        // number of poll results (one separately for each poll and party)
  int<lower=0> K;                        // number of unique parties
  int<lower=0> R;                        // number of unique elections
  int<lower=0> P;                        // number of unique polls
  int<lower=0> n_B;                      // number of rows of B-splines
  int<lower=0> KR;                       // number of unique party-election combinations

  vector<lower=0, upper=1>[N] poll;      // poll estimate for each poll and party
  vector<lower=0, upper=1>[KR] vote;     // election results for each party

  vector<lower=0>[P] n;                  // sample size

  matrix[n_B, P] B;                      // B-spline matrix
  
  int<lower=1, upper=KR> kr_id[N];       // party-election identifier
  int<lower=1, upper=R> r_id[P];         // election identifier
  int<lower=1, upper=K> k_id[N];         // party identifier
  int<lower=1, upper=P> p_id[N];         // poll identifier
  
  vector[K-1] zero_r[R];                 // zero mean matrix for mvn distribution

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
  
  // time-varying party-election bias
  row_vector[n_B] beta_sc[KR-R]; // beta for K-1 parties due to redundant parametrisation
  real mu_beta; // mean for non-centerred parametrisation
  real<lower=0> sig_beta; // sd for non-centerred parametrisation

  // election-party variance
  vector[KR] phi_sc;
  real<lower=0> sig_phi; // sd for non-centerred common party sd

}

transformed parameters{
  
  // redundant bias parameters for all parties
  vector[K] alpha[R];
  row_vector[n_B] beta_raw[KR-R];
  row_vector[n_B] beta[KR];

  // variance parameter
  vector[KR] phi;
  
  // party specific sd for mvn distribution
  vector<lower=0>[K-1] tau_alpha; 
  
  // non-centered parametrisation 
  tau_alpha = tau_alpha_sc * sd_tau_alpha;
  for(i in 1:(KR-R)){
    beta_raw[i] = mu_beta + sig_beta * beta_sc[i];
  }  
  phi = sig_phi * phi_sc;
  
  // redundant parametrisation
  for(i in 1:R) {
    alpha[i] = append_row(alpha_raw[i],0);
    beta[((i-1)*K+1):((i*K)-1)] = beta_raw[((i-1)*(K-1)+1):(i*(K-1))];
    beta[(i-1)*K+K] = rep_row_vector(0, n_B);
  }   

}

model {
  
  vector[N] log_p; // unscaled mean on log scale
  vector[N] p; // scaled mean 
  vector[N] log_sigma2; // variance on log scale
  vector[N] sigma; // variance

  // hyper-priors
  sd_tau_alpha ~ normal(0, 0.2)T[0,];

  mu_beta ~ normal(0, 0.2);
  sig_beta ~ normal(0, 0.2)T[0,];
  
  sig_phi ~ normal(0, 0.2)T[0,];
  
  for(i in 1:(K-1)){
    tau_alpha_sc[i] ~ normal(0, 1)T[0,];
  }
  
  for(i in 1:(KR-R)){
    beta_sc[i] ~ std_normal();
  }
  phi_sc ~ std_normal(); 

  // cholesky factor of correlation matrix
  lkj_alpha_corr ~ lkj_corr_cholesky(2);

  // multivariate normal priors for parameter estimates
  alpha_raw ~ multi_normal_cholesky(zero_r, diag_pre_multiply(tau_alpha, lkj_alpha_corr));

  // mean model
  for(i in 1:N){
    log_p[i] = log_vote[kr_id[i]] + 
      alpha[r_id[p_id[i]],k_id[i]]  + 
      dot_product(beta[kr_id[i]], B[,p_id[i]]);
  }
  
  for(i in 1:P){
    p[((i-1)*K+1):(i*K)] = softmax(log_p[((i-1)*K+1):(i*K)]);   // rescale p to sum to 1
  }
  
  // variance model
  log_sigma2 = log((p .* (1-p))./n[p_id]) + phi[kr_id];
  sigma = sqrt(exp(log_sigma2)); // transform to normal scale
  
  // poll estimates  
  poll ~ normal(p, sigma);
     
}

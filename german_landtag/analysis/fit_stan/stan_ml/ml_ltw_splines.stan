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
  
  vector[K-1] zero_r[R];                    // matrix of zeros for party error
  vector[K] zero_Kr[R];                    // matrix of zeros for party error

}

transformed data {
  
  // transform election outcome to logit scale
  vector[KR] log_vote;
  log_vote = log(vote);
  
}


parameters {

  // Hyperpriors  
  
    // party-election bias
    vector[K-1] alpha_raw[R];
    cholesky_factor_corr[K-1] lkj_alpha_corr; // correlation matrix of common party bias
    real<lower=0> sd_tau_alpha;
    vector<lower=0>[K-1] tau_alpha_sc; // untransformed common party sd

    // time-varying party-election bias
    vector[K-1] beta_raw[R,n_B];
    cholesky_factor_corr[K-1] lkj_beta_corr; // correlation matrix of common party bias
    real<lower=0> sd_tau_beta;
    vector<lower=0>[K-1] tau_beta_sc; // untransformed common party sd
    
    // election-party variance
    vector[K] phi[R];
    cholesky_factor_corr[K] lkj_phi_corr; // correlation matrix of common party bias
    real<lower=0> sd_tau_phi;
    vector<lower=0>[K] tau_phi_sc; // untransformed common party sd
  
}

transformed parameters{
  
  vector[K] alpha[R];
  vector[K] beta[R,n_B];
  
  // party-election bias 
  vector<lower=0>[K] tau_alpha; 
  
  // party-election time trend bias
  vector<lower=0>[K] tau_beta;
  
  // matrix with b-spline coefficiants for each party adn election
  matrix[KR, n_B] splines_matrix;
  
  // party-election variance
  vector<lower=0>[K] tau_phi; // transformed common pollbias sd
  
  // party specific standard deviations for mvn
  tau_alpha = tau_alpha_sc * sd_tau_alpha;
  tau_beta = tau_beta_sc * sd_tau_beta;
  tau_phi = tau_phi_sc * sd_tau_phi;

  // redundant parametrisation
  for(i in 1:R) {
    alpha[i] = append_row(alpha_raw[i],0);
    for(j in 1:n_B){
          beta[i,j] = append_row(beta_raw[i,j], 0);
    }
  }   
    
  // reshape beta to match format for mean computation
  for(i in 1:n_B) {
    for(j in 1:R) {
          splines_matrix[((j-1)*K+1):(j*K),i] = beta[j,i];
    }
  }
}


model {
  
  vector[N] log_p; // unscaled mean on log scale
  vector[N] p; // mean on log scale
  vector[N] log_sigma; // variance on log scale
  vector[N] sigma; // variance

  // hyper-priors
  sd_tau_alpha ~ normal(0, 0.2)T[0,];
  sd_tau_beta ~ normal(0, 0.2)T[0,];
  sd_tau_phi ~ normal(0, 0.05)T[0,];
  
  for(i in 1:K){
    tau_alpha_sc[i] ~ normal(0, 1)T[0,];
    tau_beta_sc[i] ~ normal(0, 1)T[0,];
    tau_phi_sc[i] ~ normal(0, 1)T[0,];
    }

  // cholesky factor of correlation matrix
  lkj_alpha_corr ~ lkj_corr_cholesky(2);
  lkj_beta_corr ~ lkj_corr_cholesky(2);
  lkj_phi_corr ~ lkj_corr_cholesky(2);
  
  // multivariate normal priors for parameter estimates
  alpha_raw ~ multi_normal_cholesky(zero_r, diag_pre_multiply(tau_alpha, lkj_alpha_corr));
  for(i in 1:n_B){
      beta_raw[,i] ~ multi_normal_cholesky(zero_Kr, diag_pre_multiply(tau_beta, lkj_beta_corr));
  }
  phi ~ multi_normal_cholesky(zero_r, diag_pre_multiply(tau_phi, lkj_phi_corr));

  // mean model
  for(i in 1:N){
    log_p[i] = log_vote[kr_id[i]] + 
      alpha[r_id[p_id[i]],k_id[i]] +
      splines_matrix[kr_id[i],]*B[,p_id[i]];
  }
  
  // rescale p to sum to 1
  for(i in 1:P){
    p[((i-1)*K+1):(i*K)] = softmax(log_p[((i-1)*K+1):(i*K)]);
  }
  
  //variance model
  for(i in 1:N) {
    log_sigma[i] = log(p[i]*(1-p[i])/n[p_id[i]]) + 
    phi[ r_id[p_id[i]], k_id[i]];
  }
  
  sigma = exp(log_sigma);
  
  // poll estimates  
  poll ~ normal(p, sqrt(sigma));
    
     
}

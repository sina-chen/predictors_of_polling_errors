data {
  
  int<lower=1> N;                          // number of obs. (polls) 
  int<lower=1> SY;                         // number of elections (state crossed election year)
  
  vector<lower=0, upper=1>[N] poll;        // two-party poll share
  vector<lower=0, upper=1>[SY] vote;       // two -party vote share

  vector<lower=0, upper=1>[N] t;           // days to election
  vector<lower=1>[N] sample_size;          // poll sample size
  
  int<lower=1, upper=SY> sy_id[N];         // election id

} 

transformed data {
  
  vector[SY] logit_vote;
  logit_vote = logit(vote); // transform to logit scale
  
}

parameters {
  
  // hyper parameters for non-centered parametrization of election bias
  real mu_alpha;
  real<lower=0> sig_alpha;
  vector[SY] alpha_sc;
  
  // hyper parameters for non-centered parametrization of time trend
  real mu_beta1;
  real<lower=0> sig_beta1;
  vector[SY] beta1_sc;

  // hyper parameters for non-centered parametrization of additional variance
  real<lower=0> sig_phi2;
  vector<lower=-0>[SY] phi2_sc; // lower bound: https://discourse.mc-stan.org/t/non-centered-parameterisation-with-boundaries/2950/3
  
  // vector<lower=0>[SY] phi2;   // additional variance to SRS
    
  
} 

transformed parameters {
  
  vector[SY] alpha; // election specific bias
  vector[SY] beta1; // time trend
  vector<lower=0>[SY] phi2;   // additional variance to SRS
  
  // non-centered parametrization of election specific bias
  alpha = mu_alpha + sig_alpha * alpha_sc;

  // non-centered parametrization of time trend
  beta1 = mu_beta1 + sig_beta1 * beta1_sc;

  // non-centered parametrization of additonal variance
  phi2 = sig_phi2 * phi2_sc;

}

model {
  
  vector[N] logit_p;
  vector[N] p;
  vector[N] sigma2;

  // hyper priors
  mu_alpha ~ normal(0,0.2);
  sig_alpha ~ normal(0,0.2) T[0,]; // half normal due to bounds
  alpha_sc ~ normal(0,1);  
  
  mu_beta1 ~ normal(0,0.2);
  sig_beta1 ~ normal(0,0.2) T[0,]; 
  beta1_sc ~ normal(0,1);
  
  sig_phi2 ~ normal(0,0.05) T[0,];
  phi2_sc ~ normal(0,1);
  
  // phi2 ~ normal(0, sig_phi2);
  
  // mean model
  for(i in 1:N){
    logit_p[i] = logit_vote[sy_id[i]] + 
      alpha[sy_id[i]] + 
      beta1[sy_id[i]] * t[i]; 
  }
  p = inv_logit(logit_p); // rescale mean
  
  // variance model
  for(i in 1:N){
    sigma2[i] = p[i]*(1-p[i])/sample_size[i] + 
      phi2[sy_id[i]];    
  }
  
  // poll estimate  
  poll ~ normal(p, sqrt(sigma2));
  
}

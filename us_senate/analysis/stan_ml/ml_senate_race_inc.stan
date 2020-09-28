data {
  
  int<lower=1> N;                          //  Number of obs. (polls) 
  int<lower=1> SY;                         //  Number of elections (state crossed election year)

  real<lower=0, upper=1> poll[N];          //  two-party poll share
  real<lower=0, upper=1> vote[SY];         //  two -party vote share
  
  vector[SY] race_rep;                      // race Rep. dummy (1 = non white)
  vector[SY] race_dem;                      // race Dem. dummy (1 = non white)
  vector[SY] inc_dummy;                     // incumbency dummy (1 = incumbent is participating in election)
  vector[N] t;                             //  days to election scaled between 0 and 1

  vector<lower=1>[N] sample_size;          //  poll sample size
  
  int<lower=1, upper=SY> sy_id[N];         //  identifies election (state - year)
} 

transformed data {
  vector[SY] logit_vote;
  for (i in 1:SY){
    logit_vote[i] = logit(vote[i]);
  }
}

parameters {
  
  // Hyper-parameters

    // Mean model

    real mu_alpha;
    real<lower=0> sig_alpha;
    
    real mu_beta1;
    real<lower=0> sig_beta1;
    
    real mu_beta2;
    real<lower=0> sig_beta2;
    
    real mu_beta3;
    real<lower=0> sig_beta3;
    
    // Variance model
    
    real mu_gamma1;
    real<lower=0> sig_gamma1;

    real mu_tau;
    real<lower=0> sig_tau;

  // Scaled Parameters
    
    // Mean model

    vector[SY] alpha_sc;
    real beta1_sc;
    real beta2_sc;
    vector[SY] beta3_sc;
    
    // Variance model
    
    real gamma1_sc;
    vector[SY] tau_sc;
    
} 

transformed parameters {
  
  // Mean model
  
    // Parameters
    
    vector[SY] alpha;                         
    real beta1;
    real beta2;
    vector[SY] beta3;

    real gamma1;
    vector[SY] tau; 

  // Mean model
  
    alpha = mu_alpha + sig_alpha * alpha_sc;
    beta1 = mu_beta1 + sig_beta1 * beta1_sc;
    beta2 = mu_beta2 + sig_beta2 * beta2_sc;
    beta3 = mu_beta3 + sig_beta3 * beta3_sc;

  // Variance model
  
    gamma1 = mu_gamma1 + sig_gamma1 * gamma1_sc;
    tau = mu_tau + sig_tau * tau_sc;
    
}

model {
  
  vector[N] logit_p;
  vector[N] p;
  vector[N] log_sigma;
  vector[N] sigma;
  
  // Mean model
  
    // Hyper priors
    
    mu_alpha ~ normal(0,0.2);
    sig_alpha ~ normal(0,0.2) T[0,]; // half normal due to bounds
    
    mu_beta1 ~ normal(0,0.2);
    sig_beta1 ~ normal(0,0.2) T[0,]; // half normal due to bounds
    
    mu_beta2 ~ normal(0,0.2);
    sig_beta2 ~ normal(0,0.2) T[0,]; // half normal due to bounds
    
    mu_beta3 ~ normal(0,0.2);
    sig_beta3 ~ normal(0,0.2) T[0,]; // half normal due to bounds

    // Priors
    
    alpha_sc ~ normal(0,1);
    beta1_sc ~ normal(0,1);
    beta2_sc ~ normal(0,1);
    beta3_sc ~ normal(0,1);
  
  // Vairiance model
  
    // Hyper priors
    
    mu_gamma1 ~ normal(0,0.05);
    sig_gamma1 ~ normal(0,0.05) T[0,]; // half normal due to bounds

    mu_tau ~ normal(0,0.05);
    sig_tau ~ normal(0,0.05) T[0,]; // half normal due to bounds

    // Priors

    gamma1_sc ~ normal(0,1);
    tau_sc ~ normal(0,1);

  
  // Model for Polling Data
  
  for(i in 1:N){
    logit_p[i] = logit_vote[sy_id[i]] + 
                  alpha[sy_id[i]] + 
                  beta1 * race_rep[sy_id[i]] +
                  beta2 * race_dem[sy_id[i]] +
                  beta3[sy_id[i]] * t[i] ;
    p[i] = inv_logit(logit_p[i]);
    log_sigma[i] = log(p[i]*(1-p[i])/sample_size[i]) + 
                    tau[sy_id[i]] + 
                    gamma1 * inc_dummy[sy_id[i]];
    sigma[i] = exp(log_sigma[i]);
    poll[i] ~ normal(p[i],sqrt(sigma[i]));
  }
  
}
 data {
  
  int<lower=1> N;                          //  Number of obs total
  int<lower=1> SY;                         //  Number of elections (state crossed election year)
  int<lower=1> SWY;                         //  Number of elections (state crossed election year)

  real<lower=0, upper=1> poll[N];          //  individual polls, Rep/(Rep + Dem)
  real<lower=0, upper=1> vote[SY];         //  state election outcome 
  
  vector[N] c_int;                           //  campaign intensity scaled between 0 and 1
  vector[N] c_bal;                           //  campaign balance scaled between 0 and 1
  vector[SY] ev;                           //  electoral votes scaled between 0 and 1
  vector[N] after_con;                     //  dummy: 1 = after Rep. covention
  vector[N] dte;                           //  days to election scaled between 0 and 1

  vector<lower=1>[N] sample_size;          //  poll sample size
  
  int<lower=1, upper=SY> sy_id[N];         //  identifies election (state - year)
  int<lower=1, upper=SWY> swy_id[N];         //  identifies election (state - year)
} 

transformed data {
  vector[SY] logit_vote;
  for (i in 1:SY){
    logit_vote[i] = logit(vote[i]);
  }
}

parameters {
  
  // Mean model

    // Hyper-parameters
    
    real mu_alpha;
    real<lower=0> sig_alpha;
    
    real mu_beta1;
    real<lower=0> sig_beta1;
    
    real mu_beta2;
    real<lower=0> sig_beta2;
    
    // Scaled Parameters
    
    vector[SY] alpha_sc;
    vector[SWY] beta1_sc;
    vector[SY] beta2_sc;
    
  // Variance model
  
  // Hyper-parameters
    
    real mu_gamma1;
    real<lower=0> sig_gamma1;
    
    real mu_gamma2;
    real<lower=0> sig_gamma2;
    
    real mu_gamma3;
    real<lower=0> sig_gamma3;
    
    real mu_tau;
    real<lower=0> sig_tau;

    // Scaled Parameters
    
    vector[SWY] gamma1_sc;
    real gamma2_sc;
    real gamma3_sc;
    vector[SY] tau_sc;
    
} 

transformed parameters {
  
  // Mean model
  
    // Parameters
    
    vector[SY] alpha;                         
    vector[SWY] beta1;
    vector[SY] beta2;

  // Variance model
  
    // Parameters
    
    vector[SWY] gamma1;
    real gamma2;
    real gamma3;
    vector[SY] tau; 
    
  // Mean model
  
    alpha = mu_alpha + sig_alpha * alpha_sc;
    beta1 = mu_beta1 + sig_beta1 * beta1_sc;
    beta2 = mu_beta2 + sig_beta2 * beta2_sc;

  // Variance model
  
    gamma1 = mu_gamma1 + sig_gamma1 * gamma1_sc;
    gamma2 = mu_gamma2 + sig_gamma2 * gamma2_sc;
    gamma3 = mu_gamma3 + sig_gamma3 * gamma3_sc;
    tau = mu_tau + sig_tau * tau_sc;
    
}

model {
  
  vector[N] logit_p;
  vector[N] p;
  vector[N] log_sigma_sq;
  vector[N] sigma_sq;
  
  // Mean model
  
    // Hyper priors
    
    mu_alpha ~ normal(0,0.04);
    sig_alpha ~ normal(0,0.04) T[0,]; // half normal due to bounds
    
    mu_beta1 ~ normal(0,0.04);
    sig_beta1 ~ normal(0,0.04) T[0,]; // half normal due to bounds
    
    mu_beta2 ~ normal(0,0.04);
    sig_beta2 ~ normal(0,0.04) T[0,]; // half normal due to bounds

    // Priors
    
    alpha_sc ~ normal(0,1);
    beta1_sc ~ normal(0,1);
    beta2_sc ~ normal(0,1);
  
  // Vairiance model
  
    // Hyper-priors
    
    mu_gamma1 ~ normal(0,0.01);
    sig_gamma1 ~ normal(0,0.01) T[0,]; // half normal due to bounds

    mu_gamma2 ~ normal(0,0.01);
    sig_gamma2 ~ normal(0,0.01) T[0,]; // half normal due to bounds
    
    mu_gamma3 ~ normal(0,0.01);
    sig_gamma3 ~ normal(0,0.01) T[0,]; // half normal due to bounds
    
    mu_tau ~ normal(0,0.01);
    sig_tau ~ normal(0,0.01) T[0,]; // half normal due to bounds
    
    // Priors

    gamma1_sc ~ normal(0,1);
    gamma2_sc ~ normal(0,1);
    gamma3_sc ~ normal(0,1);
    tau_sc ~ normal(0,1);
   
  
  // Model for Polling Data
  
  for(i in 1:N){
    logit_p[i] = logit_vote[sy_id[i]] + 
                  alpha[sy_id[i]] + 
                  beta1[swy_id[i]] * c_bal[i] +
                  beta2[sy_id[i]] * dte[i] ;
    p[i] = inv_logit(logit_p[i]);
    log_sigma_sq[i] = log(p[i] * (1 - p[i]) / sample_size[i]) + 
                           tau[sy_id[i]] +
                           gamma1[swy_id[i]] * c_int[i] +
                           gamma2 * after_con[i] +
                           gamma3 * ev[sy_id[i]];
    sigma_sq[i] = exp(log_sigma_sq[i]);
    poll[i] ~ normal(p[i], sqrt(sigma_sq[i]));
  }
  
}


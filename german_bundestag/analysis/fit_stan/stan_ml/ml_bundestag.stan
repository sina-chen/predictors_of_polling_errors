data {
  
  int<lower=0> N;                        // number of poll results (one separately for each poll and party)
  int<lower=0> K;                        // number of unique parties
  int<lower=0> KR;                       // number of unique party-election combinations
  int<lower=0> R;                        // number of unique elections
  int<lower=0> I;                        // number of unique institutes
  int<lower=0> KI;                       // number of unique party-institute combinations

  vector<lower=0, upper=1>[N] poll;      // poll estimate for each poll and party
  vector<lower=0, upper=1>[KR] vote;     // election results for each party

  vector<lower=0, upper=1>[N] t;         // between 0 and 1 scaled days to election
  vector<lower=0>[N] n;                  // sample size
  
  int<lower=1, upper=K> k_id[N];         // party identifier
  int<lower=1, upper=KR> kr_id[N];       // party-election identifier
  int<lower=1, upper=KI> ki_id[N];       // party-institute identifier

  vector[K] zero1[1];                    // matrix of zeros for party error
  vector[K] zero2[R];                    // matrix of zeros for party error
  vector[K] zero3[I];                    // matrix of zeros for party error

}

transformed data {
  
  vector[KR] logit_vote;
  for (i in 1:(KR)){
    logit_vote[i] = logit(vote[i]);
  }
  
}

parameters {
  
  // Mean model

    // Party bias
    vector[K] alpha_k;
    cholesky_factor_corr[K] lkj_k_corr; // correlation matrix of common party bias
    real<lower=0> sd_sigma_k;
    vector<lower=0>[K] sigma_k_raw; // untransformed common party sd
    
    // Party-election bias
    vector[K] alpha_kr[R];
    cholesky_factor_corr[K] lkj_kr_corr; // correlation matrix of common party bias
    real<lower=0> sd_sigma_kr;
    vector<lower=0>[K] sigma_kr_raw; // untransformed common party sd
    
    // Party-institute bias
    vector[K] alpha_ki[I];
    cholesky_factor_corr[K] lkj_ki_corr; // correlation matrix of common party bias
    real<lower=0> sd_sigma_ki;
    vector<lower=0>[K] sigma_ki_raw; // untransformed common party sd    
    
    // Time-varying party-election bias 
    vector[K] beta_kr[R];
    cholesky_factor_corr[K] lkj_kr_tv_corr; // correlation matrix of common party bias
    real<lower=0> sd_sigma_kr_tv;
    vector<lower=0>[K] sigma_kr_tv_raw; // untransformed common party sd
    
    // Time-varying party-election bias^2 
    vector[K] beta_kr2[R];
    cholesky_factor_corr[K] lkj_kr_tv2_corr; // correlation matrix of common party bias
    real<lower=0> sd_sigma_kr_tv2;
    vector<lower=0>[K] sigma_kr_tv2_raw; // untransformed common party sd

  
  // Variance model
  
    // Election-party bias
    vector[K] phi[R];
    cholesky_factor_corr[K] lkj_kr_var_corr; // correlation matrix of common party bias
    real<lower=0> sd_sigma_kr_var;
    vector<lower=0>[K] sigma_kr_var_raw; // untransformed common party sd

}

transformed parameters{
  
  // Mean model
  
    // Party bais
    vector<lower=0>[K] sigma_k; 
  
    // Party-election bias 
    vector[KR] bias_kr;
    vector<lower=0>[K] sigma_kr; 

    // Party-institute bias
    vector[KI] bias_ki;
    vector<lower=0>[K] sigma_ki; 
    
    // Party-election time trend bias 
    vector[KR] bias_kr_tv;
    vector<lower=0>[K] sigma_kr_tv; 
    
    // Party-election squared time trend bias 
    vector[KR] bias_kr_tv2;
    vector<lower=0>[K] sigma_kr_tv2; 
    

  // Variance model
  
    // Party-election variance
    vector[KR] var_kr;
    vector<lower=0>[K] sigma_kr_var; // transformed common pollbias sd


  // Mean model
  
    sigma_k = sigma_k_raw * sd_sigma_k;
    sigma_kr = sigma_kr_raw * sd_sigma_kr;
    sigma_ki = sigma_ki_raw * sd_sigma_ki;
    sigma_kr_tv = sigma_kr_tv_raw * sd_sigma_kr_tv;
    sigma_kr_tv2 = sigma_kr_tv2_raw * sd_sigma_kr_tv2;
    

  // Variance model
  
    sigma_kr_var = sigma_kr_var_raw * sd_sigma_kr_var;
    
  // Transform multinomial distrobuted error matrices to vector
  
  for(i in 1:K){
    
    bias_kr[((i-1) * R + 1) : (i * R)] = to_vector(alpha_kr[,i]);      
    bias_ki[((i-1) * I + 1) : (i * I)] = to_vector(alpha_ki[,i]);      
    bias_kr_tv[((i-1) * R + 1) : (i * R)] = to_vector(beta_kr[,i]);      
    bias_kr_tv2[((i-1) * R + 1) : (i * R)] = to_vector(beta_kr2[,i]);      
    var_kr[((i-1) * R + 1) : (i * R)] = to_vector(phi[,i]); 
  }
    
}

model {

  vector[N] logit_p;
  vector[N] p;
  vector[N] log_sigma;
  vector[N] sigma;  

  // Mean model
  
    // Hyper-priors

    sd_sigma_k ~ normal(0, 0.2)T[0,];
    sd_sigma_kr ~ normal(0, 0.2)T[0,];
    sd_sigma_ki ~ normal(0, 0.2)T[0,];
    sd_sigma_kr_tv ~ normal(0, 0.2)T[0,];
    sd_sigma_kr_tv2 ~ normal(0, 0.2)T[0,];

    for(i in 1:K){
      sigma_k_raw[i] ~ normal(0, 1)T[0,];
      sigma_kr_raw[i] ~ normal(0, 1)T[0,];
      sigma_ki_raw[i] ~ normal(0, 1)T[0,];
      sigma_kr_tv_raw[i] ~ normal(0, 1)T[0,];
      sigma_kr_tv2_raw[i] ~ normal(0, 1)T[0,];
    }
        
    lkj_k_corr ~ lkj_corr_cholesky(2);
    lkj_kr_corr ~ lkj_corr_cholesky(2);
    lkj_ki_corr ~ lkj_corr_cholesky(2);
    lkj_kr_tv_corr ~ lkj_corr_cholesky(2);
    lkj_kr_tv2_corr ~ lkj_corr_cholesky(2);
    
    // Priors

    alpha_k ~ multi_normal_cholesky(zero1, diag_pre_multiply(sigma_k, lkj_k_corr));
    alpha_kr ~ multi_normal_cholesky(zero2, diag_pre_multiply(sigma_kr, lkj_kr_corr));
    alpha_ki ~ multi_normal_cholesky(zero3, diag_pre_multiply(sigma_ki, lkj_ki_corr));
    beta_kr ~ multi_normal_cholesky(zero2, diag_pre_multiply(sigma_kr_tv, lkj_kr_tv_corr));
    beta_kr2 ~ multi_normal_cholesky(zero2, diag_pre_multiply(sigma_kr_tv2, lkj_kr_tv2_corr));

    
  // Variance model
  
    // Hyper-priors

    sd_sigma_kr_var ~ normal(0, 0.2)T[0,];

    for(i in 1:K){
      sigma_kr_var_raw[i] ~ normal(0, 1)T[0,];
    }
    
    lkj_kr_var_corr ~ lkj_corr_cholesky(2);
    
    // Priors

    phi ~ multi_normal_cholesky(zero2, diag_pre_multiply(sigma_kr_var, lkj_kr_var_corr));
 
 
  // Mean model
  for(i in 1:N){
    logit_p[i] = logit_vote[kr_id[i]] + 
      alpha_k[k_id[i]] + 
      bias_kr[kr_id[i]] + 
      bias_ki[ki_id[i]] + 
      bias_kr_tv[kr_id[i]] * t[i] + 
      bias_kr_tv2[kr_id[i]] * t[i]^2;
  }
  
  p = inv_logit(logit_p);
  
  // Variance model
  for(i in 1:N){
    log_sigma[i] = log(p[i]*(1-p[i])/n[i]) +
    var_kr[kr_id[i]];
  }
  
  sigma = exp(log_sigma);
  
  poll ~ normal(p, sqrt(sigma));

}


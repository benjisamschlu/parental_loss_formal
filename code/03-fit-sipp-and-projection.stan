// matrix projection for parents pair for child from a single race/ethnicity group
data {
  // dimensions
  int <lower=1> N_RACE;  // race
  int <lower=1> N_AGE_C; // children age
  // projected data
  array[N_RACE] vector[N_AGE_C] m_a_lost_mom;
  array[N_RACE] vector[N_AGE_C] m_a_lost_dad; 
  array[N_RACE] vector[N_AGE_C] p_a_lost_mom; 
  array[N_RACE] vector[N_AGE_C] p_a_lost_dad;
  // design matrices for the log-linear model
  matrix[N_RACE * N_AGE_C * 4, 24] ll_X_phi;
  matrix[N_RACE * N_AGE_C * 4, 24] ll_X_theta;
  // sipp estimates
  array[N_RACE, N_AGE_C] vector <lower=-1, upper=1>[4] p_a_sipp;
  array[N_RACE, N_AGE_C] vector <lower=-1>[4] p_a_sipp_se;
  array[N_RACE, N_AGE_C] vector <lower=-1, upper=1>[5] m_a_sipp;
  array[N_RACE, N_AGE_C] vector <lower=-1>[5] m_a_sipp_se;
}

parameters {
  // sipp prevalence (constraint to sum to 1)
  array[N_RACE, N_AGE_C] simplex[4] phi_msm; 
  // sipp incidence
  array[N_RACE, N_AGE_C] simplex[4] theta_one; # constrain sum of theta_1_# to less than 0
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> theta_2_4;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> theta_3_4;
  // log-linear model parameters
  vector[cols(ll_X_phi)] phi_ll_beta;
  vector[cols(ll_X_theta)] theta_ll_beta;
  // standard deviations
  real<lower=0> phi_ll_sigma, theta_ll_sigma, theta_rw_sigma;
  real<lower=0> phi_sigma, theta_sigma;
}

transformed parameters {
  // projection prevalence
  // start at age 1 and append assumed constants at birth (0) later
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> phi_mom;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> phi_dad;
  // projection incidence
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> theta_mom;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> theta_dad;
  // link to the SIPP data
  // assume both parents alive for all at birth
  // state: 1 - none; 2 - mom; 3 - dad; 4 - both;;
  // trans: 1 - none to mom; 2 - none to dad; 3 - none to both; 4 - mom to both; 5 - dad to both
  for (r in 1:N_RACE) {
    for (a in 1:N_AGE_C) {
      phi_mom[r, a] = phi_msm[r, a, 2] + phi_msm[r, a, 4];
      phi_dad[r, a] = phi_msm[r, a, 3] + phi_msm[r, a, 4];
      theta_mom[r, a] = ((theta_one[r, a, 1] + theta_one[r, a, 3]) * phi_msm[r, a, 1]
                        + theta_3_4[r, a] * phi_msm[r, a, 3]) / (1 - phi_mom[r, a]);
      theta_dad[r, a] = ((theta_one[r, a, 2] + theta_one[r, a, 3]) * phi_msm[r, a, 1]
                        + theta_2_4[r, a] * phi_msm[r, a, 2]) / (1 - phi_dad[r, a]);
    }
  }
}

model {
  // SIPP and projection
  phi_sigma ~ exponential(5);
  theta_sigma ~ exponential(5);
  for (r in 1:N_RACE) {
    for (a in 1:N_AGE_C) {
      for (s in 1:4) {
        if (p_a_sipp_se[r, a, s] > 0) { // only sample non-missing survey estimates
          p_a_sipp[r, a, s] ~ normal(phi_msm[r, a, s], p_a_sipp_se[r, a, s]);
        }
      }
      for (t in 1:5) {
        if (m_a_sipp_se[r, a, t] > 0) { // only sample non-missing survey estimates
          if (t <= 3) {
            m_a_sipp[r, a, t] ~ normal(theta_one[r, a, t], m_a_sipp_se[r, a, t]);
          }
          if (t == 4) {
            m_a_sipp[r, a, t] ~ normal(theta_2_4[r, a], m_a_sipp_se[r, a, t]);
          }
          if (t == 5) {
            m_a_sipp[r, a, t] ~ normal(theta_3_4[r, a], m_a_sipp_se[r, a, t]);
          }
        }
      }    
    }
    // observed outputs from the projection model 
    p_a_lost_mom[r] ~ normal(phi_mom[r], phi_sigma);
    p_a_lost_dad[r] ~ normal(phi_dad[r], phi_sigma);
    m_a_lost_mom[r] ~ normal(theta_mom[r], theta_sigma);
    m_a_lost_dad[r] ~ normal(theta_dad[r], theta_sigma);
  }
  
  // phis
  phi_ll_beta ~ normal(0, 1);
  phi_ll_sigma ~ exponential(1);

  for (r in 1:N_RACE) {
    int X_ind = (r - 1) * N_AGE_C * 4;
    logit(phi_msm[r, , 1]) ~ normal(ll_X_phi[(X_ind + 1):(X_ind + N_AGE_C)]
                                * phi_ll_beta,
                                phi_ll_sigma);
    logit(phi_mom[r]) ~ normal(ll_X_phi[(X_ind + N_AGE_C + 1):(X_ind + N_AGE_C * 2)]
                           * phi_ll_beta, 
                           phi_ll_sigma);
    logit(phi_dad[r]) ~ normal(ll_X_phi[(X_ind + N_AGE_C * 2 + 1):(X_ind + N_AGE_C * 3)]
                           * phi_ll_beta,
                           phi_ll_sigma);
    logit(phi_msm[r, , 4]) ~ normal(ll_X_phi[(X_ind + 1):(X_ind + N_AGE_C)]
                                * phi_ll_beta,
                                phi_ll_sigma);
    for (a in 1:N_AGE_C) {
      target += log_inv_logit(phi_msm[r, a, 1]) + log1m_inv_logit(phi_msm[r, a, 1]);
      target += log_inv_logit(phi_mom[r, a]) + log1m_inv_logit(phi_mom[r, a]);
      target += log_inv_logit(phi_dad[r, a]) + log1m_inv_logit(phi_dad[r, a]);
      target += log_inv_logit(phi_msm[r, a, 4]) + log1m_inv_logit(phi_msm[r, a, 4]);
    }
  }

  // thetas
  theta_rw_sigma ~ exponential(5);
  theta_ll_beta ~ normal(0, 1);
  theta_ll_sigma ~ exponential(5);

  for (r in 1:N_RACE) {
    theta_one[r, 1, 3] ~ exponential(20);
    for (a in 2:N_AGE_C) {
      theta_one[r, a, 3] ~ lognormal(log(theta_one[r, a - 1, 3]), theta_rw_sigma);
    }
    int X_ind = (r - 1) * N_AGE_C * 4;
    theta_one[r, , 1] ~ lognormal(ll_X_theta[(X_ind + 1):(X_ind + N_AGE_C)]
                                  * theta_ll_beta, 
                                  theta_ll_sigma);
    theta_one[r, , 2] ~ lognormal(ll_X_theta[(X_ind + N_AGE_C + 1):(X_ind + N_AGE_C * 2)]
                                  * theta_ll_beta, 
                                  theta_ll_sigma);
    theta_2_4[r] ~ lognormal(ll_X_theta[(X_ind + N_AGE_C * 2 + 1):(X_ind + N_AGE_C * 3)]
                                  * theta_ll_beta, 
                                  theta_ll_sigma);
    theta_3_4[r] ~ lognormal(ll_X_theta[(X_ind + N_AGE_C * 3 + 1):(X_ind + N_AGE_C * 4)]
                             * theta_ll_beta, 
                             theta_ll_sigma);
  }
}


generated quantities {
  array[N_RACE, N_AGE_C] vector<lower=0, upper=1>[5] theta_msm; 
  for (r in 1:N_RACE) {
    for (a in 1:N_AGE_C) {
      theta_msm[r, a, 1] = theta_one[r, a, 1];
      theta_msm[r, a, 2] = theta_one[r, a, 2];
      theta_msm[r, a, 3] = theta_one[r, a, 3];
      theta_msm[r, a, 4] = theta_2_4[r, a];
      theta_msm[r, a, 5] = theta_3_4[r, a];
    }
  }

  // posterior-predictives
  array[N_RACE, N_AGE_C] vector[4] p_a_msm_pp;
  array[N_RACE, N_AGE_C] vector[5] m_a_msm_pp;

  array[N_RACE, N_AGE_C] real p_a_lost_mom_pp;
  array[N_RACE, N_AGE_C] real p_a_lost_dad_pp;
  array[N_RACE, N_AGE_C] real m_a_lost_mom_pp;
  array[N_RACE, N_AGE_C] real m_a_lost_dad_pp;
  
  for (r in 1:N_RACE) {
    // SIPP
    for (a in 1:N_AGE_C) {
      for (s in 1:4) {
        if (p_a_sipp_se[r, a, s] > 0) {
          p_a_msm_pp[r, a, s] = normal_rng(phi_msm[r, a, s], p_a_sipp_se[r, a, s]);
        } else {
          p_a_msm_pp[r, a, s] = 0;
        }
      }
      for (t in 1:5) {
        if (m_a_sipp_se[r, a, t] > 0) {
          m_a_msm_pp[r, a, t] = normal_rng(theta_msm[r, a, t], m_a_sipp_se[r, a, t]);
        } else {
          m_a_msm_pp[r, a, t] = 0;
        }
      }
    }

    // projection
    p_a_lost_mom_pp[r] = normal_rng(phi_mom[r], phi_sigma);
    p_a_lost_dad_pp[r] = normal_rng(phi_dad[r], phi_sigma);
    m_a_lost_mom_pp[r] = normal_rng(theta_mom[r], theta_sigma);
    m_a_lost_dad_pp[r] = normal_rng(theta_dad[r], theta_sigma);
  }
}

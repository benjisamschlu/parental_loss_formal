// matrix projection for parents pair for child from a single race/ethnicity group
data {
  // dimensions
  int <lower=1> N_RACE;  // race
  int <lower=1> N_AGE_C; // children age
  // design matrices for the linear model
  matrix[N_RACE * N_AGE_C, 6] ll_X;
  // sipp estimates
  array[N_RACE, N_AGE_C] vector <lower=0, upper=1>[4] p_a_sipp;
  array[N_RACE, N_AGE_C] vector <lower=0>[4] p_a_sipp_se;
  array[N_RACE, N_AGE_C] vector <lower=0, upper=1>[5] m_a_sipp;
  array[N_RACE, N_AGE_C] vector <lower=0>[5] m_a_sipp_se;
}

parameters {
  // sipp prevalence (constraint to sum to 1)
  array[N_RACE, N_AGE_C] simplex[4] p_a; 
  // sipp incidence
  array[N_RACE, N_AGE_C] simplex[4] m_a; // constrain sum of m_a_1_# to less than 0
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> m_a_2_4;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> m_a_3_4;
 // log-linear model parameters
  array[4] vector[cols(ll_X)] p_a_ll_beta;
  // standard deviations
  array[4] real<lower=0> p_a_ll_sigma;
  real<lower=0> m_a_rw_sigma;
}

transformed parameters {
  // prevalence by each parent
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> p_a_mom;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> p_a_dad;
  // assume both parents alive for all at birth
  // state: 1 - none; 2 - mom; 3 - dad; 4 - both;;
  // trans: 1 - none to mom; 2 - none to dad; 3 - none to both; 4 - mom to both; 5 - dad to both
  for (r in 1:N_RACE) {
    for (a in 1:N_AGE_C) {
      p_a_mom[r, a] = p_a[r, a, 2] + p_a[r, a, 4];
      p_a_dad[r, a] = p_a[r, a, 3] + p_a[r, a, 4];
    }
  }
}

model {
  // SIPP 
  for (r in 1:N_RACE) {
    for (a in 1:N_AGE_C) {
      for (s in 1:4) {
        if (p_a_sipp_se[r, a, s] > 0) { // only sample non-missing survey estimates
          p_a_sipp[r, a, s] ~ normal(p_a[r, a, s], p_a_sipp_se[r, a, s]);
        }
      }
      for (t in 1:5) {
        if (m_a_sipp_se[r, a, t] > 0) { // only sample non-missing survey estimates
          if (t <= 3) {
            m_a_sipp[r, a, t] ~ normal(m_a[r, a, t], m_a_sipp_se[r, a, t]);
          }
          if (t == 4) {
            m_a_sipp[r, a, t] ~ normal(m_a_2_4[r, a], m_a_sipp_se[r, a, t]);
          }
          if (t == 5) {
            m_a_sipp[r, a, t] ~ normal(m_a_3_4[r, a], m_a_sipp_se[r, a, t]);
          }
        }
      } 
    }
  }

  // proportions
  for (i in 1:4) {
    p_a_ll_beta[i] ~ normal(0, 1);
    p_a_ll_sigma[i] ~ exponential(1);
  }

  for (r in 1:N_RACE) {
    int X_ind = (r - 1) * N_AGE_C;
    logit(p_a[r, , 1]) ~ normal(
        ll_X[(X_ind + 1):(X_ind + N_AGE_C)] * p_a_ll_beta[1], p_a_ll_sigma[1]);
    logit(p_a_mom[r]) ~ normal(
        ll_X[(X_ind + 1):(X_ind + N_AGE_C)] * p_a_ll_beta[2], p_a_ll_sigma[2]);
    logit(p_a_dad[r]) ~ normal(
        ll_X[(X_ind + 1):(X_ind + N_AGE_C)] * p_a_ll_beta[3], p_a_ll_sigma[3]);
    logit(p_a[r, , 4]) ~ normal(
        ll_X[(X_ind + 1):(X_ind + N_AGE_C)] * p_a_ll_beta[4], p_a_ll_sigma[4]);
    for (a in 1:N_AGE_C) {
      target += log_inv_logit(p_a[r, a, 1]) + log1m_inv_logit(p_a[r, a, 1]);
      target += log_inv_logit(p_a_mom[r, a]) + log1m_inv_logit(p_a_mom[r, a]);
      target += log_inv_logit(p_a_dad[r, a]) + log1m_inv_logit(p_a_dad[r, a]);
      target += log_inv_logit(p_a[r, a, 4]) + log1m_inv_logit(p_a[r, a, 4]);
    }
  }

  // rates
  m_a_rw_sigma ~ exponential(1);

  for (r in 1:N_RACE) {
    m_a[r, 1, 1] ~ exponential(20);
    m_a[r, 1, 2] ~ exponential(20);
    m_a[r, 1, 3] ~ exponential(40);
    m_a_2_4[r, 1] ~ exponential(20);
    m_a_3_4[r, 1] ~ exponential(20);
    for (a in 2:N_AGE_C) {
      m_a[r, a, 1] ~ lognormal(log(m_a[r, a - 1, 1]), m_a_rw_sigma);
      m_a[r, a, 2] ~ lognormal(log(m_a[r, a - 1, 2]), m_a_rw_sigma);
      m_a[r, a, 3] ~ lognormal(log(m_a[r, a - 1, 3]), m_a_rw_sigma);
      m_a_2_4[r, a] ~ lognormal(log(m_a_2_4[r, a - 1]), m_a_rw_sigma);
      m_a_3_4[r, a] ~ lognormal(log(m_a_3_4[r, a - 1]), m_a_rw_sigma);
    }
  }
}

generated quantities {
  array[N_RACE, N_AGE_C] vector<lower=0, upper=1>[5] m_a_msm; 
  for (r in 1:N_RACE) {
    for (a in 1:N_AGE_C) {
      m_a_msm[r, a, 1] = m_a[r, a, 1];
      m_a_msm[r, a, 2] = m_a[r, a, 2];
      m_a_msm[r, a, 3] = m_a[r, a, 3];
      m_a_msm[r, a, 4] = m_a_2_4[r, a];
      m_a_msm[r, a, 5] = m_a_3_4[r, a];
    }
  }
  
  // incidence by each parent
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> m_a_mom;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> m_a_dad;
  // assume both parents alive for all at birth
  // state: 1 - none; 2 - mom; 3 - dad; 4 - both;;
  // trans: 1 - none to mom; 2 - none to dad; 3 - none to both; 4 - mom to both; 5 - dad to both
  for (r in 1:N_RACE) {
    for (a in 1:N_AGE_C) {
      m_a_mom[r, a] = ((m_a[r, a, 1] + m_a[r, a, 3]) * p_a[r, a, 1]
                        + m_a_3_4[r, a] * p_a[r, a, 3]) / (1 - p_a_mom[r, a]);
      m_a_dad[r, a] = ((m_a[r, a, 2] + m_a[r, a, 3]) * p_a[r, a, 1]
                        + m_a_2_4[r, a] * p_a[r, a, 2]) / (1 - p_a_dad[r, a]);
    }
  }

  // posterior-predictives
  array[N_RACE, N_AGE_C] vector[4] p_a_msm_pp;
  array[N_RACE, N_AGE_C] vector[5] m_a_msm_pp;

  for (r in 1:N_RACE) {
    // SIPP
    for (a in 1:N_AGE_C) {
      for (s in 1:4) {
        if (p_a_sipp_se[r, a, s] > 0) {
          p_a_msm_pp[r, a, s] = normal_rng(p_a[r, a, s], p_a_sipp_se[r, a, s]);
        } else {
          p_a_msm_pp[r, a, s] = 0;
        }
      }
      for (t in 1:5) {
        if (m_a_sipp_se[r, a, t] > 0) {
          m_a_msm_pp[r, a, t] = normal_rng(m_a_msm[r, a, t], m_a_sipp_se[r, a, t]);
        } else {
          m_a_msm_pp[r, a, t] = 0;
        }
      }
    }
  }
}

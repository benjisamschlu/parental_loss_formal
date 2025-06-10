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
  array[N_RACE] vector[N_AGE_C] m_a_lost_mom_se;
  array[N_RACE] vector[N_AGE_C] m_a_lost_dad_se;
  array[N_RACE] vector[N_AGE_C] p_a_lost_mom_se; 
  array[N_RACE] vector[N_AGE_C] p_a_lost_dad_se;
  // sipp estimates
  array[N_RACE, N_AGE_C] vector <lower=-1, upper=1>[4] p_a_sipp;
  array[N_RACE, N_AGE_C] vector <lower=-1>[4] p_a_sipp_se;
  array[N_RACE, N_AGE_C] vector <lower=-1, upper=1>[5] m_a_sipp;
  array[N_RACE, N_AGE_C] vector <lower=-1>[5] m_a_sipp_se;
}

parameters {
  // sipp prevalence (constraint to sum to 1)
  array[N_RACE, N_AGE_C] simplex[4] p_a; 
  // sipp incidence
  array[N_RACE, N_AGE_C] simplex[4] m_a; // constrain sum of m_a_1_# to less than 0
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> m_a_2_4;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> m_a_3_4;
  // log-linear model parameters
  // standard deviations
  real<lower=0> p_a_rw_sigma, m_a_rw_sigma;
}

transformed parameters {
  // prevalence by each parent
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> p_a_mom;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> p_a_dad;
  // incidence by each parent
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> m_a_mom;
  array[N_RACE, N_AGE_C] real<lower=0, upper=1> m_a_dad;
  // assume both parents alive for all at birth
  // state: 1 - none; 2 - mom; 3 - dad; 4 - both;;
  // trans: 1 - none to mom; 2 - none to dad; 3 - none to both; 4 - mom to both; 5 - dad to both
  for (r in 1:N_RACE) {
    for (a in 1:N_AGE_C) {
      p_a_mom[r, a] = p_a[r, a, 2] + p_a[r, a, 4];
      p_a_dad[r, a] = p_a[r, a, 3] + p_a[r, a, 4];
      m_a_mom[r, a] = ((m_a[r, a, 1] + m_a[r, a, 3]) * p_a[r, a, 1]
                        + m_a_3_4[r, a] * p_a[r, a, 3]) / (1 - p_a_mom[r, a]);
      m_a_dad[r, a] = ((m_a[r, a, 2] + m_a[r, a, 3]) * p_a[r, a, 1]
                        + m_a_2_4[r, a] * p_a[r, a, 2]) / (1 - p_a_dad[r, a]);
    }
  }
}

model {
  for (r in 1:N_RACE) {
    // Projection
    p_a_lost_mom[r] ~ normal(p_a_mom[r], p_a_lost_mom_se[r]);
    p_a_lost_dad[r] ~ normal(p_a_dad[r], p_a_lost_dad_se[r]);
    m_a_lost_mom[r] ~ normal(m_a_mom[r], m_a_lost_mom_se[r]);
    m_a_lost_dad[r] ~ normal(m_a_dad[r], m_a_lost_dad_se[r]);
    // SIPP
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
  p_a_rw_sigma ~ exponential(5);
  
  for (r in 1:N_RACE) {
    p_a[r, 1, ] ~ exponential(20);
    for (a in 2:N_AGE_C) {
      p_a[r, a, ] ~ lognormal(log(p_a[r, a - 1, ]), p_a_rw_sigma);
    }
  }

  // rates
  m_a_rw_sigma ~ exponential(5);
  
  for (r in 1:N_RACE) {
    m_a[r, 1, 1] ~ exponential(20);
    m_a[r, 1, 2] ~ exponential(20);
    m_a[r, 1, 3] ~ exponential(20);
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

    // projection
    p_a_lost_mom_pp[r] = normal_rng(p_a_mom[r], p_a_lost_mom_se[r]);
    p_a_lost_dad_pp[r] = normal_rng(p_a_dad[r], p_a_lost_dad_se[r]);
    m_a_lost_mom_pp[r] = normal_rng(m_a_mom[r], m_a_lost_mom_se[r]);
    m_a_lost_dad_pp[r] = normal_rng(m_a_dad[r], m_a_lost_dad_se[r]);
  }
}

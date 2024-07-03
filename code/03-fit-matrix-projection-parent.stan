// matrix projection for parents pair for child from a single race/ethnicity group
data {
  // dimensions
  int <lower=1> MAX_AGE; // max age considered
  int <lower=0> MIN_AGE_F_MOM; // minimum age with fertility counts for moms
  int <lower=0> MAX_AGE_F_MOM; // maximum age with fertility counts for moms
  int <lower=0> MIN_AGE_F_DAD; // minimum age with fertility counts for dads
  int <lower=0> MAX_AGE_F_DAD; // maximum age with fertility counts for dads
  int <lower=1> N_TIME; // # # of time steps
  // fertility of parents (time by age)
  array[N_TIME] vector <lower=0> [MAX_AGE_F_MOM - MIN_AGE_F_MOM + 1] r_fert_mom;
  array[N_TIME] vector <lower=0> [MAX_AGE_F_DAD - MIN_AGE_F_DAD + 1] r_fert_dad;
  // number of deaths for potential parents only
  array[N_TIME, MAX_AGE - MIN_AGE_F_MOM + 1] int <lower=0> n_deaths_mom;
  array[N_TIME, MAX_AGE - MIN_AGE_F_DAD + 1] int <lower=0> n_deaths_dad;
  // population counts for potential parents only
  array[N_TIME, MAX_AGE - MIN_AGE_F_MOM + 1] int <lower=0> n_pop_mom;
  array[N_TIME, MAX_AGE - MIN_AGE_F_DAD + 1] int <lower=0> n_pop_dad;
  // sipp estimates
  array[MAX_AGE] vector<lower=-1, upper=1>[4] p_a_sipp;
  array[MAX_AGE] vector<lower=-1>[4]p_a_sipp_se;
  array[MAX_AGE] vector<lower=-1, upper=1>[5] m_a_sipp;
  array[MAX_AGE] vector<lower=-1>[5] m_a_sipp_se;
}

transformed data {
  // number of ages considered
  int <lower=1> N_AGE_C; // children
  int <lower=1> N_AGE_P_MOM; // mothers until death
  int <lower=1> N_AGE_P_DAD; // fathers until death
  int <lower=1> N_AGE_F_MOM; // mothers while fertile
  int <lower=1> N_AGE_F_DAD; // fathers while fertile
  N_AGE_C = MAX_AGE + 1; // include 0 at birth 
  N_AGE_P_MOM = MAX_AGE - MIN_AGE_F_MOM + 1;
  N_AGE_P_DAD = MAX_AGE - MIN_AGE_F_DAD + 1;
  N_AGE_F_MOM = MAX_AGE_F_MOM - MIN_AGE_F_MOM + 1; 
  N_AGE_F_DAD = MAX_AGE_F_DAD - MIN_AGE_F_DAD + 1;
  // mortaility rates
  array[N_TIME, N_AGE_P_MOM] real <lower=0, upper=1> M_X_T_MOM;
  array[N_TIME, N_AGE_P_DAD] real <lower=0, upper=1> M_X_T_DAD;
  for (t in 1:N_TIME) {
    for (x in 1:N_AGE_P_MOM) {
      M_X_T_MOM[t, x] = n_deaths_mom[t, x] / n_pop_mom[t, x];
    }
    for (x in 1:N_AGE_P_DAD) {
      M_X_T_DAD[t, x] = n_deaths_dad[t, x] / n_pop_dad[t, x];
    }
  }

  // construct array of parent transition matrices, U tilde
  // the last 1 row is for mortality rates
  array[N_TIME] matrix[N_AGE_P_MOM + 1, N_AGE_P_MOM + 1] Utilde_mom;
  array[N_TIME] matrix[N_AGE_P_DAD + 1, N_AGE_P_DAD + 1] Utilde_dad;
  for (t in 1:N_TIME) {
    Utilde_mom[t] = rep_matrix(0, N_AGE_P_MOM + 1, N_AGE_P_MOM + 1);
    Utilde_dad[t] = rep_matrix(0, N_AGE_P_DAD + 1, N_AGE_P_DAD + 1);
    // survival
    Utilde_mom[t][2:, :N_AGE_P_MOM] = diag_matrix(1 - to_vector(M_X_T_MOM[t])); 
    Utilde_dad[t][2:, :N_AGE_P_DAD] = diag_matrix(1 - to_vector(M_X_T_DAD[t])); 
    // mortality
    Utilde_mom[t][N_AGE_P_MOM + 1, :N_AGE_P_MOM] = to_row_vector(M_X_T_MOM[t]); 
    Utilde_dad[t][N_AGE_P_DAD + 1, :N_AGE_P_DAD] = to_row_vector(M_X_T_DAD[t]); 
  }
  // average number of parent of age {row} for child age {col}
  // last row represents parent deaths during the previous time period
  array[N_TIME] matrix <lower=0>[N_AGE_P_MOM + 1, N_AGE_C] K_a_t_mom;
  array[N_TIME] matrix <lower=0>[N_AGE_P_DAD + 1, N_AGE_C] K_a_t_dad;
  // projection for t from 1 to T
  for (t in 1:N_TIME) {
    // local variable for previous time index
    int t_prev; 
    t_prev = max(1, t - 1); // initialize with a stable population
    K_a_t_mom[t] = rep_matrix(0, N_AGE_P_MOM + 1, N_AGE_C);
    K_a_t_dad[t] = rep_matrix(0, N_AGE_P_DAD + 1, N_AGE_C);
    // parent's age distribution at birth
    vector[N_AGE_F_MOM] Births_x_t_mom;
    vector[N_AGE_F_DAD] Births_x_t_dad;
    Births_x_t_mom = r_fert_mom[t] .* to_vector(n_pop_mom[t, :N_AGE_F_MOM]);
    Births_x_t_dad = r_fert_dad[t] .* to_vector(n_pop_dad[t, :N_AGE_F_DAD]);
    K_a_t_mom[t][:N_AGE_F_MOM, 1] = Births_x_t_mom / sum(Births_x_t_mom); 
    K_a_t_dad[t][:N_AGE_F_DAD, 1] = Births_x_t_dad / sum(Births_x_t_dad); 
    // parent's survival
    for (a in 2:N_AGE_C) {
      K_a_t_mom[t][, a] = (Utilde_mom[t_prev] * (K_a_t_mom[t_prev][ , a - 1]));
      K_a_t_dad[t][, a] = (Utilde_dad[t_prev] * (K_a_t_dad[t_prev][ , a - 1]));
    }
  }
  // link to the SIPP data
  // incidence
  vector[N_AGE_C - 1] m_a_loss_mom; 
  vector[N_AGE_C - 1] m_a_loss_dad; 
  // prevalence 
  // don't save 0 prevalence at birth
  vector[N_AGE_C - 2] p_a_loss_mom; 
  vector[N_AGE_C - 2] p_a_loss_dad; 
  for (a in 1:(N_AGE_C - 1)) {
    if (a > 1) {
      p_a_loss_mom[a - 1] = 1 - sum(K_a_t_mom[N_TIME][:N_AGE_P_MOM, a]);
      p_a_loss_dad[a - 1] = 1 - sum(K_a_t_mom[N_TIME][:N_AGE_P_DAD, a]);
    }
    real p_alive_mom;
    real p_alive_dad;
    p_alive_mom = sum(K_a_t_mom[N_TIME][, a]);
    p_alive_dad = sum(K_a_t_dad[N_TIME][, a]);
    if (p_alive_mom > 0) 
      m_a_loss_mom[a] = K_a_t_mom[N_TIME][N_AGE_P_MOM + 1, a] / sum(K_a_t_mom[N_TIME][, a]);
    else 
      m_a_loss_mom[a] = 0;

    if (p_alive_dad > 0) 
      m_a_loss_dad[a] = K_a_t_dad[N_TIME][N_AGE_P_DAD + 1, a] / sum(K_a_t_dad[N_TIME][, a]);
    else 
      m_a_loss_dad[a] = 0;
  }
}

parameters {
  // sipp prevalence (constraint to sum to 1)
  // start at age 1 and append assumed constants at birth (0) later
  array[N_AGE_C - 2] simplex[4] phi_sipp; 
  // sipp incidence
  array[N_AGE_C - 1] vector<lower=0, upper=1>[5] theta_sipp;
  // standard deviations
  real<lower=0> phi_sigma_mom;
  real<lower=0> phi_sigma_dad;
  real<lower=0> theta_sigma_mom;
  real<lower=0> theta_sigma_dad;
}

transformed parameters {
  // projection prevalence
  // start at age 1 and append assumed constants at birth (0) later
  array[N_AGE_C - 2] real<lower=0, upper=1> phi_mom;
  array[N_AGE_C - 2] real<lower=0, upper=1> phi_dad;
  // projection incidence (upper limit)
  array[N_AGE_C - 1] real<lower=0> theta_mom;
  array[N_AGE_C - 1] real<lower=0> theta_dad;
  // link to the SIPP data
  // assume both parents alive for all at birth
  theta_mom[1] = theta_sipp[1, 1] + theta_sipp[1, 3];
  theta_dad[1] = theta_sipp[1, 2] + theta_sipp[1, 3];
  for (a in 2:(N_AGE_C - 1)) {
    phi_mom[a - 1] = phi_sipp[a - 1, 2] + phi_sipp[a - 1, 4];
    phi_dad[a - 1] = phi_sipp[a - 1, 3] + phi_sipp[a - 1, 4];
    theta_mom[a] = ((theta_sipp[a, 1] + theta_sipp[a, 3]) * phi_sipp[a - 1, 1] 
                     + theta_sipp[a, 5] * phi_sipp[a - 1, 2]);
    theta_dad[a] = ((theta_sipp[a, 2] + theta_sipp[a, 3]) * phi_sipp[a - 1, 2] 
                     + theta_sipp[a, 4] * phi_sipp[a - 1, 3]);
  }
}

model {
  p_a_loss_mom ~ normal(phi_mom, phi_sigma_mom);
  p_a_loss_dad ~ normal(phi_dad, phi_sigma_dad);
  m_a_loss_mom ~ normal(theta_mom, theta_sigma_mom);
  m_a_loss_dad ~ normal(theta_dad, theta_sigma_dad);

  for (a in 2:(N_AGE_C - 1)) {
    for (state in 1:4) {
      if (p_a_sipp_se[a, state] >= 0) { # only sample non-missing survey estimates
        p_a_sipp[a, state] ~ normal(phi_sipp[a - 1, state], p_a_sipp_se[a, state]);
      }
    }
  }

  for (a in 1:(N_AGE_C - 1)) {
    for (trans in 1:5) {
      if (m_a_sipp_se[a, trans] >= 0) { # only sample non-missing survey estimates
        m_a_sipp[a, trans] ~ normal(theta_sipp[a - 1, trans], m_a_sipp_se[a, trans]);
      }
    }    
  }

  phi_sipp[1] ~ dirichlet([95, 2, 2, 1]); 
  phi_mom[1] ~ exponential(5);
  phi_dad[1] ~ exponential(5);
  theta_mom[1] ~ exponential(5);
  theta_dad[1] ~ exponential(5);
  theta_sipp[1] ~ exponential(5);
  for (a in 2:(N_AGE_C - 1)) {
    if (a < (N_AGE_C - 1)) {
      phi_sipp[a] ~ normal(phi_sipp[a - 1], 1); 
      phi_mom[a] ~ normal(phi_mom[a - 1], 1); 
      phi_dad[a] ~ normal(phi_dad[a - 1], 1); 
    }
    theta_sipp[a] ~ normal(theta_sipp[a - 1], 1);
    theta_mom[a] ~ normal(theta_mom[a - 1], 1);
    theta_dad[a] ~ normal(theta_dad[a - 1], 1);
  }

  phi_sigma_mom ~ lognormal(0, 1);
  phi_sigma_dad ~ lognormal(0, 1);
  theta_sigma_mom ~ lognormal(0, 1);
  theta_sigma_dad ~ lognormal(0, 1);
}

generated quantities {
  
}

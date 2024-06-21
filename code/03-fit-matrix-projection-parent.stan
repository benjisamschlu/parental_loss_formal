// matrix projection for a single parent/child sex and a single race/ethnicity group
data {
  // dimensions
  int <lower=1> MAX_AGE; // max age considered
  int <lower=0> MIN_AGE_F; // minimum age with fertility counts
  int <lower=0> MAX_AGE_F; // maximum age with fertility counts
  int <lower=1> N_TIME; // # # of time steps
  // fertility of parents (time by age)
  array[N_TIME] vector <lower=0> [MAX_AGE_F - MIN_AGE_F + 1] r_fert;
  // number of deaths for potential parents only
  array[N_TIME, MAX_AGE - MIN_AGE_F + 1] int <lower=0> n_deaths;
  // population counts for potential parents only
  array[N_TIME, MAX_AGE - MIN_AGE_F + 1] int <lower=0> n_pop;
}

transformed data {
  // number of ages considered
  int <lower=1> N_AGE_C; // children
  int <lower=1> N_AGE_P; // parents until death
  int <lower=1> N_AGE_F; // fertile
  N_AGE_C = MAX_AGE + 1; // include 0 at birth 
  N_AGE_P = MAX_AGE - MIN_AGE_F + 1;
  N_AGE_F = MAX_AGE_F - MIN_AGE_F + 1; // new parents: only consider fertile ages
  array[N_TIME, N_AGE_F] int <lower=0> n_births; // birth counts
  for (t in 1:N_TIME) {
    vector[N_AGE_F] n_pop_fertile;
    n_pop_fertile = to_vector(n_pop[t, :N_AGE_F]);
    n_births[t] = to_int(round(to_array_1d(r_fert[t] .* n_pop_fertile)));
  }
}

parameters {
  array[N_TIME] vector <lower=0> [N_AGE_F] Births_x_t; // parameter for mean fertility rate
  array[N_TIME] vector <lower=0> [N_AGE_P] Deaths_x_t; // parameter for mean mortality rate
}

transformed parameters {
  // construct array of parent transition matrices, U tilde
  // the last 1 row is for mortality rates
  array[N_TIME] matrix[N_AGE_P + 1, N_AGE_P + 1] Utilde;
  for (t in 1:N_TIME) {
    vector[N_AGE_P] M_x_t;
    M_x_t = Deaths_x_t[t] ./ to_vector(n_pop[t]);
    Utilde[t] = rep_matrix(0, N_AGE_P + 1, N_AGE_P + 1);
    Utilde[t][2:, :N_AGE_P] = diag_matrix(1 - M_x_t); // survival
    Utilde[t][N_AGE_P + 1, :N_AGE_P] = M_x_t'; // mortality
  }
  // average number of parent of age {row} for child age {col}
  // last row represents parent deaths during the previous time period
  array[N_TIME] matrix <lower=0>[N_AGE_P + 1, N_AGE_C] K_a_t;
  // projection for t from 1 to T
  for (t in 1:N_TIME) {
    // local variable for previous time index
    int t_prev; 
    t_prev = max(1, t - 1); // initialize with a stable population
    // parent's age distribution at birth
    K_a_t[t] = rep_matrix(0, N_AGE_P + 1, N_AGE_C);
    K_a_t[t][:N_AGE_F, 1] = Births_x_t[t] / sum(Births_x_t[t]); 
    for (a in 2:N_AGE_C) {
      K_a_t[t][, a] = (Utilde[t_prev] * (K_a_t[t_prev][ , a - 1]));
    }
  }

  // link to the SIPP data here
  vector[N_AGE_C - 1] M_a_loss; // mean parent loss incidence
  vector[N_AGE_C - 1] P_a_loss; // mean parent loss prevalence  
  for (a in 1:(N_AGE_C - 1)) {
    P_a_loss[a] = 1 - sum(K_a_t[N_TIME][:N_AGE_P, a]);
    real p_alive;
    p_alive = sum(K_a_t[N_TIME][, a]);
    if (p_alive > 0) 
      M_a_loss[a] = K_a_t[N_TIME][N_AGE_P + 1, a] / sum(K_a_t[N_TIME][, a]);
    else 
      M_a_loss[a] = 0;
  }
}

model {
  for (t in 1:N_TIME) {    
    n_births[t] ~ poisson(Births_x_t[t]);
    n_deaths[t] ~ poisson(Deaths_x_t[t]);
    Births_x_t[t] ~ exponential(0.001);
    Deaths_x_t[t] ~ exponential(0.001);
  }
  // link to the SIPP data here
  // get m_a_loss/p_a_loss from SIPP
  // m_a_loss ~ < >(M_a_loss);
  // p_a_loss ~ < >(p_a_loss);
}

generated quantities {
  
}

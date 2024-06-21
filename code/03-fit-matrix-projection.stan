// matrix projection for a single parent/child sex and a single race/ethnicity group
data {
  // dimensions
  int <lower=1> MAX_AGE_C; // max age considered for children
  int <lower=0> MIN_AGE_F; // minimum age with fertility counts
  int <lower=0> MAX_AGE_F; // maximum age with fertility counts
  int <lower=1> N_TIME; // # # of time steps
  // average number of children at max time
  // matrix[MAX_AGE_C, MAX_AGE_F - MIN_AGE_F + 1] a_x_T;
  // fertility of parents (time by age)
  array[N_TIME] row_vector <lower=0> [MAX_AGE_F - MIN_AGE_F + 1] r_fert;
  // number of deaths, include age 0, exclude max age
  array[N_TIME, MAX_AGE_C] int <lower=0> n_deaths;
  // population counts, include age 0, exclude max age
  array[N_TIME, MAX_AGE_C] int <lower=0> n_pop; 
  // sex ratio at birth
  real <lower=0, upper=1> frac_child_sex; // use 0.49 for female and 0.51 for male
}

transformed data {
  // number of ages considered
  int <lower=1> N_AGE_C;
  int <lower=1> N_AGE_F;
  N_AGE_C = MAX_AGE_C + 1; // child: include 0 at birth 
  N_AGE_F = MAX_AGE_F - MIN_AGE_F + 1; // new parents: only consider fertile ages
  // birth counts
  array[N_TIME, N_AGE_F] int <lower=0> n_births;
  for (t in 1:N_TIME) {
    row_vector [N_AGE_F] n_pop_parents;
    n_pop_parents = to_row_vector(n_pop[t, (MIN_AGE_F + 1):(MAX_AGE_F + 1)]); // age starts at 0
    n_births[t] = to_int(round(to_array_1d(r_fert[t] .* n_pop_parents)));
  }
  // construct array of child surival matrices, U
  array[N_TIME] matrix[N_AGE_C, N_AGE_C] U;
  for (t in 1:N_TIME) {
    // survival rates
    vector [N_AGE_C - 1] Ut;
    for (a in 1:(N_AGE_C - 1)) {
      Ut[a] = 1 - n_deaths[t, a] / n_pop[t, a];
    }
    U[t] = rep_matrix(0, N_AGE_C, N_AGE_C);
    U[t][2:, :(N_AGE_C - 1)] = diag_matrix(Ut);
  }
  // average number of children of age {row} for parent age {col}
  array[N_TIME] matrix <lower=0> [N_AGE_C, N_AGE_F] a_x_t;
  // projection for t from 1 to T
  for (t in 1:N_TIME) {
    int t_prev; // local variable for previous time index
    t_prev = max(1, t - 1); // initialize with a stable population
    // 0 children in the lower triange (age of parent < age of child + min fertility age)
    a_x_t[t] = rep_matrix(0, N_AGE_C, N_AGE_F); 
    a_x_t[t][1, ] = frac_child_sex * r_fert[t_prev]; // child births
    for (x in 2:N_AGE_F) { 
      a_x_t[t][2:, x] = (U[t_prev] * (a_x_t[t_prev][, x - 1]))[2:];
    }
  }
}

parameters {
  array[N_TIME, MAX_AGE_C] real<lower=0> u;
  array[N_TIME, N_AGE_F] real<lower=0> f;
}

transformed parameters {
  // matrix <lower=0>[N_age_child, N_age_parent]a_x_T_mean; // at time T only
  // a_x_T_mean = a_x_t[T];
  // need to connect with the SIPP data here
}

model {
  for (t in 1:N_TIME) {
    n_deaths[t] ~ poisson(u[t]);
    u[t] ~ exponential(.001);
    n_births[t] ~ poisson(f[t]);
    f[t] ~ exponential(.001);
  }
}

# helper functions for fitting stan models
get_stan_data <- function(sipp_pr, sipp_mx, select_race,
                          projected_mom_dist = NULL,
                          projected_dad_dist = NULL) {
    ### the function formats input data for stan ###
    # filter race if selected
    select_race <- sort(select_race)
    MAX_AGE <- AGES[length(AGES) - 1] # do not fit the last interval
    df_p_a <- sipp_pr |>
        filter(race %in% select_race, age < MAX_AGE) |>
        arrange(age, race)
    df_m_a <- sipp_mx |>
        filter(race %in% select_race, age < MAX_AGE) |>
        arrange(age, race)
    N_RACE <- length(select_race)
    
    # sipp data
    N_AGE_C <- df_p_a |> pull("age") |> n_distinct()
    p_a_sipp <- df_p_a |>
        select(
            "pr_lost_none",
            "pr_lost_mom",
            "pr_lost_dad",
            "pr_lost_both"
        ) |>
        mutate(across(everything(), ~ replace_na(.x, -1))) |>
        as.matrix() |>
        array(dim = c(N_RACE, N_AGE_C, 4))
    p_a_sipp_se <- df_p_a |>
        select(
            "se_pr_lost_none",
            "se_pr_lost_mom",
            "se_pr_lost_dad",
            "se_pr_lost_both"
        ) |>
        mutate(across(everything(), ~ replace_na(.x, -1))) |>
        as.matrix() |>
        array(dim = c(N_RACE, N_AGE_C, 4))
    m_a_sipp <- df_m_a |>
        select(
            "mx_lost_none_to_lost_mom",
            "mx_lost_none_to_lost_dad",
            "mx_lost_none_to_lost_both",
            "mx_lost_mom_to_lost_both",
            "mx_lost_dad_to_lost_both"
        ) |>
        mutate(across(everything(), ~ replace_na(.x, 0))) |>
        as.matrix() |>
        array(dim = c(N_RACE, N_AGE_C, 5))
    m_a_sipp_se <- df_m_a |>
        select(
            "se_mx_lost_none_to_lost_mom",
            "se_mx_lost_none_to_lost_dad",
            "se_mx_lost_none_to_lost_both",
            "se_mx_lost_mom_to_lost_both",
            "se_mx_lost_dad_to_lost_both"
        ) |>
        mutate(across(everything(), ~ replace_na(.x, 0))) |>
        as.matrix() |>
        array(dim = c(N_RACE, N_AGE_C, 5))
    
    # design matrix for the linear models
    covars <- data.frame(
        age = rep(unique(df_p_a$age), N_RACE),
        race = rep(select_race, each = N_AGE_C)
    )
    ll_X <- model.matrix(~ 1 + age * race, data = covars)
    
    
    stopifnot(
        expres = {
            dim(p_a_sipp)[2] == N_AGE_C
            dim(m_a_sipp)[2] == N_AGE_C
            all(dim(p_a_sipp) == dim(p_a_sipp_se))
            all(dim(m_a_sipp) == dim(m_a_sipp_se))
        }
    )
    
    stan_data <- list(
        N_RACE = N_RACE,
        N_AGE_C = N_AGE_C,
        ll_X = ll_X,
        p_a_sipp = p_a_sipp,
        p_a_sipp_se = p_a_sipp_se,
        m_a_sipp = m_a_sipp,
        m_a_sipp_se = m_a_sipp_se
    )
    
    if (is.null(projected_mom_dist)) { return(stan_data) }
    
    # projection data
    projected_mom_dist <- projected_mom_dist |>
        filter(race %in% select_race) |>
        arrange(age, race)
    projected_dad_dist <- projected_dad_dist |>
        filter(race %in% select_race) |>
        arrange(age, race)
    
    m_a_lost_mom <- projected_mom_dist |>
        pull("m_lost") |>
        array(dim = c(N_RACE, N_AGE_C))
    
    m_a_lost_dad <- projected_dad_dist |>
        pull("m_lost") |>
        array(dim = c(N_RACE, N_AGE_C))
    
    p_a_lost_mom <- projected_mom_dist |>
        pull("p_lost") |>
        array(dim = c(N_RACE, N_AGE_C))
    
    p_a_lost_dad <- projected_dad_dist |>
        pull("p_lost") |>
        array(dim = c(N_RACE, N_AGE_C))
    
    # stan data
    stan_data <- c(
        stan_data,
        list(
            m_a_lost_mom = m_a_lost_mom,
            m_a_lost_dad = m_a_lost_dad,
            p_a_lost_mom = p_a_lost_mom,
            p_a_lost_dad = p_a_lost_dad
        )
    )
    
    if (any(grepl("sim_", colnames(projected_dad_dist)))) {
        m_a_lost_mom_se <- projected_mom_dist |>
            pull("sim_se_m_lost") |>
            array(dim = c(N_RACE, N_AGE_C))
        
        m_a_lost_dad_se <- projected_dad_dist |>
            pull("sim_se_m_lost") |>
            array(dim = c(N_RACE, N_AGE_C))
        
        p_a_lost_mom_se <- projected_mom_dist |>
            pull("sim_se_p_lost") |>
            array(dim = c(N_RACE, N_AGE_C))
        
        p_a_lost_dad_se <- projected_dad_dist |>
            pull("sim_se_p_lost") |>
            array(dim = c(N_RACE, N_AGE_C))
        
        stan_data <- c(
            stan_data,
            list(
                m_a_lost_mom_se = m_a_lost_mom_se,
                m_a_lost_dad_se = m_a_lost_dad_se,
                p_a_lost_mom_se = p_a_lost_mom_se,
                p_a_lost_dad_se = p_a_lost_dad_se
            )
        )
    }
    
    return(stan_data)
}

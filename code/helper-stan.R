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
    
    # design matrix for the log-linear models
    n_state <- 4
    covars <- data.frame(
        age = rep(unique(df_p_a$age), n_state * N_RACE),
        states = factor(rep(1:n_state, each = N_AGE_C)),
        race = rep(select_race, each = N_AGE_C * n_state)
    )
    ll_X_phi <- model.matrix(~ 1 + age * states * race, data = covars)
    
    n_transition <- 4
    covars <- data.frame(
        age = rep(unique(df_p_a$age), n_transition * N_RACE),
        transitions = factor(rep(1:n_transition, each = N_AGE_C)),
        race = rep(select_race, each = N_AGE_C * n_transition)
    )
    ll_X_theta <- model.matrix(~ 1 + age * transitions * race, data = covars)
    
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
        ll_X_phi = ll_X_phi,
        ll_X_theta = ll_X_theta,
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

compute_lt_single <- function(phis, thetas, lt) {
    compute_ex <- function(Lx, lx) {
        rev(cumsum(rev(Lx))) / lx
    }
    colnames(phis) <- paste0("phi_", 1:4)
    colnames(thetas) <- paste0("theta_", c(1,1,1,2,3), "_", c(2:4,4,4))
    
    # restrict the last age interval - all children eventually lose both parents
    phis <- rbind(phis, c(0, 0, 0, 1))
    thetas <- rbind(thetas, c(0, 0, 0, 0, 0))
    
    bind_cols(lt, phis, thetas) |>
        mutate( # compute Lx_i
            Lx_1 = Lx * phi_1,
            Lx_2 = Lx * phi_2,
            Lx_3 = Lx * phi_3,
            Lx_4 = Lx * phi_4
        ) |>
        mutate( # compute dx_i
            dx_1 = dx * phi_1,
            dx_2 = dx * phi_2,
            dx_3 = dx * phi_3,
            dx_4 = dx * phi_4
        ) |>
        mutate( # compute dx_ij = mx_ij * Lx_i;
            dx_1_2 = theta_1_2 * Lx_1,
            dx_1_3 = theta_1_3 * Lx_1,
            dx_1_4 = theta_1_4 * Lx_1,
            dx_2_4 = theta_2_4 * Lx_2,
            dx_3_4 = theta_3_4 * Lx_3
        ) |>
        mutate( # compute ex_i
            ex_1 = compute_ex(Lx_1, lx),
            ex_2 = compute_ex(Lx_2, lx),
            ex_3 = compute_ex(Lx_3, lx),
            ex_4 = compute_ex(Lx_4, lx),
            ex = compute_ex(Lx, lx)
        ) |>
        mutate( # compute lx_i
            lx_1 = c(first(lx), first(lx) - cumsum(
                dx_1_2 + dx_1_3 + dx_1_4 + dx_1
            )[-length(lx)]),
            lx_2 = c(0, cumsum(
                dx_1_2 - dx_2_4 - dx_2
            )[-length(lx)]),
            lx_3 = c(0, cumsum(
                dx_1_3 - dx_3_4 - dx_3
            )[-length(lx)]),
            lx_4 = c(0, cumsum(
                dx_1_4 + dx_2_4 + dx_3_4 - dx_4
            )[-length(lx)])
        )
}

compute_lt <- function(fit, lt, select_race) {
    extracted <- rstan::extract(fit, permuted = TRUE)
    n_sim <- nrow(extracted[[1]])
    select_race <- sort(select_race)
    samps <- lapply(seq(n_sim), function(i){
        phis <- lapply(
            seq(length(select_race)),
            function(j) { extracted[["phi_msm"]][i,j,,] }
        )
        thetas <- lapply(
            seq(length(select_race)),
            function(j) { extracted[["theta_msm"]][i,j,,] }
        )
        phi_mom <- lapply(
            seq(length(select_race)),
            function(j) { extracted[["phi_mom"]][i,j,] }
        )
        phi_dad <- lapply(
            seq(length(select_race)),
            function(j) { extracted[["phi_dad"]][i,j,] }
        )
        theta_mom <- lapply(
            seq(length(select_race)),
            function(j) { extracted[["theta_mom"]][i,j,] }
        )
        theta_dad <- lapply(
            seq(length(select_race)),
            function(j) { extracted[["theta_dad"]][i,j,] }
        )
        lapply(
            seq(length(select_race)),
            function(j) { 
                lt <- filter(lt, race == select_race[j])
                compute_lt_single(phis[[j]], thetas[[j]], lt) |>
                    mutate(
                        unc_phi_mom = c(phi_mom[[j]], 1),
                        unc_phi_dad = c(phi_dad[[j]], 1),
                        unc_theta_mom = c(theta_mom[[j]], -1),
                        unc_theta_dad = c(theta_dad[[j]], -1),
                        race = select_race[j]
                    )
            }
        ) |>
            bind_rows()
    }) |> 
        bind_rows()
    cols <- colnames(samps)
    cols <- cols[grep("^(lx)|(dx)|(Lx)|(ex)|(phi)|(theta)", cols)]
    samps |>
        summarise(
            .by = c("race", "age"),
            across(all_of(cols), mean, .names = "{.col};mean"),
            across(all_of(cols), sd, .names = "{.col};sd"),
            across(all_of(cols), ~ quantile(.x, .025), .names = "{.col};q025"),
            across(all_of(cols), ~ quantile(.x, .05), .names = "{.col};q050"),
            across(all_of(cols), ~ quantile(.x, .1), .names = "{.col};q100"),
            across(all_of(cols), ~ quantile(.x, .25), .names = "{.col};q250"),
            across(all_of(cols), ~ quantile(.x, .5), .names = "{.col};q500"),
            across(all_of(cols), ~ quantile(.x, .75), .names = "{.col};q750"),
            across(all_of(cols), ~ quantile(.x, .9), .names = "{.col};q900"),
            across(all_of(cols), ~ quantile(.x, .95), .names = "{.col};q950"),
            across(all_of(cols), ~ quantile(.x, .975), .names = "{.col};q975")
        ) |>
        pivot_longer(
            -c("race", "age"), 
            names_sep = ";", 
            names_to = c("param", "name")
        ) |>
        pivot_wider(id_cols = c("race", "age", "param"))
}

extract_pp <- function(fit, select_race) {
    extracted <- rstan::extract(fit, permuted = TRUE)
    select_race <- sort(select_race)
    pp_names <- names(extracted)[grepl("_pp$", names(extracted))]
    
    lapply(
        pp_names,
        function(param) {
            d <- dim(extracted[[param]])
            if (grepl("_msm_", param)) {
                if (grepl("^p_a_", param)) {
                    nms <- paste0("pr_", 1:4)
                } else {
                    nms <- paste0("mx_", c(1,1,1,2,3), "_", c(2:4,4,4))
                }
                matrix(
                    extracted[[param]], 
                    nrow = prod(d[-length(d)]), 
                    ncol = d[length(d)]
                ) |>
                    as.data.frame() |>
                    rename_with(~ nms) |>
                    mutate(
                        race = rep(select_race, prod(d[c(1,3)])),
                        age = rep(AGES[-c(length(AGES) - c(0, 1))], each = prod(d[1:2]))
                    ) |>
                    pivot_longer(
                        tidyselect::matches("(pr_)|(mx_)"),
                        names_to = "param"
                    )
            } else {
                nm <- str_replace(param, "m_a_lost", "unc_mx") |>
                    str_replace("p_a", "unc_pr") |>
                    str_replace("_pp", "")
                out <- data.frame(
                    race = rep(select_race, prod(d[1:3])),
                    age = rep(AGES[-c(length(AGES) - c(0, 1))], each = prod(d[1:2]))
                ) 
                out[[nm]] <- c(extracted[[param]])
                out |>
                    pivot_longer(
                        tidyselect::matches("(pr_)|(mx_)"),
                        names_to = "param"
                    )
            }
        }
    ) |>
    bind_rows()
}

extract_sigmas <- function(fit) {
    sigs <- rstan::summary(
        fit, 
        pars = names(fit)[grepl("sigma", names(fit))], 
        probs = c(.025, .05, .1, .25, .5, .75, .9, .95, .975)
    )$summary
    sigs |>
        as_tibble() |>
        select(mean:`97.5%`, -se_mean) |>
        mutate(param = rownames(sigs)) |>
        rename_with(
            function(x) {
                q <- str_extract(x, "\\d+\\.?\\d?") |>
                    as.numeric()
                paste0("q", str_pad(q * 10, 3, pad = "0"))
            },
            `2.5%`:`97.5%`
        )
}

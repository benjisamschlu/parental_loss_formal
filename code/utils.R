# common variables
RACES <- c(
    "all", 
    "non-hispanic white", 
    "non-hispanic black", 
    # "non-hispanic asian", 
    # "non-hispanic other", 
    "hispanic"
)
SEXES <- c("all", "male", "female")
AGE_INT <- 5
AGES <- c(seq(0, 65, by = AGE_INT), Inf) # left closed breaks
STATES <- c(
    `1` = "lost_none",
    `2` = "lost_mom",
    `3` = "lost_dad",
    `4` = "lost_both"
)
PERIODS <- c(1, seq(10, 30, by = 10))

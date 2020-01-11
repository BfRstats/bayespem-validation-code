add.config(
    verbose = TRUE,               # if TRUE, additional messages will be output for each script that is being run
    validation_study = list(
        data_random_seed = 4,     # random seed for reproducible data generation
        n_parsets = 20,           # number of parameter sets
        n_reps = 20,              # number of replicate datasets for each parameter set
    
        # statistical parameters
        se_range = c(0.6, 1),     # range for true sensitivity values
        sp_range = c(0.6, 1),     # range for true specificity values
        pi_range = c(0, 1),       # range for true prevalence values
        
        # design parameters
        n_se_range = c(50, 100, 200, 500, 1000, 2000, 5000),      # possible values for the sample size of a study that has been conducted to validate the sensitivity of the diagnostic test
        n_sp_range = c(50, 100, 200, 500, 1000, 2000, 5000),      # possible values for the sample size of a study that has been conducted to validate the specificity of the diagnostic test
        n_range    = c(50, 2000),                                 # possible values for the size of the sample that the diagnostic test has been applied to in order to estimate a prevalence
        
        # JAGS parameters
        n_chains = 3,             # number of MCMC chains
        n_samples = 20000         # total number of samples that runjags uses to assess convergence
    )
)

#' extract validation study config to global environment so that the parameters become available to all scripts:
list2env(config$validation_study, globalenv())

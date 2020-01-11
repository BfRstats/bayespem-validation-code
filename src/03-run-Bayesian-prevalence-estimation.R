#' # Bayesian estimation
t0_script <- Sys.time()
if (config$verbose) message(" Running script 03")

#' ## JAGS Model String
jags_model_string <- "model {

    pi ~  dbeta(pi_prior[1], pi_prior[2])    # prevalence
    se ~  dbeta(se_prior[1], se_prior[2])    # sensitivity
    sp ~  dbeta(sp_prior[1], sp_prior[2])    # specificity
    ap <- pi*se + (1-pi)*(1-sp)              # apparent prevalence
    x  ~  dbin(ap, n)                        # number of positive samples
    
    #inits# pi, se, sp, .RNG.seed, .RNG.name
    #monitor# pi, se, sp

}"

#' ## JAGS Inits Function
jags_inits_function <- function(chain) {
    stopifnot(chain %in% c(1, 2, 3, 4, 5))
    pi <- c(0.2, 0.5, 0.8, 0.9, 0.7)[chain]
    se <- c(0.9, 0.8, 0.7, 0.61, 0.75)[chain]
    sp <- c(0.7, 0.9, 0.8, 0.85, 0.95)[chain]
    .RNG.seed <- c(1, 2, 3, 4, 5)[chain]
    .RNG.name <- c("base::Mersenne-Twister",
                   "base::Super-Duper", 
                   "base::Marsaglia-Multicarry",
                   "base::Wichmann-Hill",
                   "base::Mersenne-Twister")[chain]
    inits.list <- list(.RNG.seed = .RNG.seed,
                       .RNG.name = .RNG.name,
                       pi = pi, se = se, sp = sp)
    return(inits.list)
}

#' ## Simulation
t0 <- Sys.time()
n_cpu <- detectCores()
if (config$verbose) message(sprintf("  Setting up cluster on %d CPUs for parallel execution", n_cpu), appendLF = FALSE)
.suppressOutput({
    sfInit(parallel = TRUE, cpus = n_cpu, type = "SOCK")
    sfExport("jags_model_string", "jags_inits_function", 
             "n_chains", "n_samples", "n_reps")
    sfLibrary(runjags)
    sfLibrary(dplyr)
    sfLibrary(data.table)
})
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' wrapper for autorun.jags on a parameter set that can be parallelized for the
#' repetitions done on a parameter set
parset_autorun <- function(rep_id) {
    dat <- dataset[rep_id, ]
    jags_dat <- list(x = dat$x, n = dat$n, 
                     pi_prior = c(1, 1),
                     se_prior = c(dat$a_se, dat$b_se),
                     sp_prior = c(dat$a_sp, dat$b_sp))
    success <- try({
        jags_res <- autorun.jags(model = jags_model_string, 
                                 data  = dump.format(jags_dat),
                                 inits = jags_inits_function,
                                 max.time = "2m",
                                 startsample = n_samples,
                                 n.chains = n_chains, 
                                 method = "rjags", 
                                 silent.jags = TRUE,
                                 plots = FALSE)
    })
    if (inherits(success, "try-error")) {
        nas <- rep(NA_real_, 3)
        res <- data.table(Parameter = c("pi", "se", "sp"),
                          Lower95 = nas, Median = nas, Upper95 = nas,
                          Mean = nas, SD = nas, Mode = nas,
                          MCerr = nas, "MC%ofSD" = nas, SSeff = nas, AC.10 = nas, psrf = nas,
                          "2.5%" = nas, "25%" = nas, "50%" = nas, "75%" = nas, "97.5%" = nas)
    } else {
        res <- cbind(summary(jags_res), jags_res$summary$quantiles) %>% 
            as.data.table(keep.rownames = TRUE)
        colnames(res)[1] <- "Parameter"
    }
    res <- cbind(data_id = rep(dat$data_id, n_chains), res)
    return(res)
}

# t_start <- Sys.time()
t0 <- Sys.time()
if (config$verbose) message("   Running Bayesian estimation model for parameter sets")
results_list <- list()
for (i in 1:n_parsets) {
    t0_parset <- Sys.time()
    
    pars <- parsets %>%
        as_tibble() %>% 
        filter(par_id == i)
    if (config$verbose) message(sprintf("    #%d (prev = %.3f, n = %d, se = %.3f, n_se = %d, sp = %.3f, n_sp = %d)",
                                        i, pars$pi_true, pars$n, pars$se_true, pars$n_se, pars$sp_true, pars$n_sp), 
                                appendLF = FALSE)
    dataset <- datasets %>% 
        as_tibble() %>% 
        filter(par_id == i) %>% 
        as.data.table()
    sfExport("dataset")
    
    parset_results <- sfLapply(1:n_reps, parset_autorun)
    results_list[[i]] <- rbindlist(parset_results)
    
    # n_failed <- sum(is.na(results_list[[i]]$Mean)) / n_chains
    if (config$verbose) message(sprintf(": %s", interval(t0_parset, Sys.time()) %>% as.duration() %>% signif(digits = 2)))
}
jags_results <- rbindlist(results_list)
if (config$verbose) message(sprintf("Total run time for Bayesian estimation: %s", 
                                    interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' stop the cluster:
if (config$verbose) message("  Stopping cluster")
.suppressOutput(sfStop())

#' ## Calculate bias and coverage (does the HDI contain the true prevalence?):
t0 <- Sys.time()
if (config$verbose) message("  Calculating bias and coverage ", appendLF = FALSE)
true_idx <- datasets[, .(data_id, par_id, rep_id, 
                         pi = pi_true, se = se_true, sp = sp_true)] %>% 
    as_tibble() %>% 
    gather(Parameter, Target, pi:sp) %>% 
    as.data.table()

jags_results <- merge(true_idx, jags_results, by = c("data_id", "Parameter"), all.y = TRUE)
jags_results[, `:=`(Bias_Mean  = Mean   - Target,
                    RangeHDI   = Upper95 - Lower95,
                    WithinHDI  = .withinRange(Target, Lower95, Upper95),
                    RangeETI   = `97.5%` - `2.5%`,
                    WithinETI  = .withinRange(Target, `2.5%`, `97.5%`)),
             by = c("data_id", "Parameter")]

jags_results[, Parameter := as.factor(Parameter)]

n_failed_runs <- sum(is.na(jags_results$Median)) / n_chains

estimates_Bayesian <- jags_results %>% 
    as_tibble() %>% 
    filter(Parameter == "pi") %>% 
    mutate(method = "Bayesian-Mean") %>% 
    select(data_id,
           method,
           value = Mean,
           bias = Bias_Mean) %>% 
    as.data.table()
setkey(estimates_Bayesian, data_id)
estimates_Bayesian[dataset_cases, data_case := i.data_case]
estimates_Bayesian <- estimates_Bayesian[data_case %in% c("1", "2", "3"), 
                                         .(data_id, data_case, method, value, bias)]

#' Bayesian Highest Density Interval (HDI):
confints_Bayesian <- jags_results %>% 
    as_tibble() %>% 
    filter(Parameter == "pi") %>% 
    select(data_id, lcl = Lower95, ucl = Upper95, 
                  length = RangeHDI, pi_true = Target, withinCI = WithinHDI) %>% 
    mutate(method = "Bayesian-HDI") %>% 
    as.data.table()
setkey(confints_Bayesian, data_id)
confints_Bayesian[dataset_cases, data_case := i.data_case]
confints_Bayesian <- confints_Bayesian[data_case %in% c("1", "2", "3"), 
                                         .(data_id, data_case, method, lcl, ucl, length, withinCI)]

if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' Cache
t0 <- Sys.time()
# cache("jags_results")
cache("estimates_Bayesian")
cache("confints_Bayesian")
if (config$verbose) message(sprintf("  Caching run time: %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

if (config$verbose) message(sprintf(
    "  Total script run time: %s", 
    interval(t0_script, Sys.time()) %>% 
        as.duration() %>% 
        signif(digits = 2)
))
rm("t0", "t0_script", "t0_parset", "i", "pars", "true_idx", "parset_results", "results_list", "jags_results")

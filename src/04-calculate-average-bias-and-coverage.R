#' # Point Estimates and Biases
#'
t0_script <- Sys.time()
if (config$verbose) message(" Running script 04")

#' Combine Rogan-Gladen and Bayesian estimates:
estimates <- rbindlist(list(
    estimates_Rogan_Gladen, 
    estimates_Bayesian
), use.names = TRUE)
estimates[, method := factor(method)]

#' Add ID's for parameter set and repetition, and the Rogan-Gladen case:
setkey(estimates, data_id)
estimates[datasets, `:=`(par_id = i.par_id,
                         rep_id = i.rep_id)]
setcolorder(estimates, c("data_id", "data_case", "par_id", "rep_id", "method", "value", "bias"))
# estimates[dataset_cases, data_case := i.data_case]

#' ## Averages
#' 
#' Calculate average biases by parameter set and estimation method, and add 
#' true parameter set values:
t0 <- Sys.time()
if (config$verbose) message("  Calculating average bias by parameter set and estimation method", appendLF = FALSE) 
deviation_by_parset <- estimates[, .(bias    = mean(bias, na.rm = TRUE),
                                     mad     = median(abs(bias), na.rm = TRUE),
                                     n_group = .N), 
                                 by = c("par_id", "method")]
setkey(deviation_by_parset, par_id)
deviation_by_parset[parsets, `:=`(pi_true = i.pi_true,
                                  se_true = i.se_true, 
                                  sp_true = i.sp_true)]
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' # Confidence Intervals
t0 <- Sys.time()
if (config$verbose) message("  Calculating coverage by parameter set and CI method", appendLF = FALSE)

#' First, combine conventional and Bayesian confidence intervals:
confints <- rbindlist(list(
    confints_Clopper_Pearson,
    confints_Sterne,
    confints_Blaker,
    confints_Lang_Reiczigel,
    confints_Wald_Rogan_Gladen, 
    confints_Bayesian
), use.names = TRUE)[order(data_id, method), ]
confints[, method := factor(method,
                            levels = c("Bayesian-HDI",
                                       "Lang-Reiczigel", "Wald-Rogan-Gladen",
                                       "Sterne", "Blaker", "Clopper-Pearson"))]
setkey(confints, data_id)
confints[datasets, `:=`(par_id = i.par_id,
                        rep_id = i.rep_id)]
setcolorder(confints, c("data_id", "data_case", "par_id", "rep_id", "method", "lcl", "ucl", "length", "withinCI"))
# confints[dataset_cases, data_case := i.data_case]

#' ## Coverage
#' 
#' Proportion within CI and average CI length by parameter set and method:
coverage_by_parset <- confints[, .(prop_withinCI = mean(withinCI, na.rm = TRUE),
                                   av_lengthCI = mean(length, na.rm = TRUE),
                                   n_group = .N), 
                               by = .(par_id, method)]
setkey(coverage_by_parset, par_id)
coverage_by_parset[parsets, `:=`(pi_true = i.pi_true,
                                 se_true = i.se_true, 
                                 sp_true = i.sp_true)]

#' Absolute deviation from nominal value (95%):
coverage_by_parset[, abs_dev := abs(prop_withinCI - 0.95)]
setcolorder(coverage_by_parset, c("par_id", "n_group", "method", "prop_withinCI", "abs_dev", "av_lengthCI", 
                                  "pi_true", "se_true", "sp_true"))

if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' Cache
t0 <- Sys.time()
cache("deviation_by_parset")
cache("coverage_by_parset")
if (config$verbose) message(sprintf("  Caching run time: %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

if (config$verbose) message(sprintf(
    "  Total script run time: %s", 
    interval(t0_script, Sys.time()) %>% 
        as.duration() %>% 
        signif(digits = 2)
))

rm("t0", "t0_script")

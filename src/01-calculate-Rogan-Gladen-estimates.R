#' # Rogan-Gladen point estimate of true prevalence (RGE)
#' We calculate the point estimate, (forcing it to a proportion if
#' necessary), whether the conditions for a valid proportions are 
#' satisfied, and the bias of the RGE:
t0_script <- Sys.time()
if (config$verbose) message(" Running script 01")

t0 <- Sys.time()
if (config$verbose) message("  Calculating Rogan-Gladen estimates and biases", appendLF = FALSE)
estimates_Rogan_Gladen <- datasets[, .(par_id, rep_id, data_id, pi_true, x, n, se, sp)]
estimates_Rogan_Gladen[, `:=`(value     = .rge(x, n, se, sp),             # RGE
                              data_case = .check.case(x, n, se, sp)),     # data set case according to conditions 4 in the article
                       by = data_id]

#' extract data set cases for future use:
dataset_cases <- estimates_Rogan_Gladen[, .(data_id, data_case)]
dataset_cases[, data_case := factor(data_case)]

#' calculate bias for each estimate:
estimates_Rogan_Gladen[, `:=`(bias   = value - pi_true, 
                              method = "Rogan-Gladen"),
                       by = data_id]

#' only keep valid cases and certain columns:
estimates_Rogan_Gladen <- estimates_Rogan_Gladen[data_case %in% c("1", "2", "3"), 
                                                 .(data_id, data_case, method, value, bias)]

if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' For a few data sets, invalid tests may be produced by accident
#' (where se + sp > 1 is not satisfied):
n_invalid_datasets <- nrow(dataset_cases[!(data_case %in% c("1", "2", "3")), ])

#' Cache
t0 <- Sys.time()
cache("estimates_Rogan_Gladen")
cache("dataset_cases")
cache("n_invalid_datasets")
if (config$verbose) message(sprintf("  Caching run time: %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

if (config$verbose) message(sprintf(
    "  Total script run time: %s", 
    interval(t0_script, Sys.time()) %>% 
        as.duration() %>% 
        signif(digits = 2)
))
rm("t0", "t0_script")

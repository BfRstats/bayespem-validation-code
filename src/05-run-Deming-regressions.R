#' # Run Deming regression for bias and CI lengths

t0_script <- Sys.time()
if (config$verbose) message(" Running script 05")

#' ## Deming regression of the Bayesian-Mean's bias vs the RGE's bias
t0 <- Sys.time()
if (config$verbose) message("  Running Deming regression on biases", appendLF = FALSE)
#' Combine Rogan-Gladen and Bayesian estimates:
estimates <- rbindlist(list(
    estimates_Rogan_Gladen, 
    estimates_Bayesian
), use.names = TRUE)
bias_df <- estimates %>% 
    as_tibble() %>% 
    select(data_id, data_case, method, bias) %>% 
    spread(method, bias)
bias_corr_coeff <- cor(bias_df$"Rogan-Gladen", bias_df$"Bayesian-Mean")
bias_deming <- mcreg(bias_df$"Rogan-Gladen", bias_df$"Bayesian-Mean",
                     method.reg = "Deming", rng.seed = 76,
                     na.rm = TRUE)
# bias_deming@para
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))


#' ## Deming regression of the Bayesian-HDI's length vs the Lang-Reiczigel CI's length
t0 <- Sys.time()
if (config$verbose) message("  Running Deming regression on CI lengths", appendLF = FALSE)
#' First, combine conventional and Bayesian confidence intervals:
confints <- rbindlist(list(
    confints_Clopper_Pearson,
    confints_Sterne,
    confints_Blaker,
    confints_Lang_Reiczigel,
    confints_Wald_Rogan_Gladen, 
    confints_Bayesian
), use.names = TRUE)[order(data_id, method), ] %>% 
    droplevels()
confints[, method := factor(method,
                            levels = c("Bayesian-HDI",
                                       "Lang-Reiczigel", "Wald-Rogan-Gladen",
                                       "Sterne", "Blaker", "Clopper-Pearson"))]
cilength_df <- confints[method %in% c("Bayesian-HDI", "Lang-Reiczigel"), ] %>% 
    as_tibble() %>% 
    select(data_id, data_case, method, length) %>% 
    spread(method, length)
cilength_corr_coeff <- cor(cilength_df$"Lang-Reiczigel", cilength_df$"Bayesian-HDI")
cilength_deming <- mcreg(x = cilength_df$"Lang-Reiczigel", 
                         y = cilength_df$"Bayesian-HDI", 
                         method.reg = "Deming", na.rm = TRUE, rng.seed = 54)
# cilength_deming@para
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))


#' Cache
t0 <- Sys.time()
# cache("estimates_by_datacase")
cache("bias_df")
cache("bias_corr_coeff")
cache("bias_deming")
cache("cilength_df")
cache("cilength_corr_coeff")
cache("cilength_deming")
if (config$verbose) message(sprintf("  Caching run time: %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

if (config$verbose) message(sprintf(
    "  Total script run time: %s", 
    interval(t0_script, Sys.time()) %>% 
        as.duration() %>% 
        signif(digits = 2)
))
rm("t0", "t0_script")

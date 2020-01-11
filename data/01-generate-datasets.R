#' # Generate data sets
#'
t0_script <- Sys.time()

set.seed(data_random_seed)

#' ## Set of all parameter combinations
t0 <- Sys.time()
if (config$verbose) message(sprintf("  Generating %d parameter sets", n_parsets), appendLF = FALSE)
parsets <- data.frame(
    par_id  = 1:n_parsets,
    pi_true = runif(n_parsets, min = pi_range[1], max = pi_range[2]),
    se_true = runif(n_parsets, min = se_range[1], max = se_range[2]),
    n_se    = sample(n_se_range, size = n_parsets, replace = TRUE),
    sp_true = runif(n_parsets, min = sp_range[1], max = sp_range[2]),
    n_sp    = sample(n_sp_range, size = n_parsets, replace = TRUE),
    # k       = k,
    n       = sample(seq(from = n_range[1], to = n_range[2]), 
                     size = n_parsets, replace = TRUE)
)
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Datasets
t0 <- Sys.time()
if (config$verbose) message(sprintf("  Generating %d data sets", n_parsets * n_reps), appendLF = FALSE)
datasets <- expand.grid(rep_id = 1:n_reps, par_id = 1:n_parsets) %>% 
    # dplyr::as_data_frame() %>% 
    as_tibble() %>% 
    mutate(data_id = .generate.data.id(par_id, rep_id, n_reps)) %>% 
    select(data_id, par_id, rep_id) %>% 
    left_join(parsets, by = "par_id")
n_datasets <- nrow(datasets)

datasets %<>% 
    mutate(
        x_se = rbinom(n_datasets, size = n_se, prob = se_true),
        se = x_se / n_se,
        a_se = n_se * se + 1,
        b_se = n_se * (1 - se) + 1,
        
        x_sp = rbinom(n_datasets, size = n_sp, prob = sp_true),
        sp = x_sp / n_sp,
        a_sp = n_sp * sp + 1,
        b_sp = n_sp * (1 - sp) + 1,
        
        ap_true = pi_true * se_true + (1 - pi_true) * (1 - sp_true),
        # p_pos = 1 - (1 - ap_true) ^ k,
        
        # x = rbinom(n_datasets, size = n, prob = p_pos),
        x = rbinom(n_datasets, size = n, prob = ap_true),
        ap = x / n
    ) %>% 
    select(data_id, par_id, rep_id, pi_true,
           se_true, se, x_se, n_se, a_se, b_se,
           sp_true, sp, x_sp, n_sp, a_sp, b_sp,
           ap_true, ap, x,    n)  # p_pos, k, 
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Convert to data.table
t0 <- Sys.time()
if (config$verbose) message("  Converting to data.tables", appendLF = FALSE)
parsets <- as.data.table(parsets)
datasets <- as.data.table(datasets)
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Cache
t0 <- Sys.time()
cache("parsets")
cache("datasets")
if (config$verbose) message(sprintf("  Caching run time: %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

if (config$verbose) message(sprintf(
    "  Total script run time: %s", 
    interval(t0_script, Sys.time()) %>% 
        as.duration() %>% 
        signif(digits = 2)
))
rm("t0", "t0_script")

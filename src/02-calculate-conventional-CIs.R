#' # Confidence intervals
#' 
#' We calculate the Clopper-Pearson, Sterne, Blake, and
#' Lang-Reiczigel 95% confidence intervals (CI) for the 
#' Rogan-Gladen prevalence estimates and use parallelization.
t0_script <- Sys.time()
if (config$verbose) message(" Running script 02")

t0 <- Sys.time()
n_cpu <- detectCores()
n_chunks <- n_cpu
if (config$verbose) message(sprintf("  Setting up cluster on %d CPUs for parallel execution", n_cpu), appendLF = FALSE)
.suppressOutput({
    sfInit(parallel = TRUE, cpus = n_cpu, type = "SOCK")
    sfExport("datasets", "n_chunks")
    sfSource("lib/01-helpers.R")
    sfSource("lib/02-ci4prev-Reiczigel_et_al.R")
    sfLibrary(data.table)
})
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Clopper-Pearson
meth <- "Clopper-Pearson"
t0 <- Sys.time()
if (config$verbose) message(sprintf("   Calculating %s CIs", meth), appendLF = FALSE)
results <- sfLapply(1:n_chunks, .confint.parallel, method = meth)
confints_Clopper_Pearson <- rbindlist(results)
#' get rid of invalid data set cases:
setkey(confints_Clopper_Pearson, data_id)
confints_Clopper_Pearson[dataset_cases, data_case := i.data_case]
confints_Clopper_Pearson <- confints_Clopper_Pearson[data_case %in% c("1", "2", "3"), ]
#' calculate CI length:
confints_Clopper_Pearson[, length := ucl - lcl, by = data_id]
#' does the CI include the true prevalence?
confints_Clopper_Pearson[datasets, pi_true := i.pi_true]
confints_Clopper_Pearson[, withinCI := .withinRange(pi_true, lcl, ucl), 
                         by = data_id]
confints_Clopper_Pearson <- confints_Clopper_Pearson[, .(data_id, data_case, method, lcl, ucl, length, withinCI)]
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Wald-Rogan-Gladen
meth <- "Wald-Rogan-Gladen"
t0 <- Sys.time()
if (config$verbose) message(sprintf("   Calculating %s CIs", meth), appendLF = FALSE)
results <- sfLapply(1:n_chunks, .confint.parallel, method = meth)
confints_Wald_Rogan_Gladen <- rbindlist(results)
#' get rid of invalid data set cases:
setkey(confints_Wald_Rogan_Gladen, data_id)
confints_Wald_Rogan_Gladen[dataset_cases, data_case := i.data_case]
confints_Wald_Rogan_Gladen <- confints_Wald_Rogan_Gladen[data_case %in% c("1", "2", "3"), ]
#' calculate CI length:
confints_Wald_Rogan_Gladen[, length := ucl - lcl, by = data_id]
#' does the CI include the true prevalence?
confints_Wald_Rogan_Gladen[datasets, pi_true := i.pi_true]
confints_Wald_Rogan_Gladen[, withinCI := .withinRange(pi_true, lcl, ucl), 
                           by = data_id]
confints_Wald_Rogan_Gladen <- confints_Wald_Rogan_Gladen[, .(data_id, data_case, method, lcl, ucl, length, withinCI)]
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Lang-Reiczigel
meth <- "Lang-Reiczigel"
t0 <- Sys.time()
if (config$verbose) message(sprintf("   Calculating %s CIs", meth), appendLF = FALSE)
results <- sfLapply(1:n_chunks, .confint.parallel, method = meth)
confints_Lang_Reiczigel <- rbindlist(results)
#' get rid of invalid data set cases:
setkey(confints_Lang_Reiczigel, data_id)
confints_Lang_Reiczigel[dataset_cases, data_case := i.data_case]
confints_Lang_Reiczigel <- confints_Lang_Reiczigel[data_case %in% c("1", "2", "3"), ]
#' calculate CI length:
confints_Lang_Reiczigel[, length := ucl - lcl, by = data_id]
#' does the CI include the true prevalence?
confints_Lang_Reiczigel[datasets, pi_true := i.pi_true]
confints_Lang_Reiczigel[, withinCI := .withinRange(pi_true, lcl, ucl), 
                        by = data_id]
confints_Lang_Reiczigel <- confints_Lang_Reiczigel[, .(data_id, data_case, method, lcl, ucl, length, withinCI)]
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Blaker
meth <- "Blaker"
t0 <- Sys.time()
if (config$verbose) message(sprintf("   Calculating %s CIs", meth), appendLF = FALSE)
results <- sfLapply(1:n_chunks, .confint.parallel, method = meth)
confints_Blaker <- rbindlist(results)
#' get rid of invalid data set cases:
setkey(confints_Blaker, data_id)
confints_Blaker[dataset_cases, data_case := i.data_case]
confints_Blaker <- confints_Blaker[data_case %in% c("1", "2", "3"), ]
#' calculate CI length:
confints_Blaker[, length := ucl - lcl, by = data_id]
#' does the CI include the true prevalence?
confints_Blaker[datasets, pi_true := i.pi_true]
confints_Blaker[, withinCI := .withinRange(pi_true, lcl, ucl), 
                by = data_id]
confints_Blaker <- confints_Blaker[, .(data_id, data_case, method, lcl, ucl, length, withinCI)]
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Sterne
meth <- "Sterne"
t0 <- Sys.time()
if (config$verbose) message(sprintf("   Calculating %s CIs", meth), appendLF = FALSE)
results <- sfLapply(1:n_chunks, .confint.parallel, method = meth)
confints_Sterne <- rbindlist(results)
#' get rid of invalid data set cases:
setkey(confints_Sterne, data_id)
confints_Sterne[dataset_cases, data_case := i.data_case]
confints_Sterne <- confints_Sterne[data_case %in% c("1", "2", "3"), ]
#' calculate CI length:
confints_Sterne[, length := ucl - lcl, by = data_id]
#' does the CI include the true prevalence?
confints_Sterne[datasets, pi_true := i.pi_true]
confints_Sterne[, withinCI := .withinRange(pi_true, lcl, ucl), 
                by = data_id]
confints_Sterne <- confints_Sterne[, .(data_id, data_case, method, lcl, ucl, length, withinCI)]
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' stop the cluster:
if (config$verbose) message("  Stopping cluster")
.suppressOutput(sfStop())


#' Cache
t0 <- Sys.time()
cache("confints_Clopper_Pearson")
cache("confints_Wald_Rogan_Gladen")
cache("confints_Lang_Reiczigel")
cache("confints_Blaker")
cache("confints_Sterne")
if (config$verbose) message(sprintf("  Caching run time: %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

if (config$verbose) message(sprintf(
    "  Total script run time: %s", 
    interval(t0_script, Sys.time()) %>% 
        as.duration() %>% 
        signif(digits = 2)
))
rm("t0", "t0_script", "results", "meth", "n_cpu", "n_chunks")

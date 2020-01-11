#*******************************************************************************************
#' # Confidence interval function for parallel use
#' 

.confint.parallel <- function(i, method, conf = 0.95) {
    # `n_chunks` and `datasets` must be made available via `sfExport()`
    n_total <- nrow(datasets)
    chunksize <- n_total %/% n_chunks
    if (i < n_chunks) {
        DT <- datasets[((i - 1)*chunksize + 1):(i*chunksize), ]
    } else {
        DT <- datasets[((i - 1)*chunksize + 1):n_total, ]
    }
    
    # use confidence interval calculation method as defined in 
    # 01-helpers.R and 02-ci4prev-Reiczigel_et_al.R:
    ci <- switch(method,
               "Clopper-Pearson"   = .confint.Clopper_Pearson(DT, conf = conf),
               "Lang-Reiczigel"    = .confint.Lang_Reiczigel(DT, conf = conf),
               "Blaker"            = .confint.Blaker(DT, conf = conf),
               "Sterne"            = .confint.Sterne(DT, conf = conf),
               "Wald-Rogan-Gladen" = .confint.Wald_Rogan_Gladen(DT, conf = conf))
    return(ci)
}

#*******************************************************************************************
#' # Helper functions
#'
# .runjags.options <- list(mode.continuous = TRUE)

#' allow for cleaner output when sourcing scripts in the /src directory that use parallel execution:
.suppressOutput <- function(expr) {
    capture.output(suppressMessages(expr), file = NULL)
}

#' generate consecutive data set id's:
.generate.data.id <- function(par_id, rep_id, n_reps = 1000) {
    as.integer((par_id - 1)*n_reps + rep_id)
}

#' get parameter set id and replicate number:
.get.idx <- function(data_id, n_reps = 1000) {
    par_id <- data_id %/% n_reps + 1
    rep_id <- data_id %% n_reps
    as.data.frame(cbind(data = data_id, par = par_id, rep = rep_id))
}

#' Rogan-Gladen estimate:
.rge <- function(x, n, se, sp, force_proportion = TRUE) {
    # any test must satisfy se + sp > 1
    if (se + sp == 1) return(NA_real_)  # using just NA will cause problems in data.tables
    
    # Rogan-Gladen estimate
    rge <- (x/n + sp - 1)/(se + sp - 1)
    if (force_proportion) rge <- min(1, max(0, rge))
    rge
}

#' Youden index:
.youden <- function(se, sp) {
    se + sp - 1
}

#' check data set case according to conditions 4 from the article:
.check.case <- function(x, n, se, sp) {
    if (se + sp == 1) return("0")
    ap <- x/n
    I_1 <- (1 - sp < se)                         # condition 4a from the article
    I_2 <- (1 - sp <= ap)                        # condition 4b from the article
    I_3 <- (ap <= se)                            # condition 4c from the article
    cond <- ifelse(I_1 && I_2 && I_3,  "1",      # RGE is a valid proportion
            ifelse(I_1 && !I_2 && I_3, "2",      # RGE censored at 0
            ifelse(I_1 && I_2 && !I_3, "3",      # RGE censored at 1
            ifelse(!I_1 && !I_2 && I_3, "4",     # degenerate case
            ifelse(!I_1 && I_2 && !I_3, "5",     # degenerate case
            ifelse(!I_1 && !I_2 && !I_3, "6",    # degenerate case
            NA_character_
    ))))))
    return(cond)
}

#' adjust confidence limits and force range [0, 1]:
.adj.conflimit <- function(cl, se, sp) {
    # any test must satisfy se + sp > 1
    if (!(se + sp > 1)) return(NA_real_)  # using just NA will cause problems in data.tables
    
    # adjust confidence limit according to se and sp and force range [0, 1]
    cl.adj <- (cl + sp - 1) / (se + sp - 1)
    return(min(1, max(0, cl.adj)))
}

#' check if value is within range:
.withinRange <- function(x, low, upp) {
    # is x within the range [low, upp]?
    if (any(is.na(c(low, upp)))) return( NA )
    else return( findInterval(x, c(low, upp)) == 1 )
}

#' Bayesian Highest Density Interval (HDI):
.hdi <- function(data) {
    # data - mcmc list
    res <- as.data.frame(HDInterval::hdi(data)) %>% 
        dplyr::add_rownames("hdi_bound") %>%
        tidyr::gather(parameter, value, -hdi_bound) %>% 
        tidyr::spread(hdi_bound, value) %>% 
        dplyr::mutate(parameter = factor(parameter))
    return(res)
}

#' Maximum Likelihood Estimate (MLE):
.mle <- function(xP, nP, seP, spP, k, force_proportion = TRUE) {
    # any test must satisfy se + sp > 1
    if (!(seP + spP > 1)) return(NA_real_)  # using just NA will cause problems in data.tables
    
    # Maximum Likelihood Estimate
    pi_mle <- 1 - ((seP - xP/nP)/(seP + spP - 1))^(1/k)
    if (force_proportion) pi_mle <- min(1, max(0, pi_mle))
    pi_mle
}

#' # Functions from Cowling et al. (1999)

#' this is the function g(P, eta, theta) from method 6 in Cowling et al. (1999)
.g <- function(P, seP, spP) {
    g <- (seP - P) / (seP + spP - 1)
    return(g)
}

#' variance of the MLE estimate according to method 6 in Cowling et al. (1999); eq.(8)
.var.mle <- function(xP, nP, seP, n_seP, spP, n_spP, k) {
    P <- xP / nP
    g <- .g(P, seP, spP)
    v <- g^(2/k - 2) / (k^2 * (seP + spP - 1)) * ( P*(1-P)/nP + seP*(1-seP)/n_seP * (1 - g)^2 + spP*(1-spP)/n_spP * g^2 )
    return(v)
}

.cowling6.int <- function(xP, nP, seP, n_seP, spP, n_spP, k, alpha = 0.05, force_proportion = TRUE) {
    pi_mle <- .mle(xP, nP, seP, spP, k, force_proportion = FALSE)
    sqrt_var <- sqrt( .var.mle(xP, nP, seP, n_seP, spP, n_spP, k) )
    z <- qnorm(1 - alpha/2)
    lower <- pi_mle - z * sqrt_var
    upper <- pi_mle + z * sqrt_var
    ci <- c(lower, upper)
    if (force_proportion) ci <- pmin(1, pmax(0, ci))
    return(ci)
}

.confint.Cowling_Method6 <- function(DT, conf = 0.95) {
    DT <- copy(as.data.table(DT))
    
    DT[, method := "Cowling (Method 6)"]
    DT[, c("lcl", "ucl") := as.list(.cowling6.int(xP, nP, seP, n_seP, spP, n_spP, k, alpha = 1 - conf)), 
       by = data_id]
    
    DT[, .(data_id, method, lcl, ucl)]
}

#' confidence interval according to Wald-Rogan-Gladen:
.confint.Wald_Rogan_Gladen <- function(DT, conf = 0.95) {
    # For k=1, Method #6 in Cowling et al. (1999) is the same as Rogan-Gladen
    DT <- copy(as.data.table(DT))
    
    DT[, method := "Wald-Rogan-Gladen"]
    DT[, c("lcl", "ucl") := as.list(.cowling6.int(x, n, se, n_se, sp, n_sp, k = 1, alpha = 1 - conf)), 
       by = data_id]
    
    DT[, .(data_id, method, lcl, ucl)]
}

#' check whether p-value is within the lower or upper 2.5% tail of
#' the prior predictive distribution
.check.prior.pred.pval <- function(p_below, p_above, alpha = 0.05) {
    case <- ifelse(p_below <= alpha/2, "2",
                   ifelse(p_above <= alpha/2, "3",
                          "1"
                   )
            )
    return(case)
}

.check.prior.pred.pval.fixprev <- function(p_below_pi0, p_above_pi1, alpha = 0.05) {
    # check whether p-value is 
    # - case 2: within lower 5% tail for assumed prevalence of zero
    # - case 3: within upper 5% tail for assumed prevalence of one
    # - case 1: neither of the other two cases
    case <- ifelse(p_below_pi0 <= alpha, "2",
                   ifelse(p_above_pi1 <= alpha, "3",
                          "1"
                   )
    )
    return(case)
}

.bounded.mode <- function(x, limits = c(0, 1), estimator = "boundarykernel", gridsize = 201) {
    # calculate mode of bounded kernel density estimate
    xs <- sort(x)
    xd <- sort(x, decreasing = TRUE)
    x.min <- max(2*xs[1] - xs[10], limits[1]) #min(x)
    x.max <- min(2*xd[1] - xd[10], limits[2])
    b <- bde(dataPoints = x, 
             dataPointsCache = seq(x.min, x.max, length.out = gridsize), 
             lower.limit = limits[1],
             upper.limit = limits[2],
             estimator = estimator,
             options = list(corrected = TRUE))
    x <- getdataPointsCache(b)
    y <- getdensityCache(b)
    return(x[y == max(y)])
}


#' # Generate LaTeX tables (can then be copied into manuscript)
t0_script <- Sys.time()
if (config$verbose) message(" Running script 09")

#' # Tables
escape = FALSE

#' ## Table 1
cat("****************************\n*** Tab. 1 *****************\n****************************")
tab1 <- tibble(
        Parameter = c("\\emph{Statistical}",
                      "$\\quad\\quad \\TrueSENS$", 
                      "$\\quad\\quad \\TrueSPEC$", 
                      "$\\quad\\quad \\TruePREV$", 
                      "\\emph{Design}",
                      "$\\quad\\quad n_{\\SENS}$", 
                      "$\\quad\\quad n_{\\SPEC}$",
                      "$\\quad\\quad n$"),
        Description = c("",
                        "True sensitivity",
                        "True specificity", 
                        "True prevalence", 
                        "",
                        "Sample size for a sensitivity validation study",
                        "Sample size for a specificity validation study",
                        "Sample size for a test application"),
        "Values sampled from" = c("",
                                  sprintf("$\\mathcal{U}\\left(%g,\\ %g\\right)$", se_range[1], se_range[2]),
                                  sprintf("$\\mathcal{U}\\left(%g,\\ %g\\right)$", sp_range[1], sp_range[2]),
                                  sprintf("$\\mathcal{U}\\left(%d,\\ %d\\right)$", pi_range[1], pi_range[2]),
                                  "",
                                  sprintf("$\\{%s\\}$", paste(n_se_range, collapse = ",\\ ")),
                                  sprintf("$\\{%s\\}$", paste(n_sp_range, collapse = ",\\ ")),
                                  sprintf("$\\mathcal{U}\\left(%d,\\ %d\\right)$", n_range[1],  n_range[2]))
)
kable(tab1, "latex", booktabs = TRUE, linesep = c('', '', '', '\\addlinespace'), align = "lll", escape = escape) %>% 
    print()

#' ## Table 2
cat("\n****************************\n*** Tab. 2 *****************\n****************************")
tab2 <- parsets[1:10, ] %>% 
    as_tibble() %>% 
    select(par_id, se_true, sp_true, pi_true, n_se, n_sp, n) %>% 
    rename("Parameter set" = par_id,
           "$\\TrueSENS$"  = se_true, 
           "$\\TrueSPEC$"  = sp_true, 
           "$\\TruePREV$"  = pi_true, 
           "$n_{\\SENS}$"  = n_se, 
           "$n_{\\SPEC}$"  = n_sp,
           "$n$"           = n)
kable(tab2, "latex", booktabs = TRUE, linesep = "", align = "lrrrrrr", escape = escape) %>% 
    add_header_above(c("", "\\\\emph{Statistical}" = 3, "\\\\emph{Design}" = 3), escape = escape) %>% 
    print()

#' ## Table 3
cat("\n****************************\n*** Tab. 3 *****************\n****************************")
tab3 <- datasets[1:10, ] %>% 
    as_tibble() %>% 
    select(data_id, se_true, sp_true, pi_true, n_se, n_sp, n, x_se, x_sp, x, se, sp, ap) %>% 
    rename("Data set"      = data_id,
           "$\\TrueSENS$"  = se_true, 
           "$\\TrueSPEC$"  = sp_true, 
           "$\\TruePREV$"  = pi_true, 
           "$n_{\\SENS}$"  = n_se, 
           "$n_{\\SPEC}$"  = n_sp,
           "$n$"           = n,
           "$x_{\\SENS}$"  = x_se, 
           "$x_{\\SPEC}$"  = x_sp,
           "$x$"           = x,
           "$\\SENS$"      = se, 
           "$\\SPEC$"      = sp,
           "$\\AP$"        = ap)
kable(tab3, "latex", booktabs = TRUE, linesep = "", align = "lrrrrrrrrrrrr", escape = escape) %>% 
    add_header_above(c("", "\\\\emph{Statistical}" = 3, "\\\\emph{Design}" = 3, "\\\\emph{Data}" = 3, "\\\\emph{MLE}" = 3), escape = escape) %>% 
    print()

if (config$verbose) message(sprintf(
    "  Total script run time: %s", 
    interval(t0_script, Sys.time()) %>% 
        as.duration() %>% 
        signif(digits = 2)
))

#' # Generate Figure 2 (containing two subplots)
t0_script <- Sys.time()
if (config$verbose) message(" Running script 06")

#' Combine Rogan-Gladen and Bayesian estimates:
estimates <- rbindlist(list(
    estimates_Rogan_Gladen, 
    estimates_Bayesian
), use.names = TRUE)
estimates[, method := factor(method)]

#' Create an "all" data case to include all data sets (for easier faceted plotting):
estimates_datacaseAll <- copy(estimates)
estimates_datacaseAll <- estimates_datacaseAll[, data_case := "all"]
estimates_by_datacase <- rbindlist(list(estimates_datacaseAll, estimates)) %>% 
    droplevels()
estimates_by_datacase[, data_case := factor(data_case, 
                                            levels = c("all", "1", "2", "3"))] %>% 
    droplevels()
estimates_by_datacase[, abs_dev := abs(bias)]

#' Labeller function for faceted plot
data_case_tab <- addmargins(table(estimates_by_datacase[method == "Rogan-Gladen", ]$data_case))
label_datacaseAll <- sprintf("All data sets (n = %d)", data_case_tab["all"])
label_datacase1 <- sprintf("Case 1 (n = %d): $0 \\leq RGE \\leq 1$",  max(0, data_case_tab["1"], na.rm = TRUE))
label_datacase2 <- sprintf("Case 2 (n = %d): $RGE < 0$",  max(0, data_case_tab["2"], na.rm = TRUE))
label_datacase3 <- sprintf("Case 3 (n = %d): $RGE > 1$",  max(0, data_case_tab["3"], na.rm = TRUE))
datacase_labeller <- function(labels) {
    lapply(labels, function(values) {
        values <- switchv(as.character(values),
                          "all" = TeX(label_datacaseAll),
                          "1" = TeX(label_datacase1),
                          "2" = TeX(label_datacase2),
                          "3" = TeX(label_datacase3))
        values <- paste0("list(", values, ")")
        lapply(values, function(expr) c(parse(text = expr)))
    })
}
box_width <- 0.3
errorbar_width <- 0.5

#' ## Figure 2a
t0 <- Sys.time()
if (config$verbose) message("  Setting subplot a", appendLF = FALSE)
plot2a <- ggplot(estimates_by_datacase[data_case %in% c("all", "1", "2", "3"), ], 
                 aes(x = method, y = bias)) +
    geom_violin(col = "darkgray", fill = "gray", alpha = 0.5) +
    stat_boxplot(geom = "errorbar", width = errorbar_width,
                 col = "black") +
    geom_boxplot(col = "black", outlier.shape = NA,
                 width = box_width, show.legend = FALSE) +
    facet_wrap(~ data_case, ncol = 2, labeller = datacase_labeller) +
    ylab("Bias") +
    coord_flip(ylim = c(-0.25, 0.25)) +
    theme_minimal_grid(font_size = 12) +
    theme(axis.title.x = element_text(margin = margin(1, 0, 0, 0, "lines")),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey80")) +
    panel_border()
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Figure 2b
t0 <- Sys.time()
if (config$verbose) message("  Setting subplot b", appendLF = FALSE)
plot2b <- ggplot(bias_df, aes(x = `Rogan-Gladen`, y = `Bayesian-Mean`)) +
    geom_hex(aes(alpha = log10(..count..)), bins = 50, 
             fill = "black") +
    geom_abline(slope = 1, intercept = 0, 
                color = "grey85", 
                linetype = "solid") +
    geom_abline(intercept = bias_deming@para[1],
                slope = bias_deming@para[2],
                linetype = "dashed") + 
    scale_x_continuous("Bias Rogan-Gladen",
                       breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)) +
    scale_y_continuous("Bias Bayesian-Mean",
                       breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75)) +
    scale_alpha_continuous("Log of count within hexbin", breaks = 0:5) +
    scale_fill_continuous(guide = FALSE) +
    annotate("text", size = 4,  x = -0.65, y = 0.6, hjust = 0,
             label = sprintf("Regression line slope:\n%.3f (%.3f, %.3f)", 
                             bias_deming@para[2, 1],
                             bias_deming@para[2, 3],
                             bias_deming@para[2, 4])) +
    coord_equal(xlim = c(-0.75, 0.75), ylim = c(-0.75, 0.75)) +
    theme_minimal_grid(font_size = 12) +
    theme(legend.position = "top", 
          legend.justification = "right",
          legend.margin = margin(0, 0, -0.25, 0, "lines"),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 7),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
          axis.title.x = element_text(margin = margin(1, 0, 0, 0, "lines")),
          axis.title.y = element_text(margin = margin(0, 1, 0, 0, "lines"))) +
    panel_border() +
    guides(alpha = guide_legend(nrow = 1))
# plot2b
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Figure 2
t0 <- Sys.time()
if (config$verbose) message("  Combining subplots, generating and saving figure", appendLF = FALSE)
plot2 <- plot_grid(plot2a, plot2b, ncol = 2, align = "v", axis = "tb",
                   rel_widths = c(2, 1), 
                   labels = "auto", label_size = 16, hjust = c(-1, -2), vjust = 1.5)
cowplot::save_plot(filename = file.path("graphs", "Fig2.pdf"), 
                   plot = plot2,
                   base_height = 4,
                   base_asp = 10/3, 
                   bg = "white")
cowplot::save_plot(filename = file.path("graphs", "Fig2.png"), 
                   plot = plot2,
                   base_height = 4,
                   base_asp = 10/3, 
                   bg = "white")
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

if (config$verbose) message(sprintf(
    "  Total script run time: %s", 
    interval(t0_script, Sys.time()) %>% 
        as.duration() %>% 
        signif(digits = 2)
))
rm("t0", "t0_script", "estimates_datacaseAll")

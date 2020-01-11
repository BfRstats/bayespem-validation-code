#' # Generate Figure 4 (containing two subplots)
t0_script <- Sys.time()
if (config$verbose) message(" Running script 08")

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
#' ## Confidence intervals
#' 
#' Create an "all" data case to include all data sets (for easier faceted plotting):
confints_datacaseAll <- copy(confints)
confints_datacaseAll[, `:=`(data_case = "all")]
confints_by_datacase <- rbindlist(list(confints_datacaseAll, confints))
confints_by_datacase[, data_case := factor(data_case, 
                                           levels = c("all", "1", "2", "3"))]
 
#' Labeller function for faceted plot
# confints <- confints[data_case %in% c("1", "2", "3"), ]
data_case_tab <- addmargins(table(confints_by_datacase[method == "Wald-Rogan-Gladen", ]$data_case)) # choose one method to avoid double counting
label_datacaseAll <- sprintf("All Data Sets (n = %d)", data_case_tab["all"])
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

#' ## Figure 4a
t0 <- Sys.time()
if (config$verbose) message("  Setting up subplot a", appendLF = FALSE)
plot4a <- ggplot(confints_by_datacase, 
                 aes(x = method, y = length)) +
    stat_boxplot(geom = "errorbar", width = errorbar_width,
                 col = "black") +
    geom_boxplot(col = "black", outlier.shape = NA,
                 width = box_width, show.legend = FALSE) +
    facet_wrap(~ data_case, ncol = 2, labeller = datacase_labeller) +
    scale_y_continuous("Length of CI") +
    coord_flip(ylim = c(0, 0.5)) +
    theme_minimal_grid(font_size = 12) +
    theme(axis.title.x = element_text(margin = margin(1, 0, 0, 0, "lines")),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey80")) +
    panel_border()
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Figure 4b
t0 <- Sys.time()
if (config$verbose) message("  Setting up subplot b", appendLF = FALSE)
plot4b <- ggplot(cilength_df, aes(x = `Lang-Reiczigel`, y = `Bayesian-HDI`)) +
    geom_hex(aes(alpha = log10(..count..)), bins = 50,
             fill = "black") +
    geom_abline(slope = 1, intercept = 0, 
                color = "grey85", 
                linetype = "solid") +
    geom_abline(intercept = cilength_deming@para[1],
                slope = cilength_deming@para[2],
                linetype = "dashed") + 
    scale_x_continuous("Length of Lang-Reiczigel CI",
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_y_continuous("Length of Bayesian HDI",
                       breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_alpha_continuous("Log of count within hexbin", breaks = 0:5) +
    scale_fill_continuous(guide = FALSE) +
    annotate("text", size = 4,
             x = 0.05, y = 0.9, hjust = 0,
             label = sprintf("Regression line slope:\n%.3f (%.3f, %.3f)",
                             cilength_deming@para[2, 1],
                             cilength_deming@para[2, 3],
                             cilength_deming@para[2, 4])) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal_grid(font_size = 12) +
    theme(legend.position = "top", 
          legend.justification = "right",
          legend.margin = margin(0, 0, -0.25, 0, "lines"),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 7),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
          # plot.margin = unit(c(0.5, 1, 0.5, ), "lines"),
          axis.title.x = element_text(margin = margin(1, 0, 0, 0, "lines")),
          axis.title.y = element_text(margin = margin(0, 1, 0, 0, "lines"))) +
    panel_border() +
    guides(alpha = guide_legend(nrow = 1))
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Figure 4
t0 <- Sys.time()
if (config$verbose) message("  Combining subplots, generating and saving figure", appendLF = FALSE)
plot4 <- plot_grid(plot4a, plot4b, ncol = 2, align = "v", axis = "tb",
                   rel_widths = c(2, 1), 
                   labels = "auto", label_size = 16, hjust = c(-1, -2), vjust = 1.5)
cowplot::save_plot(filename = file.path("graphs", "Fig4.pdf"), 
                   plot = plot4,
                   base_height = 4,
                   base_asp = 10/3, 
                   bg = "white")
cowplot::save_plot(filename = file.path("graphs", "Fig4.png"), 
                   plot = plot4,
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
rm("t0", "t0_script", "confints_datacaseAll")

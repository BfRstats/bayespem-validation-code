#' # Generate Figure 3 (containing an inset)
t0_script <- Sys.time()
if (config$verbose) message(" Running script 07")
 
box_width <- 0.3
errorbar_width <- 0.5

#' ## Outer plot
t0 <- Sys.time()
if (config$verbose) message("  Setting up outer plot", appendLF = FALSE)
plot3_outer <- ggplot(coverage_by_parset,
                aes(x = method, y = prop_withinCI)) +
    geom_hline(yintercept = 0.95, col = "gray", size = 0.5, linetype = "solid") +
    geom_hline(yintercept = c(0.9, 1), col = "gray", size = 0.5, linetype = "dashed") +
    stat_boxplot(geom = "errorbar", width = errorbar_width,
                 col = "black") +
    geom_boxplot(col = "black",
                 outlier.shape = 1, outlier.size = 3,
                 width = box_width, show.legend = FALSE) +
    geom_boxplot(col = "black", alpha = 0,
                 outlier.shape = 1, outlier.size = 3,
                 width = box_width, show.legend = FALSE) +
    scale_y_continuous("Coverage", labels = scales::percent_format(accuracy = 1),
                       breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1)) +
    coord_flip(ylim = c(0.25, 1)) + 
    theme_minimal_grid(font_size = 12) +
    theme(axis.title.x = element_text(margin = margin(1, 0, 0, 0, "lines")),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),) +
    panel_border()
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Inset plot
t0 <- Sys.time()
if (config$verbose) message("  Setting up inset plot", appendLF = FALSE)
plot3_inset <- ggplot(coverage_by_parset[method %in% c("Bayesian-HDI", "Lang-Reiczigel"),],
                      aes(x = method, y = prop_withinCI)) +
    geom_hline(yintercept = 0.95, col = "gray", size = 0.5, linetype = "solid") +
    geom_hline(yintercept = c(0.9, 1), col = "gray", size = 0.5, linetype = "dashed") +
    stat_boxplot(geom = "errorbar", width = errorbar_width,
                 col = "black") +
    geom_boxplot(col = "black", 
                 outlier.shape = 1, outlier.size = 3,
                 width = box_width, show.legend = FALSE) +
    geom_boxplot(col = "black", alpha = 0,
                 outlier.shape = 1, outlier.size = 3,
                 width = box_width, show.legend = FALSE) +
    coord_flip() +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "lines")) +
    panel_border(colour = "black")
if (config$verbose) message(sprintf(": %s", interval(t0, Sys.time()) %>% as.duration() %>% signif(digits = 2)))

#' ## Figure 3
t0 <- Sys.time()
if (config$verbose) message("  Combining plots, generating and saving figure", appendLF = FALSE)
g <- ggplotGrob(plot3_inset)
plot3 <- plot3_outer + 
    annotation_custom(g, ymin = 0.25, ymax = 0.75,
                      xmin = 0.5, xmax = 2.5)
cowplot::save_plot(filename = file.path("graphs", "Fig3.pdf"), 
                   plot = plot3,
                   base_height = 4,
                   base_asp = 10/3, 
                   bg = "white")
cowplot::save_plot(filename = file.path("graphs", "Fig3.png"), 
                   plot = plot3,
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
rm("t0", "t0_script")

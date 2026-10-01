library(tidyverse)
library(grid)
library(gridExtra)
library(fixest)
library(patchwork)

out_dir <- "tables_and_charts"
to_analyze <- read_csv("data/built/round_level_data.csv", show_col_types = FALSE)

# ---- Bar charts (Figures 2, 3) ----

cluster_mean_se <- function(d, v) {
  d <- filter(d, !is.na(.data[[v]]))
  if (nrow(d) == 0 || n_distinct(d$subject_id) < 2) return(c(mean = NA_real_, se = NA_real_))
  fit <- feols(as.formula(paste(v, "~ 1")), data = d, vcov = ~subject_id)
  c(mean = unname(coef(fit)[1]), se = unname(se(fit)[1]))
}

# balanced = equal weight on Take and Give (Figure 2)
estimate <- function(d, v, balanced) {
  if (balanced) {
    tk <- cluster_mean_se(filter(d, draw_mode == "take"), v)
    gv <- cluster_mean_se(filter(d, draw_mode == "give"), v)
    est <- 0.5 * tk[["mean"]] + 0.5 * gv[["mean"]]
    se  <- 0.5 * sqrt(tk[["se"]]^2 + gv[["se"]]^2)
  } else {
    e <- cluster_mean_se(d, v)
    est <- e[["mean"]]; se <- e[["se"]]
  }
  c(est = est, lo = est - 1.96 * se, hi = est + 1.96 * se)
}

summarise_bars <- function(d, balanced = FALSE) {
  d %>%
    group_by(x_lab) %>%
    group_modify(\(g, k) {
      err <- estimate(g, "error", balanced)
      lost <- estimate(g, "exp_score_lost", balanced)
      tibble(error_rate = err[["est"]], error_lo = err[["lo"]], error_hi = err[["hi"]],
             points_lost = lost[["est"]], lost_lo = lost[["lo"]], lost_hi = lost[["hi"]])
    }) %>%
    ungroup()
}

own_vs_other <- function(df, other_label) {
  df %>%
    filter(category %in% c("ego_opt", "other_opt")) %>%
    mutate(x_lab = if_else(category == "ego_opt", "Own", other_label))
}

bar_palette <- c(
  "Own" = "#B30000", "Computer Player's" = "#1E4B9B", "Opponent's" = "#1E4B9B",
  "Teammate's" = "#1E4B9B", "Focal Player's" = "#E69F00", "Non-Focal Player's" = "#00BFC4"
)

bar_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 8)),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    plot.margin = margin(8, 6, 0, 6)
  )

bar_plot <- function(df, yvar, ylab = NULL) {
  is_err <- yvar == "error_rate"
  pre <- if (is_err) "error" else "lost"
  df <- df %>%
    arrange(x_lab != "Own", x_lab != "Focal Player's", x_lab) %>%
    mutate(x = seq_len(n()))
  ggplot(df, aes(x = x, y = .data[[yvar]], fill = x_lab)) +
    geom_col(width = 0.30) +
    geom_errorbar(aes(ymin = .data[[paste0(pre, "_lo")]], ymax = .data[[paste0(pre, "_hi")]]),
                  width = 0.05, linewidth = 0.5, color = "black") +
    scale_fill_manual(values = bar_palette) +
    scale_x_continuous(limits = c(0.55, nrow(df) + 0.45), breaks = df$x, labels = df$x_lab, expand = c(0, 0)) +
    scale_y_continuous(breaks = if (is_err) seq(0, 1, 0.25) else c(0, 0.4, 0.8, 1.2, 1.6),
                       labels = if (is_err) c("0.00", "0.25", "0.50", "0.75", "1.00") else c("0.00", "0.40", "0.80", "1.20", "1.60"),
                       expand = c(0, 0)) +
    coord_cartesian(ylim = if (is_err) c(0, 1) else c(0, 1.6), expand = FALSE, clip = "on") +
    labs(x = NULL, y = ylab) +
    bar_theme
}

three_panel_page <- function(panels, titles) {
  cols <- imap(panels, \(p, i) arrangeGrob(
    grobs = list(bar_plot(p, "error_rate", if (i == 1) "Error Rate"),
                 bar_plot(p, "points_lost", if (i == 1) "Points Lost")),
    ncol = 1))
  bold <- \(s, size) gpar(fontsize = size, fontface = "bold")
  heads <- unlist(map2(c("a.", "b.", "c."), titles, \(l, t) list(
    textGrob(l, x = 0.02, y = 0.95, hjust = 0, vjust = 1, gp = bold(l, 15)),
    textGrob(t, gp = bold(t, 14)))), recursive = FALSE)
  arrangeGrob(
    grobs = c(heads, cols, list(textGrob("Score That Participant's Choice Impacts Most", gp = bold("", 14)))),
    layout_matrix = rbind(1:6, c(7, 7, 8, 8, 9, 9), rep(10, 6)),
    widths = unit(c(0.08, 1, 0.08, 1, 0.08, 1), "null"),
    heights = unit(c(0.10, 1, 0.10), "null"),
    padding = unit(0.3, "line")
  )
}

fig2_ego <- function(df) {
  three_panel_page(list(
    summarise_bars(own_vs_other(df, "Computer Player's"), balanced = TRUE),
    summarise_bars(own_vs_other(filter(df, action_game_type == "adversarial"), "Opponent's"), balanced = TRUE),
    summarise_bars(own_vs_other(filter(df, action_game_type == "cooperative"), "Teammate's"), balanced = TRUE)
  ), c("All", "Adversarial", "Cooperative"))
}

fig3_focal <- function(df) {
  focal <- df %>%
    filter(!is.na(focal_works)) %>%
    mutate(x_lab = if_else(focal_works, "Focal Player's", "Non-Focal Player's"))
  three_panel_page(list(
    summarise_bars(focal),
    summarise_bars(own_vs_other(filter(df, draw_mode == "take"), "Computer Player's")),
    summarise_bars(own_vs_other(filter(df, draw_mode == "give"), "Computer Player's"))
  ), c("All", "Take", "Give"))
}

save_grob <- function(g, file) {
  png(file, width = 11.15, height = 6.35, units = "in", res = 300)
  grid.newpage()
  grid.draw(g)
  dev.off()
}

# ---- Heterogeneity diamond (Figure 5) ----

diamond_counts <- function(df) {
  df %>%
    filter(category %in% c("ego_opt", "other_opt")) %>%
    mutate(ego_choice = as.integer((category == "ego_opt") == (play1_opt == 1)),
           panel = if_else(draw_mode == "take", "Take", "Give")) %>%
    group_by(subject_id, panel) %>%
    summarise(n_rounds = n(), ego_all = sum(ego_choice), errors = n_rounds - sum(play1_opt), .groups = "drop") %>%
    count(ego_all, errors, panel, n_rounds)
}

diamond_plot <- function(counts, panel_name) {
  d <- filter(counts, panel == panel_name)
  total <- max(d$n_rounds)
  half <- total / 2
  ticks <- seq(0, total, by = total / 4)

  p <- ggplot() +
    geom_segment(aes(x = half, xend = half, y = 0, yend = total), linetype = "dotted", linewidth = 0.6, color = "black") +
    geom_path(data = tibble(x = c(0, half, total, half, 0), y = c(half, total, half, 0, half)),
              aes(x = x, y = y), linewidth = 0.8, color = "black") +
    geom_segment(data = tibble(x = 0, xend = total, y = -3.0, yend = -3.0), aes(x = x, xend = xend, y = y, yend = yend),
                 arrow = arrow(ends = "both", length = unit(0.16, "inches")), linewidth = 0.6) +
    geom_segment(data = tibble(x = half, xend = half, y = -3.8, yend = -2.2), aes(x = x, xend = xend, y = y, yend = yend),
                 linewidth = 0.6) +
    annotate("text", x = 0, y = -4.25, label = "Always Computer's", hjust = 0, size = 3.5) +
    annotate("text", x = total, y = -4.25, label = "Always Their Own", hjust = 1, size = 3.5) +
    annotate("text", x = half, y = total + 0.9, label = panel_name, fontface = "bold", size = 5) +
    geom_point(data = d, aes(x = ego_all, y = errors, size = n), alpha = 0.75, color = "#5A2A83") +
    scale_size_area(max_size = 8, guide = "none") +
    coord_fixed(ratio = 1, xlim = c(-4.8, total + 0.8), ylim = c(-5.1, total + 1.0), expand = FALSE, clip = "off") +
    labs(x = NULL, y = NULL) +
    theme_void(base_size = 12) +
    theme(legend.position = "none", plot.margin = margin(6, 10, 0, 28))

  if (panel_name == "Take") {
    y_ticks <- tibble(y = ticks, x = -1.8, xend = -1.25, lab = as.character(ticks))
    p <- p +
      geom_segment(data = tibble(x = -1.8, xend = -1.8, y = 0, yend = total), aes(x = x, xend = xend, y = y, yend = yend), linewidth = 0.6) +
      geom_segment(data = y_ticks, aes(x = x, xend = xend, y = y, yend = y), linewidth = 0.5) +
      geom_text(data = y_ticks, aes(x = x - 0.25, y = y, label = lab), hjust = 1, vjust = 0.5, size = 3.2) +
      annotate("text", x = -4.3, y = half, label = "Number of Errors", angle = 90, fontface = "bold", size = 5)
  }
  p
}

fig5_hetero <- function(df) {
  counts <- diamond_counts(df)
  x_lab <- ggplot() +
    annotate("text", x = 0.5, y = 1, label = "Subobjective Score Participant Optimized", fontface = "bold", size = 5) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0.99, 1.01), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.margin = margin(10, 0, 8, 0))
  ((diamond_plot(counts, "Take") + diamond_plot(counts, "Give")) +
      plot_layout(ncol = 2) & theme(plot.margin = margin(15, 8, 0, 8))) /
    x_lab + plot_layout(heights = c(1, 0.09))
}

# ---- Error rate by stimulus features and round (Figures 4, 6) ----

line_levels <- c("Choice Impacts Own Score Most", "Choice Impacts Computer's Score Most",
                 "Own Score is Salient", "Computer's Score is Salient")
line_palette <- setNames(c("#B30000", "#1E4B9B", "#E69F00", "#00BFC4"), line_levels)

# Each round counts once toward its category line and once toward its salience line
error_by <- function(df, xvar) {
  df %>%
    filter(category %in% c("ego_opt", "other_opt"), !is.na(focal_works), !is.na(.data[[xvar]])) %>%
    transmute(x = .data[[xvar]], error,
              category_lab = if_else(category == "ego_opt", line_levels[1], line_levels[2]),
              salience_lab = if_else(focal_works, line_levels[3], line_levels[4])) %>%
    pivot_longer(c(category_lab, salience_lab), values_to = "line_lab") %>%
    mutate(line_lab = factor(line_lab, levels = line_levels)) %>%
    group_by(x, line_lab) %>%
    summarise(error_rate = mean(error), n = n(), .groups = "drop")
}

line_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(r = 8)),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

line_plot <- function(d, size_range) {
  ggplot(d, aes(x = x, y = error_rate, color = line_lab, size = n, weight = n)) +
    geom_point(alpha = 0.60) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
    scale_color_manual(values = line_palette, breaks = line_levels) +
    scale_size_continuous(range = size_range, guide = "none")
}

fig4_theme <- line_theme +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.margin = margin(t = -2, r = 0, b = -4, l = 0),
    legend.box.margin = margin(t = -2, r = 0, b = -4, l = 0),
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 4, b = 0)),
    plot.margin = margin(0, 2, -6, 2),
    aspect.ratio = 0.42,
    plot.tag = element_text(face = "bold", size = 15)
  )
fig4_legend <- guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 3.0, alpha = 1)))

fig4_stimuli <- function(df) {
  by_cost <- error_by(df, "score_lost_not_opt")
  max_cost <- max(1, floor(max(by_cost$x)))
  p_cost <- line_plot(by_cost, c(1.5, 4.2)) +
    scale_x_continuous(limits = c(1, max_cost), breaks = seq(1, max_cost, by = 1), expand = c(0.02, 0.02)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
                       expand = c(0.005, 0.005)) +
    labs(x = "Points Lost if Error", y = "Error Rate", tag = "a") +
    fig4_theme + fig4_legend
  p_dots <- line_plot(error_by(df, "dots_on_screen"), c(0.8, 2.6)) +
    scale_x_continuous(breaks = c(12, 18, 24), labels = c("12", "18", "24"), expand = c(0.02, 0.02)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = c(0.005, 0.005)) +
    labs(x = "Total Dots on Screen", y = NULL, tag = "b") +
    fig4_theme + theme(axis.title.y = element_blank()) + fig4_legend
  (p_cost + p_dots) + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")
}

fig6_learning <- function(df) {
  d <- error_by(df, "round_num_within")
  line_plot(d, c(2.3, 2.6)) +
    scale_x_continuous(breaks = sort(unique(d$x)), expand = c(0.02, 0.02)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = c("0.00", "0.25", "0.50", "0.75", "1.00"),
                       expand = c(0.01, 0.01)) +
    labs(x = "Round Number", y = "Error Rate") +
    line_theme +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 4),
      axis.title.x = element_text(face = "bold", size = 14, margin = margin(t = 8, b = 8)),
      plot.margin = margin(4, 4, 2, 4),
      aspect.ratio = 0.43
    ) +
    guides(color = guide_legend(nrow = 4, byrow = TRUE, override.aes = list(size = 3.0, alpha = 1)))
}

# ---- Write figures ----
# File names match the draft's Overleaf: fig6_dots_dist = Figure 4, fig7_learn = Figure 6.

samps <- list(
  main          = to_analyze,
  pass_comps    = to_analyze %>% filter(all_comps_one_try),                       # Appendix A
  exclude_early = to_analyze %>% filter(!round_number %in% c(1, 2, 13, 14)),      # Appendix B
  adv_only      = to_analyze %>% filter(action_game_type == "adversarial"),       # Appendix D
  coop_only     = to_analyze %>% filter(action_game_type == "cooperative")        # Appendix D
)

save_gg <- function(p, file, w, h) ggsave(file, p, width = w, height = h, dpi = 300, bg = "white")

for (nm in names(samps)) {
  df <- samps[[nm]]
  dir <- file.path(out_dir, nm)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (nm %in% c("main", "pass_comps", "exclude_early")) {
    save_grob(fig2_ego(df), file.path(dir, "fig2_ego.png"))
    save_grob(fig3_focal(df), file.path(dir, "fig3_focal.png"))
    save_gg(fig4_stimuli(df), file.path(dir, "fig6_dots_dist.png"), 9.8, 3.4)
  }
  save_gg(fig5_hetero(df), file.path(dir, "fig5_hetero.png"), 13.2, 5.1)
  if (nm == "main") save_gg(fig6_learning(df), file.path(dir, "fig7_learn.png"), 9.8, 3.6)
}

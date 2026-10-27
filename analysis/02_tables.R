library(tidyverse)
library(fixest)

out_dir <- "tables_and_charts"
paper_dir <- "../paper_nhb"

mirror <- function(file) {
  if (!dir.exists(paper_dir)) return(invisible())
  dest <- file.path(paper_dir, sub("^tables_and_charts/(main/)?", "", file))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  file.copy(file, dest, overwrite = TRUE)
}

to_analyze <- read_csv("data/built/round_level_data.csv", show_col_types = FALSE) %>%
  mutate(
    category = relevel(factor(category, levels = c("ego_opt", "other_opt", "both_opt")), ref = "ego_opt"),
    draw_mode = relevel(factor(draw_mode, levels = c("take", "give")), ref = "take"),
    action_game_type = relevel(factor(action_game_type, levels = c("adversarial", "cooperative")), ref = "adversarial")
  )

hetero <- read_csv("data/built/participant_level_data.csv", show_col_types = FALSE) %>%
  mutate(draw_mode = relevel(factor(draw_mode, levels = c("take", "give")), ref = "take"))

# ---- Helpers ----

stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

lin_test_p <- function(model, coefs) {
  b <- coef(model)[names(coefs)]
  V <- vcov(model)[names(coefs), names(coefs), drop = FALSE]
  t <- sum(coefs * b) / sqrt(drop(t(coefs) %*% V %*% coefs))
  2 * pt(-abs(t), degrees_freedom(model, "t"))
}

strip_zero <- function(x) gsub("(?<![0-9])0\\.", ".", gsub("(?<![0-9])-0\\.", "-.", x, perl = TRUE), perl = TRUE)
fmt_num <- function(x) strip_zero(formatC(x, digits = 3, format = "f"))
fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

coef_row <- function(m, term) {
  ct <- as.data.frame(summary(m)$coeftable)
  if (!term %in% rownames(ct)) return(NULL)
  list(estimate = ct[term, "Estimate"], std.error = ct[term, "Std. Error"], p.value = ct[term, "Pr(>|t|)"])
}

# Each number is split at the decimal point so columns can be aligned with \hphantom padding
split_dec <- function(s) {
  parts <- strsplit(s, ".", fixed = TRUE)[[1]]
  list(left = parts[1], right = if (length(parts) > 1) parts[2] else "")
}
coef_parts <- function(x, p) {
  sp <- split_dec(fmt_num(abs(x)))
  list(left = paste0(if (x < 0) "-" else "", sp$left), right = paste0(sp$right, stars(p)))
}
se_parts <- function(x) {
  sp <- split_dec(fmt_num(abs(x)))
  list(left = paste0("(", if (x < 0) "-" else "", sp$left), right = paste0(sp$right, ")"))
}
pval_parts <- function(p) {
  if (is.na(p)) return(list(left = "", right = ""))
  if (p < .001) return(list(left = "<", right = paste0("001", stars(p))))
  sp <- split_dec(fmt_num(p))
  list(left = sp$left, right = paste0(sp$right, stars(p)))
}

column_widths <- function(models, term_map, pval_rows = NULL) {
  map(seq_along(models), \(j) {
    parts <- list()
    for (term in names(term_map)) {
      rr <- coef_row(models[[j]], term)
      if (!is.null(rr)) parts <- c(parts, list(coef_parts(rr$estimate, rr$p.value), se_parts(rr$std.error)))
    }
    for (row in pval_rows) parts <- c(parts, list(pval_parts(row[[j]])))

    lefts  <- map_chr(parts, "left")
    rights <- map_chr(parts, "right")
    n_stars <- max(c(0, nchar(str_extract(rights, "\\*+$") %>% replace_na(""))))
    left_t  <- paste0(strrep("8", max(c(0, nchar(lefts)))),
                      if (any(grepl("-", lefts, fixed = TRUE))) "-" else "",
                      if (any(grepl("<", lefts, fixed = TRUE))) "<" else "",
                      if (any(grepl("(", lefts, fixed = TRUE))) "(" else "")
    right_t <- paste0(strrep("8", max(c(0, nchar(rights)))),
                      if (any(grepl("\\)$", rights))) ")" else "",
                      strrep("*", n_stars))
    list(left_max = max(c(0, nchar(lefts))), right_max = max(c(0, nchar(rights))),
         left_t = if (left_t == "") "8" else left_t, right_t = if (right_t == "") "8" else right_t)
  })
}

phantom <- function(n, template) {
  if (n <= 0) return("")
  tmpl <- paste0(template, strrep(substr(template, 1, 1), n + 5))
  paste0("\\hphantom{", substr(tmpl, 1, n), "}")
}

cell <- function(parts, w) {
  if (parts$left == "" && parts$right == "") return("")
  paste0("\\mbox{", phantom(w$left_max - nchar(parts$left), w$left_t), parts$left, ".",
         parts$right, phantom(w$right_max - nchar(parts$right), w$right_t), "}")
}

row <- function(label, cells) paste0("   ", paste(c(label, cells), collapse = " & "), "\\\\")

pval_row <- function(label, vals, widths) row(label, map2_chr(vals, widths, \(v, w) cell(pval_parts(v), w)))

write_reg_table <- function(models, term_map, file, dv_label, note_after_stats,
                            stats_rows = NULL, pval_rows = NULL, pval_title = NULL, width = "\\linewidth",
                            group_header = NULL) {
  n <- length(models)
  widths <- column_widths(models, term_map, pval_rows)
  note <- "$^{*}p<.10$, $^{**}p<.05$, $^{***}p<.01$."

  body <- unlist(map(names(term_map), \(term) {
    rr <- map(models, coef_row, term = term)
    c(row(term_map[[term]], map2_chr(rr, widths, \(r, w) if (is.null(r)) "" else cell(coef_parts(r$estimate, r$p.value), w))),
      row("", map2_chr(rr, widths, \(r, w) if (is.null(r)) "" else cell(se_parts(r$std.error), w))))
  }))

  footer <- character(0)
  if (!note_after_stats) footer <- c(footer, paste0("   \\multicolumn{", n + 1, "}{l}{\\scriptsize ", note, "}\\\\"))
  if (!is.null(pval_rows)) {
    footer <- c(footer, "   \\midrule", row(pval_title, rep("", n)),
                map2_chr(names(pval_rows), pval_rows, \(lbl, v) pval_row(paste0("\\hspace{1em}", lbl), v, widths)))
  }
  if (!is.null(stats_rows)) footer <- c(footer, "   \\midrule", stats_rows)
  if (note_after_stats) footer <- c(footer, paste0("   \\multicolumn{", n + 1, "}{l}{\\footnotesize ", note, "}\\\\"))

  writeLines(c(
    "\\begingroup", "\\centering",
    paste0("\\begin{tabular*}{", width, "}{@{\\extracolsep{\\fill}}l", strrep("c", n), "}"),
    "   \\toprule", group_header,
    paste0("   ", dv_label, " & ", paste0("\\multicolumn{1}{c}{(", seq_len(n), ")}", collapse = " & "), "\\\\"),
    "   \\midrule",
    body, footer,
    "   \\bottomrule", "\\end{tabular*}", "\\par\\endgroup"
  ), file)
  mirror(file)
}

obs_row <- function(models) row("Observations", map_chr(models, \(m) fmt_int(m$nobs)))

fe_reg <- function(fml, data) feols(fml, data = data, cluster = ~subject_id)

ego_p <- function(m, v = NULL) {
  terms <- if (is.null(v)) c("categoryother_opt", "categoryother_opt:draw_modegive")
           else paste0(c("categoryother_opt:", "categoryother_opt:draw_modegive:"), v)
  lin_test_p(m, setNames(c(1, .5), terms))
}
foc_p <- function(m, v = NULL) {
  lin_test_p(m, setNames(1, paste0("categoryother_opt:draw_modegive", if (is.null(v)) "" else paste0(":", v))))
}

# ---- Table 1 (main/), A1 (pass_comps/), B1 (exclude_early/) ----

term_map_main <- c(
  "categoryother_opt" = "Least impact on own score",
  "action_game_typecooperative" = "Cooperative",
  "categoryother_opt:draw_modegive" = "Least $\\times$ Give",
  "categoryother_opt:action_game_typecooperative" = "Least $\\times$ Cooperative",
  "draw_modegive:action_game_typecooperative" = "Give $\\times$ Cooperative",
  "categoryother_opt:draw_modegive:action_game_typecooperative" = "Least $\\times$ Give $\\times$ Cooperative"
)

samps <- list(
  main          = to_analyze,
  pass_comps    = to_analyze %>% filter(all_comps_one_try),
  exclude_early = to_analyze %>% filter(!round_number %in% c(1, 2, 13, 14))
)

for (nm in names(samps)) {
  df <- samps[[nm]]
  models <- list(
    fe_reg(error ~ category*draw_mode + action_game_type | subject_id, df),
    fe_reg(error ~ category*draw_mode + category*action_game_type | subject_id, df),
    fe_reg(error ~ category*draw_mode*action_game_type | subject_id, df)
  )
  dir.create(file.path(out_dir, nm), recursive = TRUE, showWarnings = FALSE)
  write_reg_table(models, term_map_main, file.path(out_dir, nm, "fig4_reg.tex"),
                  "Dependent Var.: Probability of an error", note_after_stats = TRUE,
                  stats_rows = obs_row(models))
}

main_dir <- file.path(out_dir, "main")

models_lost <- list(
  fe_reg(exp_score_lost ~ category*draw_mode + action_game_type | subject_id, to_analyze),
  fe_reg(exp_score_lost ~ category*draw_mode + category*action_game_type | subject_id, to_analyze),
  fe_reg(exp_score_lost ~ category*draw_mode*action_game_type | subject_id, to_analyze)
)
write_reg_table(models_lost, term_map_main, file.path(main_dir, "fig4_reg_lost.tex"),
                "Dependent Var.: Points lost", note_after_stats = TRUE, stats_rows = obs_row(models_lost))

# ---- Table C1: moderation by error cost, complexity, round ----

term_map_mod1 <- c(
  "categoryother_opt" = "Least",
  "action_game_typecooperative" = "Cooperative",
  "categoryother_opt:draw_modegive" = "Least $\\times$ Give",
  "cost_c" = "Error Cost",
  "categoryother_opt:cost_c" = "Least $\\times$ Error Cost",
  "draw_modegive:cost_c" = "Give $\\times$ Error Cost",
  "categoryother_opt:draw_modegive:cost_c" = "Least $\\times$ Give $\\times$ Error Cost",
  "dots_c" = "Complexity",
  "categoryother_opt:dots_c" = "Least $\\times$ Complexity",
  "draw_modegive:dots_c" = "Give $\\times$ Complexity",
  "categoryother_opt:draw_modegive:dots_c" = "Least $\\times$ Give $\\times$ Complexity",
  "round_num_within_c" = "Round Num.",
  "categoryother_opt:round_num_within_c" = "Least $\\times$ Round Num.",
  "draw_modegive:round_num_within_c" = "Give $\\times$ Round Num.",
  "categoryother_opt:draw_modegive:round_num_within_c" = "Least $\\times$ Give $\\times$ Round Num."
)

mods <- c("cost_c", "dots_c", "round_num_within_c")
models_mod1 <- list()
ego_vals <- foc_vals <- all_vals <- c()
for (v in mods) {
  m_no  <- fe_reg(as.formula(paste("error ~ category*draw_mode + action_game_type +", v, "| subject_id")), to_analyze)
  m_int <- fe_reg(as.formula(paste("error ~ category*draw_mode*", v, "+ action_game_type | subject_id")), to_analyze)
  models_mod1 <- c(models_mod1, list(m_no, m_int))
  ego_vals <- c(ego_vals, NA, ego_p(m_int, v))
  foc_vals <- c(foc_vals, NA, foc_p(m_int, v))
  all_vals <- c(all_vals, lin_test_p(m_no, setNames(1, v)), NA)
}
write_reg_table(models_mod1, term_map_mod1, file.path(main_dir, "tab_mod1.tex"),
                "DV: Probability of an error", note_after_stats = FALSE,
                pval_rows = list(Egocentrism = ego_vals, Focalism = foc_vals, Overall = all_vals),
                pval_title = "Moderation t-test p-values")

# ---- Table C2: moderation by individual differences ----

term_map_mod2 <- c(
  "categoryother_opt" = "Least",
  "action_game_typecooperative" = "Cooperative",
  "categoryother_opt:draw_modegive" = "Least $\\times$ Give",
  "educ_rank_c" = "Ed. Level",
  "categoryother_opt:educ_rank_c" = "Least $\\times$ Ed. Level",
  "draw_modegive:educ_rank_c" = "Give $\\times$ Ed. Level",
  "categoryother_opt:draw_modegive:educ_rank_c" = "Least $\\times$ Give $\\times$ Ed. Level",
  "crt_c" = "CRT",
  "categoryother_opt:crt_c" = "Least $\\times$ CRT",
  "draw_modegive:crt_c" = "Give $\\times$ CRT",
  "categoryother_opt:draw_modegive:crt_c" = "Least $\\times$ Give $\\times$ CRT",
  "numeracy_c" = "Numeracy",
  "categoryother_opt:numeracy_c" = "Least $\\times$ Numeracy",
  "draw_modegive:numeracy_c" = "Give $\\times$ Numeracy",
  "categoryother_opt:draw_modegive:numeracy_c" = "Least $\\times$ Give $\\times$ Numeracy"
)

mods <- c("educ_rank_c", "crt_c", "numeracy_c")
models_mod2 <- map(mods, \(v) fe_reg(as.formula(paste("error ~ category*draw_mode*", v, "+ action_game_type | subject_id")), to_analyze))
write_reg_table(models_mod2, term_map_mod2, file.path(main_dir, "tab_mod2.tex"),
                "DV: Probability of an error", note_after_stats = FALSE,
                pval_rows = list(Egocentrism = map2_dbl(models_mod2, mods, ego_p),
                                 Focalism = map2_dbl(models_mod2, mods, foc_p)),
                pval_title = "Moderation t-test p-values", width = ".84\\linewidth")

# ---- Table C5: by pair of rounds ----

term_map_round <- c(
  "categoryother_opt" = "Least",
  "action_game_typecooperative" = "Cooperative",
  "categoryother_opt:draw_modegive" = "Least $\\times$ Give"
)

models_round <- map(1:6, \(rg) feols(error ~ category*draw_mode + action_game_type | subject_id,
                                    data = filter(to_analyze, round_grp_within == rg),
                                    cluster = ~subject_id, notes = FALSE))
write_reg_table(models_round, term_map_round, file.path(main_dir, "regs_round_grp_within.tex"),
                "DV: Probability of an error", note_after_stats = FALSE,
                pval_rows = list(Egocentrism = map_dbl(models_round, ego_p),
                                 Focalism = map_dbl(models_round, foc_p)),
                pval_title = "Mechanism t-test p-values")

# ---- Tables C3, C4: who is classified as a payoff / own-score optimizer ----

term_map_class <- c(
  "(Intercept)" = "Constant",
  "draw_modegive" = "Give",
  "educ_rank_c" = "Ed. Level",
  "crt_c" = "CRT",
  "numeracy_c" = "Numeracy"
)

hetero_classif <- hetero %>%
  left_join(to_analyze %>% group_by(subject_id) %>% summarise(lost = sum(exp_score_lost)), by = "subject_id") %>%
  mutate(payoff_optimizer = as.integer(opt_all > 22),
         own_score_optimizer = as.integer(ego_all > 17))

perf_models <- map(c("opt_all", "lost"), \(dv) map(c("educ_rank_c", "crt_c", "numeracy_c", "educ_rank_c + crt_c + numeracy_c"),
  \(x) feols(as.formula(paste(dv, "~ draw_mode +", x)), data = hetero_classif, vcov = "hetero")))
write_reg_table(list_flatten(perf_models), term_map_class, file.path(main_dir, "tab_perf_mod.tex"),
                "", note_after_stats = TRUE,
                group_header = c("   DV: & \\multicolumn{4}{c}{Optimal choices} & \\multicolumn{4}{c}{Points lost}\\\\",
                                 "   \\cmidrule(lr){2-5}\\cmidrule(lr){6-9}"),
                stats_rows = row("Observations", rep(fmt_int(nrow(hetero_classif)), 8)))

for (dv in c("payoff_optimizer", "own_score_optimizer")) {
  models <- map(c("educ_rank_c", "crt_c", "numeracy_c", "educ_rank_c + crt_c + numeracy_c"),
                \(x) feols(as.formula(paste(dv, "~ draw_mode +", x)), data = hetero_classif, vcov = "hetero"))
  write_reg_table(models, term_map_class,
                  file.path(main_dir, if (dv == "payoff_optimizer") "tab_opt_class_mod.tex" else "tab_own_class_mod.tex"),
                  if (dv == "payoff_optimizer") "DV: Payoff optimizer" else "DV: Own-score optimizer",
                  note_after_stats = TRUE,
                  stats_rows = row("Observations", rep(fmt_int(nrow(hetero_classif)), length(models))))
}

# ---- Error rates by cell of the design ----

cells <- to_analyze %>%
  filter(category %in% c("ego_opt", "other_opt")) %>%
  group_by(draw_mode, action_game_type) %>%
  summarise(n = n_distinct(subject_id),
            most = mean(error[category == "ego_opt"]),
            least = mean(error[category == "other_opt"]), .groups = "drop") %>%
  arrange(draw_mode, action_game_type)

writeLines(c(
  "\\begingroup", "\\centering",
  "\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}llcccc}",
  "   \\toprule",
  "   Framing & Role & Most & Least & Least $-$ Most & Participants\\\\",
  "   \\midrule",
  pmap_chr(cells, \(draw_mode, action_game_type, n, most, least) row(
    str_to_title(as.character(draw_mode)), c(str_to_title(as.character(action_game_type)), fmt_num(most), fmt_num(least), fmt_num(least - most), fmt_int(n)))),
  "   \\bottomrule", "\\end{tabular*}", "\\par\\endgroup"
), file.path(main_dir, "tab_cell_errors.tex"))
mirror(file.path(main_dir, "tab_cell_errors.tex"))

# ---- Heterogeneity within Adversarial and Cooperative rounds ----

fmt_p <- \(p) if (p < .001) "<.001" else strip_zero(sprintf("%.3f", p))
n_pct <- \(x) sprintf("%d (%d\\%%)", sum(x), round(100 * mean(x)))

role_subj <- to_analyze %>%
  filter(category %in% c("ego_opt", "other_opt")) %>%
  mutate(own = (category == "ego_opt") == (error == 0)) %>%
  group_by(subject_id, draw_mode, action_game_type) %>%
  summarise(own = sum(own), n = n(), .groups = "drop") %>%
  mutate(comp = n - own)

role_cells <- role_subj %>%
  group_by(action_game_type, draw_mode) %>%
  summarise(own_half = n_pct(own > 6), comp_half = n_pct(comp > 6),
            own_cons = n_pct(own >= 10), comp_cons = n_pct(comp >= 10), .groups = "drop") %>%
  arrange(action_game_type, draw_mode)

role_tests <- map(c("adversarial", "cooperative"), \(r) {
  k <- role_subj %>% filter(action_game_type == r) %>%
    group_by(draw_mode) %>% summarise(own = sum(own >= 10), comp = sum(comp >= 10)) %>%
    column_to_rownames("draw_mode")
  test <- \(a, b) sprintf("%d vs.\\ %d, $p$ %s", a, b, if (a + b == 0) "= 1" else {
    p <- binom.test(a, a + b)$p.value
    if (p < .001) "< .001" else paste("=", fmt_p(p))
  })
  c(test(k["take", "own"], k["take", "comp"]),
    test(sum(k$own), sum(k$comp)),
    test(k["take", "own"] + k["give", "comp"], k["take", "comp"] + k["give", "own"]))
})

writeLines(c(
  "\\begingroup", "\\centering",
  "\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}llcccc}",
  "   \\toprule",
  "   & & \\multicolumn{2}{c}{More than 6 of 12 rounds} & \\multicolumn{2}{c}{At least 10 of 12 rounds}\\\\",
  "   \\cmidrule(lr){3-4}\\cmidrule(lr){5-6}",
  "   Role & Framing & Own score & Computer's score & Own score & Computer's score\\\\",
  "   \\midrule",
  pmap_chr(role_cells, \(action_game_type, draw_mode, own_half, comp_half, own_cons, comp_cons) row(
    str_to_title(as.character(action_game_type)), c(str_to_title(as.character(draw_mode)), own_half, comp_half, own_cons, comp_cons))),
  "   \\bottomrule", "\\end{tabular*}", "\\par\\medskip",
  "\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}lcc}",
  "   \\toprule",
  "   Binomial test (at least 10 of 12 rounds) & Adversarial & Cooperative\\\\",
  "   \\midrule",
  row("Take: own vs.\\ computer's score", c(role_tests[[1]][1], role_tests[[2]][1])),
  row("Both framings: own vs.\\ computer's score", c(role_tests[[1]][2], role_tests[[2]][2])),
  row("Both framings: focal vs.\\ non-focal score", c(role_tests[[1]][3], role_tests[[2]][3])),
  "   \\bottomrule", "\\end{tabular*}", "\\par\\endgroup"
), file.path(main_dir, "tab_hetero_roles.tex"))
mirror(file.path(main_dir, "tab_hetero_roles.tex"))

# ---- Table D1: self-reported strategic focus ----

focus_labels <- c(
  "Both of the above were equally relevant." = "All dots equally",
  "The amount of red dots in each pod."      = "Dots that scored for participant",
  "The amount of blue dots in each pod."     = "Dots that scored for computer",
  "Something else."                          = "Something else",
  "Not sure."                                = "Not sure"
)

focus <- map(c("strat_overall", "strat_adv", "strat_coop"), \(v) {
  hetero %>%
    mutate(answer = factor(.data[[v]], levels = names(focus_labels))) %>%
    count(draw_mode, answer, .drop = FALSE) %>%
    group_by(draw_mode) %>%
    mutate(cell = sprintf("%d (%d\\%%)", n, round(100 * n / sum(n)))) %>%
    ungroup() %>%
    select(-n) %>%
    pivot_wider(names_from = draw_mode, values_from = cell) %>%
    select(answer, take, give)
}) %>%
  reduce(left_join, by = "answer") %>%
  mutate(label = focus_labels[as.character(answer)])

writeLines(c(
  "\\begingroup\\small\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}lcccccc}",
  "   \\toprule",
  "   & \\multicolumn{2}{c}{\\textbf{Overall}} & \\multicolumn{2}{c}{\\textbf{Adversarial}} & \\multicolumn{2}{c}{\\textbf{Cooperative}}\\\\",
  "   \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "   \\textbf{Self-reported focus} & \\textbf{Take} & \\textbf{Give} & \\textbf{Take} & \\textbf{Give} & \\textbf{Take} & \\textbf{Give}\\\\",
  "   \\midrule",
  pmap_chr(select(focus, label, take.x, give.x, take.y, give.y, take, give),
           \(label, ...) row(label, c(...))),
  "   \\bottomrule", "\\end{tabular*}", "\\par\\endgroup"
), file.path(main_dir, "tab_strategy_focus.tex"))
mirror(file.path(main_dir, "tab_strategy_focus.tex"))

# ---- Table E1: response times ----

term_map_rt <- c(
  "categoryother_opt" = "Least impact on own score",
  "action_game_typecooperative" = "Cooperative",
  "categoryother_opt:draw_modegive" = "Least $\\times$ Give"
)

models_rt <- list(
  fe_reg(time_to_decide ~ category*draw_mode + action_game_type | subject_id + round_grp_number, to_analyze),
  fe_reg(time_to_decide ~ category*draw_mode + action_game_type | subject_id + round_grp_number, filter(to_analyze, always_opt))
)
write_reg_table(models_rt, term_map_rt, file.path(main_dir, "tab_response_time.tex"),
                "DV: Response time (s)", note_after_stats = TRUE, stats_rows = obs_row(models_rt))

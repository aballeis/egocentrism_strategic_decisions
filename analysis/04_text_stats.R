library(tidyverse)
library(fixest)

# Prints the statistics quoted in the text of the draft.

to_analyze <- read_csv("data/built/round_level_data.csv", show_col_types = FALSE) %>%
  mutate(
    category = relevel(factor(category, levels = c("ego_opt", "other_opt", "both_opt")), ref = "ego_opt"),
    draw_mode = relevel(factor(draw_mode, levels = c("take", "give")), ref = "take"),
    action_game_type = relevel(factor(action_game_type, levels = c("adversarial", "cooperative")), ref = "adversarial"),
    focal_works = as.numeric(focal_works)
  )

fe_reg <- function(fml) feols(fml, data = to_analyze, cluster = ~subject_id)

# Linear combination of coefficients, normal approximation
lin_comb <- function(m, coefs) {
  b <- coef(m)[names(coefs)]
  V <- vcov(m)[names(coefs), names(coefs), drop = FALSE]
  est <- sum(coefs * b)
  se <- sqrt(drop(t(coefs) %*% V %*% coefs))
  c(est = est, t = est / se, p = 2 * (1 - pnorm(abs(est / se))), lo = est - 1.96 * se, hi = est + 1.96 * se)
}

# Single coefficient, fixest t-based inference
coef_stat <- function(m, term) {
  ct <- coeftable(m)[term, ]
  ci <- confint(m)[term, ]
  c(est = ct[["Estimate"]], t = ct[["t value"]], p = ct[["Pr(>|t|)"]], lo = ci[[1]], hi = ci[[2]])
}

report <- function(label, s, digits = 3) {
  f <- \(x) formatC(x, format = "f", digits = digits)
  p <- if (s[["p"]] < 0.001) "< 0.001" else paste("=", formatC(s[["p"]], format = "f", digits = 3))
  cat(sprintf("%-55s est = %s  t = %s  p %s  95%% CI [%s, %s]\n",
              label, f(s[["est"]]), formatC(s[["t"]], format = "f", digits = 3), p, f(s[["lo"]]), f(s[["hi"]])))
}

# Figure 2a: egocentrism, pooled
ctrl <- "category + action_game_type + draw_mode | subject_id"
m_err  <- fe_reg(as.formula(paste("error ~", ctrl)))
m_lost <- fe_reg(as.formula(paste("exp_score_lost ~", ctrl)))
m_pay  <- fe_reg(as.formula(paste("round_payoff ~", ctrl)))
report("Fig 2a: error rate, least vs most own impact", coef_stat(m_err, "categoryother_opt"))
report("Fig 2a: points lost", coef_stat(m_lost, "categoryother_opt"))
pay <- coef_stat(m_pay, "categoryother_opt")
report("Fig 2a: round payoff ($)", pay, digits = 4)
cat(sprintf("Fig 2a: bonus change relative to own-impact rounds = %.2f%%\n",
            100 * pay[["est"]] / mean(to_analyze$round_payoff[to_analyze$category == "ego_opt"], na.rm = TRUE)))

# Figure 2b/c: egocentrism in adversarial vs cooperative
m_ac <- fe_reg(error ~ category * action_game_type + draw_mode | subject_id)
report("Fig 2b/c: cooperative - adversarial", coef_stat(m_ac, "categoryother_opt:action_game_typecooperative"))
report("Fig 2b: egocentrism, adversarial", lin_comb(m_ac, c(categoryother_opt = 1)))
report("Fig 2c: egocentrism, cooperative",
       lin_comb(m_ac, c(categoryother_opt = 1, `categoryother_opt:action_game_typecooperative` = 1)))

# Figure 3a: focalism
m_foc <- feols(error ~ focal_works + draw_mode + action_game_type | subject_id,
               data = filter(to_analyze, !is.na(focal_works)), cluster = ~subject_id)
report("Fig 3a: focal vs non-focal", coef_stat(m_foc, "focal_works"))

# Figure 3b/c: egocentrism gap by Take/Give
m_tg <- fe_reg(error ~ category * draw_mode + action_game_type | subject_id)
report("Fig 3b: Take gap (least - most own impact)", lin_comb(m_tg, c(categoryother_opt = 1)))
report("Fig 3c: Give gap", lin_comb(m_tg, c(categoryother_opt = 1, `categoryother_opt:draw_modegive` = 1)))
report("Fig 3b/c: Give gap - Take gap", lin_comb(m_tg, c(`categoryother_opt:draw_modegive` = 1)))

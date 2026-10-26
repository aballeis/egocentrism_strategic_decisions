library(tidyverse)
library(fixest)

setFixest_notes(FALSE)

to_analyze <- read_csv("data/built/round_level_data.csv", show_col_types = FALSE) %>%
  mutate(
    category = relevel(factor(category, levels = c("ego_opt", "other_opt")), ref = "ego_opt"),
    draw_mode = relevel(factor(draw_mode, levels = c("take", "give")), ref = "take"),
    action_game_type = relevel(factor(action_game_type, levels = c("adversarial", "cooperative")), ref = "adversarial"),
    focal_works = as.numeric(focal_works),
    own = as.integer((category == "ego_opt") == (error == 0))
  )
participants <- read_csv("data/built/participant_level_data.csv", show_col_types = FALSE)

L  <- "categoryother_opt"
LG <- "categoryother_opt:draw_modegive"
LC <- "categoryother_opt:action_game_typecooperative"
LGC <- "categoryother_opt:draw_modegive:action_game_typecooperative"

fe_reg <- function(fml, data = to_analyze) feols(fml, data = data, cluster = ~subject_id)

lin_comb <- function(m, coefs) {
  b <- coef(m)[names(coefs)]
  V <- vcov(m)[names(coefs), names(coefs), drop = FALSE]
  est <- sum(coefs * b)
  se <- sqrt(drop(t(coefs) %*% V %*% coefs))
  df <- degrees_freedom(m, "t")
  q <- qt(0.975, df)
  list(est = est, se = se, t = est / se, df = df, p = 2 * pt(-abs(est / se), df), lo = est - q * se, hi = est + q * se)
}
lc <- function(m, ...) lin_comb(m, c(...))

f_p <- \(p) if (p < .001) "p < .001" else paste0("p = ", sub("^0", "", sprintf("%.3f", p)))
f_pct <- \(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d), "%")
f_int <- \(x) format(x, big.mark = ",", trim = TRUE)
f_pp <- \(s, d = 0) sprintf(sprintf("%%.%1$df pp (95%%%% CI [%%.%1$df, %%.%1$df], t(%%d) = %%.2f, %%s)", d),
                            100 * s$est, 100 * s$lo, 100 * s$hi, s$df, s$t, f_p(s$p))
f_pts <- \(s) sprintf("%.2f points (95%% CI [%.2f, %.2f], t(%d) = %.2f, %s)", s$est, s$lo, s$hi, s$df, s$t, f_p(s$p))
f_raw <- \(s) sprintf("est = %.4f, SE = %.4f", s$est, s$se)

rows <- list()
add <- function(section, location, statistic, value, raw = "") {
  rows[[length(rows) + 1]] <<- tibble(section, location, statistic, value, raw)
}

# ---- Sample ----

n_subj <- n_distinct(to_analyze$subject_id)
first_role <- to_analyze %>% filter(round_number == 1) %>% count(draw_mode, action_game_type)
add("Sample", "Intro; Results opening; Discussion", "Participants / decisions / rounds each",
    sprintf("%d participants, %s decisions, %d rounds", n_subj, f_int(nrow(to_analyze)), nrow(to_analyze) / n_subj))
add("Sample", "Results opening; SI Section 1 title and captions", "Passed all comprehension checks on first try",
    sprintf("%s (%d participants)", f_pct(mean(participants$all_comps_one_try), 1), sum(participants$all_comps_one_try)))
n_cond <- \(df) df %>% distinct(subject_id, draw_mode) %>% count(draw_mode) %>% deframe()
add("Sample", "Fig. 2 caption; Supp. Fig. 6 caption", "Participants per framing condition, full sample", sprintf("Take N = %d, Give N = %d", n_cond(participants)[["take"]], n_cond(participants)[["give"]]))
add("Sample", "Supp. Fig. 1 caption", "Participants per framing condition, passed comprehension checks", sprintf("Take N = %d, Give N = %d", n_cond(filter(participants, all_comps_one_try))[["take"]], n_cond(filter(participants, all_comps_one_try))[["give"]]))
add("Sample", "Methods, Participants", "Participants per cell (framing x first role)",
    paste(sprintf("%s/%s-first = %d", first_role$draw_mode, first_role$action_game_type, first_role$n), collapse = "; "))
add("Sample", "Methods, Participants", "Gender",
    sprintf("%s male, %s female, %s other (%d)", f_pct(mean(participants$gender == "Male")),
            f_pct(mean(participants$gender == "Female")), f_pct(mean(!participants$gender %in% c("Male", "Female"))),
            sum(!participants$gender %in% c("Male", "Female"))))
race <- participants %>%
  mutate(race = if_else(race %in% c("Multiple / mixed-race", "Other"), "Multiple, mixed, or other", race)) %>%
  count(race, sort = TRUE)
add("Sample", "Methods, Participants", "Race/ethnicity",
    paste(sprintf("%s %s", race$race, f_pct(race$n / sum(race$n))), collapse = "; "),
    paste(sprintf("%s = %d", race$race, race$n), collapse = "; "))
add("Sample", "Methods, Participants", "Age range (mean)",
    sprintf("%.0f to %.0f (mean = %.0f)", min(participants$age), max(participants$age), mean(participants$age)))
add("Sample", "Methods, Incentives", "Bonus range (mean)",
    sprintf("$%.2f to $%.2f (mean = $%.2f)", min(participants$bonus), max(participants$bonus), mean(participants$bonus)))
add("Sample", "Methods, Participants", "Consented / returned / incomplete (294 / 34 / 14)",
    "Not computable from the replication data (Prolific records)")

pools <- map_dfr(c("take_adv", "give_adv", "take_coop", "give_coop"), \(p) {
  parts <- strsplit(p, "_")[[1]]
  read_csv(file.path("interface/stimuli", paste0("draft_sample2_", p, ".csv")), show_col_types = FALSE) %>%
    summarise(pool = p, n = n(), most = sum(.data[[paste0("category_", parts[2], "_", parts[1])]] == "ego_opt"))
})
add("Sample", "Methods, Generation of Pods-and-Dots stimuli", "Most stimuli per pool (range)",
    sprintf("%d to %d of %d", min(pools$most), max(pools$most), max(pools$n)),
    paste(sprintf("%s = %d", pools$pool, pools$most), collapse = "; "))

# ---- Egocentrism and focalism ----

reg1 <- fe_reg(error ~ category*draw_mode + action_game_type | subject_id)
reg2 <- fe_reg(error ~ category*draw_mode + category*action_game_type | subject_id)
reg3 <- fe_reg(error ~ category*draw_mode*action_game_type | subject_id)
lost1 <- fe_reg(exp_score_lost ~ category*draw_mode + action_game_type | subject_id)
foc <- fe_reg(error ~ focal_works + action_game_type | subject_id)
foc_lost <- fe_reg(exp_score_lost ~ focal_works + action_game_type | subject_id)

ego <- lc(reg1, setNames(c(1, .5), c(L, LG)))
take_gap <- lc(reg1, setNames(1, L))
give_gap <- lc(reg1, setNames(c(1, 1), c(L, LG)))
focal_b3 <- lc(reg1, setNames(1, LG))
foc_fw <- lc(foc, focal_works = 1)
coop_diff <- lc(reg2, setNames(1, LC))
adv_gap <- lc(reg2, setNames(c(1, .5), c(L, LG)))
coop_gap <- lc(reg2, setNames(c(1, 1, .5), c(L, LC, LG)))
triple <- lc(reg3, setNames(1, LGC))
lost <- lc(lost1, setNames(c(1, .5), c(L, LG)))

add("Main", "Intro; Results, Egocentrism; Discussion", "Egocentrism gap (Table 1 reg. 1, b1 + b3/2)", f_pp(ego), f_raw(ego))
add("Main", "Results, Egocentrism; SI Table 4", "Points lost per round (reg. 1, b1 + b3/2)", f_pts(lost), f_raw(lost))
add("Main", "Results, Egocentrism", "Gap larger in Cooperative than Adversarial (Table 1 reg. 2, Least x Coop)", f_pp(coop_diff), f_raw(coop_diff))
add("Main", "Intro; Results, Egocentrism (Fig. 2b,c)", "Egocentrism gap, Adversarial (reg. 2)", f_pp(adv_gap), f_raw(adv_gap))
add("Main", "Intro; Results, Egocentrism (Fig. 2b,c)", "Egocentrism gap, Cooperative (reg. 2)", f_pp(coop_gap), f_raw(coop_gap))
add("Main", "Results, Focalism (Fig. 3a)", "Focal vs non-focal error rate", f_pp(foc_fw), f_raw(foc_fw))
add("Main", "Results, Evidence of focalism", "Non-focal minus focal error rate (Take-Least, Give-Most vs Take-Most, Give-Least)", f_pp(lc(foc, focal_works = -1)), f_raw(lc(foc, focal_works = -1)))
add("Main", "Results, Evidence of focalism", "Non-focal minus focal points lost per round", f_pts(lc(foc_lost, focal_works = -1)), f_raw(lc(foc_lost, focal_works = -1)))
add("Main", "Table 2 caption; Results, Focalism", "Focalism test (reg. 1, b3)", f_pp(focal_b3), f_raw(focal_b3))
add("Main", "Table 2 caption", "Egocentrism test p-value (reg. 1)", f_p(ego$p))
add("Main", "Intro; Results, Focalism (Fig. 3b)", "Take gap (reg. 1, b1)", f_pp(take_gap), f_raw(take_gap))
add("Main", "Intro; Results, Focalism (Fig. 3c); Discussion", "Give gap (reg. 1, b1 + b3)", f_pp(give_gap), f_raw(give_gap))

err_rates <- to_analyze %>% group_by(draw_mode) %>% summarise(rate = mean(error))
tg <- feols(error ~ draw_mode, data = to_analyze, cluster = ~subject_id)
tg_diff <- lc(tg, draw_modegive = 1)
add("Main", "Results, Focalism", "Overall error rate, Give / Take",
    sprintf("Give %s, Take %s", f_pct(err_rates$rate[err_rates$draw_mode == "give"], 1), f_pct(err_rates$rate[err_rates$draw_mode == "take"], 1)),
    sprintf("Give = %.4f, Take = %.4f", err_rates$rate[err_rates$draw_mode == "give"], err_rates$rate[err_rates$draw_mode == "take"]))
add("Main", "Results, Focalism", "Overall error rate, Give - Take (round level, clustered by subject)",
    sprintf("%.1f pp (95%% CI [%.1f, %.1f], t(%d) = %.2f, %s)", 100 * tg_diff$est, 100 * tg_diff$lo, 100 * tg_diff$hi, tg_diff$df, tg_diff$t, f_p(tg_diff$p)),
    f_raw(tg_diff))
add("Main", "Results, Focalism", "Give framing effect on gap, Coop vs Adv (Table 1 reg. 3, b6)", f_pp(triple), f_raw(triple))

order_df <- to_analyze %>%
  group_by(subject_id) %>%
  mutate(least = as.integer(category == "other_opt"),
         coop_first = as.integer(action_game_type[round_number == 1] == "cooperative")) %>%
  ungroup()
ord <- lc(fe_reg(error ~ least + least:coop_first | subject_id, order_df), `least:coop_first` = 1)
add("Main", "Methods, Regression analyses (order test)", "Least x CoopFirst (gamma2)", f_pp(ord, 1), f_raw(ord))

# ---- Heterogeneity ----

by_subj <- function(df) df %>%
  group_by(subject_id, draw_mode) %>%
  summarise(n = n(), own = sum(own), opt = sum(1 - error), .groups = "drop") %>%
  mutate(comp = n - own)
share <- \(d, cond, f) mean(f(filter(d, draw_mode == cond)))
w_share <- \(d, f_take, f_give) mean(c(share(d, "take", f_take), share(d, "give", f_give)))
k_pool <- \(d, f_take, f_give) sum(f_take(filter(d, draw_mode == "take"))) + sum(f_give(filter(d, draw_mode == "give")))
binom_p <- \(a, b) binom.test(a, a + b)$p.value

all24 <- by_subj(to_analyze)
add("Heterogeneity", "Results, Heterogeneity p1", "Payoff optimizers (>= 23 of 24), Take / Give",
    sprintf("Take %s, Give %s", f_pct(share(all24, "take", \(d) d$opt >= 23)), f_pct(share(all24, "give", \(d) d$opt >= 23))))
for (cond in c("take", "give")) {
  add("Heterogeneity", "Results, Heterogeneity p2", paste0("Own > half / computer > half of 24 rounds, ", str_to_title(cond)),
      sprintf("own %s, computer %s", f_pct(share(all24, cond, \(d) d$own > 12)), f_pct(share(all24, cond, \(d) d$comp > 12))))
}

for (role in c("adversarial", "cooperative")) {
  d <- by_subj(filter(to_analyze, action_game_type == role))
  for (cond in c("take", "give")) {
    add("Heterogeneity", "Intro p1 (Adversarial) / p2 (Cooperative)",
        sprintf("%s rounds, %s: own > 6 of 12 / computer > 6 of 12", str_to_title(role), str_to_title(cond)),
        sprintf("own %s, computer %s", f_pct(share(d, cond, \(x) x$own > 6)), f_pct(share(d, cond, \(x) x$comp > 6))))
  }
  add("Heterogeneity", "Intro p1 (Adversarial) / p2 (Cooperative)",
      sprintf("%s rounds, pooled (all participants): own > 6 / computer > 6", str_to_title(role)),
      sprintf("own %s, computer %s", f_pct(mean(d$own > 6)), f_pct(mean(d$comp > 6))))
  add("Heterogeneity", "Intro p1 (Adversarial) / p2 (Cooperative)",
      sprintf("%s rounds, Take/Give equally weighted: own > 6 / computer > 6", str_to_title(role)),
      sprintf("own %s, computer %s", f_pct(w_share(d, \(x) x$own > 6, \(x) x$own > 6)), f_pct(w_share(d, \(x) x$comp > 6, \(x) x$comp > 6))))
}

thresh_p <- 2 * pbinom(6, 24, .5)
add("Heterogeneity", "Results, Heterogeneity p3; Methods, Heterogeneity", "Two-sided binomial p for > 17 of 24", f_p(thresh_p), sprintf("%.4f", thresh_p))
add("Heterogeneity", "SI Table 5 caption", "Two-sided binomial p for at least 10 of 12", f_p(2 * pbinom(2, 12, .5)), sprintf("%.4f", 2 * pbinom(2, 12, .5)))
own18 <- \(d) d$own > 17
comp18 <- \(d) d$comp > 17
k_take <- c(own = sum(own18(filter(all24, draw_mode == "take"))), comp = sum(comp18(filter(all24, draw_mode == "take"))))
add("Heterogeneity", "Results, Heterogeneity p3", "Take: consistent own / computer (> 17 of 24), binomial test",
    sprintf("own %s, computer %s (%d vs %d, %s)", f_pct(share(all24, "take", own18)), f_pct(share(all24, "take", comp18)),
            k_take[["own"]], k_take[["comp"]], f_p(binom_p(k_take[["own"]], k_take[["comp"]]))))
add("Heterogeneity", "Results, Heterogeneity p3", "Give: consistent own / computer (> 17 of 24)",
    sprintf("own %s, computer %s", f_pct(share(all24, "give", own18)), f_pct(share(all24, "give", comp18))))
ego_k <- c(k_pool(all24, own18, own18), k_pool(all24, comp18, comp18))
foc_k <- c(k_pool(all24, own18, comp18), k_pool(all24, comp18, own18))
add("Heterogeneity", "Results, Heterogeneity p3", "Pooled: consistent own vs computer (binomial test)",
    sprintf("%s vs %s, %s", f_pct(ego_k[1] / nrow(all24)), f_pct(ego_k[2] / nrow(all24)), f_p(binom_p(ego_k[1], ego_k[2]))),
    sprintf("counts %d vs %d", ego_k[1], ego_k[2]))
add("Heterogeneity", "Results, Heterogeneity p3", "Pooled: consistent focal vs non-focal (binomial test)",
    sprintf("%s vs %s, %s", f_pct(foc_k[1] / nrow(all24)), f_pct(foc_k[2] / nrow(all24)), f_p(binom_p(foc_k[1], foc_k[2]))),
    sprintf("counts %d vs %d", foc_k[1], foc_k[2]))

strat <- participants %>%
  mutate(group = case_when(str_detect(strat_overall, "red") ~ "own", str_detect(strat_overall, "blue") ~ "comp",
                           str_detect(strat_overall, "Both") ~ "both", TRUE ~ "other")) %>%
  left_join(select(all24, subject_id, own), by = "subject_id")
is_g <- \(g) \(d) d$group == g
sr_ego <- c(k_pool(strat, is_g("own"), is_g("own")), k_pool(strat, is_g("comp"), is_g("comp")))
sr_foc <- c(k_pool(strat, is_g("own"), is_g("comp")), k_pool(strat, is_g("comp"), is_g("own")))
add("Heterogeneity", "SI Table 6 caption", "Self-report own vs computer (binomial test)",
    sprintf("%d vs %d, %s", sum(strat$group == "own"), sum(strat$group == "comp"), f_p(binom_p(sr_ego[1], sr_ego[2]))),
    sprintf("counts %d vs %d", sr_ego[1], sr_ego[2]))
add("Heterogeneity", "SI Table 6 caption", "Self-report focal vs non-focal (binomial test)",
    sprintf("%d vs %d, %s", sum(strat$group == "own" & strat$draw_mode == "take") + sum(strat$group == "comp" & strat$draw_mode == "give"),
            sum(strat$group == "comp" & strat$draw_mode == "take") + sum(strat$group == "own" & strat$draw_mode == "give"),
            f_p(binom_p(sr_foc[1], sr_foc[2]))),
    sprintf("counts %d vs %d", sr_foc[1], sr_foc[2]))
for (v in c("strat_adv", "strat_coop")) {
  g <- case_when(str_detect(participants[[v]], "red") ~ "own", str_detect(participants[[v]], "blue") ~ "comp", TRUE ~ "other")
  take <- participants$draw_mode == "take"
  k <- c(own = sum(g == "own"), comp = sum(g == "comp"),
         foc = sum(g == "own" & take) + sum(g == "comp" & !take), non = sum(g == "comp" & take) + sum(g == "own" & !take))
  add("Heterogeneity", "SI Table 6 caption", sprintf("Self-report, %s rounds: own vs computer; focal vs non-focal (raw counts, binomial)", sub("strat_", "", v)),
      sprintf("%d vs %d, %s; %d vs %d, %s", k[["own"]], k[["comp"]], f_p(binom_p(k[["own"]], k[["comp"]])),
              k[["foc"]], k[["non"]], f_p(binom_p(k[["foc"]], k[["non"]]))))
}
for (g in c("both", "own")) {
  bal <- strat %>% filter(group == g) %>% group_by(draw_mode) %>% summarise(m = mean(own / 24)) %>% pull(m) %>% mean()
  reg <- feols(I(own / 24) ~ i(group, ref = g) + draw_mode, data = strat, vcov = "hetero")
  adj <- coef(reg)[["(Intercept)"]] + coef(reg)[["draw_modetake"]] * mean(strat$draw_mode == "take")
  add("Heterogeneity", "SI Table 6 caption",
      sprintf("Own-score pod share, self-report '%s', controlling for Take/Give", g),
      sprintf("%s (Take/Give equally weighted); %s (regression-adjusted)", f_pct(bal, 1), f_pct(adj, 1)))
}

# ---- Moderators, individual differences, learning ----

ego_t <- \(m, v) lc(m, setNames(c(1, .5), paste0(c(L, LG), ":", v)))
foc_t <- \(m, v) lc(m, setNames(1, paste0(LG, ":", v)))
for (v in c("cost_c", "dots_c", "round_num_within_c")) {
  m_no <- fe_reg(as.formula(paste("error ~ category*draw_mode + action_game_type +", v, "| subject_id")))
  m_int <- fe_reg(as.formula(paste("error ~ category*draw_mode*", v, "+ action_game_type | subject_id")))
  o <- lc(m_no, setNames(1, v))
  if (v == "cost_c") add("Moderation", "Results, Error cost & complexity", "Error cost: change in error rate per point at stake", f_pp(o, 1), f_raw(o))
  add("Moderation", "Results, Error cost & complexity; Results, Learning; SI Table 7 caption",
      sprintf("%s: overall error rate (SI Table 7)", v), sprintf("b = %.4f, t(%d) = %.2f, %s", o$est, o$df, o$t, f_p(o$p)))
  add("Moderation", "Results, Error cost & complexity; Results, Learning; SI Table 7 caption",
      sprintf("%s: moderates egocentrism / focalism (SI Table 7)", v), sprintf("egocentrism %s; focalism %s", f_p(ego_t(m_int, v)$p), f_p(foc_t(m_int, v)$p)))
}
for (v in c("educ_rank_c", "crt_c", "numeracy_c")) {
  m_int <- fe_reg(as.formula(paste("error ~ category*draw_mode*", v, "+ action_game_type | subject_id")))
  add("Moderation", "Results, Individual differences; SI Table 8 caption",
      sprintf("%s: moderates egocentrism / focalism (SI Table 8)", v), sprintf("egocentrism %s; focalism %s", f_p(ego_t(m_int, v)$p), f_p(foc_t(m_int, v)$p)))
}

ind <- participants %>%
  left_join(to_analyze %>% group_by(subject_id) %>% summarise(lost = sum(exp_score_lost)), by = "subject_id") %>%
  mutate(payoff_optimizer = as.integer(opt_all > 22), own_score_optimizer = as.integer(ego_all > 17))
for (v in c("educ_rank_c", "crt_c", "numeracy_c")) {
  for (dv in c("payoff_optimizer", "opt_all", "lost", "own_score_optimizer")) {
    s <- lc(feols(as.formula(paste(dv, "~ draw_mode +", v)), data = ind, vcov = "hetero"), setNames(1, v))
    add("Moderation", "Results, Individual differences; SI Tables 10, 11",
        sprintf("%s predicting %s (participant level, hetero SEs)", v, dv), sprintf("b = %.3f, t(%d) = %.2f, %s", s$est, s$df, s$t, f_p(s$p)))
  }
}

rg <- map_chr(1:6, \(g) {
  m <- fe_reg(error ~ category*draw_mode + action_game_type | subject_id, filter(to_analyze, round_grp_within == g))
  sprintf("pair %d: ego %s, focal %s", g, f_p(lc(m, setNames(c(1, .5), c(L, LG)))$p), f_p(lc(m, setNames(1, LG))$p))
})
add("Moderation", "Results, Learning (SI Table 12)", "Egocentrism / focalism by round pair", paste(rg, collapse = "; "))

fig7_n <- to_analyze %>%
  filter(!is.na(focal_works)) %>%
  transmute(x = round_num_within, cat = category, sal = focal_works) %>%
  pivot_longer(c(cat, sal), values_transform = as.character) %>%
  count(x, name, value)
add("Moderation", "Supp. Fig. 13 caption", "Point sizes (N per round number and line)", sprintf("N = %d to N = %d", min(fig7_n$n), max(fig7_n$n)))

rt <- fe_reg(time_to_decide ~ category*draw_mode + action_game_type | subject_id + round_grp_number)
add("Moderation", "Methods, Regression analyses; SI Table 13", "Participants choosing the payoff-maximizing pod in all 24 rounds",
    sprintf("%d participants", n_distinct(to_analyze$subject_id[to_analyze$always_opt])))
add("Moderation", "SI Table 13", "Response time, full sample: Least / Coop / Least x Give / egocentrism",
    paste(c(f_p(lc(rt, setNames(1, L))$p), f_p(lc(rt, action_game_typecooperative = 1)$p), f_p(lc(rt, setNames(1, LG))$p),
            f_p(lc(rt, setNames(c(1, .5), c(L, LG)))$p)), collapse = " / "))

sat <- to_analyze %>% filter(action_game_type == "adversarial", exp_score_ego != 0) %>% mutate(pos = exp_score_ego > 0)
sat_m <- fe_reg(time_to_decide ~ pos | subject_id + round_grp_number, sat)
sat_s <- lc(sat_m, posTRUE = -1)
sat_base <- mean(fitted(sat_m) - coef(sat_m)[["posTRUE"]] * sat$pos)
sat_raw <- sat %>% group_by(pos) %>% summarise(m = mean(time_to_decide)) %>% deframe()
add("Satisficing", "Results, Satisficing; Methods, Regression analyses", "Response time, Adversarial, own-score difference positive vs negative (participant + round-block FE)",
    sprintf("%.1f seconds faster (%.1f vs. %.1f seconds, 95%% CI [%.2f, %.2f], t(%d) = %.2f, %s)", sat_s$est, sat_base + coef(sat_m)[["posTRUE"]], sat_base,
            sat_s$lo, sat_s$hi, sat_s$df, sat_s$t, f_p(sat_s$p)), f_raw(sat_s))
add("Satisficing", "Methods, Regression analyses", "Response time, unadjusted means (positive vs negative)",
    sprintf("%.1f vs. %.1f seconds (difference %.1f seconds)", sat_raw[["TRUE"]], sat_raw[["FALSE"]], sat_raw[["FALSE"]] - sat_raw[["TRUE"]]))

# ---- Robustness samples ----

samps <- list(pass_comps = filter(to_analyze, all_comps_one_try), exclude_early = filter(to_analyze, !round_number %in% c(1, 2, 13, 14)))
for (nm in names(samps)) {
  d <- samps[[nm]]
  r1 <- fe_reg(error ~ category*draw_mode + action_game_type | subject_id, d)
  r2 <- fe_reg(error ~ category*draw_mode + category*action_game_type | subject_id, d)
  r3 <- fe_reg(error ~ category*draw_mode*action_game_type | subject_id, d)
  for (x in list(list("Egocentrism", lc(r1, setNames(c(1, .5), c(L, LG)))), list("Focalism (b3)", lc(r1, setNames(1, LG))),
                 list("Take gap", lc(r1, setNames(1, L))), list("Give gap", lc(r1, setNames(c(1, 1), c(L, LG)))),
                 list("Coop - Adv gap", lc(r2, setNames(1, LC))), list("Least x Give x Coop", lc(r3, setNames(1, LGC))))) {
    add("Robustness", "Results opening (restricted samples)", sprintf("%s: %s", nm, x[[1]]), f_pp(x[[2]]), f_raw(x[[2]]))
  }
}

# ---- Sensitivity ----

mde <- \(s) 2.8 * s$se
add("Sensitivity", "Methods, Sample size", "MDE egocentrism (2.8 x SE, reg. 1)", sprintf("%.1f pp", 100 * mde(ego)), sprintf("SE = %.4f", ego$se))
add("Sensitivity", "Methods, Sample size", "MDE focalism, focal vs non-focal scale (2.8 x SE)", sprintf("%.1f pp", 100 * mde(foc_fw)), sprintf("SE = %.4f", foc_fw$se))
add("Sensitivity", "Methods, Sample size", "MDE Adv vs Coop gap difference (2.8 x SE, reg. 2)", sprintf("%.1f pp", 100 * mde(coop_diff)), sprintf("SE = %.4f", coop_diff$se))

# ---- Write ----

out <- bind_rows(rows)
md <- c("# Numbers check", "",
        "Generated by `analysis/numbers.R`. Every statistic quoted in the paper text, with where it appears.",
        "Linear combinations use t(clusters - 1); round-level models cluster by subject_id; participant-level models use hetero SEs.", "")
for (sec in unique(out$section)) {
  s <- filter(out, section == sec)
  md <- c(md, paste("##", sec), "", "| Location | Statistic | Value | Unrounded |", "|---|---|---|---|",
          sprintf("| %s | %s | %s | %s |", s$location, s$statistic, s$value, s$raw), "")
}
writeLines(md, "tables_and_charts/numbers_check.md")

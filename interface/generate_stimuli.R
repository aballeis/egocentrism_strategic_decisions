library(tidyverse)

# Generates the stimulus files the game draws rounds from. Each stimulus is two pods (A, B)
# with 1-8 red dots (column 1) and 1-8 blue dots (column 2).

# Candidate stimuli: no ties in red, blue, red+blue, or red-blue across the two pods
make_pool <- function(n_needed = 6000, chunk = 20000) {
  out <- matrix(NA_integer_, 0, 4)
  seen <- character(0)
  while (nrow(out) < n_needed) {
    reds  <- t(replicate(chunk, sample.int(8, 2)))
    blues <- t(replicate(chunk, sample.int(8, 2)))
    ok <- (reds[, 1] + blues[, 1] != reds[, 2] + blues[, 2]) & (reds[, 1] - blues[, 1] != reds[, 2] - blues[, 2])
    dots <- cbind(reds[ok, 1], blues[ok, 1], reds[ok, 2], blues[ok, 2])
    keys <- apply(dots, 1, paste, collapse = "_")
    dots <- dots[!keys %in% seen, , drop = FALSE]
    seen <- c(seen, keys[!keys %in% seen])
    out <- rbind(out, dots[seq_len(min(nrow(dots), n_needed - nrow(out))), , drop = FALSE])
  }
  colnames(out) <- c("A1", "A2", "B1", "B2")
  as_tibble(out)
}

# Pod with the larger / smaller value, and the other pod
hi <- \(a, b) if_else(a > b, "A", "B")
lo <- \(a, b) if_else(a < b, "A", "B")
flip <- \(p) if_else(p == "A", "B", "A")

# Category: which player's score the optimal first pick most affects
categorize <- function(ego, other, opt) {
  case_when(ego == other ~ "both_opt", opt == ego ~ "ego_opt", opt == other ~ "other_opt", TRUE ~ "neither_opt")
}

derive <- function(d) {
  d %>%
    mutate(
      sum1 = hi(A1 + A2, B1 + B2), sum2 = flip(sum1),
      dif1 = hi(A1 - A2, B1 - B2), dif2 = flip(dif1),
      sum_val1 = pmax(A1 + A2, B1 + B2), sum_val2 = pmin(A1 + A2, B1 + B2),
      dif_val1 = pmax(A1 - A2, B1 - B2), dif_val2 = pmin(A1 - A2, B1 - B2),
      take_adv_ego1 = hi(A1, B1), take_adv_ego2 = flip(take_adv_ego1),
      give_adv_ego1 = lo(A1, B1), give_adv_ego2 = flip(give_adv_ego1),
      take_adv_other1 = hi(A2, B2), take_adv_other2 = flip(take_adv_other1),
      give_adv_other1 = lo(A2, B2), give_adv_other2 = flip(give_adv_other1),
      take_adv_opt1 = sum1, take_adv_opt2 = sum2,
      give_adv_opt1 = sum2, give_adv_opt2 = sum1,
      take_coop_ego1 = take_adv_ego1, take_coop_ego2 = take_adv_ego2,
      give_coop_ego1 = give_adv_ego1, give_coop_ego2 = give_adv_ego2,
      take_coop_other1 = give_adv_other1, take_coop_other2 = give_adv_other2,
      give_coop_other1 = take_adv_other1, give_coop_other2 = take_adv_other2,
      take_coop_opt1 = dif1, take_coop_opt2 = dif2,
      give_coop_opt1 = dif2, give_coop_opt2 = dif1,
      category_adv_take  = categorize(take_adv_ego1, take_adv_other1, take_adv_opt1),
      category_adv_give  = categorize(give_adv_ego1, give_adv_other1, give_adv_opt1),
      category_coop_take = categorize(take_coop_ego1, take_coop_other1, take_coop_opt1),
      category_coop_give = categorize(give_coop_ego1, give_coop_other1, give_coop_opt1)
    )
}

set.seed(1234)
pool <- derive(make_pool())

# One file per game: 1000 stimuli where exactly one player's best pod is optimal
out_dir <- "interface/stimuli"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
for (game in c("adv_take", "coop_take", "adv_give", "coop_give")) {
  parts <- str_split_1(game, "_")
  pool %>%
    filter(!.data[[paste0("category_", game)]] %in% c("neither_opt", "both_opt")) %>%
    slice_sample(n = 1000) %>%
    write_csv(file.path(out_dir, paste0("draft_sample2_", parts[2], "_", parts[1], ".csv")))
}

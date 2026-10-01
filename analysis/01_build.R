library(tidyverse)
library(DBI)
library(RSQLite)
library(janitor)
library(jsonlite)

# Game data was recorded twice: by the game app and as a JSON log
# passed back to Qualtrics. For each participant, use the Qualtrics log if it has all 24 rounds,
# otherwise the app database if it does; otherwise drop the participant.

dir.create("data/built", showWarnings = FALSE)
tz <- "America/New_York"
parse_ts <- \(x) as.POSIXct(x, format = "%m/%d/%Y, %I:%M:%S %p", tz = tz)

qualtrics <- read_csv("data/qualtrics_data.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  slice(-c(1, 2))  # Qualtrics label rows

con <- dbConnect(SQLite(), "data/fly_app_data.sqlite")
subjects <- dbGetQuery(con, "SELECT * FROM subjects")
actions  <- dbGetQuery(con, "SELECT * FROM game_actions") %>% distinct()
dbDisconnect(con)

# ---- Round-level game data from each source ----

app_rounds <- subjects %>%
  select(subject_id, qualtrics_id, condition) %>%
  left_join(actions, by = c("subject_id", "condition" = "action_condition")) %>%
  mutate(source = "app")

parse_game_log <- function(json) {
  x <- tryCatch(fromJSON(json), error = function(e) NULL)
  if (is.null(x) || length(x) == 0) return(tibble())
  as_tibble(x)
}

qual_rounds <- qualtrics %>%
  transmute(qualtrics_id = response_id, log = map(game_log_json, parse_game_log)) %>%
  unnest(log) %>%
  mutate(source = "qualtrics")

game_cols <- c("subject_id", "qualtrics_id", "action_game_type", "draw_is_take", "draw_mode",
               "round_number", "round_grp_number", "round_start_time", "timestamp1", "move1",
               "round_payoff", "A1", "A2", "B1", "B2",
               "category_adv_take", "category_adv_give", "category_coop_take", "category_coop_give")

rounds <- bind_rows(
  app_rounds  %>% select(all_of(game_cols), source) %>% mutate(across(everything(), as.character)),
  qual_rounds %>% select(all_of(game_cols), source) %>% mutate(across(everything(), as.character))
)

source_choice <- rounds %>%
  group_by(qualtrics_id) %>%
  summarise(
    subject_id = first(na.omit(subject_id[order(source != "app")])),
    source = case_when(
      sum(source == "qualtrics" & !is.na(round_number)) == 24 ~ "qualtrics",
      sum(source == "app" & !is.na(round_number)) == 24 ~ "app",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(source))

rounds <- rounds %>%
  semi_join(source_choice, by = c("qualtrics_id", "subject_id", "source")) %>%
  type_convert(col_types = cols(.default = col_guess(), move1 = col_character(),
                                round_start_time = col_character(), timestamp1 = col_character()))

# ---- Participant-level survey data ----

attempts <- function(json) {
  x <- tryCatch(fromJSON(json), error = function(e) NULL)
  c(quiz_attempts = as.integer(x$quiz$attempts %||% NA), quiz2_attempts = as.integer(x$quiz2$attempts %||% NA))
}

survey <- qualtrics %>%
  transmute(
    qualtrics_id = response_id,
    quiz = map(quiz_info, attempts),
    crt = str_detect(crt1_1, "^5") + str_detect(crt1_2, "^5") + str_detect(crt1_3, "^47"),
    numeracy = str_detect(num1, ".1") + str_detect(num2, "^10") + str_detect(num3, "^500"),
    educ_rank = case_when(
      str_detect(educ, "Less than") ~ 1,
      str_detect(educ, "High school") ~ 2,
      str_detect(educ, "Some College") ~ 3,
      str_detect(educ, "Bach") ~ 4,
      str_detect(educ, "Post") ~ 5
    ),
    strat_overall = overall_attention_1
  ) %>%
  unnest_wider(quiz)

# Fall back to the app's record of comprehension-check attempts if Qualtrics is missing it
survey <- survey %>%
  left_join(subjects %>% distinct(qualtrics_id, app_q1 = quiz_attempts, app_q2 = quiz2_attempts), by = "qualtrics_id") %>%
  mutate(quiz_attempts = coalesce(quiz_attempts, as.integer(app_q1)),
         quiz2_attempts = coalesce(quiz2_attempts, as.integer(app_q2)),
         all_comps_one_try = quiz_attempts + quiz2_attempts == 2) %>%
  select(-app_q1, -app_q2)

# ---- Analysis variables ----

# Score difference (adversarial) or sum (cooperative) if the participant picks `pod` ("A"/"B").
# Take: participant gets the picked pod's red dots, computer gets the other pod's blue dots.
# Give: computer gets the picked pod's blue dots, participant gets the other pod's red dots.
pod_score <- function(pod, A1, A2, B1, B2, take, adv) {
  own   <- if (pod == "A") if_else(take, A1, B1) else if_else(take, B1, A1)
  other <- if (pod == "A") if_else(take, B2, A2) else if_else(take, A2, B2)
  if_else(adv, own - other, own + other)
}
centered <- \(x) x - median(x, na.rm = TRUE)

round_data <- rounds %>%
  inner_join(survey, by = "qualtrics_id") %>%
  mutate(
    take = draw_is_take == 1,
    adv = action_game_type == "adversarial",
    category = case_when(
      adv & take ~ category_adv_take, adv & !take ~ category_adv_give,
      !adv & take ~ category_coop_take, !adv & !take ~ category_coop_give
    ),
    score_A = pod_score("A", A1, A2, B1, B2, take, adv),
    score_B = pod_score("B", A1, A2, B1, B2, take, adv),
    opt1 = if_else(score_A > score_B, "A", "B"),
    play1_opt = move1 == opt1,
    error = 1 - play1_opt,
    exp_score = if_else(move1 == "A", score_A, score_B),
    exp_score_lost = pmax(score_A, score_B) - exp_score,
    # Pod that maximizes the participant's own score vs. the computer's (helps/hurts it as needed)
    ego_pod = if_else(take, if_else(A1 >= B1, "A", "B"), if_else(A1 <= B1, "A", "B")),
    other_pod = if_else(take == adv, if_else(A2 >= B2, "A", "B"), if_else(A2 <= B2, "A", "B")),
    heur_ego = move1 == ego_pod,
    score_lost_not_opt = abs(if_else(ego_pod == "A", score_A, score_B) - if_else(other_pod == "A", score_A, score_B)),
    focal_works = category == "both_opt" | if_else(take, category == "ego_opt", category == "other_opt"),
    dots_on_screen = A1 + A2 + B1 + B2,
    round_num_within = (round_number %% 12) + 12 * (round_number %% 12 == 0),
    round_grp_within = if_else(round_number < 13, round_grp_number, round_grp_number - 6),
    time_to_decide = as.numeric(difftime(parse_ts(timestamp1), parse_ts(round_start_time), units = "secs")),
    dots_c = centered(dots_on_screen),
    cost_c = centered(score_lost_not_opt),
    round_num_within_c = round_num_within - 6.5,
    crt_c = centered(crt),
    numeracy_c = centered(numeracy),
    educ_rank_c = centered(educ_rank)
  ) %>%
  group_by(subject_id) %>%
  mutate(always_opt = all(play1_opt)) %>%
  ungroup() %>%
  select(subject_id, draw_mode, action_game_type, round_number, round_grp_number, round_num_within,
         round_grp_within, A1, A2, B1, B2, move1, opt1, category, play1_opt, error, heur_ego, focal_works,
         exp_score, exp_score_lost, score_lost_not_opt, dots_on_screen, round_payoff, time_to_decide,
         always_opt, all_comps_one_try, crt, numeracy, educ_rank, strat_overall,
         dots_c, cost_c, round_num_within_c, crt_c, numeracy_c, educ_rank_c) %>%
  arrange(subject_id, round_number)

participant_data <- round_data %>%
  group_by(subject_id, draw_mode, all_comps_one_try, educ_rank, crt, numeracy, educ_rank_c, crt_c, numeracy_c, strat_overall) %>%
  summarise(ego_all = sum(heur_ego), opt_all = sum(play1_opt), error_rate = mean(error), .groups = "drop")

write_csv(round_data, "data/built/round_level_data.csv")
write_csv(participant_data, "data/built/participant_level_data.csv")

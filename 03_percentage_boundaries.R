# Unique batsmen appearing in match_deliveries; with at least 1 ball faced
batsmen_striker <- match_deliveries %>%
  distinct(delivery_batsman)
nrow(batsmen_striker) # 514 rows

# Number of matches by batsmen
batsmen_matches <- match_deliveries %>%
  distinct(delivery_batsman, id) %>%
  group_by(delivery_batsman) %>%
  summarise(n_matches = n()) %>%
  ungroup() %>%
  arrange(desc(n_matches))

# Descriptive statistics of number of matches
summary(batsmen_matches$n_matches) # Median - 8, Mean - 21.97
ggplot(batsmen_matches, aes(n_matches)) +
  geom_histogram(binwidth = 10)

# Restrict to batsmen who have played at least 20 matches
batsmen_matches <- batsmen_matches %>%
  filter(n_matches >= 20) # 151 batsmen

# Restrict the match_deliveries data only to the above batsmen
batsmen_match_details <- match_deliveries %>%
  inner_join(batsmen_matches, by = c("delivery_batsman" = "delivery_batsman"))

# Ensure that extras are treated properly
table(batsmen_match_details$delivery_extras_type)
noballs <- batsmen_match_details %>%
  filter(delivery_extras_type == "noballs", delivery_runs_total > 1)
byes <- batsmen_match_details %>%
  filter(delivery_extras_type == "byes", delivery_runs_batsman > 1)
legbyes <- batsmen_match_details %>%
  filter(delivery_extras_type == "legbyes", delivery_runs_batsman > 1)
wides <- batsmen_match_details %>%
  filter(delivery_extras_type == "wides", delivery_runs_batsman > 1)
penalty <- batsmen_match_details %>%
  filter(delivery_extras_type == "penalty")
batsmen_match_details <- batsmen_match_details %>%
  filter(!(delivery_extras_type %in% c("wides", "penalty")))

# Create 2 fields - one to store runs from boundaries and another to store
# the rest
table(batsmen_match_details$delivery_runs_non_boundary)
batsmen_match_details$delivery_runs_batsman[
  batsmen_match_details$delivery_runs_non_boundary == 1]
batsmen_match_runs <- batsmen_match_details %>%
  mutate(
    runs_boundary = case_when(
      delivery_runs_non_boundary == 0 &
        (delivery_runs_batsman %in% c(4, 6)) ~ delivery_runs_batsman,
      TRUE ~ 0L
    ),
    runs_non_boundary = case_when(
      delivery_runs_non_boundary == 1 |
        !(delivery_runs_batsman %in% c(4, 6)) ~ delivery_runs_batsman,
      TRUE ~ 0L
    )
  ) %>%
  select(id, innings_num, delivery_batsman, delivery_runs_batsman,
         runs_boundary, runs_non_boundary)
nrow(subset(
  batsmen_match_runs,
  delivery_runs_batsman != runs_boundary + runs_non_boundary
))

# Calculate totals for each batsmen
batsmen_runs_summary <- batsmen_match_runs %>%
  group_by(delivery_batsman) %>%
  summarise(
    delivery_runs_batsman = sum(delivery_runs_batsman),
    runs_boundary = sum(runs_boundary),
    runs_non_boundary = sum(runs_non_boundary)
  ) %>%
  ungroup() %>%
  arrange(desc(delivery_runs_batsman))
nrow(subset(
  batsmen_runs_summary,
  delivery_runs_batsman != runs_boundary + runs_non_boundary
))
quantile(batsmen_runs_summary$delivery_runs_batsman, probs = seq(0, 1, 0.1))
batsmen_runs_summary <- batsmen_runs_summary %>%
  filter(delivery_runs_batsman > 1000)
nrow(batsmen_runs_summary)

# Calculate percentage scored in boundaries
batsmen_runs_summary <- batsmen_runs_summary %>%
  mutate(runs_boundary_pct = runs_boundary / delivery_runs_batsman)
summary(batsmen_runs_summary$runs_boundary_pct)
ggplot(batsmen_runs_summary, aes(runs_boundary_pct)) +
  geom_histogram(binwidth = 0.05)

# Unique batsmen appearing in match_deliveries; with at least 1 ball faced
batsmen_striker <- match_deliveries %>%
  distinct(delivery_batsman)
nrow(batsmen_striker) # 461 rows

# Number of matches by batsmen
batsmen_matches <- match_deliveries %>%
  distinct(delivery_batsman, id) %>%
  group_by(delivery_batsman) %>%
  summarise(n_matches = n()) %>%
  ungroup() %>%
  arrange(desc(n_matches))

# Descriptive statistics of number of matches
summary(batsmen_matches$n_matches) # Median - 8, Mean - 20.64
ggplot(batsmen_matches, aes(n_matches)) +
  geom_histogram(binwidth = 10)

# Restrict to batsmen who have played at least 20 matches
batsmen_matches <- batsmen_matches %>%
  filter(n_matches >= 20) # 137 batsmen

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

# Calculate cumulative runs for each player and match
batsmen_match_cumruns <- batsmen_match_details %>%
  select(delivery_batsman, id, delivery_over, delivery_ball,
         delivery_runs_batsman) %>%
  arrange(delivery_batsman, id, delivery_over, delivery_ball) %>%
  group_by(delivery_batsman, id) %>%
  mutate(ball_number = 1:n(),
         cum_runs = cumsum(delivery_runs_batsman))

# Restrict to innings where the batsman played at least 10 balls
batsmen_match_ballsfaced <- batsmen_match_cumruns %>%
  summarise(total_balls = max(ball_number)) %>%
  filter(total_balls >= 10)
batsmen_match_cumruns <- batsmen_match_cumruns %>%
  inner_join(batsmen_match_ballsfaced,
             by = c("delivery_batsman" = "delivery_batsman", "id" = "id")) %>%
  arrange(delivery_batsman, id, ball_number)

# Calculate the cumulative strike rate
summary(batsmen_match_cumruns$strike_rate) # Median - 1.136, Mean - 1.146
batsmen_match_cumruns <- batsmen_match_cumruns %>%
  mutate(strike_rate = cum_runs / ball_number)
ggplot(batsmen_match_cumruns, aes(strike_rate)) +
  geom_histogram(binwidth = 0.25)

# Kohli strike rate
kohli_match_cumruns <- batsmen_match_cumruns %>%
  filter(delivery_batsman == "V Kohli")
ggplot(kohli_match_cumruns,
       aes(ball_number, strike_rate, colour = as.factor(id))) +
  geom_line() +
  ggtitle("Virat Kohli - Cumulative strike rate by ball") +
  xlab("Ball Number") +
  ylab("Strike Rate") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15)
  )

# Kohli Top 10 innings by runs
kohli_match_top10 <- kohli_match_cumruns %>%
  group_by(id) %>%
  summarise(total_runs = sum(delivery_runs_batsman)) %>%
  arrange(desc(total_runs))
kohli_match_cumruns2 <- kohli_match_cumruns %>%
  inner_join(filter(kohli_match_top10, row_number() <= 10),
             by = c("id" = "id"))
ggplot(kohli_match_cumruns2,
       aes(ball_number, strike_rate, colour = as.factor(id))) +
  geom_line(alpha = 0.1) +
  geom_smooth(se = FALSE, alpha = 0.5) +
  ggtitle("Virat Kohli - Cumulative strike rate by ball - Top 10 matches") +
  xlab("Ball Number") +
  ylab("Strike Rate") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15)
  )

# Player top 10
player_cumruns_topn <- function(delivery_batsman_name, top_n = 10) {
  
  player_match_cumruns <- batsmen_match_cumruns %>%
    filter(delivery_batsman == delivery_batsman_name)
  player_match_top10 <- player_match_cumruns %>%
    group_by(id) %>%
    summarise(total_runs = sum(delivery_runs_batsman)) %>%
    arrange(desc(total_runs))
  player_match_cumruns <- player_match_cumruns %>%
    inner_join(filter(player_match_top10, row_number() <= top_n),
               by = c("id" = "id"))
  
  ggplot(player_match_cumruns,
         aes(ball_number, strike_rate, colour = as.factor(id))) +
    geom_line(alpha = 0.1) +
    geom_smooth(se = FALSE, alpha = 0.5) +
    ggtitle(paste0(delivery_batsman_name,
                   " - Cumulative strike rate by ball - Top 10 matches")) +
    xlab("Ball Number") +
    ylab("Strike Rate") +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 15)
    )
  
}
player_cumruns_topn(delivery_batsman_name = "DA Warner")
player_cumruns_topn(delivery_batsman_name = "AB de Villiers")

# Acceleration
batsmen_match_halves <- batsmen_match_cumruns %>%
  mutate(half_01 = ball_number / total_balls > 0.5) %>%
  group_by(delivery_batsman, id, half_01) %>%
  filter(row_number() == n())
batsmen_match_halves2 <- batsmen_match_halves %>%
  ungroup() %>%
  mutate(
    diff_rate = if_else(half_01, strike_rate - lag(strike_rate), 0)
  ) %>%
  filter(half_01)
summary(batsmen_match_halves2$diff_rate) # Median - 0.1282 Mean - 0.1334
ggplot(batsmen_match_halves2, aes(diff_rate)) +
  geom_histogram(binwidth = 0.05)
batsmen_acceleration <- batsmen_match_halves2 %>%
  group_by(delivery_batsman) %>%
  summarise(
    total_runs = sum(cum_runs),
    avg_diff_strike_rate = mean(diff_rate),
    median_diff_strike_rate = median(diff_rate)
  ) %>%
  arrange(desc(median_diff_strike_rate))
ggplot(batsmen_acceleration, aes(total_runs)) +
  geom_histogram(binwidth = 100)
batsmen_acceleration <- batsmen_acceleration %>%
  filter(total_runs >= 500)
batsmen <- factor(batsmen_acceleration$delivery_batsman)
ggplot(batsmen_acceleration,
       aes(reorder(delivery_batsman, median_diff_strike_rate),
           median_diff_strike_rate)) +
  geom_col(fill = "lightblue", colour = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Difference in strike rate between 1st and 2nd half of innings") +
  xlab("Batsman") +
  ylab("Median difference in strike rate") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15)
  ) +
  coord_flip()

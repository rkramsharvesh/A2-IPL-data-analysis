# Load necessary packages
library(readr)
library(dplyr)
library(readxl)
library(fitdistrplus)
library(janitor)
library(ggplot2)

setwd('D:\\Masters\\VCU\\Classes\\SCMA\\R\\A2')

#### 1. Import Data in R and Python ####
# Load and clean data
ball_data <- read_csv("IPL_ball_by_ball_updated till 2024.csv") %>% clean_names()
salary_data <- read_excel("IPL SALARIES 2024.xlsx", sheet = 1) %>% clean_names()

names(ball_data)

#### 2. Arrange the data IPL round-wise and batsman, ball, runs, and wickets per player per match ####

# Summarise runs, balls faced, and dismissals
ball_summary <- ball_data %>%
  group_by(season, match_id, striker) %>%
  summarise(
    balls = n(),
    runs = sum(runs_scored, na.rm = TRUE),
    wickets = sum(wicket_type %in% c("caught", "bowled", "lbw", "stumped", "hit wicket", "caught and bowled"), na.rm = TRUE),
    .groups = 'drop'
  )
head(ball_summary)

#### 3. Top 3 run-getters and top 3 wicket-takers in each IPL round ####
# Top 3 Run-Getters (striker-based)
top_runs <- ball_summary %>%
  group_by(season) %>%
  arrange(desc(runs)) %>%
  slice_head(n = 3) %>%
  ungroup()
top_runs
head(top_runs)

# Top 3 Wicket-Takers (bowler-based)
bowler_summary <- ball_data %>%
  group_by(season, match_id, bowler) %>%
  summarise(
    wickets = sum(wicket_type %in% c("caught", "bowled", "lbw", "stumped", "hit wicket", "caught and bowled"), na.rm = TRUE),
    .groups = "drop"
  )
bowler_summary

top_wickets <- bowler_summary %>%
  group_by(season, bowler) %>%
  summarise(total_wickets = sum(wickets), .groups = "drop") %>%
  arrange(season, desc(total_wickets)) %>%
  group_by(season) %>%
  slice_head(n = 3) %>%
  ungroup()
top_wickets

#### 4. Fit the most appropriate distribution for runs scored and wickets taken by the top 3 batsmen and bowlers in the last 3 IPL tournaments ####

# Get last 3 seasons
latest_seasons <- sort(unique(ball_summary$season), decreasing = TRUE)[1:3]
latest_seasons

# Filter top batsmen and bowlers
top_players <- ball_summary %>%
  filter(season %in% latest_seasons) %>%
  group_by(striker) %>%
  summarise(
    total_runs = sum(runs),
    total_wickets = sum(wickets),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_runs)) %>%
  slice(1:30)
top_players

# Fit distributions for runs
fit_norm_runs <- fitdist(top_players$total_runs, "norm")
fit_gamma_runs <- fitdist(top_players$total_runs, "gamma")
fit_lognorm_runs <- fitdist(top_players$total_runs, "lnorm")
fit_weibull_runs <- fitdist(top_players$total_runs, "weibull")

gofstat(list(fit_norm_runs, fit_gamma_runs, fit_lognorm_runs, fit_weibull_runs),
        fitnames = c("Normal", "Gamma", "Lognormal", "Weibull"))

plot(fit_norm_runs)
plot(fit_gamma_runs)
plot(fit_lognorm_runs)
plot(fit_weibull_runs)

descdist(top_players$total_runs)

# Fit distributions for wickets
fit_pois_wickets <- fitdist(top_players$total_wickets, "pois")
fit_nbinom_wickets  <- fitdist(top_players$total_wickets, "nbinom")
fit_geom_wickets    <- fitdist(top_players$total_wickets, "geom")

plot(fit_pois_wickets)
plot(fit_nbinom_wickets)
plot(fit_geom_wickets)

gofstat(list(fit_pois_wickets, fit_nbinom_wickets, fit_geom_wickets),
        fitnames = c("Poisson", "Negative Binomial", "Geometric"))

#### 5. Best Fit distribution for Player assigned - RR PANT ####

rr_pant <- ball_summary %>%
  filter(striker == "RR Pant")
head(rr_pant)

if (nrow(rr_pant) > 0) {
  # Distribution of runs
  fit_rrpant_runs <- fitdist(rr_pant$runs, "nbinom")
  plot(fit_rrpant_runs)
  
  # Distribution of wickets (if any)
  fit_rrpant_wkts <- fitdist(rr_pant$wickets, "pois")
  plot(fit_rrpant_wkts)
} else {
  print("RR Pant not found in striker column.")
}

#### 6. Relationship between player’s performance and salary (Correlation) ####

# Get the most recent season
latest_season <- max(ball_data$season, na.rm = TRUE)

# Filter for the latest season only
latest_season_data <- ball_data %>%
  filter(season == latest_season)

# Redefine ball_summary
ball_summary <- ball_data %>%
  group_by(season, match_id, striker) %>%
  summarise(
    balls = n(),
    runs = sum(runs_scored, na.rm = TRUE),
    wickets = sum(wicket_type %in% c(
      "caught", "bowled", "lbw", "stumped", "hit wicket", "caught and bowled"
    ), na.rm = TRUE),
    .groups = 'drop'
  )


# Compute average match-wise performance per player
player_avg_perf <- ball_summary %>%
  group_by(striker) %>%
  summarise(
    avg_runs = mean(runs, na.rm = TRUE),
    avg_wickets = mean(wickets, na.rm = TRUE),
    .groups = "drop"
  )
head(player_avg_perf)

# Merge with salary and clean salary column
merged <- player_avg_perf %>%
  inner_join(salary_data, by = c("striker" = "player")) %>%
  mutate(salary_numeric = as.numeric(gsub(",", "", rs))) %>%
  filter(!is.na(salary_numeric))
head(merged)

# Correlation for latest season only
cor_avg_runs <- cor(merged$avg_runs, merged$salary_numeric, use = "complete.obs")
cor_avg_wickets <- cor(merged$avg_wickets, merged$salary_numeric, use = "complete.obs")

cat("Correlation between average runs (latest season) and salary:", round(cor_avg_runs, 3), "\n")
cat("Correlation between average wickets (latest season) and salary:", round(cor_avg_wickets, 3), "\n")

#### 7. ECDF Plot to compare performances ####

# Define players of interest
selected_players <- c("RR Pant", "V Kohli", "RG Sharma", "MS Dhoni", "HH Pandya")

# Filter and select match-wise run data
ecdf_data <- ball_summary %>%
  filter(season %in% latest_seasons, striker %in% selected_players) %>%
  dplyr::select(striker, match_id, runs)

# Plot ECDF
ggplot(ecdf_data, aes(x = runs, color = striker)) +
  stat_ecdf(geom = "step", size = 1) +
  labs(
    title = "ECDF of Match-wise Runs (Last 3 IPL Seasons)",
    x = "Runs Scored in a Match",
    y = "Cumulative Probability",
    color = "Player"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#### 8. Simulating RR Pant's performance over 200 matches ####
# Assuming Negative Binomial fit is already done
nb_size <- fit_rrpant_runs$estimate["size"]
nb_mu   <- fit_rrpant_runs$estimate["mu"]

# Simulate 200 innings
set.seed(42)
sim_runs <- rnbinom(200, size = nb_size, mu = nb_mu)

# Plot histogram
hist(sim_runs,
     breaks = 20,
     main = "Simulated Match-wise Runs for RR Pant (200 Matches)",
     xlab = "Runs",
     col = "skyblue",
     border = "white")

# Summary statistics
cat("Summary of simulated runs:\n")
print(summary(sim_runs))
cat("\n")

# Key probabilities
p_30  <- mean(sim_runs >= 30)
p_50  <- mean(sim_runs >= 50)
p_100 <- mean(sim_runs >= 100)

# Output probabilities
cat("Probability of scoring 30 or more:", round(p_30 * 100, 1), "%\n")
cat("Probability of scoring 50 or more:", round(p_50 * 100, 1), "%\n")
cat("Probability of scoring 100 or more:", round(p_100 * 100, 1), "%\n")

# Quantiles
cat("\nRun Distribution Quantiles:\n")
print(quantile(sim_runs, probs = c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99)))


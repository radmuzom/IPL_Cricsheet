# Load match level data ---------------------------------------------------

cricsheet_ipl_load_meta <- function(input_file) {

  # Assign the match id based on the file name
  match_id <- str_extract(input_file, "[0-9]+")
  match_id <- parse_integer(match_id)
  writeLines(as.character(match_id))
  
  # Load the input file
  input_data <- yaml.load_file(input_file)
  
  # Metadata table
  # meta_version <- input_data$meta$data_version
  # meta_created <- ymd(input_data$meta$created)
  # meta_revision <- input_data$meta$revision
  # metadata <- tibble(
  #   id = match_id,
  #   version = meta_version,
  #   created = meta_created,
  #   revision = meta_revision
  # )
  
  # Match information table
  info <- input_data$info
  info_city <- ifelse("city" %in% names(info), info$city, NA)
  info_date <- ymd(info$dates) # Assume IPL match will be played only on a day
  info_player_of_match <- ifelse("player_of_match" %in% names(info),
                                 info$player_of_match, NA)
  info_venue <- ifelse("venue" %in% names(info), info$venue, NA)
  info_neutral_venue <- ifelse("neutral_venue" %in% names(info),
                               info$neutral_venue, 0)
  # Ignore competition, gender, overs
  match_info <- tibble(
    id = match_id,
    city = info_city,
    date = info_date,
    player_of_match = info_player_of_match,
    venue = info_venue,
    neutral_venue = info_neutral_venue
  )
  
  # Match teams table
  info_teams <- info$teams
  match_teams <- tibble(
    id = rep(match_id, 2),
    teams = info_teams
  )
  
  # Match toss table
  info_toss_winner <- info$toss$winner
  info_toss_decision <- info$toss$decision
  match_toss <- tibble(
    id = match_id,
    winner = info_toss_winner,
    decision = info_toss_decision
  )
  
  # Match umpires
  info_umpires <- info$umpires
  match_umpires <- tibble(
    id = rep(match_id, 2),
    umpires = info_umpires
  )
  
  # Match outcomes
  info_outcome <- input_data$info$outcome
  info_winner <- NA
  info_result <- NA
  info_result_margin <- NA
  info_eliminator <- NA
  if ("winner" %in% names(info_outcome)) {
    info_winner <- info_outcome$winner
    info_eliminator <- "N"
    info_result <- ifelse("runs" %in% names(info_outcome$by),
                          "runs", "wickets")
    info_result_margin <- ifelse("runs" %in% names(info_outcome$by),
                                 info_outcome$by$runs,
                                 info_outcome$by$wickets)
  } else if ("eliminator" %in% names(info_outcome)) {
    info_winner <- info_outcome$eliminator
    info_eliminator <- "Y"
    info_result <- info_outcome$result
  }
  info_method <- ifelse("method" %in% names(info_outcome),
                        info_outcome$method, NA)
  match_outcome <- tibble(
    id = match_id,
    winner = info_winner,
    result = info_result,
    result_margin = info_result_margin,
    eliminator = info_eliminator,
    method = info_method
  )

  # Return a list of tables
  retlist <- list(match_info = match_info, match_teams = match_teams,
                  match_toss = match_toss, match_umpires = match_umpires,
                  match_outcome = match_outcome)
  return(retlist)
}

# Read all the IPL data
filenames <- list.files("data", pattern = "*.yaml", full.names = TRUE)
ipl_data <- map(filenames, cricsheet_ipl_load_meta)

# Store all the data as individual data frames
table_names <- c("match_info", "match_teams", "match_toss",
                 "match_umpires", "match_outcome")
map(table_names, function(x) {
  temp <- map(ipl_data, extract2, x)
  assign(x, bind_rows(temp), pos = 1)
})

# Clean up
rm(table_names)
rm(ipl_data)
rm(filenames)

# Load ball by ball data --------------------------------------------------

process_delivery <- function(delivery) {
  delivery_name <- names(delivery)
  delivery_double <- as.double(delivery_name)
  
  delivery_over <- trunc(delivery_double)
  delivery_ball <- round((delivery_double - trunc(delivery_double)) * 10)
  
  delivery_batsman <- delivery[[delivery_name]]$batsman
  delivery_non_striker <- delivery[[delivery_name]]$non_striker
  delivery_bowler <- delivery[[delivery_name]]$bowler
  
  delivery_runs_batsman <- as.integer(delivery[[delivery_name]]$runs$batsman)
  delivery_runs_extras <- as.integer(delivery[[delivery_name]]$runs$extras)
  delivery_runs_total <- as.integer(delivery[[delivery_name]]$runs$total)
  delivery_runs_non_boundary <- ifelse("non_boundary"
                                       %in%
                                         names(delivery[[delivery_name]]$runs),
                                       1, 0)
  
  delivery_wicket <- ifelse("wicket" %in% names(delivery[[delivery_name]]),
                            1, 0)
  if (delivery_wicket == 1) {
    delivery_wicket_kind <- delivery[[delivery_name]]$wicket$kind
    delivery_wicket_player_out <- delivery[[delivery_name]]$wicket$player_out
    delivery_wicket_fielders <- ifelse("fielders"
                                       %in%
                                         names(delivery[[delivery_name]]
                                               $wicket),
                                       paste(delivery[[delivery_name]]
                                             $wicket$fielders, collapse = ","),
                                       NA)
  } else {
    delivery_wicket_kind <- NA
    delivery_wicket_player_out <- NA
    delivery_wicket_fielders <- NA
  }
  
  delivery_extras_type <- ifelse("extras" %in% names(delivery[[delivery_name]]),
                                 names(delivery[[delivery_name]]$extras)[1],
                                 NA)
  
  return(list(delivery_over = delivery_over,
              delivery_ball = delivery_ball,
              delivery_batsman = delivery_batsman,
              delivery_non_striker = delivery_non_striker,
              delivery_bowler = delivery_bowler,
              delivery_runs_batsman = delivery_runs_batsman,
              delivery_runs_extras = delivery_runs_extras,
              delivery_runs_total = delivery_runs_total,
              delivery_runs_non_boundary = delivery_runs_non_boundary,
              delivery_wicket = delivery_wicket,
              delivery_wicket_kind = delivery_wicket_kind,
              delivery_wicket_player_out = delivery_wicket_player_out,
              delivery_wicket_fielders = delivery_wicket_fielders,
              delivery_extras_type = delivery_extras_type))
}

cricsheet_ipl_load_innings <- function(input_file) {
  
  # Assign the match id based on the file name
  match_id <- str_extract(input_file, "[0-9]+")
  match_id <- parse_integer(match_id)
  writeLines(as.character(match_id))
  
  # Load the input file
  input_data <- yaml.load_file(input_file)
  
  # Innings table
  innings <- input_data$innings
  number_of_innings <- length(input_data$innings)
  # Ignore absent_hurt, penalty_runs, declared
  i1 <- innings[[1]]$`1st innings`
  i2 <- NULL
  if (number_of_innings > 1) {
    i2 <- innings[[2]]$`2nd innings`
    teams <- c(i1$team, i2$team)
  } else {
    teams <- c(i1$team, NA)
  }
  match_innings <- tibble(
    id = rep(match_id, 2),
    innings_num = as.integer(c(1, 2)),
    innings_team = teams
  )
  
  # Deliveries table
  # Ignore replacements
  i1_deliveries <- i1$deliveries
  i1_delivery_list <- map(i1_deliveries, process_delivery)
  i1_deliveries <- bind_rows(i1_delivery_list)
  temp <- tibble(id = rep(match_id, nrow(i1_deliveries)),
                          innings_num = rep(1, nrow(i1_deliveries)))
  i1_deliveries <- bind_cols(temp, i1_deliveries)
  if (number_of_innings > 1) {
    i2_deliveries <- i2$deliveries
    i2_delivery_list <- map(i2_deliveries, process_delivery)
    i2_deliveries <- bind_rows(i2_delivery_list)
    temp <- tibble(id = rep(match_id, nrow(i2_deliveries)),
                   innings_num = rep(2, nrow(i2_deliveries)))
    i2_deliveries <- bind_cols(temp, i2_deliveries)
    match_deliveries <- bind_rows(i1_deliveries, i2_deliveries)
  } else {
    match_deliveries <- i1_deliveries
  }
  
  # Return a list of tables
  retlist <- list(match_innings = match_innings,
                  match_deliveries = match_deliveries)
  return(retlist)
}

# Read all the IPL data
filenames <- list.files("data", pattern = "*.yaml", full.names = TRUE)
ipl_data <- map(filenames, cricsheet_ipl_load_innings)

# Store all the data as individual data frames
table_names <- c("match_innings", "match_deliveries")
map(table_names, function(x) {
  temp <- map(ipl_data, extract2, x)
  assign(x, bind_rows(temp), pos = 1)
})

# Clean up
rm(table_names)
rm(ipl_data)
rm(filenames)
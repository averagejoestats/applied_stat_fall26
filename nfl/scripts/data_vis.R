
# load two seasons worth of data
source("../R/funs.R")
dat <- read_and_format_raw_season_data(2024, include_playoffs = TRUE )
dat$year <- 2024
dat0 <- read_and_format_raw_season_data(2025, include_playoffs = TRUE)
dat0$year <- 2025
dat <- rbind( dat, dat0 )
dat$playoffs <- !( dat$week %in% 1:18 )

head(dat)
tail(dat)

team_stats <- data.frame( team = unique( dat$home_team ) )
team_stats$wins_2024 <- NA
team_stats$wins_2025 <- NA
team_stats$playoffs_2024 <- NA
team_stats$playoffs_2025 <- NA


for(j in 1:nrow(team_stats)){
    tt <- team_stats$team[j]
    ihome <- dat$home_team == tt & dat$week %in% 1:18
    iaway <- dat$away_team == tt & dat$week %in% 1:18
    i2024 <- dat$year == 2024
    i2025 <- dat$year == 2025
    x <- dat$outcome
    team_stats$wins_2024[j] <-
        sum( x[ ihome & i2024 ]) + sum(1 - x[ iaway & i2024 ] )
    team_stats$wins_2025[j] <-
        sum( x[ ihome & i2025 ]) + sum(1 - x[ iaway & i2025 ] )
    team_stats$playoffs_2024[j] <- any(
        dat$playoffs & i2024 & (dat$home_team == tt | dat$away_team == tt )
    )
    team_stats$playoffs_2025[j] <- any(
        dat$playoffs & i2025 & (dat$home_team == tt | dat$away_team == tt )
    )
}

# create an abbreviation
team_stats$abbr <- c(
    "KC","PHL","ATL","BUF","NO","CHI","CIN","IND","MIA","NYG","SEA","LAC",
    "CLE","TAM","DET","SF","CAR","JAX","GB","DAL","MN","NE","WAS","TN",
    "BAL","AZ","DEN","HOU","NYJ","PIT","LV","LAR"
)

    
# first try
plot(team_stats$wins_2024, team_stats$wins_2025,
     xlim = c(0,17), ylim = c(0,17),
     xlab = "2024 wins", ylab = "2025 wins"
)


# Add teams
plot(team_stats$wins_2024, team_stats$wins_2025, type = "n",
     xlim = c(0,17), ylim = c(0,17),
     xlab = "2024 wins", ylab = "2025 wins"
)
text( team_stats$wins_2024, team_stats$wins_2025, team_stats$abbr )

# add offsets
team_stats$offset <- 0
team_stats$offset[8] <- 0.15
team_stats$offset[3] <- -0.15
team_stats$offset[28] <- 0.15
team_stats$offset[32] <- -0.15
team_stats$offset[11] <- 0.15
team_stats$offset[27] <- -0.15
plot(team_stats$wins_2024, team_stats$wins_2025, type = "n",
     xlim = c(0,17), ylim = c(0,17),
     xlab = "2024 wins", ylab = "2025 wins"
)
abline(0,1, col = "lightgray", lwd = 1)
text( team_stats$wins_2024, team_stats$wins_2025 + team_stats$offset,
     team_stats$abbr
)

    
# set the axes yourself and add gridlines
team_stats$offset <- 0
team_stats$offset[8] <- 0.15
team_stats$offset[3] <- -0.15
team_stats$offset[28] <- 0.15
team_stats$offset[32] <- -0.15
team_stats$offset[11] <- 0.15
team_stats$offset[27] <- -0.15
plot(team_stats$wins_2024, team_stats$wins_2025, type = "n",
     xlim = c(0,17), ylim = c(0,17),
     xlab = "2024 wins", ylab = "2025 wins", axes = FALSE
)
for(j in 0:17){
    abline(v=j, col = "lightgray")
    abline(h=j, col = "lightgray")
}
abline(0,1, col = "lightgray", lwd = 1)
text( team_stats$wins_2024, team_stats$wins_2025 + team_stats$offset, team_stats$abbr )
axis(1, at = 1:17)
axis(2, at = 1:17)
box()


# Add decoration for playoffs
team_stats$offset <- 0
team_stats$offset[8] <- 0.15
team_stats$offset[3] <- -0.15
team_stats$offset[28] <- 0.15
team_stats$offset[32] <- -0.15
team_stats$offset[11] <- 0.15
team_stats$offset[27] <- -0.15
plot(team_stats$wins_2024, team_stats$wins_2025, type = "n", xlim = c(0,17), ylim = c(0,17),
     xlab = "2024 wins", ylab = "2025 wins", axes = FALSE
)
for(j in 0:17){
    abline(v=j, col = "lightgray")
    abline(h=j, col = "lightgray")
}
abline(0,1, col = "lightgray", lwd = 1)
colors <- rep("black",nrow(team_stats))
colors[ team_stats$playoffs_2024 & !team_stats$playoffs_2025 ] <- "blue"
colors[ !team_stats$playoffs_2024 & team_stats$playoffs_2025 ] <- "red"
colors[ team_stats$playoffs_2024 & team_stats$playoffs_2025 ] <- "purple"
text( team_stats$wins_2024, team_stats$wins_2025 + team_stats$offset,
     team_stats$abbr, col = colors, font = 2)
axis(1, at = 0:17)
axis(2, at = 0:17)
box()
text( 1, 17, "Playoffs 2024", col = "blue", font = 2 )
text( 1, 16, "Playoffs 2025", col = "red", font = 2 )
text( 1, 15, "Playoffs 2024 & 2025", col = "purple", font = 2 )

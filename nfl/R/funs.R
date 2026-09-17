
format_raw_season_data <- function( dat, include_playoffs = FALSE ){
    
    # Remove the "Playoffs" row
    if( !is.null( dat$Date ) ){
        dat <- dat[ !dat$Date == "Playoffs", ]
    }

    # remove the playoff games rows
    if( include_playoffs ){
        dat <- dat[dat$Week %in% c(1:18,"WildCard","Division","ConfChamp","SuperBowl"), ]
    } else {
        dat <- dat[dat$Week %in% c(1:18), ]
    }
    
    # put the teams in a more useful format, record outcomes
    dat$outcome <- NA
    dat$home_points <- NA
    dat$away_points <- NA
    dat$home_yards <- NA
    dat$away_yards <- NA

    if( is.null( dat$Winner.tie ) ){ # if no winner (current season)
        dat$home_team <- dat$HomeTm
        dat$away_team <- dat$VisTm
        dat$Date <- dat$X
    } else { # otherwise (past season), then fill in information
        
        dat$home_team <- NA
        dat$away_team <- NA
        
        for(j in 1:nrow(dat)){
            if( dat$X[j] == "@" ){
                # Away Team Wins
                dat$home_team[j] <- dat$Loser.tie[j]
                dat$home_points[j] <- dat$Pts.1[j]
                dat$home_yards[j] <- dat$YdsL[j]
                dat$away_team[j] <- dat$Winner.tie[j]
                dat$away_points[j] <- dat$Pts[j]
                dat$away_yards[j] <- dat$YdsW[j]
                dat$outcome[j] <- 0
            } else {
                # Home Team Wins
                dat$home_team[j] <- dat$Winner.tie[j]
                dat$home_points[j] <- dat$Pts[j]
                dat$home_yards[j] <- dat$YdsW[j]
                dat$away_team[j] <- dat$Loser.tie[j]
                dat$away_points[j] <- dat$Pts.1[j]
                dat$away_yards[j] <- dat$YdsL[j]
                dat$outcome[j] <- 1
            }
        } 
    }
    
    vars <- c("Week","Day","Date","Time","home_team","away_team","home_points",
              "away_points","home_yards","away_yards","outcome")

    dat <- dat[ vars ]
    colnames(dat) <- tolower(colnames(dat))

    # fix the team names
    dat$home_team[dat$home_team == "St. Louis Rams"] <- "Los Angeles Rams"
    dat$away_team[dat$away_team == "St. Louis Rams"] <- "Los Angeles Rams"
    dat$home_team[dat$home_team == "Oakland Raiders"] <- "Las Vegas Raiders"
    dat$away_team[dat$away_team == "Oakland Raiders"] <- "Las Vegas Raiders"
    dat$home_team[dat$home_team == "Washington Redskins"] <- "Washington Commanders"
    dat$away_team[dat$away_team == "Washington Redskins"] <- "Washington Commanders"
    dat$home_team[dat$home_team == "Washington Football Team"] <- "Washington Commanders"
    dat$away_team[dat$away_team == "Washington Football Team"] <- "Washington Commanders"
    dat$home_team[dat$home_team == "San Diego Chargers"] <- "Los Angeles Chargers"
    dat$away_team[dat$away_team == "San Diego Chargers"] <- "Los Angeles Chargers"

    return( dat )
}

read_raw_season_data <- function( year, path = "../raw_data" ){
    dat <- read.csv( file.path( path, paste0("season",year,".csv") ), skip = 4 )
    return( dat )
}

read_and_format_raw_season_data <- function(
    year, path = "../raw_data", include_playoffs = FALSE
){
    dat <- read_raw_season_data( year, path )
    dat <- format_raw_season_data( dat, include_playoffs )
    return(dat)
}

fit_bt01 <- function( dat, weights = rep(1,nrow(dat)) ){

    # code the teams as a factor (not really needed for this script)
    teams <- sort( unique( dat$home_team ) )
    nteams <- length( teams )
    
    # construct the design matrix
    X0 <- matrix( 0, nrow(dat), nteams )
    colnames(X0) <- teams
    
    for(j in 1:nrow(dat)){
        X0[j, dat$home_team[j] ] <- 1
        X0[j, dat$away_team[j] ] <- -1
    }
    
    # drop the first column and define outcome
    X1 <- X0[ , 2:ncol(X0) ]
    y <- dat$outcome
    
    # fit the model 
    m1 <- glm( cbind( y, 1 - y ) ~ X1, family = "binomial", weights = weights )

    return(m1)
}


fit_bt02 <- function( dat, weights = rep(1,nrow(dat)) ){

    # add a team-specific home field advantage

    # 
    teams <- sort( unique( dat$home_team ) )
    nteams <- length( teams )
    
    # construct the design matrix
    X0 <- matrix( 0, nrow(dat), nteams + nteams )
    colnames(X0) <- c( teams, paste0("home_", teams) )
    
    for(j in 1:nrow(dat)){
        X0[j, dat$home_team[j] ] <- 1
        X0[j, dat$away_team[j] ] <- -1
        X0[j, paste0("home_", dat$home_team[j]) ] <- 1
    }
    
    # drop the first column and define outcome
    X1 <- X0[ , -1 ]
    y <- dat$outcome
   
    # fit the model 
    m1 <- glm( cbind( y, 1 - y ) ~ X1, family = "binomial", weights = weights )

    return(m1)
}

fit_bt03 <- function( dat, weights = rep(1,nrow(dat)) ){

    # add a team-specific home field advantage
    # allow teams from different years to be different teams

    dat$home_team_year <- paste( dat$home_team, dat$year )
    dat$away_team_year <- paste( dat$away_team, dat$year )

    # 
    teams <- sort( unique( dat$home_team ) )
    teamyears <- sort( unique( dat$home_team_year ) )
    nteams <- length( teams )
    nteamyears <- length( teamyears )
    
    # construct the design matrix
    X0 <- matrix( 0, nrow(dat), nteamyears + nteams )
    colnames(X0) <- c( teamyears, paste("home", teams) )
    
    for(j in 1:nrow(dat)){
        X0[j, dat$home_team_year[j] ] <- 1
        X0[j, dat$away_team_year[j] ] <- -1
        X0[j, paste("home", dat$home_team[j]) ] <- 1
    }
    
    # drop the arizona cardinals column
    X1 <- X0[ , -grep("Arizona",colnames(X0)) ]
    y <- dat$outcome
   
    # fit the model 
    m1 <- glm( cbind( y, 1 - y ) ~ X1, family = "binomial", weights = weights )

    return(m1)
}

fit_bt04 <- function( dat, weights = rep(1,nrow(dat)) ){

    # add a team-specific home field advantage
    # allow teams from different years to be different teams

    dat$home_team_year <- paste( dat$home_team, dat$year )
    dat$away_team_year <- paste( dat$away_team, dat$year )

    # 
    teams <- sort( unique( dat$home_team ) )
    teamyears <- sort( unique( dat$home_team_year ) )
    nteams <- length( teams )
    nteamyears <- length( teamyears )
    
    # construct the design matrix
    X0 <- matrix( 0, nrow(dat), nteamyears + 2*nteams )
    colnames(X0) <- c( teamyears, paste("home", teams), paste("week",teams) )
    
    for(j in 1:nrow(dat)){
        X0[j, dat$home_team_year[j] ] <- 1
        X0[j, dat$away_team_year[j] ] <- -1
        X0[j, paste("home", dat$home_team[j]) ] <- 1
        X0[j, paste("week", dat$home_team[j]) ] <- as.numeric( dat$week[j] )
    }
    
    # drop the arizona cardinals column
    X1 <- cbind( rep(1,nrow(X0)), as.numeric(dat$week), X0[ , -grep("Arizona",colnames(X0)) ] )
    y <- dat$outcome
   
    # fit the model 
    m1 <- glm( cbind( y, 1 - y ) ~ X1, family = "binomial", weights = weights )

    return(m1)
}

fit_bt05 <- function( dat, weights = rep(1,nrow(dat)) ){

    # linear change in strength over week
    dat$week <- as.numeric(dat$week)
    
    # code the teams as a factor (not really needed for this script)
    teams <- sort( unique( dat$home_team ) )
    nteams <- length( teams )
    
    # construct the design matrix
    X0 <- matrix( 0, nrow(dat), nteams + nteams )
    colnames(X0) <- c(teams, paste0("slope",teams) )
    
    for(j in 1:nrow(dat)){
        X0[j, dat$home_team[j] ] <- 1
        X0[j, dat$away_team[j] ] <- -1
        X0[j, paste0("slope",dat$home_team[j]) ] <- dat$week[j]
        X0[j, paste0("slope",dat$away_team[j]) ] <- -dat$week[j]
    }
    
    # drop the first column and define outcome
    X1 <- X0[ , -c(1,nteams+1) ]
    y <- dat$outcome
    
    # fit the model 
    m1 <- glm( cbind( y, 1 - y ) ~ X1, family = "binomial", weights = weights )

    return(m1)
}


predict_bt01 <- function( dat, mod, shrink ){

    # code the teams as a factor (not really needed for this script)
    teams <- sort( unique( dat$home_team ) )
    nteams <- length( teams )
    
    # construct the design matrix
    X0 <- matrix( 0, nrow(dat), nteams )
    colnames(X0) <- teams
    
    for(j in 1:nrow(dat)){
        X0[j, dat$home_team[j] ] <- 1
        X0[j, dat$away_team[j] ] <- -1
    }
    
    # drop the first column and define outcome
    X1 <- cbind( rep(1,nrow(X0)), X0[ , 2:ncol(X0) ] )
    colnames(X1)[1] <- "(Intercept)"
    
    # define the coefficients AND SHRINK THEM
    cfs <- mod$coefficients * shrink
    names(cfs) <- gsub("X1","",names(cfs))

    # check that names match
    if( !all.equal( colnames(X1), names(cfs) ) ){
        stop("design matrix and parameter names differ")
    }

    # calculate the probs
    logitprobs <- as.vector( X1 %*% cfs )
    probs <- exp( logitprobs )/( 1 + exp(logitprobs) )
    
    # add to dat
    dat$home_probs <- probs

    return(dat)
}


score_probs <- function( dat ){
    ii <- dat$outcome == 1 
    log_score <- sum(log(dat$home_probs[ii])) + sum(log((1-dat$home_probs)[!ii])) 
    return( log_score )
}


# calculate some statistics
point_differential <- function( dat ){
    teams <- sort( unique( dat$home_team ) )
    ptdiff <- rep(NA, length(teams))
    names(ptdiff) <- teams 

    for(j in 1:length(teams)){
        ihome <- dat$home_team == teams[j]
        iaway <- dat$away_team == teams[j]
        ptdiff[j] <- 
            sum( dat$home_points[ihome] - dat$away_points[ihome] )  +
            sum( dat$away_points[iaway] - dat$home_points[iaway] )
    }
    return(ptdiff)
}
        

# margin of victory model
fit_mov01 <- function( dat, weights = rep(1,nrow(dat)) ){

    # code the teams as a factor (not really needed for this script)
    teams <- sort( unique( dat$home_team ) )
    nteams <- length( teams )
    
    # construct the design matrix
    X0 <- matrix( 0, nrow(dat), nteams )
    colnames(X0) <- teams
    
    for(j in 1:nrow(dat)){
        X0[j, dat$home_team[j] ] <- 1
        X0[j, dat$away_team[j] ] <- -1
    }
    
    # drop the first column and define outcome
    X1 <- X0[ , 2:ncol(X0) ]
    y <- dat$home_points - dat$away_points
    
    # fit the model 
    m1 <- lm( y ~ X1, weights = weights )

    return(m1)
}


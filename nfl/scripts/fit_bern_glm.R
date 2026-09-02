
# read in the 2025 data
dat <- read.csv("../raw_data/season2025.csv", skip = 4)

# ignore the playoffs
dat <- dat[1:272,]

# put the teams in a more useful format, record outcomes
dat$home_team <- NA
dat$away_team <- NA
dat$outcome <- NA

for(j in 1:nrow(dat)){
    if( dat$X[j] == "@" ){
        dat$home_team[j] <- dat$Loser.tie[j]
        dat$away_team[j] <- dat$Winner.tie[j]
        dat$outcome[j] <- 1
    } else {
        dat$home_team[j] <- dat$Winner.tie[j]
        dat$away_team[j] <- dat$Loser.tie[j]
        dat$outcome[j] <- 0
    }
} 

# code the teams as a factor (not really needed for this script)
dat$home_team <- as.factor( dat$home_team )
dat$away_team <- as.factor( dat$away_team )
levels( dat$home_team )
all.equal( levels( dat$home_team ), levels( dat$away_team ) )
teams <- levels( dat$home_team )
nteams <- length( teams )

# construct the design matrix
X0 <- matrix( 0, nrow(dat), nteams )
colnames(X0) <- teams

for(j in 1:nrow(dat)){
    X0[j, dat$away_team[j] ] <- 1
    X0[j, dat$home_team[j] ] <- -1
}

# double check
dat[1,]
X0[1,,drop=FALSE]

# note that design matrix is singular
svd(X0)$d

# drop the first column
X1 <- X0[ , 2:ncol(X0) ]
svd(X1)$d

# define outcome ( to make code below shorter)
y <- dat$outcome

# fit the model and summarize
m1 <- glm( cbind( y, 1 - y ) ~ X1, family = "binomial" )
summary(m1)



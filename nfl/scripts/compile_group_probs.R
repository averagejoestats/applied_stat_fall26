
groups <- paste0("group",1:9)

# first check that their probs sum to 1
for(j in seq_along(groups)){
    fname <- paste0("../../../nfl_probs/", groups[j], ".csv")
    dat <- read.csv(fname)
    print( groups[j] )
    print( range( rowSums( dat[ c("away_prob","home_prob") ] ) ) )
}

# read in the template
dat <- read.csv("../templates/game_prob_template_2026.csv")
dat$home_prob <- NULL
dat$away_prob <- NULL

# put them all in one dataset
for(j in seq_along(groups)){
    fname <- paste0("../../../nfl_probs/", groups[j], ".csv")
    this <- read.csv(fname)
    dat[ groups[j] ] <- round( this$home_prob, 8 )
}

# save the file as a csv
write.csv( dat, file = "../group_probs.csv", row.names = FALSE, quote = FALSE )

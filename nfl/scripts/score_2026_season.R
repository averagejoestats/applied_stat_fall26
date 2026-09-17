
source("../R/funs.R")
# load the season results and parse them
results <- read_and_format_raw_season_data( 2026 )

# make sure outcome is NA when there is no data
results$outcome[ is.na( results$home_points) ] <- NA

#write.csv( results, file = "../raw_data/results2026.csv", row.names = FALSE, quote = FALSE)
# manually input some results
#results <- read.csv("../raw_data/results2026.csv")

# load the groups' probabilities
groups <- read.csv("../group_probs.csv")
colnames(groups)[c(1,3:4)] <- c("week","away_team","home_team")

# put the outcomes in the groups dataset
join_vars <- c("week","home_team","away_team")
groups <- dplyr::left_join( groups, results[ c(join_vars,"outcome") ], by = join_vars )

# calculate the scores
ngroups <- 9
scores <- rep(NA, ngroups)
names( scores ) <- paste0("group",1:ngroups)

home_win <- which( groups$outcome == 1 )
away_win <- which( groups$outcome == 0 )
for(j in 1:ngroups){
    probs <- groups[[paste0("group",j)]]
    scores[j] <- sum( log( probs[home_win] ) ) + sum( log( 1 - probs[away_win] ) )
}

# print the scores
x <- round( scores, 4 )
data.frame( score = x, diff = x - max(x), avg = x/( length(home_win) + length(away_win) ) )

# add score columns to the dataset
for(j in 1:ngroups){ groups[[ paste0("score",j) ]] <- NA }

# calculate the scores
for(j in 1:ngroups){
    probs <- groups[[paste0("group",j)]]
    groups[[ paste0("score",j) ]][ home_win ] <- log( probs[home_win] )
    groups[[ paste0("score",j) ]][ away_win ] <- log( 1 - probs[away_win] )
}

# plot the scores
cols <- c("black","red","blue","magenta","cyan","darkgreen","goldenrod","brown","gray")
not_na <- which( !is.na( groups$outcome ) )
plot( not_na, rep(0,length(not_na)), type = "n", ylim = c(-2,0) )
for(j in 1:9){
    lines( not_na, groups[[ paste0("score",j) ]][ not_na ], type = "o", col = cols[j] )
}
legend("bottomleft", legend = 1:9, pch = 1, col = cols )

    

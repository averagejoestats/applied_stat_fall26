
dat <- read.csv("../raw_data/season2026.csv")

dat$tie_prob <- NA
dat$home_prob <- NA
dat$away_prob <- NA

dat[,7:10]

colnames(dat)[3] <- "date"

vars <- c("Week","date","VisTm","HomeTm","Time","tie_prob","home_prob","away_prob")

head( dat[ vars ] )

fname <- "../templates/game_prob_template_2026.csv"
write.csv( dat[ vars ], row.names = FALSE, quote = FALSE, file = fname )

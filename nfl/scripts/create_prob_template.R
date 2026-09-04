
dat <- read.csv("../raw_data/season2026.csv")

dat$home_prob <- NA
dat$away_prob <- NA

dat[,7:10]

colnames(dat)[3] <- "date"

vars <- c("Week","date","HomeTm","VisTm","Time","home_prob","away_prob")
dat <- dat[ vars ]

colnames(dat)[3] <- "home_team"
colnames(dat)[4] <- "away_team"
head( dat )

fname <- "../templates/game_prob_template_2026.csv"
write.csv( dat, row.names = FALSE, quote = FALSE, file = fname )

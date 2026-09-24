
source("../R/funs.R")

# read data and subset to northbound direction
dat <- read.csv("../data/bus_stop_data_v1.csv")
direction <- 7
dat <- dat[ dat$route_direction_id == direction, ]

# calculate the stop time and get all of the stops in order
dat$stop_time <- dat$departure_time - dat$arrival_time
stops <- get_stops_df( dat, direction )

# let's just look at the first six stops
ns <- 6
dat1 <- dat[ dat$geo_node_name %in% stops$name[1:ns], ]
size <- 2000
ii <- sample( 1:nrow(dat1), ns*size )
dat1 <- dat1[ii,]

# boxplot - horrible
plot( as.factor(dat1$geo_node_name), dat1$stop_time )

# transformation?
plot( as.factor(dat1$geo_node_name), sqrt(dat1$stop_time) )

# try a jitter plot
plot(
    NA,
    xlim = c(0,ns+1),
    ylim = sqrt(c(0,max(dat1$stop_time,na.rm = TRUE))),
    type = "n",
    xlab = "Stop", ylab = "Stop Time (sec)", axes = FALSE
)
dat1$jitter <- rnorm(nrow(dat1))
for(j in 1:ns){
    st <- stops$name[j]
    ii <- which( dat1$geo_node_name == st )
    points( j + 0.1*dat1$jitter[ii], sqrt( dat1$stop_time[ii] ), cex = 0.5 )
}
axis( 1, at = 1:ns, labels = stops$name[1:6] )
x <- c(0,10,100,400,900,1600)
axis( 2, at = sqrt(x), labels = x )
box()

# try a different transformation
zpos <- 1
plot(
    NA,
    xlim = c(0,ns+1),
    ylim = c(zpos, log( max(dat$stop_time,na.rm = TRUE)) ),
    type = "n",
    xlab = "Stop", ylab = "Stop Time (sec)", axes = FALSE
)
dat1$jitter <- rnorm(nrow(dat1))
for(j in 1:ns){
    st <- stops$name[j]
    ii <- which( dat1$geo_node_name == st )
    y <- log( dat1$stop_time[ii] )
    y[ y == -Inf ] <- zpos
    points( j + 0.1*dat1$jitter[ii], y, cex = 0.5 )
}
axis( 1, at = 1:ns, labels = stops$name[1:6] )
x <- c(10,100,1000,2000)
axis( 2, at = log(x), labels = x, lwd = 2 )
axis( 2, at = zpos, labels = 0, lwd = 2 )
box(col = "lightgray")


# Fix the names
par( mar = c(8,4,4,1) )
plot(
    NA,
    xlim = c(0,ns+1),
    ylim = sqrt(c(0,max(dat1$stop_time,na.rm = TRUE))),
    type = "n",
    xlab = "", ylab = "Stop Time (sec)", axes = FALSE
)
dat1$jitter <- rnorm(nrow(dat1))
for(j in 1:ns){
    st <- stops$name[j]
    ii <- which( dat1$geo_node_name == st )
    points( j + 0.1*dat1$jitter[ii], sqrt( dat1$stop_time[ii] ), cex = 0.5 )
}
axis( 1, at = 1:ns, labels = FALSE )
x <- c(0,10,100,400,900,1600)
axis( 2, at = sqrt(x), labels = x )
box()
text(
    x = 1:ns,
    y = -4,
    labels = gsub("GRAND @ ", "", stops$name[1:ns] ),
    srt = 30,     # rotation angle in degrees
    adj = 1,      # right-align so each label ends at its tick
    xpd = TRUE,   # allow drawing outside the plot region
    cex = 0.8
)




# plot stops 2 through 7 instead
ns <- 6
dat1 <- dat[ dat$geo_node_name %in% stops$name[2:(ns+1)], ]
size <- 2000
ii <- sample( 1:nrow(dat1), ns*size )
dat1 <- dat1[ii,]

# Fix the names
par( mar = c(8,4,4,1) )
plot(
    NA,
    xlim = c(0.5,ns+0.5),
    ylim = sqrt(c(0,max(dat1$stop_time,na.rm = TRUE))),
    type = "n",
    xlab = "", ylab = "Stop Time (sec)", axes = FALSE
)
dat1$jitter <- rnorm(nrow(dat1))
for(j in 1:ns){
    st <- stops$name[j+1]
    ii <- which( dat1$geo_node_name == st )
    points( j + 0.1*dat1$jitter[ii], sqrt( dat1$stop_time[ii] ), cex = 0.5 )
}
axis( 1, at = 1:ns, labels = FALSE )
x <- c(0,10,40,100,200,400,800,1600)
axis( 2, at = sqrt(x), labels = x )
box()
text(
    x = 1:ns,
    y = -2,
    labels = gsub("GRAND @ ", "", stops$name[2:(ns+1)] ),
    srt = 30,     # rotation angle in degrees
    adj = 1,      # right-align so each label ends at its tick
    xpd = TRUE,   # allow drawing outside the plot region
    cex = 0.8
)






# spacing for multiple plots
dev.new()
par(mfrow=c(2,2))
plot( dat1$boardings, dat1$alightings, xlab = "Boardings",
     ylab = "Alightings", main = "Alightings vs. Boardings" )
plot( dat1$boardings, dat1$stop_time, xlab = "Boardings",
     ylab = "Stop Time", main = "Stop Time vs. Boardings" )
plot( dat1$alightings, dat1$stop_time, xlab = "Alightings",
     ylab = "Stop Time", main = "Stop Time vs. Alightings" )
plot( dat1$boardings, dat1$adherence, xlab = "Boardings",
     ylab = "Adherence", main = "Stop Time vs. Adherence" )

# set margins manually, and move titles down
dev.new()
par(mfrow=c(2,2), mar = c(5,4,2,1) )
plot( dat1$boardings, dat1$alightings, xlab = "Boardings",
     ylab = "Alightings")
title(line = 0.5, main = "Alightings vs. Boardings" )
plot( dat1$boardings, dat1$stop_time, xlab = "Boardings",
     ylab = "Stop Time")
title(line = 0.5, main = "Stop Time vs. Boardings" )
plot( dat1$alightings, dat1$stop_time, xlab = "Alightings",
     ylab = "Stop Time")
title(line = 0.5, main = "Stop Time vs. Alightings" )
plot( dat1$boardings, dat1$adherence, xlab = "Boardings",
     ylab = "Adherence")
title(line = 0.5, main = "Stop Time vs. Adherence" )

# set margins manually, and move x labels up a bit
dev.new()
par(mfrow=c(2,2), mar = c(4,4,2,1) )
plot( dat1$boardings, dat1$alightings, ylab = "Alightings", xlab = "")
title(line = 0.5, main = "Alightings vs. Boardings", xlab = "" )
title(line = 2.2, xlab = "Boardings" )
plot( dat1$boardings, dat1$stop_time, ylab = "Stop Time", xlab = "")
title(line = 0.5, main = "Stop Time vs. Boardings" )
title(line = 2.2, xlab = "Boardings" )
plot( dat1$alightings, dat1$stop_time, ylab = "Stop Time", xlab = "")
title(line = 0.5, main = "Stop Time vs. Alightings" )
title(line = 2.2, xlab = "Alightings" )
plot( dat1$boardings, dat1$adherence, ylab = "Adherence", xlab = "")
title(line = 0.5, main = "Stop Time vs. Adherence" )
title(line = 2.2, xlab = "Adherence" )




plot( dat$stop_time, dat$boardings + dat$alightings )
plot( sqrt( dat$stop_time ), dat$boardings + dat$alightings )
plot( sqrt( dat$stop_time ), sqrt(dat$boardings + dat$alightings) )

plot( sqrt( dat$stop_time ), sqrt(dat$boardings + dat$alightings), pch = 16, cex = 0.5 )

dat$jitter <- rnorm(nrow(dat))

plot( sqrt( dat$stop_time ), 0.05*dat$jitter + sqrt(dat$boardings + dat$alightings), pch = 16, cex = 0.5 )


ii <- sample( 1:nrow(dat), 200000 )
plot( sqrt( dat$stop_time[ii] ), 0.05*dat$jitter[ii] + sqrt(dat$boardings[ii] + dat$alightings[ii]), pch = 16, cex = 0.3 )

ii <- sample( 1:nrow(dat), 200000 )
plot( 0.05*dat$jitter[ii] + sqrt(dat$boardings[ii] + dat$alightings[ii]), sqrt(dat$stop_time[ii]), pch = 16, cex = 0.3 )

ii <- sample( 1:nrow(dat), 200000 )
plot( 0.03*dat$jitter[ii] + sqrt(dat$boardings[ii] + dat$alightings[ii]), sqrt(dat$stop_time[ii]), pch = 16, cex = 0.3, xlim = c(0,8) )

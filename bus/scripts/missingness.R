
#dat <- read.csv("../sample_transit_data_old/sample_bus_stop_data.csv")
#mean( is.na( dat$ARRIVAL_TIME ) )

dat <- read.csv("sample_transit_data/sample_bus_stop_data.csv")
mean( is.na( dat$ACTUAL_ARRIVAL_TIME ) )
head(dat)

# lowercase the column names
colnames(dat) <- tolower( colnames(dat) )
head(dat)

# code the date column as a date
dat$date <- as.Date( dat$date )

# ------
# Figure out the sequence of stops on the northbound and southbound routes
# ------
get_stop_sequence <- function( dat, direction_id ){

    # get a unique identifier for trips
    dat$my_trip_id <- paste( dat$calendar_id, dat$trip_id )

    # get the unique stops and trips
    ii <- dat$route_direction_id == direction_id
    stops <- unique( dat$geo_node_name[ ii ] )
    trips <- unique( dat$my_trip_id[ ii ] )

    # store the time differences in a data frame
    time_diffs <- data.frame( stop = stops, tot_diff = 0 )
    blank_df <- time_diffs

    # loop over trips
    #for(j in 1:length(trips)){
    for(j in 1:100){

        # get the arrival times for this trip
        ii <- dat$my_trip_id == trips[j]
        this_df <- data.frame( stop = dat$geo_node_name[ii], arrival = dat$actual_arrival_time[ii] )
        this_df <- dplyr::left_join( blank_df, this_df, by = "stop" )            

        # if all times are recorded, and number of stops matches, record the time differences
        if( all( !is.na( this_df$arrival ) ) && nrow(blank_df) == nrow(this_df) ){
            time_diffs$tot_diff <- time_diffs$tot_diff + ( this_df$arrival - this_df$arrival[1] )
            print(j)
        }
    }

    # order them by the total differences
    ord <- order( time_diffs$tot_diff )
    return( time_diffs$stop[ord] )

}

nb_stops <- get_stop_sequence(dat, 7 )
sb_stops <- get_stop_sequence(dat, 9 )


# visually check that they make sense
par( mfrow = c(1,2) )
dd <- data.frame( stop = nb_stops, lon = NA, lat = NA )
for(j in 1:nrow(dd)){
    ii <- dat$geo_node_name == dd$stop[j]
    dd$lon[j] <- dat$longitude[ii][1]/10^7
    dd$lat[j] <- dat$latitude[ii][1]/10^7
}
plot( dd$lon, dd$lat, type = "l" )
text( dd$lon, dd$lat, 1:nrow(dd) )
dd <- data.frame( stop = sb_stops, lon = NA, lat = NA )
for(j in 1:nrow(dd)){
    ii <- dat$geo_node_name == dd$stop[j]
    dd$lon[j] <- dat$longitude[ii][1]/10^7
    dd$lat[j] <- dat$latitude[ii][1]/10^7
}
plot( dd$lon, dd$lat, type = "l" )
text( dd$lon, dd$lat, 1:nrow(dd) )

# create a unique trip identifier by combining the calendar_id and the trip_id
dat$my_trip_id <- paste( dat$calendar_id, dat$trip_id )

# write a function to select a random trip, and plot the missingness pattern for the
# arrival and departure times for that trip. Use three different colors for the
# missingness: if all times are recorded, use gree; if arrival is missing but not
# departure, use blue, and if departure is misssing but not arrival, use yellow,
# and if both are missing, use red.
# they should be plotted in the order of the stops on the route,
# which can be determined by the stop_sequence argument.
# The function should take as input the data frame, the direction_id, and the stop_sequence, and
# should return a plot of the missingness pattern for a random trip in that direction.
plot_missingness <- function( dat, direction_id, stop_sequence, my_trip_id = NULL ){

    # get the data for this direction
    ii <- dat$route_direction_id == direction_id
    dat <- dat[ii,]

    # generate a random trip id
    if( is.null(my_trip_id) ){
        trip_id <- sample( unique(dat$my_trip_id), 1 )
    } else {
        trip_id <- my_trip_id
    }

    # get the data for this trip
    ii <- dat$my_trip_id == trip_id
    this_df <- dat[ii,]

    # order the data frame by the stop sequence
    this_df$stop <- factor( this_df$geo_node_name, levels = stop_sequence )
    this_df <- this_df[ order( this_df$stop ), ]

    # create a color vector for the missingness
    col_vec <- rep( "green", nrow(this_df) )
    col_vec[ is.na(this_df$actual_arrival_time) & !is.na(this_df$actual_departure_time) ] <- "blue"
    col_vec[ !is.na(this_df$actual_arrival_time) & is.na(this_df$actual_departure_time) ] <- "yellow"
    col_vec[ is.na(this_df$actual_arrival_time) & is.na(this_df$actual_departure_time) ] <- "red"

    # plot the missingness pattern, using lon and lat as the coordinates
    plot(
        this_df$longitude/10^7, this_df$latitude/10^7, col = col_vec, pch = 19, cex = 2,
        xlab = "Longitude", ylab = "Latitude", main = paste("Missingness pattern for trip", trip_id)
    )
    legend(
        "topleft",
        legend = c("All recorded", "Arrival missing", "Departure missing", "Both missing"),
        col = c("green", "blue", "yellow", "red"), pch = 19
    )
    # add the 'property_tag' in one place on the plot
    text(
        this_df$longitude[1]/10^7, this_df$latitude[1]/10^7, labels = this_df$property_tag[1], pos = 4
    )
}

# write a loop to plot the missingness pattern for 100 random trips in the northbound direction 
for(i in 1:100){
    plot_missingness(dat, 7, nb_stops )
    Sys.sleep(1)
}


# calculate the proportion missing from each stop in the northbound direction
missingness_nb <- data.frame( stop = nb_stops, prop_missing_arrival = NA, prop_missing_departure = NA )
for(j in 1:nrow(missingness_nb)){
    ii <- dat$geo_node_name == missingness_nb$stop[j] & dat$route_direction_id == 7
    missingness_nb$prop_missing_arrival[j] <- mean( is.na( dat$actual_arrival_time[ii] ) )
    missingness_nb$prop_missing_departure[j] <- mean( is.na( dat$actual_departure_time[ii] ) )
}


# create a new data frame that does the following:
# it should copy the existing dataframe, and then insert additional missing values
# for departure time, arrival time, and adherence by random selecting 10% of the trips
# that have no missing values, and then random selecting a trip that does have missing values,
# and then match the missingness pattern of the trip with the missing values

# create the new dataset and figure out which trips have missing values and which do not
# make two data frames, one for the trips with no missing values, and one for the trips
# with missing values. There should be a column for the my_trip_id, and a column
# for the direction_id. 
new_dat <- dat
all_trips <- unique( new_dat$my_trip_id )
missing_rows <- is.na( new_dat$actual_arrival_time ) | is.na( new_dat$actual_departure_time )

# get the trips with missing values and those without
missing_trips <- unique( new_dat$my_trip_id[ missing_rows ] )
complete_trips <- setdiff( all_trips, missing_trips )

# create a data frame for the trips with missing values and those without
missing_dat <- new_dat[ new_dat$my_trip_id %in% missing_trips, ]
complete_dat <- new_dat[ new_dat$my_trip_id %in% complete_trips, ]

# use the duplicated function to quickly select the unique trips with missing values and those without
vars <- c("my_trip_id", "route_direction_id")
missing_trips_df <- missing_dat[!duplicated( missing_dat$my_trip_id ), vars ]
complete_trips_df <- complete_dat[!duplicated( complete_dat$my_trip_id ), vars ]

# spot check the trips in the missing_trips_df and complete_trips_df to make sure they are correct
# by plotting the missingness pattern for a few random trips in each data frame
#stops <- list()
#stops[[7]] <- nb_stops
#stops[[9]] <- sb_stops
#for(i in 1:15){
#    par(mfrow=c(1,2))
#    id <- sample( missing_trips_df$my_trip_id, 1 )
#    jj <- which( missing_trips_df$my_trip_id == id )
#    dir <- missing_trips_df$route_direction_id[jj]
#    plot_missingness( new_dat, dir, stops[[dir]], my_trip_id = id )
#    id <- sample( complete_trips_df$my_trip_id, 1 )
#    jj <- complete_trips_df$my_trip_id == id
#    dir <- complete_trips_df$route_direction_id[jj]
#    plot_missingness( new_dat, dir, stops[[dir]], my_trip_id = id )
#    Sys.sleep(1)
#}    


# sample n trips from the complete_trips_df, and for each trip, sample a trip from the missing_trips_df
# and then match the missingness pattern of the trip with the missing values to the trip with

print( mean( is.na( new_dat$actual_arrival_time ) ) )
n <- 1000
for(i in 1:n){

    # sample a trip from the complete_trips_df
    row_complete <- sample( 1:nrow(complete_trips_df), 1 )
    id_complete <- complete_trips_df$my_trip_id[row_complete]
    dir_complete <- complete_trips_df$route_direction_id[row_complete]

    # get the rows for this trip in the new_dat
    new_dat_ii <- which( new_dat$my_trip_id == id_complete )
    ss <- new_dat$geo_node_name[new_dat_ii]

    # sample a trip from the missing_trips_df from the same direction
    row_missing <- sample( which( missing_trips_df$route_direction_id == dir_complete ), 1 )
    id_missing <- missing_trips_df$my_trip_id[row_missing]
    dir_missing <- missing_trips_df$route_direction_id[row_missing]
    #print( c(dir_complete, dir_missing) )
    
    # figure out which stop names have missing values for the trip with missing values
    ii <- new_dat$my_trip_id == id_missing
    misssing_dat <- new_dat[ii,]
    missing_stops <- misssing_dat$geo_node_name[ is.na(misssing_dat$actual_arrival_time) | is.na(misssing_dat$actual_departure_time) ]

    # figure out which rows from new_dat_ii should be set to NA
    rows_to_set_na <- new_dat_ii[ new_dat$geo_node_name[new_dat_ii] %in% missing_stops ]

    # set actual_arrival_time, actual_departure_time, and adherence to NA for these rows
    new_dat$actual_arrival_time[ rows_to_set_na ] <- NA
    new_dat$actual_departure_time[ rows_to_set_na ] <- NA
    new_dat$adherence[ rows_to_set_na ] <- NA

    #print( missing_stops )
    #print( new_dat[ rows_to_set_na, c("my_trip_id","geo_node_name","route_direction_id") ] )

}


print( mean( is.na( new_dat$actual_arrival_time ) ) )

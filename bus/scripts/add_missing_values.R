
# read in the original data
dat <- read.csv("../sample_transit_data/private/bus_stop_data_v1.csv")

# create a unique trip identifier by combining the calendar_id and the trip_id
dat$my_trip_id <- as.numeric( as.factor( paste( dat$date, dat$trip_id ) ) )

# ------
# Figure out the sequence of stops on the northbound and southbound routes
# ------
get_stop_sequence <- function( dat, direction_id ){

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
        this_df <- data.frame( stop = dat$geo_node_name[ii], arrival = dat$arrival_time[ii] )
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

# create a new data frame that does the following:
# it should copy the existing dataframe, and then insert additional missing values
# for departure time, arrival time, and adherence by randomly selecting 10% of the trips
# that have no missing values, and then randomly selecting a trip that does have missing values,
# and then match the missingness pattern of the trip with the missing values

# create the new dataset and figure out which trips have missing values and which do not
# make two data frames, one for the trips with no missing values, and one for the trips
# with missing values. There should be a column for the my_trip_id, and a column
# for the direction_id. 
new_dat <- dat
all_trips <- unique( new_dat$my_trip_id )
missing_rows <- is.na( new_dat$arrival_time ) | is.na( new_dat$departure_time )

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


# sample n trips from the complete_trips_df, and for each trip, sample a trip from the missing_trips_df
# and then match the missingness pattern of the trip with the missing values to the trip with

print( mean( is.na( new_dat$arrival_time ) ) )
n <- 5000
for(i in 1:n){

    if( i %% 100 == 0 ){ print(i) }

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
    missing_dat <- new_dat[ii,]
    missing_stops <- missing_dat$geo_node_name[ is.na(missing_dat$arrival_time) | is.na(missing_dat$departure_time) ]

    # figure out which rows from new_dat_ii should be set to NA
    rows_to_set_na <- new_dat_ii[ new_dat$geo_node_name[new_dat_ii] %in% missing_stops ]

    # set arrival_time, departure_time, and adherence to NA for these rows
    new_dat$arrival_time[ rows_to_set_na ] <- NA
    new_dat$departure_time[ rows_to_set_na ] <- NA
    new_dat$adherence[ rows_to_set_na ] <- NA

    #print( missing_stops )
    #print( new_dat[ rows_to_set_na, c("my_trip_id","geo_node_name","route_direction_id") ] )

}
print( mean( is.na( new_dat$arrival_time ) ) )

fname <- "../sample_transit_data/bus_stop_data_v2.csv" 
write.csv(new_dat, row.names = FALSE, quote = FALSE, file = fname )

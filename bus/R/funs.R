

# function to get a dataframe of stops and their coordinates, in order
# discuss concept of environments
get_stops_df <- function( dat, direction ){

    # subset to the route specified in the argument
    dat <- dat[ dat$route_direction_id == direction, ]
    
    # get the unique set of stops for this direction
    stops <- sort( unique( dat$geo_node_name ) )
    
    # create a proper trip id
    dat$my_trip_id <- paste( dat$date, dat$trip_id )
    trips <- sort( unique( dat$my_trip_id ) )
    
    # find a my_trip_id with all stops and no missing values in arrival time
    # get the arrival times for this trip in the object 'arrivals'
    for(j in 1:length( trips ) ){
    
        this_trip <- trips[j]
        ii <- dat$my_trip_id == this_trip
        this_stops <- sort( unique( dat$geo_node_name[ii] ) )
        this_arrivals <- dat$arrival_time[ii] 
        this_lon <- dat$longitude[ii]
        this_lat <- dat$latitude[ii]
    
        if( identical(stops, this_stops) && all( !is.na( this_arrivals ) ) ){
            arrivals <- this_arrivals
            lon <- this_lon
            lat <- this_lat
            break
        }
    }
    
    # figure out the ordering of these arrival times, and then sort the stops accordingly
    ord <- order( arrivals )
    stops_df <- data.frame(name = stops, longitude = lon, latitude = lat)[ord,]

    return( stops_df )
}

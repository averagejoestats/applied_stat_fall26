
# read in the original data
dat0 <- read.csv("../sample_transit_data/private/sample_bus_stop_data.csv")

# lowercase the column names
colnames(dat0) <- tolower( colnames(dat0) )

# create a unique stop identifier
dat0$row_id <- as.numeric( as.factor(
    paste(dat0$calendar_id, dat0$trip_id, dat0$geo_node_name)
) )

# aggregate over unique row_ids
library("dplyr")
dat1 <- dat0 %>%
    group_by( row_id ) %>%
    summarize(
        trip_id = trip_id[1],
        trip_sequence = trip_sequence[1],
        block_id = block_id[1],
        block_abbr = block_abbr[1],
        route_id = route_id[1],
        route_abbr = route_abbr[1],
        route_direction_id = route_direction_id[1],
        service_type_id = service_type_id[1],
        property_tag = property_tag[1],
        geo_node_id = geo_node_id[1],
        latitude = latitude[1]/10^7,
        longitude = longitude[1]/10^7,
        geo_node_name = geo_node_name[1],
        public_stop_number = public_stop_number[1],
        boardings = sum(boardings),
        alightings = sum(alightings),
        arrival_time = actual_arrival_time[1],
        departure_time = actual_departure_time[1],
        adherence = adherence[1],
        date = date[1]
    ) %>%
    as.data.frame()

# write to a file
fname <- "../sample_transit_data/private/bus_stop_data_v1.csv"
write.csv(dat1, row.names = FALSE, quote = FALSE, file = fname )


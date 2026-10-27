
data_path <- "../raw_data"
zips <- list.files(pattern = "FARS.*NationalCSV.zip$", path = data_path, full.names = TRUE)
fars_list <- list()

# Read the accident file from each zip
for (z in zips) {
  files <- unzip(z, list = TRUE)$Name
  inner <- files[grepl("accident\\.csv$", files, ignore.case = TRUE)][1]
  df <- read.csv(unz(z, inner, encoding = "latin1"), colClasses = "character")
  names(df) <- tolower(names(df))
  fars_list[[z]] <- df
  cat(z, nrow(df), "crashes\n")
}

# Columns differ across years, so give every year the full set
all_cols <- unique(unlist(lapply(fars_list, names)))
for (z in names(fars_list)) {
  missing <- setdiff(all_cols, names(fars_list[[z]]))
  if (length(missing) > 0) fars_list[[z]][missing] <- NA
  fars_list[[z]] <- fars_list[[z]][all_cols]
}

# bind them together and get specific types
fars <- do.call(rbind, fars_list)
rownames(fars) <- NULL
fars <- type.convert(fars, as.is = TRUE)

# write to files. .RData is much smaller
write.csv(fars, "../processed_data/fars_accidents.csv", row.names = FALSE)
save( fars, file = "../processed_data/fars.RData" )

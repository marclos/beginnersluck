saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
get_time_human <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}


save_data_flatfile <- function(data) {
  data <- t(data)
  file_name <- paste0(paste(get_time_human(), digest(data, 
                                                     algo = "md5"), sep = "_"), ".csv")
  write.csv(x = data, file = file.path(response_dir, file_name), 
            row.names = FALSE, quote = TRUE)
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}

# utility functions

get_date <- function(edat) {
  stopifnot(class(edat)=="edat")
  date_array <- unlist(strsplit(edat$header_info['SessionDate'], '-'))
  # edat has date in mm-dd-yyyy format
  paste0(date_array[c(3,1,2)], collapse='-')
}

get_subject_number <- function(edat) {
  .get_numeric_header(edat, 'Subject')
}

get_session <- function(edat) {
  .get_numeric_header(edat, 'Session')
}

.simplify_data_frame <- function(df) {
  # remove any irrelevant columns
  unique_values <- (plyr::laply(df, function(x) { length(unique(x[!is.na(x)])) })) > 1
  df[,unique_values]
}

.get_numeric_header <- function(edat, property) {
  stopifnot(class(edat)=="edat")
  as.numeric(edat$header_info[property])
}

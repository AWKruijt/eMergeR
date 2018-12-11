# parse the .txt "recovery" version of edat/edat2 files
# based loosely on https://github.com/canlab/CanlabCore/blob/master/Misc_utilities/parse_edat_txt.m
# provides S3 edat class and as.data.frame method to retrive trial log (like an eDataAid export to csv)


#source('../R/readsplitparse.R')


# S3 constructor
edatR <- function(txt_filename) {
  if (!file.exists(txt_filename)) {
    stop('File not found.')
  }

  parsed_edat <- .parse_file(txt_filename)
  if (!is.list(parsed_edat)) {
    stop('Unable to parse file.')
  }

  structure(parsed_edat, class="edat")
}

# return trial info as a dataframe
as.data.frame.edat <- function(edat, simplify = FALSE, factors=FALSE) {
  stopifnot(class(edat)=="edat")
  trials <- edat$trial_info
  df <- plyr::rbind.fill(lapply(trials, function(x) { data.frame(t(x), stringsAsFactors=factors) }))
  if (simplify) {
    df <- .simplify_data_frame(df)
  }
  df
}




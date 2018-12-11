# AW adjusted version of ahebranks edatparser (orginal: https://github.com/ahebrank/edatparser)

# for the sake of enabling import/merge of e-prime .txt files with special characters,
# I ended up replacting the file reading method - from scan() to readlines() with option
# open = 'rb'. The resulting file is not properly read: every other line is empty and
# each non-empty line starts with "\t" (ergo: tabs are not recognized when reading in)
# Three additional lines are used to remove empty lines as well as "\t" and "\377\376"
# at the start of all lines
# All of this is done in the adjusted version of .read_text()

# next it turned necessary to also adjust the finding edges routine. I coudlnt' entirely
# figure out what the 'trick' was that caused ahebrank's version to differentiate between
# 'logframe start' lines associated with the run info versus those associated with trials
# (from the regexp it seems to have had something to do with added spaces in the trial
# related lines - but following adjustments to the .read.text or maybe jsut because my edats
# are weird, I don't have those added spaces). The adjusted version assumes that the final
# set of logframe start & logframe end surround the runinfo. Should work AS LONG AS .txt files
# are complete.


.read_text <- function(fn) {
  # never know what kind of line endings you're going to get
  # there's probably a better way to deal with this
  tryCatch({ fileIn=file(fn,open="rb",encoding="unknown")
  on.exit(close(fileIn))
  lines <- readLines(fileIn, skipNul = TRUE)
  lines[nzchar(lines, keepNA = FALSE)]
  while(any(grepl("^\t", lines)) ){
    lines <- sub("\t", "", lines)  } # repeat this substitute action untill no lines start with \t
  # this allows trials more than 2 levels deep to be parsed too
  lines <- sub("\377\376", "", lines)},
    warning = function(e) { fileIn=file(fn,open="rb",encoding="unknown")
    on.exit(close(fileIn))
    lines <- readLines(fileIn, skipNul = TRUE)
    lines <- lines[nzchar(lines, keepNA = FALSE)]
    while(any(grepl("^\t", lines)) ){
      lines <- sub("\t", "", lines)  } # repeat this substitute action untill no lines start with \t
    # this allows trials more than 2 levels deep to be parsed too
    lines <- sub("\377\376", "", lines)}
  )
}



.look_for <- function(strings, needle) {
  i <- grep(needle, strings)
  if (length(i)==0) {
    warning(sprintf('%s not found in file. Did experiment terminate early?', needle))
  }
  i
}

.parse_pairs_vector <- function(idx, dat) {
  strings <- dat[as.numeric(idx[1]):as.numeric(idx[2])]
  named_vector <- function(x) {
    nv <- c(x[2])
    names(nv) <- c(gsub('^\\s+', '', x[1]))
    nv
  }
  unlist(lapply(strsplit(strings, ': '), named_vector))
}

.parse_pairs <- function(dat, idx) {
  if (is.data.frame(idx)) {
    # windowed definition of rows
    plyr::alply(idx, 1, .parse_pairs_vector, dat=dat)
  } else {
    # just a single start and stop index
    .parse_pairs_vector(idx, dat)
  }
}

.parse_file <- function(fn) {
  dat <- .read_text(fn)

  # header info
  header_start <- .look_for(dat, '^\\*\\*\\* Header Start')
  header_end <- .look_for(dat, '^\\*\\*\\* Header End')
  hdr_info <- .parse_pairs(dat, c(header_start + 1, header_end - 1))

  # separate run and trials info
  logFrame_edges <- data.frame(start = .look_for(dat, '^\\*\\*\\* LogFrame Start') + 1,
                               end = .look_for(dat, '^\\*\\*\\* LogFrame End') - 1)

  # run info -- we'll assume that the final edge in logFrame_edges indicates the run info

  run_info <- .parse_pairs(dat, logFrame_edges[nrow(logFrame_edges),])

  # trial info -- we'll assume anything indented more than one level deep is a trial

  trial_info <- .parse_pairs(dat, logFrame_edges[(1:nrow(logFrame_edges)-1),])

  list(header_info = hdr_info, run_info = run_info, trial_info = trial_info)
}



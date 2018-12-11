# r alternative to PST's 'e-merge' program to merge e-prime output files. Note that it parses the .txt files rather than the edat files!

# the function is a rather simple addition to the edatparser function by ahebrank, which is taking care of the actual parsing of the .txt files :)

# The eMergeR function searches the currently set working directory for all files which names contain the string(s) specified in the 'filenamecontains' argument, 
# parses them using ahebrank's edatparser function, adds header-info (Subject, Session, etc) and eventually returns a dataframe with all the files parsed and merged.

# the default value of the filenamecontains argument is set to ".txt" so that calling eMergeR() tells the function to simply merge all .txt files found in the currently set working directory. 
  
Sys.setlocale('LC_ALL','C')

eMergeR <- function(filenameContains = ".txt", dir = ".") {
  
 # source('../R/edatR.R')
  
  files <- list.files(pattern = filenameContains, path = dir)
  
  require(plyr)
  
  do.call(rbind.fill,lapply(paste0(dir, "/", files), function(x) {
    
    e <- edatR(x)
    
    e$header_info <- e$header_info[ !is.na(names(e$header_info))]

    header <- as.data.frame(t(e$header_info))
    headercut <- header[,which(names(header) == "SessionDate"):length(names(header))]
    
    dat <- as.data.frame(e)
    
    dat <- dat[, grep("NA.", names(dat), invert = TRUE) ]
    dat <- dat[, !is.na(names(dat))]

    dat <- cbind(headercut, dat)
    }) )
  
}




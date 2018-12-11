# examples eMerger (parse and merge multiple e-prime generated .txt files)

df <- eMergeR() # merge all .txt files in the working directory

df <- eMergeR(dir = "deeptest") # merge all .txt files in the folder 'deeptest' that sits in the working directory

df <- eMergeR(filenameContains = "symbol") # merge all files for which the filename contains "symbol"

df <- eMergeR(filenameContains = "symbol", dir = "deeptest")

# note that if the edat2 files are also in the folder they'll be included and the function
# sometimes will throw errors as it can't parse them.
# this can be solves by adding a .txt to filenameContains
df <- eMergeR(filenameContains= glob2rx("*Flanker*.txt")) # merge all files for which the filename contains both "VAS" and ".txt" (using glob2rx to construct reg expr, * is a wildcard)


# examples edatR()  (single e-prime generated .txt file parser)

# txt file that has symbols
test <- edatR('symboltest.txt')
test.df <- as.data.frame(test)

test.df2 <- as.data.frame.edat(test)

# single run
test <- edatR('Flanker-991-1.txt')
test.df <- as.data.frame(test)

# some more
test2 <- edatR('scene-961-3.txt')
test3 <- edatR('words_norming-953-1.txt')

# multiple runs
test4 <- edatR('Arithmetic-1817-1.txt')

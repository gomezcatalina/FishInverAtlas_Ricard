#function that will create a CSV file for a data frame containing Unicode text
#this can be used instead of write.csv in R for Windows
#source: https://stackoverflow.com/questions/46137078/r-accented-characters-in-data-frame
#this is not elegant, and probably not robust


write.unicode.csv <- function(mydf, filename="") {  #mydf can be a data frame or a matrix
  linestowrite <- character( length = 1+nrow(mydf) )
  linestowrite[1] <- paste('"","', paste(colnames(mydf), collapse='","'), '"', sep="") #first line will have the column names
  if(nrow(mydf)<1 | ncol(mydf)<1) print("This is not going to work.")        #a bit of error checking
  for(k1 in 1:nrow(mydf)) {
    r <- paste('"', k1, '"', sep="") #each row will begin with the row number in quotes
    for(k2 in 1:ncol(mydf)) {r <- paste(r, paste('"', mydf[k1, k2], '"', sep=""), sep=",")}
    linestowrite[1+k1] <- r
  }
  writeLines(linestowrite, con=filename, useBytes=TRUE)
} #end of function
## Takes a subject from readSubject and converts it into a single
## dataframe
subject2df = function(rsData){
    ## loop over all types and levels and concatenate
    rdata = NULL
    for (i in 1 : length(rsData))
        rdata = rbind(rdata, rsData[[i]])
    return(rdata)
}

## Takes a subject from readSubject and converts it into a single
## dataframe
subject2df = function(subjectData){
    ## loop over all types and levels and concatenate
    rdata = NULL
    for (i in 1 : length(subjectData))
        rdata = rbind(rdata, subjectData[[i]])
    return(rdata)
}

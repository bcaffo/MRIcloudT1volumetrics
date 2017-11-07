##' @name subject2df
#' @title Takes the output from \code{readSubject} and converts it to a dataframe
#' @param subjectData the output from readSubject
#' @description Takes a subject list and converts to a data frame
#' @export

subject2df = function(subjectData){
    ## loop over all types and levels and concatenate
    rdata = NULL
    for (i in 1 : length(subjectData))
        rdata = rbind(rdata, subjectData[[i]])
    return(rdata)
}

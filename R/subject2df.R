#' @name subject2df
#' @title Takes the output from \code{readSubject} and converts it to a dataframe
#' @param subjectData the output from readSubject
#' @description Takes a subject list and converts to a data frame
#' @export
subject2df = function(subjectData){
    ## loop over all types and levels and concatenate
    rval = do.call("rbind", subjectData)
    ## type and level have been included as variables
    ## so they don't need to be included as rownames
    rownames(rval)= NULL
    rval
}


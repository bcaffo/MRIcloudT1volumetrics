#' addSubjectICV
#' @title Utility functions 
#' @param subjectData output from \code{readSubject} in the form of a dataframe
#' @description takes in a subject data frame and adds an ICV variable
#' @return data frame with ICV or TBV added
#' @importFrom dplyr filter mutate
#' @export
addSubjectICV = function(subjectData){
    level = type = NULL
    rm(list = c("level", "type"))
    l1t1 = dplyr::filter(subjectData, level == 1, type == 1)
    icv_var = sum(l1t1$volume)
    subjectData = dplyr::mutate(subjectData, icv = icv_var)
    return(subjectData)
}

#' @rdname addSubjectICV
#' @export
addSubjectTBV = function(subjectData){
    level = type = roi = NULL
    rm(list = c("level", "type", "roi"))
    
    l1t1 = filter(subjectData, level == 1, type == 1, roi != "CSF")
    tbv_var = sum(l1t1$volume)
    subjectData = mutate(subjectData, tbv = tbv_var)
    
    return(subjectData)
}

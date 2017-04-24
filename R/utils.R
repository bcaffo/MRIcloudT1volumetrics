#' addSubjectICV
#' @title Utility functions 
#' @param subjectData output from \code{readSubject} in the form of a dataframe
#' @description takes in a subject data frame and adds an ICV variable
#' @return data frame with ICV or TBV added
#' @export
addSubjectICV = function(subjectData){
    l1t1 = filter(subjectData, level == 1, type == 1)
    icv = sum(l1t1$volume)
    subjectData = mutate(subjectData, icv = icv)
    return(subjectData)
}

#' @rdname addSubjectICV
#' @export
addSubjectTBV = function(subjectData){
    l1t1 = filter(subjectData, level == 1, type == 1, roi != "CSF")
    tbv = sum(l1t1$volume)
    subjectData = mutate(subjectData, tbv = tbv)
    
    return(subjectData)
}

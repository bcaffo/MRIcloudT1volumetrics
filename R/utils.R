#' addSubjectICV
#' @title Utility functions 
#' @param subjectData output from \code{readSubject} in the form of a dataframe
#' @description takes in a subject data frame and adds an ICV or TBV variable or 
#' fixes errors \code{fixTarget} fixes the error where rawid gets replaced with target
#' and \code{fixBasalForebrain} fixes the label error with the basal forebrain in type 1 level 5
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


#' @name fixBasalForebrain
#' @title A function for fixing the basal forebrain labeling error
#' @rdname fixBasalForebrain
#' @export
fixBasalForebrain = function(subjectData){
    ## 2 on levels 2, 3, 4 (first 6 indices) 
    ## and so the ones to fix are
    ## numbers 7 through 10
    bfnos = c(7 : 10)
    bfids = grep("BasalForebrain", subjectData$roi)[bfnos]
    subjectData$roi[bfids] = c("AnteriorBasalForebrain_L",
                               "AnteriorBasalForebrain_R",
                               "PosteriorBasalForebrain_L",
                               "PosteriorBasalForebrain_R")
    subjectData
}


#' @rdname fixTarget2
#' @export
fixTarget2 = function(subjectData){
    rawid = subjectData$rawid
    
    ## add a variable that is one if there's a rawid labeled
    ## target2.img, which seems to happen for some reason
    subjectData$anyTarget = (rawid == "target2.img") * 1
    
    ## If there's no good values
    if (all(rawid == "target2.img")) stop("rawid is messed up")
        
    ## Take the value that's not target2.img

    val = unique(rawid[rawid != "target2.img"])
    
    if (length(val) > 1) {
        print(val)
        stop("More than one rawid value to correct to")
    }
    
    subjectData$rawid = val
    
    subjectData
}


#' @rdname readSubjectDf
#' @export
readSubjectDf = function(filename){
    filename %>% readSubject %>% subject2df
}
    
    
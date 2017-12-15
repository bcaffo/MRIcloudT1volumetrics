#' @name readFileList 
#' @title A function for reading a list of T1 volumetric mricloud data
#' @param fileList a textlist of files 
#' @param idList an optional list of ids, otherwise is 1 : length(fileList)
#' @description A function for reading in a collection of files of T1 volumetric output
#' from MRIcloud. The files must be a list of files
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter mutate
#' @export
readFileList = function(fileList, idList = NULL){
    if (is.null(idList)) idList = 1 : length(fileList)
    else if (length(idList) != length(fileList)) {
        stop("idList and fileList must have equal length")
    }
   
    rval = NULL
    for  (i in 1 : length(fileList)){
        f = fileList[i]
        tempDat = readSubject(f) %>%
            subject2df() %>% 
            addSubjectICV() %>%
            addSubjectTBV() %>%
            mutate(id = idList[i])
        rval = rbind(rval, tempDat)
    }
    rval
}

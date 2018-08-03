#' @name readFileList 
#' @title A function for reading a list of T1 volumetric mricloud data
#' @param fileList a textlist of files 
#' @param idList an optional list of ids, otherwise is 1 : length(fileList)
#' @description A function for reading in a collection of files of T1 volumetric output
#' from MRIcloud. The files must be a list of files
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter mutate
#' @export
readFileList = function(fileList, idList = NULL, fixT2 = FALSE, fixBF = FALSE){
    if (is.null(idList)) idList = 1 : length(fileList)
    else if (length(idList) != length(fileList)) {
        stop("idList and fileList must have equal length")
    }

    ## read in each file, convert to data frame,
    ## add icv and tbv and id variable
    lapply(1 : length(fileList), function(i) {
            
            ## Take the file and read it in
            f = fileList[i] %>%
                readSubjectDf()
            
            ## If required, fix the basaForebrain and
            ## target2 errors
            if (fixT2) f = f %>% fixTarget2()
            if (fixBF) f = f %>% fixBasalForebrain()
            
            ## Finish reading it in
            f %>% 
                mutate(id = idList[i]) %>%
                addSubjectICV() %>%
                addSubjectTBV()
        }
    ) %>% do.call(what = "rbind") #convert to data frame
}

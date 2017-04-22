#' @name readFileList
#' @param fileList a textlist of files 
#' @param levelList an optional list of levels to include, needs equal length as \code{typeList}
#' @param typeList an optional list of types to include, needs equal length to \code{levelList}

readFileList = function(fileList, levelList = 5, typeList = 1){
    if (length(levelList) != length(typeList)) stop("levelList and typeList must have equal length")
    rval = NULL
    for  (i in 1 : length(fileList)){
        f = fileList[i]
        tempDat = readSubject(f) %>%
            subject2df() %>% 
            filter(level %in% levelList, type %in% typeList) %>%
            mutate(id = i)
        rval = rbind(rval, tempDat)
    }
    rval
}
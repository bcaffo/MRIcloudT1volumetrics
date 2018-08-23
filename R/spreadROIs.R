#' @name spreadROIs
#' @title Converts the long data format to wide for ROI data
#' @param dat the output from \code{readFileList}
#' @description this function just used tidyverse spread for output specifically from \code{readFileList}
#' only does this for volume and only if you haven't modified the data frame. 
#' should be ok if you select type and level
#' @importFrom dplyr group_by n ungroup select
#' @importFrom tidyr unite spread 
#' @export
spreadROIs = function(dat){
    volume = key = id = std = NULL
    rm(list= c("id", "std", "key", "volume"))
    roi = type = level = temp = NULL
    rm(list= c("roi", "type", "level", "temp"))
    
    tbv = icv = NULL
    rm(list= c("tbv", "icv"))
    
    out = dat %>%
        ##
        ##
        ## This is now fixed. If your data shows this error
        ## run fixBasalForebrain when loading them in or add
        ## fixBF = TRUE in readFilelist
        ##
        ## This step shouldn't be necessary, add a temporary variable
        ## so that ROI names are unique. For some reason basalForebrain is 
        ## repeated twice in level 5 type 1. Have emails out to see
        ## if this is an error or if there's some other difference in the 
        ## two basal forebrain measures
    group_by(id) %>%
        mutate(temp = 1 : n()) %>% 
        ungroup() %>% 
        ## If the above ever gets fixed, remove temp from here
        ## This just adds something to make the ROI names unique 
        ## at level 5
        ##
        ##
        unite(key, roi, type, level, temp, sep = "_", remove = TRUE) %>%  
        select(-min, -max, -mean, -std) %>% 
        ## Want a unique ICV and TBV per subject. However
        ## I can't figure out any way other than converting the 
        ## numeric values to character then back
        mutate(icv = as.character(icv), tbv = as.character(tbv)) %>% 
        spread(key = key, value = volume) %>%
        mutate(icv = as.numeric(icv), tbv = as.numeric(tbv))
}


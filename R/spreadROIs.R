#' @name spreadROIs
#' @title Converts the long data format to wide for ROI data
#' @param dat the output from \code{readFileList}
#' @description this function just used tidyverse spread for output specifically from \code{readFileList}
#' only does this for volume and only if you haven't modified the data frame. should be ok if you select type and level
#' @export
spreadROIs = function(dat){
        out = dat %>% 
            unite(key, roi, type, level, sep = "_", remove = TRUE) %>%  
            select(-min, -max, -mean, -std) %>% 
            ## Want a unique ICV and TBV per subject. However
            ## I can't figure out any way other than converting the 
            ## numeric values to character then back
            mutate(icv = as.character(icv), tbv = as.character(tbv)) %>% 
            spread(key = key, value = volume) %>%
            mutate(icv = as.numeric(icv), tbv = as.numeric(tbv))
}


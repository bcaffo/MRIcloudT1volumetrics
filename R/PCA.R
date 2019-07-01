#' @name PCA
#' @title A function for compositional principal component analysis of ROIs across age
#' @author Luchao Qi
#' @param data use readFileList function to get MRI data frame
#' @param infodir path to lookup table of patient info
#' @param Type optinal,type of MRI sequences(T1-weighted, T2-weighted, etc.), default = 1
#' @param Level optional,level of MRI, default = 1
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter mutate rename left_join group_by select
#' @importFrom tidyr spread
#' @export

PCA = function(data, infodir,Type = 1, Level = 1){
    # preprocessing
    dat = data %>%
        filter(type == Type, level == Level) %>%
        select(rawid,roi,volume) %>%
        spread(roi,volume)
    # change the format of rawid as is compatible with patient info
    dat$rawid  = as.numeric(sapply(strsplit(dat$rawid, "_"),function(x) x[1]))
    # compositions, weight of each roi
    dat = data.frame(rawid = dat$rawid ,
                     t(apply(dat,1, function(i) i[-1]/sum(i[-1])))
                      )
    # patient info
    info = read.csv(infodir, header = T) %>%
        rename('rawid' = 'Subject')
    # merge data frame by rawid
    dat = left_join(dat,info[,c('rawid','Age')], by = 'rawid') %>% select(-rawid) %>% na.omit()

    # pca
    pca.result = by(dat ,INDICES = dat$Age, FUN = function(i){
        prcomp(i %>% select(-Age),scale. = T)
    })
    # dat %>% filter(Age == '36+')%>% select(-Age)
    # prcomp(dat %>% filter(Age == '36+') %>% select(-Age),scale. = T)

    return(pca.result)

}


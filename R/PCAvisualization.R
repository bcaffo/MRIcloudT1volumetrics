#' @name PCAvisualization
#' @title A function for visualization of principal component rotations
#' @author Luchao Qi
#' @param pca.result should use function MRIPCA to get a list of pca result
#' @param PC index of PC, default = 1
#' @import ggplot2
#' @importFrom ggplot2 aes
#' @importFrom data.table melt
#' @export

PCAvisualization = function(pca.result, PC = 1){
    pca.rotations = sapply(pca.result, function(i) i$rotation)
    dat_visualization = lapply(pca.rotations, function(i){
      prop.table(abs(i),margin = 2) %>%
        melt() %>%
        `colnames<-` (c('roi','Comp','value')) %>%
        filter(Comp == paste0('PC',PC)) %>% select(c(roi,value))
      }) %>%
      do.call(what = "rbind") #convert to data frame
    dat_visualization = dat_visualization %>%
      mutate(Age = sapply(strsplit(rownames(dat_visualization), "[.]"),function(x) x[1])
             ) # rownames reformat


    ggplot(dat_visualization, aes(x = Age,y = value,colour = roi,group = roi)) +
      geom_line() +
      labs(x = 'Age', y = 'Weight', title = paste0('Compositional analysis: weight of ROIs in PC',PC))
}

#' @title readSubject
#' @description A utility function for parsing the output of MRIcloud volumetric output
#' @details  This function takes in a filename that consist of volumetric output from
#' MRIcloud. It then parses it into a list of dataframes of the various combinations 
#' of levels and outputs. It requires the full path of to the file as text input.
#' @author Brian Caffo
#' @name readSubject
#' @param fileLoc the location of the volumetric processed output text file
#' @return A list containing one list element per type/level combination
#' @keywords MRI
#' @export
#' @examples 
#' fileLoc = system.file("extdata", 
#' "kirby127a_3_1_ax_283Labels_M2_corrected_stats.txt", 
#' package="MRIcloudT1volumetrics")
#' test = readSubject(fileLoc)
#' test[[1]]
#' @importFrom utils read.table
readSubject = function(fileLoc){

    ## read in all of the text file
    fullData = readLines(fileLoc)

    ## get the indices of the levels and type labels
    ## These might be common across subjects; they seem to be
    ## however, it's more conservative to do it every time
    tlidx = t(apply(expand.grid(1 : 2, 1 : 5), 1, 
        function(x) {
            tlname = paste("Type", x[1],
                           "-L", x[2],
                           " Statistics", sep = "")
            c(x, grep(tlname, fullData))
        }
        )
    )
    colnames(tlidx) = c("type", "level", "index")
    tlidx = tlidx[order(tlidx[,3]),]

    ## the number of breakpoints
    last = nrow(tlidx)
    
    ## loop over the indices and read in the data
    subjectData = lapply(1 : last, 
        function(i){
            ## These constants were just obtained by inspection of the raw files
            startline = tlidx[i, 3] + 1
            if (i == last) endline = length(fullData)
            else endline = tlidx[i + 1, 3] - 1
            toparse = paste(fullData[startline : endline], "",
                            collapse = "\n")
            tc = textConnection(toparse)
            dat = utils::read.table(tc,
                             fill = TRUE,
                             stringsAsFactors = FALSE)
            colnames(dat) = c("rawid", "roi", "volume", "min",	"max",	"mean",	"std")
            dat$type = tlidx[i,1]
            dat$level = tlidx[i,2]
            close(tc)
            return(dat)
       }
    )
    names(subjectData) = paste("t", tlidx[,1], "l", tlidx[,2], sep = "")

    return(subjectData)
}


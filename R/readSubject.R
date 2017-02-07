## reads in a subject given the full path to the file
## requires the files to be in the specific format for this dataset that I'm looking at
## Brian Caffo 2/6/2017

roiDir = "D:/kyrana/T1 ROI volumes/T1 ROI volumes/"
fileList = dir(roiDir)
subjects = sapply(strsplit(fileList, "_"), function(x) x[2])
fileloc =  paste(roiDir, fileList[1], sep = "")


readSubject = function(fileloc){

    ## read in all of the text file
    fullData = readLines(fileloc)

    ## get the indices of the levels and type labels
    ## I have no idea if these are common across subjects, they seem to be
    ## however, it's more conservative to do it every time
    tlidx = t(apply(expand.grid(1 : 2, 1 : 5), 1, 
        function(x) {
            tlname = paste("Type", x[1], "-L", x[2], " Statistics", sep = "")
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
            startline = tlidx[i, 3] + 1
            if (i == last) endline = length(fullData)
            else endline = tlidx[i + 1, 3] - 1
            toparse = paste(fullData[startline : endline], "", collapse = "\n")
            dat = read.table(textConnection(toparse), fill = TRUE)[,1 : 3]
            colnames(dat) = c("id", "roi", "volume")
            dat$type = tlidx[i,1]
            dat$level = tlidx[i,2]
            return(dat)
       }
    )
    names(subjectData) = paste("type", tlidx[,1], "level", tlidx[,2], sep = "")

    return(subjectData)
}


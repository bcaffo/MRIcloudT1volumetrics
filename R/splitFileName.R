## takes in a vector of filenames and splits them
## into a dataframe
## assumes the format of the data that I have now

splitFileName = function(fnvec){
    temp = strsplit(fnvec, "_")

    dat = data.frame(
        imageType = sapply(temp, function(x) x[1]),
        id = sapply(temp, function(x) x[2]),
        date = sapply(temp, function(x) x[3])
    )
    return(dat)
    
}

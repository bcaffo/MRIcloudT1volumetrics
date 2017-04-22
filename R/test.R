library(dplyr); library(magrittr)
source("R/readSubject.R")
source("R/utils.R")
source("R/subject2df.R")
source("R/readFileList.R")
roiDir = "inst/extdata/"
files = dir(roiDir, full.names = TRUE)


dat = readFileList(files)

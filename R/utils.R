## a set of utility functions

## takes in a subject data frame and adds an ICV variable
addSubjectICV = function(subjectData){
    require(dplyr)
    l1t1 = filter(subjectData, level == 1, type == 1)
    icv = sum(l1t1$volume)
    subjectData = mutate(subjectData, volume = icv)
    return(subjectData)
}


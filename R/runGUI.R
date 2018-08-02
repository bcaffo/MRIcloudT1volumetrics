#' @name rungui
#' @title A function for running a gui that works with MRICloud output
#' @param example name of gui, currently only \code{'loaddata'}
#' @description A function that launches a gui for the loading and analysis of MRICloud output
#' currently there's only one app named \code{'loaddata'}
#' @importFrom tidyverse 
#' @export
## Adapted from Dean Attali's code
runGUI = function(example) {
    # locate all the shiny app examples that exist
    validExamples = list.files(system.file("shinyApps", package = "MRIcloudT1volumetrics"))
    
    validExamplesMsg =
        paste0(
            "Valid examples are: '",
            paste(validExamples, collapse = "', '"),
            "'")
    
    # if an invalid example is given, throw an error
    if (missing(example) || !nzchar(example) ||
        !example %in% validExamples) {
        stop(
            'Please run `runExample()` with a valid example app as an argument.\n',
            validExamplesMsg,
            call. = FALSE)
    }
    
    # find and launch the app
    appDir = system.file("shinyApps", example, package = "MRIcloudT1volumetrics")
    shiny::runApp(appDir, display.mode = "normal")
}

## Shiny app to load and use MRIcloud T1 volumetric Data

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui = fluidPage(
   
   # Application title
   titlePanel("MRICloud file explore"),

   sidebarPanel(
       fileInput("imageFiles", "Select input image txt files file, use CTRL or Shift to select multiple files.", 
                 multiple = TRUE),
       checkboxGroupInput(inputId = "levels", label = "Select level(s)",
                     choiceNames = as.character(1 : 5),
                     choiceValues = 1 : 5,
                     selected = 1 : 5),
       checkboxGroupInput(inputId = "types", label = "Select types(s)",
                          choiceNames = as.character(1 : 2),
                          choiceValues = 1 : 2,
                          selected = 1 : 2)
   ),
   
   # Show a plot of the generated distribution
   mainPanel(
       tabsetPanel(type = "tabs",
            tabPanel("Tidy Data", 
                     #downloadButton("downloadData", "Download CSV"),
                     dataTableOutput("displayTable")),
            tabPanel("Flat Data",
                     #downloadButton("downloadDataFlat", "Download CSV"),
                     dataTableOutput("displayTableFlat")),
            tabPanel("Messages", 
                     textOutput("imageDataStatus"),
                     textOutput("imageFilterStatus"), br(),
                     downloadLink("downloadData", "Download the tidy data"), br(),
                     downloadLink("downloadDataFlat", "Download the flat data")
                     )
       )
   )
)   

# Define server logic required to draw a histogram
server = function(input, output) {

    test = reactive({browser()})
    
    ## Read in the data
    dat = reactive({
        if (is.null(input$imageFiles$datapath)) NULL
        else readFileList(input$imageFiles$datapath)
    })
    
    ## if necessary filter the data by the checkboxes
    rval = reactive({
        ## If they uncheck everything, then return NULL, or if the data hasn't been read in
        if (length(input$types)  == 0 |  length(input$levels) == 0 | is.null(dat())) NULL
        else {
            fdat = dat()
            ## If everything is selected, then don't filter
            if (length(input$levels) != 5){
               lvls = as.numeric(input$levels)
               fdat = fdat %>% filter(level %in% lvls)
            }
            ## If everything is selected, then don't filter
            if (length(input$types) != 5) {
                tpes = as.numeric(input$types)
                fdat = fdat %>% filter(type %in% tpes)
            }
            fdat
        }
    })

    ## If possible, flatten the data
    rvalFlat = reactive({
        ## If it's null then don't do anything
        if(is.null(rval())) NULL
        else spreadROIs(rval())
    })

    ## Message of whether the data is loaded
    output$imageDataStatus = renderText({
        if (is.null(dat())) "Data is not loaded"
        else "Data is loaded"
    })
    output$imageFilterStatus = renderText({
        if (is.null(rval())) "Data is not filtered"
        else str_c(c("Filtered by levels {", input$levels, "}", 
                     "\nFiltered by types {", input$types, "}"), collapse = " ")
    })

    ## Display for the tidy and flattened data
    output$displayTable = renderDataTable(rval())
    output$displayTableFlat = renderDataTable(rvalFlat())

    ## Download the data
    output$downloadData = downloadHandler(
        filename = function() {"tidyFormat.csv"},
        content = function(file) write.csv(rval(), file, row.names = FALSE)
    )
    
    output$downloadDataFlat = downloadHandler(
        filename = function() {"flatFormat.csv"},
        content = function(file) write.csv(rvalFlat(), file, row.names = FALSE)
    ) 
        
}

# Run the application 
shinyApp(ui = ui, server = server)


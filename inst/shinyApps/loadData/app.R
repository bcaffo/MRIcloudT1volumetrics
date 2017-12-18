## Shiny app to load and use MRIcloud T1 volumetric Data

library(shiny)

# Define UI for application that draws a histogram
ui = fluidPage(
   
   # Application title
   titlePanel("MRICloud file loading / manipulation"),

   sidebarPanel(
       fileInput("imageFiles", "Select input image txt files file, use CTRL or Shift to select multiple files.", 
                 multiple = TRUE)
   ),
   
   # Show a plot of the generated distribution
   mainPanel(
        textOutput("prop"),
        textOutput("prop2"),
        textOutput("prop3"),
        textOutput("prop4")
   )
   
   
)   

# Define server logic required to draw a histogram
server = function(input, output) {

    output$prop = renderText({names(input$imageFiles)})
    output$prop2 = renderText({print(input$imageFiles$name)})
    output$prop3 = renderText({print(input$imageFiles$size)})
    output$prop4 = renderText({print(input$imageFiles$datapath)})
    
}

# Run the application 
shinyApp(ui = ui, server = server)


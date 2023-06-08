#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input
    sliderInput(inputId = "num", 
                label = "Choose a number", 
                value = 25, min = 1, max = 100),
    #input element for histogram
    plotOutput("hist")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #save output object to display as histogram
    #braces allow as many arguments to pass through render*()
    output$hist <- renderPlot({
        ## title <- "100 random normal values"
        #rmorm(100) returns 100 random normal values
        ## hist(rnorm(100), main = title)
        #value of slider will be accessible with: input$num
        #always shows currently value of input$num
        #if slider is set to 25, rnorm(25) will be shown
        #following syntax is: n random normal values where n is set by the user using the slider
        hist(rnorm(input$num))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

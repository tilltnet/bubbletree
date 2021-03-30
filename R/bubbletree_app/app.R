#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bubbletree)
library(tidyverse)
ui <- fluidPage(

    titlePanel("bubbletree(map) Demo"),

        fluidRow(width = 12,
            column(10,
                bubbletreeOutput("plot", height = "800px")),

            column(2,
                   wellPanel(
            sliderInput("lower",
                        "Lower cluster cutoff:",
                        min = 1,
                        max = 10,
                        value = 2),
            sliderInput("upper",
                        "Upper cluster cutoff:",
                        min = 1,
                        max = 10,
                        value = 3)
        ))))


server <- function(input, output) {

    data("labels")
    data("cluster_data")
    data("labels2")
    data("cluster_data2")

    output$plot <- renderBubbletree({
        td <- hc_to_d3(hclust_object = cluster_data,
                       cluster_range = input$lower:input$upper,
                       top_words_size = labels)



     bubbletree(td)

    })
}

shinyApp(ui = ui, server = server, options = list(height = "100%"))

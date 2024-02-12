#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({
      JO<-read.csv("https://raw.githubusercontent.com/GabinMISARA/olympique-/main/athlete_events.csv")
      
      
      ggplot(data = JO, aes(x=JO[,6], y=JO[,15])) + 
        geom_point(col = input$color)+
        labs(title = "Atlète par pays",
             x = "Pays",
             y = "Athlètes") +
        scale_color_discrete(palette = "Set1") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold"))
        
    })
}

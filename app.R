library(shiny)
require(ggplot2)
require(dplyr)
require(ggpubr)
library(tidyverse)
library(ggpubr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("The relationship between phenotypes and final yield"),
  
  # Sidebar with a slider input for the number of bins
  sidebarPanel(
    selectInput("phenotype",
                "Choose a phenotypic trait to investigate",
                list(`Growth` = list("DaysToBloom", "MedianLeafAngle", "FlagLeafLength", "FlagLeafWidth", 
                                     "ExtantLeafNumber","PlantHeight", "ThirdLeafLength", "ThirdLeafWidth"),
                     `Reproductive` = list("PaniclesPerPlot", "PanicleGrainWeight"))
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("Scatterplot")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Scatterplot <- renderPlot({
    dat_scatter <- dat[,c(input$phenotype, "EstimatedPlotYield")]
    gg <- data.frame(Yield=dat$EstimatedPlotYield, Phenotype=dat[,input$phenotype]) %>%
      ggplot(aes(x = Phenotype, y=Yield)) + geom_point() +
      stat_smooth(method = "lm")+
      stat_regline_equation(aes(label =  paste(..rr.label.., sep = "~~~~")))+
      xlab(input$phenotype)+ylab("Yield (g)")
    print(gg)
  })
}

# Bind ui and server together
shinyApp(ui, server)

library(tidytuesdayR)
library(tidyr)
library(dplyr)
library(readr)
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')
library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    titlePanel("Cocktails Categories"),
    
    sidebarPanel(
        selectInput("category", "Category", choices = sort(unique(boston_cocktails$category)), selected = "Cocktail Classics")
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Barchart", plotOutput("category"))
        )
    )
)

server <- function(input, output) {
    
    cocktail_subset <- reactive({
        boston_cocktails %>% filter(category == input$category)
    })
    output$category <- renderPlot({
        ggplot(data = cocktail_subset(), aes(x = name)) +
            geom_line(stat="count") +
            theme_bw() +
            ggtitle(paste("Number of", input$category, "Reports by Borough"))
    })
    
}

shinyApp(ui, server)



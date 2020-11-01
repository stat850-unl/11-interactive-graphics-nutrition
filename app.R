library(dplyr)
library(shiny)
library(ggplot2)
library(DT)
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')


ui <- fluidPage(
    
    titlePanel("Cocktails Categories"),
    
    sidebarPanel(
        selectInput("category", "Category", choices = sort(unique(boston_cocktails$category)), selected = "Brandy")
    ),
    
    br(),
    h3("Choose ingredient"),
    selectizeInput("ingredient", label = "Select ingredient",choices = unique(boston_cocktails$ingredient),
                   options = list(create = T)
                   ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Barchart", plotOutput("category")),
            tabPanel("Recipe",DT::dataTableOutput("recipe_table"))
        )
    )
)

server <- function(input, output) {
    
    cocktail_subset <- reactive({
        boston_cocktails %>% 
            filter(category == input$category) %>% 
            #filter(ingredient == input$ingredient) %>% 
            count(ingredient) %>% 
            mutate(cnt = n)
    })
    
    
    output$category <- renderPlot({
        ggplot(data = cocktail_subset(), aes(x = ingredient, y = cnt)) +
            xlab("Ingredient") +
            ylab("count") +
            #scale_y_discrete(limits = as.character(input$ingredient))+
            geom_point(color = 'blue') +
            theme_bw() +
            #scale_x_discrete(expand = c(0,2)) +
            coord_flip() +
            ggtitle(paste("Number of", input$category, "Reports by Borough"))
    })
    
    recipe_subset <- reactive({
        boston_cocktails %>% 
            filter(category == input$category) %>% 
            filter(ingredient == input$ingredient)
    })
    output$recipe_table <- DT::renderDataTable({

        
    })
}

shinyApp(ui, server)



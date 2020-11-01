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
            tabPanel("Summary plot", plotOutput("category")),
            tabPanel("Ingredient",DT::dataTableOutput("filter_ingredient")),
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
    
    ing_subset <- reactive({
         boston_cocktails %>% 
            filter(category == input$category) %>% 
            filter(ingredient == input$ingredient)
    })
    
    output$filter_ingredient <- DT::renderDataTable({
        DT::datatable(ing_subset(),escape = F)
    })
    
    recipe_subset <- reactive({
        chosed_row <- input$filter_ingredient_rows_selected
        a <- ing_subset()$name[chosed_row]
        boston_cocktails %>% filter(name %in% a)
    })
    
    output$recipe_table <- DT::renderDataTable({
        DT::datatable(recipe_subset(),escape = F)
    })
}

shinyApp(ui, server)



library(shiny)
library(tidyverse)

gss_shiny <- readRDS(file = "./data.rds")

# Define the user interface
ui <- fluidPage(
  titlePanel("Demographic Filters on Age and Income Relationship"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("degree", "Degree:",
                  choices = levels(gss_shiny$DEGREE)),
      selectInput("sex", "Sex:",
                  choices = levels(gss_shiny$SEX)),
      selectInput("workself", "Work Self-Employment Status:",
                  choices = levels(gss_shiny$WRKSLF)),
      selectInput("born", "Born in this Country:",
                  choices = levels(gss_shiny$BORN)),
      selectInput("race", "Race:",
                  choices = levels(gss_shiny$RACE)),
      actionButton("update", "Update Plot")
    ),
    
    mainPanel(
      plotOutput("incomeAgePlot")
    )
  )
)

server <- function(input, output) {
  data_filtered <- reactive({
    # Filter data based on input selections
    gss_shiny %>%
      filter(DEGREE == input$degree,
             SEX == input$sex,
             WRKSLF == input$workself,
             BORN == input$born,
             RACE == input$race)
  })
  
  output$incomeAgePlot <- renderPlot({
    # Ensure plot updates only when the user clicks 'Update Plot'
    input$update
    isolate({
      data_to_plot <- data_filtered()
      ggplot(data_to_plot, aes(x = AGE, y = INCOME)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", color = "blue") +
        labs(title = "Scatterplot of Age vs. Income",
             x = "Age",
             y = "Income")
    })
  })
}

shinyApp(ui = ui, server = server)

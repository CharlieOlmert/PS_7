# Load libraries 

library(shiny)
library(tidyverse)
library(stringr)
library(rsconnect)
library(rgdal)
library(shinythemes)
library(plotly)
library(scales)
library(ggrepel)

# Load data from the rds
app_data <- read_rds("shiny_data.rds")

# Change D and R to be Democratic and Republican

app_data <- app_data %>%
  mutate(Party = case_when(
    Party == "D" ~ "Democratic", 
    Party == "R" ~ "Republican"
  )) 

# Change the variable names so that they will look nicer with tooltip in ggplotly

colnames(app_data)[colnames(app_data)=="rep_adv_polls"] <- "Forecasted"
colnames(app_data)[colnames(app_data)=="rep_adv_results"] <- "Actual"
colnames(app_data)[colnames(app_data)=="district_full"] <- "Race"


v_options <- c("Percent of sample that's college educated" = "per_college_ed", 
                      "Percent of sample that's between 18 and 25" = "per_young",
                      "Percent of sample that's hispanic" = "per_hispanic", 
                      "Percent of sample that's black" = "per_black", 
                      "Percent of sample that's white" = "per_white",
                      "Percent of sample that's not white" = "per_nonwhite",
                      "Percent of sample who are likely voters" = "per_likely",
                      "Percent of sample who voted early" = "per_early")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualizing NYT Polling Accuracy"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "variable",
                    label = "Select the demographic to consider",
                    choices = v_options,
                    multiple = FALSE, 
                    selected = v_options[1])),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput(outputId = "scatterplot1"),
        plotOutput(outputId = "scatterplot2"),
        textOutput(outputId = "stats"),
        h3("Source"),
        p("The New York Times Upshot/Sienna Poll and The New York Times Election Results Coverage"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$scatterplot1 <- renderPlotly({
    ggplotly(tooltip = c("text", "x", "y"),
             ggplot(data = app_data, 
                    aes_string(x = input$variable, y = "accuracy", text = "Race")) + 
               geom_point(aes_string(color = "Party")) +
               labs(x = names(v_options[which(v_options == input$variable)]), 
                    y = "Poll accuracy", title = "Considering sample demographics and poll accuracy: Scatterplot") +
               theme(text = element_text(size = 10), 
                     axis.text.y = element_text(angle = 90, hjust = 1)) +
               scale_color_discrete(name = "Winning party")) %>% 
                config(displayModeBar = FALSE) })
  
  output$scatterplot2 <- renderPlot({
    ggplot(data = app_data, aes_string(x = input$variable, y = "accuracy")) +
      geom_smooth(method = "lm") +
      labs(x = names(v_options[which(v_options == input$variable)]), 
           y = "Poll accuracy",
           title = "Considering sample demographics and poll accuracy: Linear regression")
  })

  output$stats <- renderText({
  
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)


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


v_options <- c("Percent college educated" = "per_college_ed", 
                      "Percent between 18 and 25" = "per_young",
                      "Percent hispanic" = "per_hispanic", 
                      "Percent black" = "per_black", 
                      "Percent white" = "per_white",
                      "Percent not white" = "per_nonwhite",
                      "Percent likely voters" = "per_likely",
                      "Percent early voters" = "per_early")


na_zero <- function (x) {
  for (i in seq_along(x)) {
    x[is.na(x)] <- 0
    return(x)
  }
}


# NA's here represent 0

app_data$per_early <- na_zero(app_data$per_early)



ui <- fluidPage(fluidPage(theme = shinytheme("cerulean")),
   
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
        
        tabsetPanel(type = "tabs",
                    tabPanel("Scatterplot", plotlyOutput("scatterplot1")),
                    tabPanel("Linear regression plot", plotOutput("scatterplot2")),
                    tabPanel("Model details", textOutput("stats"))),
        
        h3("Summary"),
        p("This application allows the user to see how demographic differences among polls' samples does/does not correlate with the polls accuracy. "),
        h3("Source"),
        p("The New York Times Upshot/Sienna Poll and The New York Times Election Results Coverage"))))
        
server <- function(input, output) {
   
  output$scatterplot1 <- renderPlotly({
    ggplotly(tooltip = c("text", "x", "y"),
             ggplot(data = app_data, 
                    aes_string(x = input$variable, y = "accuracy", text = "Race")) + 
               geom_point(aes_string(color = "Party")) +
               labs(x = names(v_options[which(v_options == input$variable)]), 
                    y = "Poll accuracy", title = "Sample demographics and poll accuracy") +
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

  output$stats <- renderPrint({
    my_formula <- paste0("accuracy ~ ", input$variable)
    m1 <- summary(lm(my_formula, data = app_data))
  print(m1)
  
})
  
}

# Run the application 
shinyApp(ui = ui, server = server)


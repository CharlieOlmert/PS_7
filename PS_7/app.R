# Load libraries 

library(shiny)
library(tidyverse)
library(stringr)
library(rsconnect)
library(shinythemes)
library(plotly)
library(scales)
library(stargazer)

# Load data from the rds
app_data <- read_rds("shiny_data.rds") 

# Change D and R to be Democratic and Republican

app_data <- app_data %>%
  mutate(Party = case_when(
    Party == "D" ~ "Democratic", 
    Party == "R" ~ "Republican"
  )) 

# Change the variable name so that it looks nicer with tooltip in ggplotly

colnames(app_data)[colnames(app_data)=="district_full"] <- "Race"


# Create a vector of variable labels to use on the x axis and as choices in the drop down menu

v_options <- c("Percent college educated" = "per_college_ed", 
                      "Percent between 18 and 34" = "per_young",
                      "Percent hispanic" = "per_hispanic", 
                      "Percent black" = "per_black", 
                      "Percent white" = "per_white",
                      "Percent not white" = "per_nonwhite",
                      "Percent likely voters" = "per_likely",
                      "Percent early voters" = "per_early")

# Create a vector of variable labels to use in the plot titles

title_options <- c("sample share that's college educated" = "per_college_ed", 
               "sample share ages 18 to 34" = "per_young",
               "sample share that's hispanic" = "per_hispanic", 
               "sample share that's black" = "per_black", 
               "sample share that's white" = "per_white",
               "sample share that's not white" = "per_nonwhite",
               "sample share of likely voters" = "per_likely",
               "sample share of early voters" = "per_early")

# Revive the na_zero function to replace NAs with 0s in the per early voting column.

na_zero <- function (x) {
  for (i in seq_along(x)) {
    x[is.na(x)] <- 0
    return(x)
  }
}


app_data$per_early <- na_zero(app_data$per_early)


# Define the UI
# Use a pretty theme

ui <- fluidPage(fluidPage(theme = shinytheme("united")),
   
   # Application title
   
   titlePanel("Visualizing NYT Polling Accuracy"),
   
   # Sidebar with a select input function, letting users chose the sample demographic to analyze 
   
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "variable",
                    label = "Select the sample demographic to analyze",
                    choices = v_options,
                    multiple = FALSE, 
                    selected = v_options[1]),
        
        # And a button allowing users to download my data and further poke around if they wish 
        
        downloadButton(outputId = "download_data", 
                       label = "Download data")),
      
      # Define the main panel
      
      mainPanel(
        
        # Use a tab layout to separate the various elements
        
        tabsetPanel(type = "tabs",
                    tabPanel("About this app", htmlOutput("about")),
                    tabPanel("Scatterplot", plotlyOutput("scatterplot1")),
                    tabPanel("Linear regression plot", plotOutput("scatterplot2")),
                    tabPanel("Regression summary", htmlOutput("stats")))
        
      )))

# Define the server
        
server <- function(input, output) {

# Create a reactive that changes the printed text from "weak positive" to "weak negative" when the
# coefficient changes accordingly
# and that changes the printed text from "is significant" to "is not significant" when 
# the p-value changes accordingly 
    
weak_strong <- reactive({
  my_formula <- paste0("accuracy ~ ", input$variable)
  m1 <- summary(lm(my_formula, data = app_data))
  
  if (m1$coefficients[2] > 0 ) {
    weak_strong <- "weak positive"
  } else {
    weak_strong <- "weak negative"
  }
  
})
  
is_sig  <- reactive({
  my_formula <- paste0("accuracy ~ ", input$variable)
  m1 <- summary(lm(my_formula, data = app_data))
  fstat <- m1$fstatistic 
  pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = F)
  
  if (pval < .05) { 
    is_sig <- "is"
  } else {
      is_sig <- "is not"
    }})


# Define the output for the download data button
   
output$download_data <- downloadHandler(
  filename = "data.csv",
  content = function(file) {
    write.csv(app_data, file)
  }
)


# Define the output for the interactive scatterplot
# Use Plotly and tooltip to allow users to mouse over the points for more information
# Plot accuracy on the y axis and the user-selected variable on the x-axis
# Color the points by party and make the axis labels and title reactive
# Add some formatting for the plot size and text
# I cannot change the position of the legend. This a Plotly bug.
# Remove the ugly Plotly display bar
  
output$scatterplot1 <- renderPlotly({
    ggplotly(tooltip = c("text"),
             ggplot(data = app_data, 
                    aes_string(x = input$variable, y = "accuracy", text = "Race")) + 
               geom_point(aes_string(color = "Party")) +
               labs(x = names(v_options[which(v_options == input$variable)]), 
                    y = "Poll accuracy", 
                    title = paste("Poll accuracy and", names(title_options[which(v_options == input$variable)]))) +
               scale_color_manual(name = "Winning party", values = c("blue", "red"))) %>% 
                config(displayModeBar = FALSE)}) 

# Define the output for the regression plot
# Use geom smooth to plot the regression line for accuracy and the user selected variable
# Make the axis labels reactive
# Add a reactive title

output$scatterplot2 <- renderPlot({
    ggplot(data = app_data, aes_string(x = input$variable, y = "accuracy")) +
      geom_smooth(method = "lm") +
     geom_point(alpha = 0.5) +
      labs(x = names(v_options[which(v_options == input$variable)]), 
           y = "Poll accuracy",
           title = paste("Regressing accuracy against", names(title_options[which(v_options == input$variable)])))
  })


# Define the summary text output
# Regress accuracy against the user-selected variable
# Save the summary of the model and extract the p-value from the model
# Create a reactive text ouput in which the 1) r squared, p value, and significance explanation change 
# in response to the user selected variable 

output$stats <- renderPrint({
    my_formula <- paste0("accuracy ~ ", input$variable)
    m0 <- (lm(my_formula, data = app_data))
    m1 <- summary(m0)
    fstat <- m1$fstatistic 
    pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = F)
    
    
    HTML(paste(tags$ul(
               tags$li("The correlation coefficient is: ", strong(m1$coefficients[2]), 
               ". This is the slope of the regression line and means that the variables have a ", weak_strong(), " relationship."),
               tags$li("The multiple r-squared is: ", strong(m1$r.squared), 
                       ". This means that roughly ", round((m1$r.squared)*100, digits = 2), "percent of the variation is explained by this variable." ),
               tags$li("The p-value is: ", strong(pval), ". This means that the result ", is_sig(), " statistically significant
                       with respect to a significance level of 0.05."))))

    
})
  
output$about <- renderUI({
  
  # Provide users with a summary of the application and instructions
  # Provide users with information on the data source
  
  str1 <- paste("Summary")
  str2 <- paste("This application allows the user to see how demographic differences among polls' samples do/do not correlate with the polls accuracy. 
    Accuracy is the difference between the predicted Republican advantage and the actual Republican advantage subtracted from 100. 
                The application includes polling data from 65 total races, including 5 Senate races, one governor race, and 59 House races.")
 str3 <- paste("Instructions") 
 str4 <- paste("Click through the above tabs to visualize the data in different ways and use the drop-down menu to toggle between demographic sample variables to explore.
                Feel free to download your own copy of the data using the button on the left.")
  str5 <- paste("Source")
  str6 <- paste("The New York Times Upshot/Sienna Poll and The New York Times Election Results Coverage")
  
  HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})}

# Run the application 
shinyApp(ui = ui, server = server)


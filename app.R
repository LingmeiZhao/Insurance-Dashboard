library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(cowplot)

path <- here('data', 'Auto_Insurance_Claims_Sample.csv')
df <- read_csv(path)

new_df1 <- df %>% group_by(`Claim Reason`) %>% summarise(n())


ui <- dashboardPage(
  dashboardHeader(title = "Insurance Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Prediction", tabName = "pediction")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("gender"),
                valueBoxOutput("average"),
                valueBoxOutput("costomers")
              ),
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Bar Plot of Employment Status of Customers in Different States",
                  plotOutput("barPlot", width = "100%", height = 500)
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Distribution of Claim Amount",
                  plotOutput("histPlot", width = "100%", height = 500)
                ),
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Ratio of Claim Reason",
                  plotOutput("circlePlot", width = "100%", height = 500)
                )
              ),
              fluidRow(
                box(width = 4,
                    selectInput("status",
                                label = "Choose a Employment Status to Display",
                                choices =c("Display All",
                                           "Disabled",
                                           "Employed",
                                           "Medical Leave",
                                           "Retired",
                                           "Unemployed"),
                                selected = "Display All")
                ),
                box(width = 4,
                    sliderInput(inputId = "bins",
                                label = "Monthes Since Last Claim",
                                min = 0,
                                max = 35,
                                value = 14)
                )
              )
      ),
      tabItem("pediction",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)

server <- function(input, output) {
  
  output$gender <- renderValueBox({
    female <- df %>%  group_by(Gender) %>% summarise(n())
    
    valueBox(
      value = "Female: 4658;  Male: 4476",
      subtitle = "Gender Distribution of Customers",
      icon = icon("balance-scale")
    )
  })
  
  output$average <- renderValueBox({
    valueBox(
      value = round(mean(df$`Claim Amount`), 2),
      subtitle = "Average Claim Amount",
      icon = icon("algolia")
    )
  })
  
  output$costomers <- renderValueBox({
    valueBox(
      value = nrow(df),
      "Number of Customers",
      icon = icon("users")
    )
  })
  
  output$histPlot <- renderPlot({
    data <- df %>% filter(`Months Since Last Claim` == input$bins) %>% select(`Claim Amount`)
    # bins <- seq(min(data), max(data), length.out = input$bins + 1)
    hist(data$`Claim Amount`, breaks = 20,  col = "#75AADB", 
         main = "Histogram of Claim Amount")
    abline(v = mean(df$`Claim Amount`), col = "red", lwd = 1)
    abline(v = median(df$`Claim Amount`), col = "darkgreen", lwd = 1)
    legend(x = "topright", # location of legend within plot area
           c("Mean", "Median"),
           col = c("red", "darkgreen"),
           lwd = c(2, 2))
  })
  
  output$barPlot <- renderPlot({
    new_df <- df %>% group_by(State, Gender) %>% count(EmploymentStatus)
    
    color <- switch(input$status, 
                    "Disabled" = "black",
                    "Employed" = "darkgreen",
                    "Medical Leave" = "darkorange",
                    "Retired"  = "blue",
                    "Unemployed" = "darkviolet")
    
    if(input$status == "Display All"){
       new_df %>% ggplot(aes(x = State, y = n, fill = Gender)) +
        geom_col(width = 0.7, alpha = 0.8) +
        facet_wrap(~EmploymentStatus, ncol = 2) + coord_flip() +
        scale_y_continuous(
          expand = expand_scale(mult = c(0, 0.05))) +
        theme_minimal_hgrid()
    }else{
      new_df %>% filter(EmploymentStatus == input$status) %>% ggplot(aes(x = State, y = n)) + 
        geom_col(width = 0.7, alpha = 0.8, fill = color) + coord_flip() +
        scale_y_continuous(
          expand = expand_scale(mult = c(0, 0.05))) + 
        theme_minimal_hgrid() +
        labs(x = "Employment Status",
             y = "Counts")
    }
  })

  output$circlePlot <- renderPlot({
    new_df1 <- df %>% group_by(`Claim Reason`) %>% summarise(reasons_number = n()) %>% 
      arrange(desc(reasons_number)) %>% mutate(p = 100*(reasons_number / sum(reasons_number)),
                                               label = str_c(round(p, 1), '%'))
    
    ggplot(new_df1, aes(x = "", y = reasons_number, fill = `Claim Reason`)) +
      geom_col(width = 0.7, alpha = 0.8) +
      coord_polar(theta = "y") +
      geom_text(aes(x ="" , y = reasons_number, label = label),
                color = "white", size = 6, position = position_stack(vjust = 0.5)) +
      theme_map() +
      labs(x = NULL,
           y = NULL,
           fill = "Claim Reason",
           title = "Number of Claims by Claim Reason")
  })
  
  
}


shinyApp(ui, server)

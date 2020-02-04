library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(cowplot)

path <- here('data', 'Auto_Insurance_Claims_Sample.csv')
df <- read_csv(path)

new_df <- df %>% group_by(State) %>% count(EmploymentStatus) 
new_df <- new_df %>% spread(key = "EmploymentStatus", value = n)
new_df <- new_df %>% group_by(State) %>%
  mutate(Disabled_percent = 100 * Disabled / (Disabled + Employed + `Medical Leave` + Retired + Unemployed))
new_df <- new_df %>% group_by(State) %>%
  mutate(Employed_percent = 100 * Employed / (Disabled + Employed + `Medical Leave` + Retired + Unemployed))

new_df <- new_df %>% group_by(State) %>%
  mutate(Med_leave_percent = 100 * `Medical Leave` / (Disabled + Employed + `Medical Leave` + Retired + Unemployed))

new_df <- new_df %>% group_by(State) %>%
  mutate(Retired_percent = 100 * Retired / (Disabled + Employed + `Medical Leave` + Retired + Unemployed))

new_df <- new_df %>% group_by(State) %>%
  mutate(Unemployed_percent = 100 * Unemployed / (Disabled + Employed + `Medical Leave` + Retired + Unemployed))

new_df$Disabled_percent <- round(new_df$Disabled_percent, 2)
new_df$Employed_percent <- round(new_df$Employed_percent, 2)
new_df$Med_leave_percent <- round(new_df$Med_leave_percent, 2)
new_df$Retired_percent <- round(new_df$Retired_percent, 2)
new_df$Unemployed_percent <- round(new_df$Unemployed_percent, 2)

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
                                choices =c("Disabled",
                                           "Employed",
                                           "Medical Leave",
                                           "Retired",
                                           "Unemployed"),
                                selected = "Employed")
                ),
                box(width = 4,
                    sliderInput(inputId = "bins",
                                label = "Number of bins:",
                                min = 5,
                                max = 50,
                                value = 30)
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
    data <- df$`Claim Amount`
    bins <- seq(min(data), max(data), length.out = input$bins + 1)
    hist(data, breaks = bins,  col = "#75AADB", 
         xlabs = "Claim Amount of Every Customer",
         main = "Histogram of Claim Amount")
    abline(v = mean(df$`Claim Amount`), col = "red", lwd = 1)
    abline(v = median(df$`Claim Amount`), col = "darkgreen", lwd = 1)
    legend(x = "topright", # location of legend within plot area
           c("Mean", "Median"),
           col = c("red", "darkgreen"),
           lwd = c(2, 2))
  })
  
  output$barPlot <- renderPlot({
    data <- switch (input$status,
                    "Disabled" = new_df$Disabled,
                    "Employed" = new_df$Employed,
                    "Medical Leave" = new_df$`Medical Leave`,
                    "Retired" = new_df$Retired,
                    "Unemployed" = new_df$Unemployed
    )
    
    color <- switch(input$status, 
                    "Disabled" = "black",
                    "Employed" = "darkgreen",
                    "Medical Leave" = "darkorange",
                    "Retired"  = "blue",
                    "Unemployed" = "darkviolet")
    
    legend <- switch(input$status, 
                     "Disabled" = "Disabled",
                     "Employed" = "Employed",
                     "Medical Leave" = "Medical Leave",
                     "Retired" = "Retired",
                     "Unemployed" = "Unemployed")
    
    
    new_df %>% ggplot(aes(x = State, y = data)) + 
      geom_bar(fill = color, colour = "black", stat = "identity") +
      labs(x = "States",
           y = "Number of Customers") +
      theme() +
      theme_half_open() 
  })
  
  output$circlePlot <- renderPlot({
    myLabel <- as.vector(new_df1$`Claim Reason`)
    myLabel <- paste(myLabel, "(", round(new_df1$`n()`/sum(new_df1$`n()`) * 100, 2), "%) ", sep = "")
    
    ggplot(new_df1, aes(x = "", y = `n()`, fill = `Claim Reason`)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme(axis.ticks = element_blank()) +
      theme(legend.title = element_blank(), legend.position = "top") +
      scale_fill_discrete(breaks = new_df1$`Claim Reason`, labels = myLabel) 
  })
}


shinyApp(ui, server)

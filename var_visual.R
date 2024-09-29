# Load necessary libraries
library(shiny)
library(ggplot2)    # For creating data visualizations
library(dplyr)      # For data manipulation
library(tidyr)      # For reshaping data

# Load data
data <- read.csv("customer_churn.csv")

# Data preprocessing
data$Churn <- factor(data$Churn, levels = c("No", "Yes"))  # Convert Churn to a factor
data$SeniorCitizen <- as.factor(data$SeniorCitizen)  # Convert SeniorCitizen to a factor
data$TotalCharges <- as.numeric(as.character(data$TotalCharges))  # Convert TotalCharges to numeric


ui <- fluidPage(
  titlePanel(
    tags$div(
      style = "text-align: center; background-color: #4CAF50; padding: 15px; border-radius: 5px;", 
      HTML("&#x1F4C8; Customer Churn Analysis Dashboard &#x1F4C8;")
    )
  ),
  br(),
  tags$head(
    tags$style(HTML("
      .tab-pane { 
        background-color: #FFFF99; 
        border-radius: 5px; 
        padding: 15px; 
      }
      .nav-tabs {
        border-bottom: 2px solid #007bff;
      }
      .nav-tabs .nav-link {
        background-color: white; /* Default tab color */
        color: #333;
        border: 1px solid transparent; 
        border-radius: 5px 5px 0 0; 
        margin-bottom: -1px; 
        transition: background-color 0.3s, color 0.3s;
      }
      .nav-tabs .nav-link:hover {
        background-color: #707070; 
        color: black;
      }
      .nav-tabs .nav-item.show .nav-link, 
      .nav-tabs .nav-link.active {
        background-color: orange !important; /* Selected tab background color */
        color: white !important; /* Ensure text color is white */
        font-weight: bold;
      }
      .tab-content {
        border: 2px solid #007bff;
        border-radius: 5px;
        margin-top: -1px; 
        background-color: white; 
        box-shadow: 0px 0px 5px rgba(0, 0, 0, 0.1);
      }
      .typing-animation {
        font-size: 16px; 
        padding: 15px; 
        white-space: nowrap;
        overflow: hidden; 
        border-right: 2px solid black; 
        width: 0; 
        animation: typing 5s steps(30, end) forwards, blink-caret 0.75s step-end infinite;
      }
      @keyframes typing {
        from { width: 0; }
        to { width: 100%; }
      }
      @keyframes blink-caret {
        from, to { border-color: transparent; }
        50% { border-color: black; }
      }
    "))
  ),
  tabsetPanel(
    # Churn Rate Overview
    tabPanel("Churn Rate Overview", br(),
             selectInput("chart_type_overview", "Select Chart Type:",
                         choices = c("Bar Graph", "Line Graph")),
             plotOutput("churn_rate")
    ),
    
    # Churn by Internet Service
    tabPanel("Churn by Internet Service", br(),
             selectInput("chart_type_internet", "Select Chart Type:",
                         choices = c("Bar Graph", "Line Graph")),
             plotOutput("churn_internet_service")
    ),
    
    # Churn by Contract Type
    tabPanel("Churn by Contract Type", br(),
             selectInput("chart_type_contract", "Select Chart Type:",
                         choices = c("Pie Chart", "Bar Graph")),
             plotOutput("churn_contract")
    ),
    
    # Churn by Payment Method
    tabPanel("Churn by Payment Method", br(),
             selectInput("chart_type_payment", "Select Chart Type:",
                         choices = c("Bar Graph", "Line Graph")),
             plotOutput("churn_payment")
    ),
    
    # Churn by Tenure
    tabPanel("Churn by Tenure", br(),
             selectInput("chart_type_tenure", "Select Chart Type:",
                         choices = c("Histogram", "Line Graph")),
             plotOutput("churn_tenure")
    ),
    
    # Churn by Senior Citizen
    tabPanel("Churn by Senior Citizen", br(),
             selectInput("chart_type_senior", "Select Chart Type:",
                         choices = c("Pie Chart", "Bar Graph")),
             plotOutput("churn_senior")
    ),
    
    # Churn by Tenure and Monthly Charges
    tabPanel("Churn by Tenure and Monthly Charges", br(),
             selectInput("chart_type_tenure_monthly", "Select Chart Type:",
                         choices = c("Scatter Plot", "Line Graph")),
             plotOutput("churn_tenure_monthly_scatter")
    ),
    tabPanel("Customer Demographics Analysis", br(),
             plotOutput("customer_demographics")
    ),
    # Updated Conclusion Tab
    tabPanel("Conclusion", br(),
             tags$div(
               style = "font-size: 16px; padding: 15px; background-color: #FFFDD0;",  # Cream background color
               h2(style = "font-weight: bold;", "Conclusion of the Customer Churn Analysis"),  # h2 heading, bold
               tags$div(
                 class = "typing-animation",  # Apply typing animation to the paragraphs
                 p("The customer churn analysis reveals key insights into the factors affecting customer retention. Overall, the following conclusions can be drawn:"),
                 p("1. ", strong("Churn Rate:"), " A significant portion of customers have opted to leave the service, highlighting the need for strategies to improve customer satisfaction."),
                 p("2. ", strong("Service Impact:"), " Customers utilizing certain internet services tend to have higher churn rates, indicating potential service quality issues or pricing concerns."),
                 p("3. ", strong("Contract Types:"), " Customers on month-to-month contracts exhibit higher churn compared to those on longer-term contracts, suggesting that promoting long-term plans may improve retention."),
                 p("4. ", strong("Payment Methods:"), " There is a noticeable variation in churn rates based on payment methods, emphasizing the importance of aligning payment options with customer preferences."),
                 p("5. ", strong("Demographic Factors:"), " Senior citizens and customers with dependents are more likely to churn, pointing to the need for tailored engagement strategies for these segments."),
                 p("In conclusion, addressing the identified factors through improved customer service, tailored marketing strategies, and service enhancements could help reduce churn rates and improve overall customer satisfaction."),
                 
                 h2(style = "font-weight: bold;", "Suggestions for Improvement"),
                 p("Based on these findings, the following steps could be taken to improve customer retention:"),
                 p("1. ", strong("Improve Service Quality:"), " Address any service quality issues related to internet services, particularly those with higher churn rates, and consider reviewing pricing models."),
                 p("2. ", strong("Promote Long-Term Contracts:"), " Create incentives for customers to switch from month-to-month contracts to long-term contracts, such as discounts or bundled offers."),
                 p("3. ", strong("Diversify Payment Options:"), " Consider offering more user-friendly payment options (e.g., auto-pay, credit card payments) to reduce the impact of payment method preferences on churn."),
                 p("4. ", strong("Targeted Customer Engagement:"), " Develop personalized engagement strategies for senior citizens and customers with dependents, such as loyalty programs or personalized communication."),
                 p("5. ", strong("Customer Feedback Mechanism:"), " Implement a feedback system where customers can easily share concerns. Analyzing this feedback will allow for more proactive improvements to reduce churn.")
                 
               )
             )
    ),
    # CSS for typing animation
    tags$head(
      tags$style(HTML("
    .typing-animation {
      font-size: 16px; 
      white-space: nowrap;
      overflow: hidden;
      border-right: 2px solid black;
      width: 0;
      animation: typing 5s steps(100, end) forwards, blink-caret 0.75s step-end infinite;
    }
    @keyframes typing {
      from { width: 0; }
      to { width: 100%; }
    }
    @keyframes blink-caret {
      from, to { border-color: transparent; }
      50% { border-color: black; }
    }
  "))
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Churn Rate Overview
  output$churn_rate <- renderPlot({
    churn_summary <- data %>%
      group_by(Churn) %>%
      summarize(Count = n()) 
    
    if (input$chart_type_overview == "Bar Graph") {
      ggplot(churn_summary, aes(x = Churn, y = Count, fill = Churn)) +
        geom_bar(stat = "identity") +
        labs(title = "Churn Rate Overview (Bar Graph)", x = "Churn Status", y = "Count") +
        theme_minimal()
    } else {
      ggplot(churn_summary, aes(x = Churn, y = Count, group = 1, color = Churn)) +
        geom_line() +
        geom_point(size = 4) +
        labs(title = "Churn Rate Overview (Line Graph)", x = "Churn Status", y = "Count") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
          axis.title.x = element_text(size = 16),              # Increase x-axis title size
          axis.title.y = element_text(size = 16),              # Increase y-axis title size
          axis.text = element_text(size = 14),                 # Increase axis label size
          legend.text = element_text(size = 14)                # Increase legend text size
        )
    }
  })
  
  # Churn by Internet Service
  output$churn_internet_service <- renderPlot({
    churn_by_internet <- data %>%
      group_by(InternetService, Churn) %>%
      summarize(Count = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = Churn, values_from = Count, values_fill = 0)
    
    if (input$chart_type_internet == "Bar Graph") {
      ggplot(churn_by_internet, aes(x = InternetService, y = Yes, fill = InternetService)) +
        geom_bar(stat = "identity") +
        labs(title = "Churn by Internet Service (Bar Graph)", x = "Internet Service Type", y = "Churned Customers") +
        theme_minimal()
    } else {
      ggplot(churn_by_internet, aes(x = InternetService, y = Yes, group = 1)) +
        geom_line(aes(color = InternetService)) +
        geom_point(aes(color = InternetService), size = 4) +
        labs(title = "Churn by Internet Service (Line Graph)", x = "Internet Service Type", y = "Churned Customers") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
          axis.title.x = element_text(size = 16),              # Increase x-axis title size
          axis.title.y = element_text(size = 16),              # Increase y-axis title size
          axis.text = element_text(size = 14),                 # Increase axis label size
          legend.text = element_text(size = 14)                # Increase legend text size
        )
    }
  })
  
  # Churn by Contract Type
  output$churn_contract <- renderPlot({
    churn_by_contract <- data %>%
      group_by(Contract) %>%
      summarize(Count = sum(Churn == "Yes"))
    
    if (input$chart_type_contract == "Pie Chart") {
      ggplot(churn_by_contract, aes(x = "", y = Count, fill = Contract)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        labs(title = "Churn by Contract Type (Pie Chart)") +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"))
    } else {
      ggplot(churn_by_contract, aes(x = Contract, y = Count, fill = Contract)) +
        geom_bar(stat = "identity") +
        labs(title = "Churn by Contract Type (Bar Graph)", x = "Contract Type", y = "Churned Customers") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
          axis.title.x = element_text(size = 16),              # Increase x-axis title size
          axis.title.y = element_text(size = 16),              # Increase y-axis title size
          axis.text = element_text(size = 14),                 # Increase axis label size
          legend.text = element_text(size = 14)                # Increase legend text size
        )
    }
  })
  
  # Churn by Payment Method
  output$churn_payment <- renderPlot({
    churn_by_payment <- data %>%
      group_by(PaymentMethod, Churn) %>%
      summarize(Count = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = Churn, values_from = Count, values_fill = 0)
    
    if (input$chart_type_payment == "Bar Graph") {
      ggplot(churn_by_payment, aes(x = PaymentMethod, y = Yes, fill = PaymentMethod)) +
        geom_bar(stat = "identity") +
        labs(title = "Churn by Payment Method (Bar Graph)", x = "Payment Method", y = "Churned Customers") +
        theme_minimal()
    } else {
      ggplot(churn_by_payment, aes(x = PaymentMethod, y = Yes, group = 1)) +
        geom_line(aes(color = PaymentMethod)) +
        geom_point(aes(color = PaymentMethod), size = 4) +
        labs(title = "Churn by Payment Method (Line Graph)", x = "Payment Method", y = "Churned Customers") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
          axis.title.x = element_text(size = 16),              # Increase x-axis title size
          axis.title.y = element_text(size = 16),              # Increase y-axis title size
          axis.text = element_text(size = 14),                 # Increase axis label size
          legend.text = element_text(size = 14)                # Increase legend text size
        )
    }
  })
  
  # Churn by Tenure
  output$churn_tenure <- renderPlot({
    if (input$chart_type_tenure == "Histogram") {
      ggplot(data, aes(x = tenure, fill = Churn)) +
        geom_histogram(bins = 20, alpha = 0.7) +
        labs(title = "Churn by Tenure (Histogram)", x = "Tenure", y = "Count") +
        theme_minimal()
    } else {
      churn_by_tenure <- data %>%
        group_by(tenure, Churn) %>%
        summarize(Count = n())
      
      ggplot(churn_by_tenure, aes(x = tenure, y = Count, group = Churn, color = Churn)) +
        geom_line() +
        geom_point() +
        labs(title = "Churn by Tenure (Line Graph)", x = "Tenure", y = "Count") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
          axis.title.x = element_text(size = 16),              # Increase x-axis title size
          axis.title.y = element_text(size = 16),              # Increase y-axis title size
          axis.text = element_text(size = 14),                 # Increase axis label size
          legend.text = element_text(size = 14)                # Increase legend text size
        )
    }
  })
  
  # Churn by Senior Citizen
  output$churn_senior <- renderPlot({
    churn_by_senior <- data %>%
      group_by(SeniorCitizen) %>%
      summarize(Count = sum(Churn == "Yes"))
    
    if (input$chart_type_senior == "Pie Chart") {
      ggplot(churn_by_senior, aes(x = "", y = Count, fill = SeniorCitizen)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y") +
        labs(title = "Churn by Senior Citizen (Pie Chart)") +
        theme_void() +
        theme(plot.title = element_text(size = 20, face = "bold"))
    } else {
      ggplot(churn_by_senior, aes(x = SeniorCitizen, y = Count, fill = SeniorCitizen)) +
        geom_bar(stat = "identity") +
        labs(title = "Churn by Senior Citizen (Bar Graph)", x = "Senior Citizen Status", y = "Churned Customers") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
          axis.title.x = element_text(size = 16),              # Increase x-axis title size
          axis.title.y = element_text(size = 16),              # Increase y-axis title size
          axis.text = element_text(size = 14),                 # Increase axis label size
          legend.text = element_text(size = 14)                # Increase legend text size
        )
    }
  })
  
  # Churn by Tenure and Monthly Charges
  output$churn_tenure_monthly_scatter <- renderPlot({
    if (input$chart_type_tenure_monthly == "Scatter Plot") {
      ggplot(data, aes(x = tenure, y = MonthlyCharges, color = Churn)) +
        geom_point(alpha = 0.6) +
        labs(title = "Churn by Tenure and Monthly Charges (Scatter Plot)", x = "Tenure", y = "Monthly Charges") +
        theme_minimal()
    } else {
      churn_by_tenure_monthly <- data %>%
        group_by(tenure, Churn) %>%
        summarize(AvgMonthlyCharges = mean(MonthlyCharges))
      
      ggplot(churn_by_tenure_monthly, aes(x = tenure, y = AvgMonthlyCharges, group = Churn, color = Churn)) +
        geom_line() +
        geom_point() +
        labs(title = "Churn by Tenure and Monthly Charges (Line Graph)", x = "Tenure", y = "Average Monthly Charges") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
          axis.title.x = element_text(size = 16),              # Increase x-axis title size
          axis.title.y = element_text(size = 16),              # Increase y-axis title size
          axis.text = element_text(size = 14),                 # Increase axis label size
          legend.text = element_text(size = 14)                # Increase legend text size
        )
    }
  })
  
  # New: Customer Demographics Analysis visualization
  output$customer_demographics <- renderPlot({
    churn_by_demo <- data %>%
      group_by(gender, Partner, Dependents, Churn) %>%
      summarize(Count = n()) %>%
      ungroup()
    
    ggplot(churn_by_demo, aes(x = gender, y = Count, fill = Churn)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ Partner + Dependents) +
      labs(title = "Churn by Demographics", x = "Gender", y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"), 
        axis.title.x = element_text(size = 16),              
        axis.title.y = element_text(size = 16),             
        axis.text = element_text(size = 14),                
        legend.text = element_text(size = 14)             
      )
  })
}

# Run the Shiny app 
shinyApp(ui = ui, server = server)

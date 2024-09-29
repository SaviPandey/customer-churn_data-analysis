# Load necessary libraries
library(shiny)      
library(ggplot2)    # For creating data visualizations
library(dplyr)      # For data manipulation
library(tidyr)      # For reshaping data

data <- read.csv("customer_churn.csv")  # Update with your file path

# Data preprocessing
data$Churn <- factor(data$Churn, levels = c("No", "Yes"))  # Convert Churn to a factor
data$SeniorCitizen <- as.factor(data$SeniorCitizen)  # Convert SeniorCitizen to a factor

# Convert TotalCharges to numeric (handling non-numeric issues)
data$TotalCharges <- as.numeric(as.character(data$TotalCharges))

# Define the user interface (UI) for the Shiny app
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
    # Create multiple tabs for different visualizations
    tabPanel("Churn Rate Overview", br(),
             plotOutput("churn_rate")
    ),
    tabPanel("Churn by Internet Service", br(),
             plotOutput("churn_internet_service")
    ),
    tabPanel("Churn by Contract Type", br(),
             plotOutput("churn_contract")
    ),
    tabPanel("Churn by Payment Method", br(),
             plotOutput("churn_payment")
    ),
    tabPanel("Churn by Tenure", br(),
             plotOutput("churn_tenure")
    ),
    tabPanel("Churn by Senior Citizen Status", br(),
             plotOutput("churn_senior")
    ),
    tabPanel("Churn by Tenure and Monthly Charges", br(),
             plotOutput("churn_tenure_monthly")
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

# Define the server logic for the Shiny app
server <- function(input, output) {
  
  # Ensure data is properly processed before use
  data <- data %>%
    mutate(
      tenure = as.numeric(tenure),  # Ensure tenure is numeric
      MonthlyCharges = as.numeric(MonthlyCharges),  # Ensure MonthlyCharges is numeric
      TotalCharges = as.numeric(gsub(" ", "", TotalCharges))  # Convert TotalCharges to numeric, handle blanks
    )
  
  # Churn rate overview visualization
  output$churn_rate <- renderPlot({
    churn_summary <- data %>%
      group_by(Churn) %>%
      summarize(Count = n()) 
    
    ggplot(churn_summary, aes(x = Churn, y = Count, fill = Churn)) +
      geom_bar(stat = "identity") +
      labs(title = "Churn Rate Overview", x = "Churn Status", y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
        axis.title.x = element_text(size = 16),              # Increase x-axis title size
        axis.title.y = element_text(size = 16),              # Increase y-axis title size
        axis.text = element_text(size = 14),                 # Increase axis label size
        legend.text = element_text(size = 14)                # Increase legend text size
      )
  })
  
  # Churn by Internet Service visualization
  output$churn_internet_service <- renderPlot({
    churn_by_internet <- data %>%
      group_by(InternetService, Churn) %>%
      summarize(Count = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = Churn, values_from = Count, values_fill = 0)
    
    ggplot(churn_by_internet, aes(x = InternetService, y = Yes, fill = InternetService)) +
      geom_bar(stat = "identity") +
      labs(title = "Churn by Internet Service", x = "Internet Service Type", y = "Churned Customers") +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
        axis.title.x = element_text(size = 16),              # Increase x-axis title size
        axis.title.y = element_text(size = 16),              # Increase y-axis title size
        axis.text = element_text(size = 14),                 # Increase axis label size
        legend.text = element_text(size = 14)                # Increase legend text size
      )
  })
  
  # Churn by Contract Type visualization
  output$churn_contract <- renderPlot({
    churn_by_contract <- data %>%
      group_by(Contract, Churn) %>%
      summarize(Count = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = Churn, values_from = Count, values_fill = 0)
    
    ggplot(churn_by_contract, aes(x = Contract, y = Yes, fill = Contract)) +
      geom_bar(stat = "identity") +
      labs(title = "Churn by Contract Type", x = "Contract Type", y = "Churned Customers") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
        axis.title.x = element_text(size = 16),              # Increase x-axis title size
        axis.title.y = element_text(size = 16),              # Increase y-axis title size
        axis.text = element_text(size = 14),                 # Increase axis label size
        legend.text = element_text(size = 14)                # Increase legend text size
      )
  })
  
  # Churn by Payment Method visualization
  output$churn_payment <- renderPlot({
    churn_by_payment <- data %>%
      group_by(PaymentMethod, Churn) %>%
      summarize(Count = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = Churn, values_from = Count, values_fill = 0)
    
    ggplot(churn_by_payment, aes(x = PaymentMethod, y = Yes, fill = PaymentMethod)) +
      geom_bar(stat = "identity") +
      labs(title = "Churn by Payment Method", x = "Payment Method", y = "Churned Customers") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
        axis.title.x = element_text(size = 16),              # Increase x-axis title size
        axis.title.y = element_text(size = 16),              # Increase y-axis title size
        axis.text = element_text(size = 14),                 # Increase axis label size
        legend.text = element_text(size = 14)                # Increase legend text size
      )
  })
  
  # Churn by Tenure visualization
  output$churn_tenure <- renderPlot({
    data %>%
      mutate(TenureCategory = cut(tenure, breaks = c(-1, 12, 24, 36, 48, 60), 
                                  labels = c("0-1 Year", "1-2 Years", "2-3 Years", "3-4 Years", "4+ Years"))) %>%
      group_by(TenureCategory, Churn) %>%
      summarize(Count = n()) %>%
      ungroup() %>%
      ggplot(aes(x = TenureCategory, y = Count, fill = Churn)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Churn by Tenure", x = "Tenure Category", y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
        axis.title.x = element_text(size = 16),              # Increase x-axis title size
        axis.title.y = element_text(size = 16),              # Increase y-axis title size
        axis.text = element_text(size = 14),                 # Increase axis label size
        legend.text = element_text(size = 14)                # Increase legend text size
      )
  })
  
  # Churn by Senior Citizen Status visualization
  output$churn_senior <- renderPlot({
    churn_by_senior <- data %>%
      group_by(SeniorCitizen, Churn) %>%
      summarize(Count = n()) %>%
      ungroup()
    
    ggplot(churn_by_senior, aes(x = SeniorCitizen, y = Count, fill = Churn)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Churn by Senior Citizen Status", x = "Senior Citizen", y = "Count") +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
        axis.title.x = element_text(size = 16),              # Increase x-axis title size
        axis.title.y = element_text(size = 16),              # Increase y-axis title size
        axis.text = element_text(size = 14),                 # Increase axis label size
        legend.text = element_text(size = 14)                # Increase legend text size
      )
  })
  
  # Churn by Tenure and Monthly Charges visualization
  output$churn_tenure_monthly <- renderPlot({
    data %>%
      mutate(TenureGroup = cut(tenure, breaks = c(0, 12, 24, 36, 48, 60), 
                               labels = c("0-1 Year", "1-2 Years", "2-3 Years", "3-4 Years", "4+ Years"))) %>%
      group_by(TenureGroup, Churn) %>%
      summarize(AverageMonthlyCharges = mean(MonthlyCharges, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(x = TenureGroup, y = AverageMonthlyCharges, fill = Churn)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Churn by Tenure and Monthly Charges", x = "Tenure Group", y = "Average Monthly Charges") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
        axis.title.x = element_text(size = 16),              # Increase x-axis title size
        axis.title.y = element_text(size = 16),              # Increase y-axis title size
        axis.text = element_text(size = 14),                 # Increase axis label size
        legend.text = element_text(size = 14)                # Increase legend text size
      )
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
        plot.title = element_text(size = 20, face = "bold"),  # Increase plot title size
        axis.title.x = element_text(size = 16),              # Increase x-axis title size
        axis.title.y = element_text(size = 16),              # Increase y-axis title size
        axis.text = element_text(size = 14),                 # Increase axis label size
        legend.text = element_text(size = 14)                # Increase legend text size
      )
  })
}

# Run the Shiny application 
shinyApp(ui = ui, server = server)

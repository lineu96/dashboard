library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(dplyr)
library(plotly)

# Function to generate sample data
generate_sample_data <- function(n = 100) {
  data.frame(
    Date = seq.Date(as.Date("2023-01-01"), by = "day", length.out = n),
    Category = sample(c("Electronics", "Clothing", "Books", "Home & Garden", "Toys"), n, replace = TRUE),
    Sales = round(runif(n, 1000, 20000), 0),
    Customers = round(runif(n, 100, 2000), 0),
    Growth = round(runif(n, -5, 15), 1)
  )
}

# Generate initial sample data
sample_data <- generate_sample_data()

ui <- page_navbar(
  title = "My Shiny App",
  theme = bs_theme(version = 5),
  nav_panel(
    title = "Tab 1",
    div(
      style = "height: 800px; overflow-y: auto;",
      layout_sidebar(
        sidebar = sidebar(
          dateRangeInput("date_range1", "Date Range:", 
                         start = min(sample_data$Date), 
                         end = max(sample_data$Date)),
          selectInput("category_filter1", "Category:", 
                      choices = c("All", unique(sample_data$Category)),
                      selected = "All")
        ),
        layout_column_wrap(
          width = 1/4,
          heights_equal = "row",
          card(
            height = 250,
            card_header(icon("chart-line"), "Sales"),
            card_body(
              h2(textOutput("sales")),
              "Total sales this period"
            ),
            class = "bg-dark text-white"
          ),
          card(
            height = 250,
            card_header(icon("database"), "Customers"),
            card_body(
              h2(textOutput("customers")),
              "Active customers"
            ),
            style = "background-color: #FFA500;"
          ),
          card(
            height = 250,
            card_header(icon("cogs"), "Growth"),
            card_body(
              h2(textOutput("growth")),
              "Average growth"
            ),
            class = "bg-dark text-white"
          ),
          card(
            height = 250,
            card_header(icon("users"), "Engagement"),
            card_body(
              h2(textOutput("engagement")),
              "Sales per customer"
            ),
            style = "background-color: #FFA500;"
          )
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            height = 500,
            card_header("Sales Over Time"),
            card_body(
              tabsetPanel(
                tabPanel("Graph", plotlyOutput("plot1")),
                tabPanel("Table", DTOutput("table1"))
              )
            ),
            style = "background-color: #FFF0D9;" # Very light orange
          ),
          card(
            height = 500,
            card_header("Category Distribution"),
            card_body(
              tabsetPanel(
                tabPanel("Graph", plotlyOutput("plot2")),
                tabPanel("Table", DTOutput("table2"))
              )
            ),
            style = "background-color: #FFE4B5;" # Light orange (Moccasin)
          )
        ),
        card(
          height = 500,
          card_header("Sales vs Customers"),
          card_body(
            tabsetPanel(
              tabPanel("Graph", plotlyOutput("plot3")),
              tabPanel("Table", DTOutput("table3"))
            )
          ),
          style = "background-color: #FFEFD5;" # Papaya Whip (another light orange)
        )
      )
    )
  ),
  nav_panel(
    title = "Tab 2",
    div(
      style = "height: 800px; overflow-y: auto;",
      layout_sidebar(
        sidebar = sidebar(
          dateRangeInput("date_range2", "Date Range:", 
                         start = min(sample_data$Date), 
                         end = max(sample_data$Date)),
          selectInput("category_filter2", "Category:", 
                      choices = c("All", unique(sample_data$Category)),
                      selected = "All")
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            height = 500,
            card_header("Daily Growth"),
            card_body(
              tabsetPanel(
                tabPanel("Graph", plotlyOutput("plot4")),
                tabPanel("Table", DTOutput("table4"))
              )
            ),
            style = "background-color: #FFE4B5;" # Light orange (Moccasin)
          ),
          card(
            height = 500,
            card_header("Customer Distribution"),
            card_body(
              tabsetPanel(
                tabPanel("Graph", plotlyOutput("plot5")),
                tabPanel("Table", DTOutput("table5"))
              )
            ),
            style = "background-color: #FFF0D9;" # Very light orange
          )
        ),
        card(
          height = 500,
          card_header("Sales by Category"),
          card_body(
            tabsetPanel(
              tabPanel("Graph", plotlyOutput("plot6")),
              tabPanel("Table", DTOutput("table6"))
            )
          ),
          style = "background-color: #FFEFD5;" # Papaya Whip (another light orange)
        )
      )
    )
  )
)

server <- function(input, output) {
  # Filter data based on inputs
  filtered_data <- reactive({
    data <- sample_data
    
    if (input$category_filter1 != "All") {
      data <- data %>% filter(Category == input$category_filter1)
    }
    
    data %>% 
      filter(Date >= input$date_range1[1] & Date <= input$date_range1[2])
  })
  
  # Update card values
  output$sales <- renderText({
    total_sales <- sum(filtered_data()$Sales)
    paste0("$", format(total_sales, big.mark = ","))
  })
  
  output$customers <- renderText({
    total_customers <- sum(filtered_data()$Customers)
    format(total_customers, big.mark = ",")
  })
  
  output$growth <- renderText({
    avg_growth <- mean(filtered_data()$Growth)
    paste0(round(avg_growth, 1), "%")
  })
  
  output$engagement <- renderText({
    sales_per_customer <- sum(filtered_data()$Sales) / sum(filtered_data()$Customers)
    paste0("$", round(sales_per_customer, 2))
  })
  
  # Plots and tables for Tab 1
  output$plot1 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Date, y = Sales)) +
      geom_line(color = "#FFA500") +
      theme_minimal() +
      labs(title = "Sales Over Time", x = "Date", y = "Sales")
    ggplotly(p)
  })
  
  output$table1 <- renderDT({
    datatable(filtered_data() %>% select(Date, Sales), options = list(pageLength = 5))
  })
  
  output$plot2 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Category, y = Sales, fill = Category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Sales by Category", x = "Category", y = "Sales") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$table2 <- renderDT({
    datatable(filtered_data() %>% 
                group_by(Category) %>% 
                summarize(Total_Sales = sum(Sales)),
              options = list(pageLength = 5))
  })
  
  output$plot3 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Sales, y = Customers, text = paste("Date:", Date))) +
      geom_point(color = "#FFA500") +
      theme_minimal() +
      labs(title = "Sales vs Customers", x = "Sales", y = "Customers")
    ggplotly(p, tooltip = "text")
  })
  
  output$table3 <- renderDT({
    datatable(filtered_data() %>% select(Sales, Customers), options = list(pageLength = 5))
  })
  
  # Plots and tables for Tab 2
  output$plot4 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Date, y = Growth)) +
      geom_line(color = "#FFA500") +
      theme_minimal() +
      labs(title = "Daily Growth", x = "Date", y = "Growth (%)")
    ggplotly(p)
  })
  
  output$table4 <- renderDT({
    datatable(filtered_data() %>% select(Date, Growth), options = list(pageLength = 5))
  })
  
  output$plot5 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Category, y = Customers, fill = Category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Customers by Category", x = "Category", y = "Customers") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$table5 <- renderDT({
    datatable(filtered_data() %>% 
                group_by(Category) %>% 
                summarize(Total_Customers = sum(Customers)),
              options = list(pageLength = 5))
  })
  
  output$plot6 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Category, y = Sales, fill = Category)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Sales Distribution by Category", x = "Category", y = "Sales") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$table6 <- renderDT({
    datatable(filtered_data() %>% 
                group_by(Category) %>% 
                summarize(Min_Sales = min(Sales),
                          Avg_Sales = mean(Sales),
                          Max_Sales = max(Sales)),
              options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)

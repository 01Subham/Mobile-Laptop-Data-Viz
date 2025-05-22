library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)

# Load data
data <- read.csv("E:/DASHBOARD/mobile_sales_data.csv", stringsAsFactors = FALSE)
data$Inward.Date <- as.Date(data$Inward.Date, format = "%Y-%m-%d")
data$Week <- floor_date(data$Inward.Date, "week")

# Custom CSS
customCSS <- tags$head(tags$style(HTML("
  body {
    background-color: #1c1c1c;
    color: #f5f5f5;
  }
  .skin-blue .main-header .logo {
    background-color: #b30000;
    color: white;
    font-weight: bold;
    transition: background-color 0.3s ease-in-out;
  }
  .skin-blue .main-header .logo:hover {
    background-color: #e60000;
  }
  .skin-blue .main-header .navbar {
    background-color: #990000;
  }
  .skin-blue .main-header .navbar .sidebar-toggle:hover {
    background-color: #ff0000 !important;
  }
  .skin-blue .main-sidebar {
    background-color: #2c2c2c;
  }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li > a:hover {
    background-color: #b30000;
    color: white;
  }
  .box {
    background-color: #2e2e2e;
    border: 1px solid #ff4d4d;
    transition: box-shadow 0.3s ease-in-out, border-color 0.3s ease-in-out;
  }
  .box:hover {
    box-shadow: 0 0 20px 5px rgba(255, 77, 77, 0.8), 0 0 10px 2px rgba(255, 255, 255, 0.4);
    border-color: #ff9999;
  }
  .box-header {
    color: #ffcccc;
  }
  .content-wrapper {
    background-color: #1c1c1c;
  }
  .selectize-input {
    background-color: #333333 !important;
    color: white !important;
    border-color: #ff4d4d !important;
  }
  .selectize-dropdown-content .option {
    background-color: #2c2c2c;
    color: white;
  }
  .selectize-dropdown-content .option.active {
    background-color: #3366cc !important;
    color: white !important;
  }
  .selectize-input.items.has-items {
    background-color: #333333;
    color: white;
  }

  /* DataTable Dark Theme */
  table.dataTable {
    background-color: #1c1c1c !important;
    color: white !important;
  }
  table.dataTable thead th {
    background-color: #2e2e2e !important;
    color: #ffcccc !important;
  }
  table.dataTable tbody td {
    background-color: #1c1c1c !important;
    color: white !important;
  }
  .dataTables_wrapper .dataTables_filter input,
  .dataTables_wrapper .dataTables_length select {
    background-color: #333333 !important;
    color: white !important;
    border: 1px solid #ff4d4d !important;
  }
  .dataTables_wrapper .dataTables_paginate .paginate_button {
    color: white !important;
  }

  .team-box {
    background-color: #2e2e2e;
    border: 1px solid #ff4d4d;
    text-align: center;
    padding: 15px;
    transition: transform 0.3s ease-in-out, box-shadow 0.3s ease-in-out;
    color: white;
    border-radius: 10px;
  }
  .team-box:hover {
    transform: translateY(-5px);
    box-shadow: 0 0 20px 5px rgba(255, 77, 77, 0.8);
  }
  .team-img {
    width: 100px;
    height: 100px;
    border-radius: 50%;
    border: 2px solid #ff9999;
    margin-bottom: 10px;
  }

  /* Social media button hover */
  .navbar-nav > li > a:hover {
    color: #ff4d4d !important;
    background-color: transparent !important;
    transition: color 0.3s ease-in-out;
  }
")))

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Sales Dashboard",
    tags$li(class = "dropdown",
            tags$ul(class = "nav navbar-nav navbar-right",
                    tags$li(
                      a(href = "https://www.facebook.com", target = "_blank",
                        icon("facebook"), style = "color: white; padding: 15px; font-size: 18px;")
                    ),
                    tags$li(
                      a(href = "https://www.instagram.com", target = "_blank",
                        icon("instagram"), style = "color: white; padding: 15px; font-size: 18px;")
                    ),
                    tags$li(
                      a(href = "https://discord.com", target = "_blank",
                        icon("discord"), style = "color: white; padding: 15px; font-size: 18px;")
                    )
            )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Raw Data", tabName = "raw_data", icon = icon("table")),
      menuItem("Team Info", tabName = "team", icon = icon("users")),
      selectInput("product", "Select Product", choices = c("All", unique(data$Product)), selected = "All"),
      selectInput("brand", "Select Brand", choices = c("All", unique(data$Brand)), selected = "All"),
      selectInput("region", "Select Region", choices = c("All", unique(data$Region)), selected = "All")
    )
  ),
  dashboardBody(
    customCSS,
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 12, plotlyOutput("timelinePlot"))
              ),
              fluidRow(
                box(width = 6, plotlyOutput("brandPie")),
                box(width = 6, plotlyOutput("regionPie"))
              )
      ),
      tabItem(tabName = "trends",
              fluidRow(
                box(width = 6, plotlyOutput("waterfallChart")),
                box(width = 6, plotlyOutput("lineGraph"))
              )
      ),
      tabItem(tabName = "raw_data",
              fluidRow(
                box(width = 12, DT::dataTableOutput("rawDataTable"))
              )
      ),
      tabItem(tabName = "team",
              fluidRow(
                column(width = 4,
                       div(class = "team-box",
                           img(src = "https://4kwallpapers.com/images/walls/thumbs_2t/21604.jpg", class = "team-img"),
                           h4("Raj Khatke"),
                           p("Video Editor")
                       )),
                column(width = 4,
                       div(class = "team-box",
                           img(src = "https://w0.peakpx.com/wallpaper/150/317/HD-wallpaper-dark-anime-shadow.jpg", class = "team-img"),
                           h4("Deepak Kanodiya"),
                           p("Data Analytics")
                       )),
                column(width = 4,
                       div(class = "team-box",
                           img(src = "https://images.squarespace-cdn.com/content/v1/5fe4caeadae61a2f19719512/340ac0db-2e41-4f76-9bc1-43de2d801fa8/Giyu+Tomioka+-+Dead+Calm", class = "team-img"),
                           h4("Subham Thakur"),
                           p("Web Developer")
                       ))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  filteredData <- reactive({
    df <- data
    if (input$product != "All") df <- df[df$Product == input$product, ]
    if (input$brand != "All") df <- df[df$Brand == input$brand, ]
    if (input$region != "All") df <- df[df$Region == input$region, ]
    df
  })
  
  output$timelinePlot <- renderPlotly({
    df <- filteredData()
    weekly_sales <- df %>%
      group_by(Week, Product) %>%
      summarise(Total_Sold = sum(Quantity.Sold, na.rm = TRUE), .groups = "drop")
    p <- ggplot(weekly_sales, aes(x = Week, y = Total_Sold, fill = Product)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Weekly Sales by Product", x = "Week", y = "Units Sold") +
      theme_minimal(base_family = "Arial") +
      theme(
        plot.background = element_rect(fill = "#1c1c1c"),
        panel.background = element_rect(fill = "#1c1c1c"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "#ffcccc"),
        legend.title = element_text(color = "#ffcccc"),
        legend.text = element_text(color = "white")
      )
    ggplotly(p)
  })
  
  output$brandPie <- renderPlotly({
    df <- filteredData()
    brand_dist <- df %>%
      count(Brand) %>%
      mutate(percent = round(n / sum(n) * 100, 1))
    plot_ly(brand_dist, labels = ~Brand, values = ~n, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial',
            marker = list(colors = RColorBrewer::brewer.pal(8, "Set3"))) %>%
      layout(title = 'Sales by Brand', paper_bgcolor = '#1c1c1c', font = list(color = 'white'))
  })
  
  output$regionPie <- renderPlotly({
    df <- filteredData()
    region_dist <- df %>%
      count(Region) %>%
      mutate(percent = round(n / sum(n) * 100, 1))
    plot_ly(region_dist, labels = ~Region, values = ~n, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial',
            marker = list(colors = RColorBrewer::brewer.pal(5, "Pastel1"))) %>%
      layout(title = 'Sales by Region', paper_bgcolor = '#1c1c1c', font = list(color = 'white'))
  })
  
  output$waterfallChart <- renderPlotly({
    df <- filteredData() %>%
      group_by(Product) %>%
      summarise(Revenue = sum(Price * Quantity.Sold, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Revenue))
    plot_ly(df, type = "waterfall", x = ~Product, y = ~Revenue,
            connector = list(line = list(color = "white")),
            decreasing = list(marker = list(color = "#ff4d4d")),
            increasing = list(marker = list(color = "#66cc66")),
            totals = list(marker = list(color = "#3366cc"))) %>%
      layout(title = "Revenue Contribution by Product (Waterfall)",
             paper_bgcolor = '#1c1c1c',
             plot_bgcolor = '#1c1c1c',
             font = list(color = "white"))
  })
  
  output$lineGraph <- renderPlotly({
    df <- filteredData() %>%
      mutate(Month = floor_date(Inward.Date, "month")) %>%
      group_by(Month) %>%
      summarise(Revenue = sum(Price * Quantity.Sold, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~Month, y = ~Revenue, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#ff4d4d", width = 3)) %>%
      layout(title = "Monthly Revenue Trend",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Total Revenue"),
             paper_bgcolor = '#1c1c1c',
             plot_bgcolor = '#1c1c1c',
             font = list(color = "white"))
  })
  
  output$rawDataTable <- DT::renderDataTable({
    df <- filteredData()
    DT::datatable(df,
                  options = list(scrollX = TRUE, scrollY = 400, pageLength = 10),
                  class = 'cell-border stripe hover')
  })
}

# Run the app
shinyApp(ui, server)




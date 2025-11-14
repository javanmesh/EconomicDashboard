# ==============================================================================
# GLOBAL ECONOMIC INDICATORS DASHBOARD
# ==============================================================================
# Author: Javan Meshack
# Description: Interactive Shiny dashboard for exploring global economic indicators
#              including GDP, unemployment, inflation, and trade data across countries
# ==============================================================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(scales)

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

# Create comprehensive economic dataset
# In production, this would connect to World Bank API or load from file
create_economic_data <- function() {
  set.seed(123)  # For reproducibility
  
  countries <- c("United States", "China", "Germany", "Japan", "United Kingdom", 
                 "France", "India", "Brazil", "Canada", "Australia",
                 "South Korea", "Mexico", "Spain", "Indonesia", "Netherlands")
  
  years <- 2018:2024
  
  # Generate realistic economic data
  data <- expand.grid(
    Country = countries,
    Year = years,
    stringsAsFactors = FALSE
  )
  
  # Base values for each country (to create realistic patterns)
  base_gdp <- c(21000, 14000, 4000, 5000, 2800, 2700, 2900, 1900, 1700, 1400,
                1600, 1300, 1400, 1100, 900)
  base_unemployment <- c(4, 4.5, 3.5, 2.5, 4, 9, 5, 11, 6, 5, 3.5, 3.5, 15, 5, 4)
  base_inflation <- c(2, 2.5, 1.5, 0.5, 2, 1.5, 4, 4, 2, 2, 1, 4, 1.5, 3, 2)
  
  # Add economic indicators with realistic trends and variations
  data <- data %>%
    group_by(Country) %>%
    mutate(
      country_idx = match(Country[1], countries),
      # GDP with growth trend and COVID impact
      GDP_Billions = base_gdp[country_idx] * (1 + (Year - 2018) * 0.03) * 
        ifelse(Year == 2020, 0.95, 1) + rnorm(n(), 0, base_gdp[country_idx] * 0.02),
      
      # Unemployment with COVID spike
      Unemployment_Rate = base_unemployment[country_idx] + 
        ifelse(Year == 2020, 3, 0) + 
        rnorm(n(), 0, 0.5),
      
      # Inflation with recent increase
      Inflation_Rate = base_inflation[country_idx] + 
        ifelse(Year >= 2021, 2, 0) + 
        rnorm(n(), 0, 0.3),
      
      # Trade balance
      Trade_Balance = rnorm(n(), base_gdp[country_idx] * 0.02, base_gdp[country_idx] * 0.01),
      
      # GDP per capita
      GDP_Per_Capita = GDP_Billions * 1000 / (runif(1, 50, 1400)) + rnorm(n(), 0, 2000),
      
      # Region classification
      Region = case_when(
        Country %in% c("United States", "Canada", "Mexico") ~ "Americas",
        Country %in% c("China", "Japan", "India", "South Korea", "Indonesia") ~ "Asia",
        Country %in% c("Germany", "United Kingdom", "France", "Spain", "Netherlands") ~ "Europe",
        Country %in% c("Brazil") ~ "South America",
        Country %in% c("Australia") ~ "Oceania"
      )
    ) %>%
    ungroup() %>%
    select(-country_idx)
  
  return(data)
}

# Generate the dataset
economic_data <- create_economic_data()

# ==============================================================================
# USER INTERFACE
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(
    title = "Global Economic Dashboard",
    titleWidth = 300
  ),
  
  # Sidebar with filters and controls
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trends Analysis", tabName = "trends", icon = icon("chart-line")),
      menuItem("Country Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    ),
    
    hr(),
    
    # Filter Controls
    h4("Filters", style = "padding-left: 15px; color: white;"),
    
    selectInput(
      "selected_countries",
      "Select Countries:",
      choices = unique(economic_data$Country),
      selected = c("United States", "China", "Germany", "Japan"),
      multiple = TRUE
    ),
    
    sliderInput(
      "year_range",
      "Year Range:",
      min = min(economic_data$Year),
      max = max(economic_data$Year),
      value = c(min(economic_data$Year), max(economic_data$Year)),
      step = 1,
      sep = ""
    ),
    
    selectInput(
      "region_filter",
      "Filter by Region:",
      choices = c("All Regions", unique(economic_data$Region)),
      selected = "All Regions"
    ),
    
    hr(),
    
    # Info box
    div(
      style = "padding: 15px; color: white; font-size: 12px;",
      icon("info-circle"),
      " This dashboard provides interactive visualization of global economic indicators.",
      br(), br(),
      "Data includes GDP, unemployment, inflation, and trade statistics."
    )
  ),
  
  # Main content body
  dashboardBody(
    # Custom CSS for better styling
    tags$head(
      tags$style(HTML("
        .box-title { font-weight: bold; font-size: 16px; }
        .small-box { border-radius: 5px; }
        .content-wrapper { background-color: #ecf0f5; }
      "))
    ),
    
    tabItems(
      # OVERVIEW TAB
      tabItem(
        tabName = "overview",
        
        h2("Economic Indicators Overview"),
        
        # Summary statistics boxes
        fluidRow(
          valueBoxOutput("avg_gdp_box", width = 3),
          valueBoxOutput("avg_unemployment_box", width = 3),
          valueBoxOutput("avg_inflation_box", width = 3),
          valueBoxOutput("countries_count_box", width = 3)
        ),
        
        fluidRow(
          # GDP Distribution
          box(
            title = "GDP Distribution by Country (Latest Year)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("gdp_bar_plot", height = 350)
          ),
          
          # Regional comparison
          box(
            title = "Average GDP by Region",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("region_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Economic Indicators Correlation",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("scatter_plot", height = 400)
          )
        )
      ),
      
      # TRENDS TAB
      tabItem(
        tabName = "trends",
        
        h2("Economic Trends Over Time"),
        
        fluidRow(
          box(
            title = "Select Indicator to Visualize",
            status = "primary",
            width = 12,
            selectInput(
              "trend_indicator",
              "Choose Economic Indicator:",
              choices = c(
                "GDP (Billions)" = "GDP_Billions",
                "Unemployment Rate (%)" = "Unemployment_Rate",
                "Inflation Rate (%)" = "Inflation_Rate",
                "Trade Balance (Billions)" = "Trade_Balance",
                "GDP Per Capita ($)" = "GDP_Per_Capita"
              ),
              selected = "GDP_Billions"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Time Series Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("trend_plot", height = 500)
          )
        ),
        
        fluidRow(
          box(
            title = "Year-over-Year Growth Rate",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("growth_plot", height = 400)
          )
        )
      ),
      
      # COMPARISON TAB
      tabItem(
        tabName = "comparison",
        
        h2("Country-by-Country Comparison"),
        
        fluidRow(
          box(
            title = "Multi-Indicator Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              "comparison_year",
              "Select Year for Comparison:",
              choices = sort(unique(economic_data$Year), decreasing = TRUE),
              selected = max(economic_data$Year)
            )
          )
        ),
        
        fluidRow(
          box(
            title = "GDP vs Unemployment",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("gdp_unemployment_plot", height = 400)
          ),
          
          box(
            title = "GDP vs Inflation",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("gdp_inflation_plot", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Comprehensive Indicators Heatmap",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("heatmap_plot", height = 500)
          )
        )
      ),
      
      # DATA TABLE TAB
      tabItem(
        tabName = "data",
        
        h2("Raw Data Explorer"),
        
        fluidRow(
          box(
            title = "Economic Data Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            downloadButton("download_data", "Download CSV"),
            br(), br(),
            DTOutput("data_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Summary Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("summary_stats")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # ==============================================================================
  # REACTIVE DATA FILTERING
  # ==============================================================================
  
  # Create reactive filtered dataset based on user selections
  filtered_data <- reactive({
    data <- economic_data
    
    # Filter by selected countries
    if (!is.null(input$selected_countries) && length(input$selected_countries) > 0) {
      data <- data %>% filter(Country %in% input$selected_countries)
    }
    
    # Filter by year range
    data <- data %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    
    # Filter by region
    if (input$region_filter != "All Regions") {
      data <- data %>% filter(Region == input$region_filter)
    }
    
    return(data)
  })
  
  # ==============================================================================
  # OVERVIEW TAB OUTPUTS
  # ==============================================================================
  
  # Value box: Average GDP
  output$avg_gdp_box <- renderValueBox({
    avg_gdp <- filtered_data() %>%
      filter(Year == max(Year)) %>%
      summarise(avg = mean(GDP_Billions, na.rm = TRUE)) %>%
      pull(avg)
    
    valueBox(
      paste0("$", round(avg_gdp, 0), "B"),
      "Average GDP",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # Value box: Average Unemployment
  output$avg_unemployment_box <- renderValueBox({
    avg_unemp <- filtered_data() %>%
      filter(Year == max(Year)) %>%
      summarise(avg = mean(Unemployment_Rate, na.rm = TRUE)) %>%
      pull(avg)
    
    valueBox(
      paste0(round(avg_unemp, 1), "%"),
      "Avg Unemployment",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  # Value box: Average Inflation
  output$avg_inflation_box <- renderValueBox({
    avg_inf <- filtered_data() %>%
      filter(Year == max(Year)) %>%
      summarise(avg = mean(Inflation_Rate, na.rm = TRUE)) %>%
      pull(avg)
    
    valueBox(
      paste0(round(avg_inf, 1), "%"),
      "Avg Inflation",
      icon = icon("chart-line"),
      color = "red"
    )
  })
  
  # Value box: Number of countries
  output$countries_count_box <- renderValueBox({
    count <- filtered_data() %>%
      distinct(Country) %>%
      nrow()
    
    valueBox(
      count,
      "Countries Analyzed",
      icon = icon("globe"),
      color = "blue"
    )
  })
  
  # GDP Bar Chart
  output$gdp_bar_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      filter(Year == max(Year)) %>%
      arrange(desc(GDP_Billions)) %>%
      head(10)
    
    p <- ggplot(plot_data, aes(x = reorder(Country, GDP_Billions), y = GDP_Billions, fill = Region)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "GDP (Billions USD)", title = NULL) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Regional GDP Plot
  output$region_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      filter(Year == max(Year)) %>%
      group_by(Region) %>%
      summarise(Avg_GDP = mean(GDP_Billions, na.rm = TRUE)) %>%
      arrange(desc(Avg_GDP))
    
    p <- ggplot(plot_data, aes(x = reorder(Region, Avg_GDP), y = Avg_GDP, fill = Region)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = NULL, y = "Average GDP (Billions USD)", title = NULL) +
      theme_minimal() +
      scale_fill_brewer(palette = "Dark2")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Scatter plot: GDP vs Unemployment correlation
  output$scatter_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      filter(Year == max(Year))
    
    p <- ggplot(plot_data, aes(x = GDP_Billions, y = Unemployment_Rate, 
                               color = Region, size = Inflation_Rate, text = Country)) +
      geom_point(alpha = 0.7) +
      labs(x = "GDP (Billions USD)", 
           y = "Unemployment Rate (%)",
           title = NULL) +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
    
    ggplotly(p, tooltip = c("text", "x", "y", "size", "color"))
  })
  
  # ==============================================================================
  # TRENDS TAB OUTPUTS
  # ==============================================================================
  
  # Time series trend plot
  output$trend_plot <- renderPlotly({
    indicator <- input$trend_indicator
    
    plot_data <- filtered_data()
    
    # Determine y-axis label based on selected indicator
    y_label <- case_when(
      indicator == "GDP_Billions" ~ "GDP (Billions USD)",
      indicator == "Unemployment_Rate" ~ "Unemployment Rate (%)",
      indicator == "Inflation_Rate" ~ "Inflation Rate (%)",
      indicator == "Trade_Balance" ~ "Trade Balance (Billions USD)",
      indicator == "GDP_Per_Capita" ~ "GDP Per Capita (USD)"
    )
    
    p <- ggplot(plot_data, aes_string(x = "Year", y = indicator, color = "Country", group = "Country")) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "Year", y = y_label, title = NULL) +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_x_continuous(breaks = unique(plot_data$Year))
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # Growth rate plot
  output$growth_plot <- renderPlotly({
    indicator <- input$trend_indicator
    
    plot_data <- filtered_data() %>%
      arrange(Country, Year) %>%
      group_by(Country) %>%
      mutate(Growth_Rate = (get(indicator) - lag(get(indicator))) / lag(get(indicator)) * 100) %>%
      filter(!is.na(Growth_Rate))
    
    p <- ggplot(plot_data, aes(x = Year, y = Growth_Rate, color = Country, group = Country)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(x = "Year", y = "Year-over-Year Growth Rate (%)", title = NULL) +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_x_continuous(breaks = unique(plot_data$Year))
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
  
  # ==============================================================================
  # COMPARISON TAB OUTPUTS
  # ==============================================================================
  
  # GDP vs Unemployment comparison
  output$gdp_unemployment_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      filter(Year == input$comparison_year)
    
    p <- ggplot(plot_data, aes(x = GDP_Billions, y = Unemployment_Rate, 
                               color = Country, text = Country)) +
      geom_point(size = 4, alpha = 0.7) +
      labs(x = "GDP (Billions USD)", 
           y = "Unemployment Rate (%)",
           title = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # GDP vs Inflation comparison
  output$gdp_inflation_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      filter(Year == input$comparison_year)
    
    p <- ggplot(plot_data, aes(x = GDP_Billions, y = Inflation_Rate, 
                               color = Country, text = Country)) +
      geom_point(size = 4, alpha = 0.7) +
      labs(x = "GDP (Billions USD)", 
           y = "Inflation Rate (%)",
           title = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # Heatmap of all indicators
  output$heatmap_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      filter(Year == input$comparison_year) %>%
      select(Country, GDP_Billions, Unemployment_Rate, Inflation_Rate, GDP_Per_Capita) %>%
      mutate(across(where(is.numeric), scale)) %>%
      tidyr::pivot_longer(cols = -Country, names_to = "Indicator", values_to = "Value")
    
    # Create matrix for heatmap
    matrix_data <- plot_data %>%
      tidyr::pivot_wider(names_from = Country, values_from = Value) %>%
      select(-Indicator) %>%
      as.matrix()
    
    rownames(matrix_data) <- c("GDP", "Unemployment", "Inflation", "GDP Per Capita")
    
    plot_ly(
      x = colnames(matrix_data),
      y = rownames(matrix_data),
      z = matrix_data,
      type = "heatmap",
      colorscale = "RdYlGn",
      reversescale = TRUE
    ) %>%
      layout(
        xaxis = list(title = "Country"),
        yaxis = list(title = "Indicator"),
        margin = list(l = 120, b = 100)
      )
  })
  
  # ==============================================================================
  # DATA TABLE TAB OUTPUTS
  # ==============================================================================
  
  # Interactive data table
  output$data_table <- renderDT({
    filtered_data() %>%
      mutate(across(where(is.numeric), ~round(., 2))) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(2, 'desc'), list(1, 'asc'))  # Sort by Year desc, then Country
        ),
        filter = 'top',
        rownames = FALSE
      )
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    data <- filtered_data() %>%
      select(GDP_Billions, Unemployment_Rate, Inflation_Rate, Trade_Balance, GDP_Per_Capita)
    
    summary(data)
  })
  
  # Download handler for CSV export
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("economic_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

shinyApp(ui = ui, server = server)


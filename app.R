#Libraries
library(shiny)
library(tidyverse)
library(bs4Dash)
library(plotly)
library(ggplot2)
library(DT)
library(dplyr)
library(countrycode)
library(readxl)
library(here)
library(sf)
library(leaflet)
library(tidygeocoder)

#load data
spending_data <- read.csv(here("military_expenditure_data.csv"))


# UI
#-----------------------------------------
ui <- bs4DashPage(
  
#Home page
  title = "Military Spending Dashboard",
  header = dashboardHeader(
    status = "pep_blue",
    title = dashboardBrand(
      title = "Military Spending Dashboard",
      color = "lightblue"
     )
  ),
  
#Sidebar for selection: Home and Dashboard
  sidebar = bs4DashSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("bar-chart")
      )
    )
  ),  
  
  
#Dashboard body
  body = bs4DashBody(
  
    tabItems(
      
      #Home Tab
      tabItem(
        tabName = "home",
        jumbotron(
            title = "Welcome to the Military Spending Dashboard",
            status = "info",
            lead = "This dashboard provides insights into spending data across different countries, regions, and years.",
            href = "https://www.sipri.org/databases/milex",
            btnName = "more info",
            "The data used for this dashboard is from STOCKHOLM INTERNATIONAL PEACE RESEARCH INSTITUTE"
          ),
        
        fluidRow(
          #Researcher information
          userBox(
            collapsible = FALSE,
            title = userDescription(
            title = "Purushottam Bhandare, Ph.D.",
            subtitle = "",
            image = "https://scholar.googleusercontent.com/citations?view_op=medium_photo&user=_SyCZCgAAAAJ&citpid=1",
            type = 2
          ),
          "Social and development researcher!"
        ),
          
          box(
            title = "My favourite quote",
            width = 6,
            collapsible = FALSE,
            blockQuote("A nation that continues year after year to spend more 
                       money on military defense than on programs of social uplift 
                       is approaching spiritual doom.- Martin Luther King Jr.", color = "lightblue")
          )
      )
   ),
      
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            width = 3,
            bs4Card(
              title = "Filters",
              width = 12,
              selectInput("region", "Select Region", choices = c("All", unique(spending_data$Region)), selected = "All"),
              uiOutput("sub_region_ui"),
              uiOutput("country_ui"),
              selectInput(
                "metric",
                "Select Metric",
                choices = c(
                  "Spending in USD" = "spend_USD",
                  "% of GDP" = "pct_GDP",
                  "Per Capita Spending" = "per_Capita",
                  "% of Government Spending" = "pct_Govt"
                ),
                selected = "spend_USD"
              ),
              
              #selectInput("year", "Select Year", choices = unique(spending_data$Year), selected = max(spending_data$Year)) 
              
              #Year range filter
              sliderInput(
                "year_range",
                "Select Year Range",
                min = min(spending_data$Year),
                max = max(spending_data$Year),
                value = c(min(spending_data$Year), max(spending_data$Year)),
                sep = ""
              )
            )
          ),
          
          
        #Main Column
          column(
            width = 9,
            #Summary boxes
            fluidRow(
              bs4ValueBoxOutput("tot_spending_usd_box", width = 3),
              bs4ValueBoxOutput("avg_pct_gdp_box", width = 3),
              bs4ValueBoxOutput("avg_per_capita_box", width = 3),
              bs4ValueBoxOutput("avg_per_gov_box", width = 3)
          ),
#          column(
#         width = 9,
            fluidRow(
              bs4Card(
                title = "Spending Map",
                width = 12,
                plotlyOutput("map_plot")
              )
            ),
            fluidRow(
              bs4Card(
                title = "Time Series Trend",
                width = 6,
                plotOutput("time_series_plot")
              ),
              bs4Card(
                title = "Regional Summary",
                width = 6,
                plotOutput("regional_summary_plot")
              )
            ),
            fluidRow(
              bs4Card(
                title = "Government Spending Allocation",
                width = 6,
                plotOutput("govt_spending_plot")
              ),
              bs4Card(
                title = "Correlation Plot",
                width = 6,
                plotOutput("correlation_plot")
              )
            ),
            fluidRow(
              bs4Card(
                title = "Data Table",
                width = 12,
                DTOutput("data_table")
              )
            )
          )
        )
      )
    )
  )
)



# Server
server <- function(input, output, session) {
  # Reactive data filtering
  filtered_global <- reactive({
    data <- spending_data
    if (input$region != "All") {
      data <- data %>% filter(Region == input$region)
    }
    if (!is.null(input$sub_region) && input$sub_region != "All") {
      data <- data %>% filter(Sub_region == input$sub_region)
    }
    if (!is.null(input$country) && length(input$country) > 0) {
      data <- data %>% filter(Country %in% input$country)
    }
    data <- data %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2])
  })
  
  # Dynamic Sub_region filter
  output$sub_region_ui <- renderUI({
    sub_regions <- if (input$region == "All") unique(spending_data$Sub_region) else unique(spending_data$Sub_region[spending_data$Region == input$region])
    selectInput("sub_region", "Select Sub_region", choices = c("All", sub_regions), selected = "All")
  })
  
  # Dynamic Country filter
  output$country_ui <- renderUI({
    countries <- if (input$region == "All" && (is.null(input$sub_region) || input$sub_region == "All")) {
      unique(spending_data$Country)
    } else if (input$region != "All" && (is.null(input$sub_region) || input$sub_region == "All")) {
      unique(spending_data$Country[spending_data$Region == input$region])
    } else {
      unique(spending_data$Country[spending_data$Sub_region == input$sub_region])
    }
    selectInput("country", "Select Country(ies)", choices = countries, multiple = TRUE)
  })
  

# Calculate summary information
  #Total Spending
  output$tot_spending_usd_box <- renderbs4ValueBox({
    tot_spending_usd <- filtered_global() %>% 
      summarise(tot = sum(spend_USD, na.rm = TRUE)) %>% 
      pull(tot)
    
    bs4ValueBox(
      value = paste0("$", format(round(tot_spending_usd, 2), big.mark = ",")),
      subtitle = "Total Spending in Millions of (USD)",
      icon = icon("dollar-sign"),
      color = "primary"
    )
  })
  
  #Average % of GDP
  output$avg_pct_gdp_box <- renderbs4ValueBox({
    avg_pct_gdp <- filtered_global() %>% 
      summarise(avg = mean(pct_GDP, na.rm = TRUE)) %>% 
      pull(avg)
    
    bs4ValueBox(
      value = paste0(round(avg_pct_gdp, 2), "%"),
      subtitle = "Average % of GDP",
      icon = icon("chart-pie"),
      color = "success"
    )
  })
  
  #Average per Capita spending
  output$avg_per_capita_box <- renderbs4ValueBox({
    avg_per_capita <- filtered_global() %>% 
      summarise(avg = mean(per_Capita, na.rm = TRUE)) %>% 
      pull(avg)
    
    bs4ValueBox(
      value = paste0("$", format(round(avg_per_capita, 2), big.mark = ",")),
      subtitle = "Average Per Capita Spending",
      icon = icon("user"),
      color = "info"
    )
  })
  
  #Average % of Govt spending
  output$avg_per_gov_box <- renderbs4ValueBox({
    avg_per_gov <- filtered_global() %>% 
      summarise(avg = mean(pct_Govt, na.rm = TRUE)) %>% 
      pull(avg)
    
    bs4ValueBox(
      value = paste0("$", format(round(avg_per_gov, 2), big.mark = ",")),
      subtitle = "Average % of Govt Spending",
      icon = icon("chart-pie"),
      color = "success"
    )
  })
  
 
  # Map Data
  map_data <- reactive({
    data <- filtered_global() %>% 
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      group_by(iso3, Country) %>% 
      summarise(across(all_of(input$metric), mean, na.rm = TRUE), .groups = "drop")  # Aggregate metric
    
    validate(need(nrow(data) > 0, "No data available for selected year range"))
    data
  })
  
  
  # output$map_plot <- renderLeaflet({
  #   leaflet(map_data()) %>%
  #     addProviderTiles(providers$Stamen.Watercolor) %>%  # Stamen Watercolor Basemap
  #     addCircles(
  #       lng = ~long, lat = ~lat, 
  #       color = ~colorNumeric("Blues", get(input$metric))(get(input$metric)),
  #       radius = ~get(input$metric) * 500,  # Scale radius (adjust as needed)
  #       popup = ~paste("<b>", Country, "</b><br>",
  #                      input$metric, ": ", round(get(input$metric), 2))
  #     ) %>%
  #     addLegend(pal = colorNumeric("Blues", map_data()[[input$metric]]), 
  #               values = map_data()[[input$metric]], 
  #               title = input$metric)
  # })
  #-----------------
  
  output$map_plot <- renderPlotly({
     plot_geo(map_data()) %>%
       add_trace(
         z = ~get(input$metric), locations = ~iso3, color = ~get(input$metric),
         colors = "Blues", text = ~paste(Country, "<br>", input$metric, ": ", round(get(input$metric), 2))
       ) %>%
       layout(
         title = paste(input$metric, "by Country for", input$year),
         geo = list(showframe = FALSE)
       )
   })
  
  # Time Series Plot
  output$time_series_plot <- renderPlot({
    data <- filtered_global()
    validate(need(nrow(data) > 0, "No data available for selection"))
    ggplot(data, aes(x = Year, y = get(input$metric), color = Country, group = Country)) +
      geom_line() +
      labs(title = paste("Trend of", input$metric, "Over Time"), y = input$metric) +
      theme_minimal()
  })
  
  
  output$regional_summary_plot <- renderPlot({
    data <- filtered_global()
    validate(need(nrow(data) > 0, "No data available for selection"))
    
    if (input$region == "All") {
      summary_data <- data %>% 
        group_by(Region) %>% 
        summarise(mean_metric = mean(get(input$metric), na.rm = TRUE)) %>%
        arrange(desc(mean_metric))  # Arrange in descending order
      
      ggplot(summary_data, aes(x = reorder(Region, -mean_metric), y = mean_metric)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = paste("Average", input$metric, "by Region"), y = input$metric) +
        theme_minimal() +
        coord_flip()
    } else if (input$sub_region == "All") {
      summary_data <- data %>% 
        group_by(Sub_region) %>% 
        summarise(mean_metric = mean(get(input$metric), na.rm = TRUE)) %>%
        arrange(desc(mean_metric))  # Arrange in descending order
      
      ggplot(summary_data, aes(x = reorder(Sub_region, -mean_metric), y = mean_metric)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = paste("Average", input$metric, "by Sub_region"), y = input$metric) +
        theme_minimal() +
        coord_flip()
    } else {
      summary_data <- data %>% 
        group_by(Country) %>% 
        summarise(mean_metric = mean(get(input$metric), na.rm = TRUE)) %>%
        arrange(desc(mean_metric))  # Arrange in descending order
      
      ggplot(summary_data, aes(x = reorder(Country, -mean_metric), y = mean_metric)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = paste("Average", input$metric, "by Country"), y = input$metric) +
        theme_minimal() +
        coord_flip()
    }
  })
  

  # Government Spending Allocation Plot
  
  output$govt_spending_plot <- renderPlot({
    data <- filtered_global() %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      group_by(Country) %>%
      summarise(pct_Govt = mean(pct_Govt, na.rm = TRUE), .groups = "drop")  # Aggregate over years
    
    validate(need(nrow(data) > 0, "No data available for selected year range"))
    
    ggplot(data, aes(x = reorder(Country, pct_Govt), y = pct_Govt)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      labs(
        title = paste("Average Government Spending (%) (", input$year_range[1], "-", input$year_range[2], ")"),
        y = "% of Govt Budget"
      ) +
      theme_minimal() +
      coord_flip()
  })
  
  
  #Correlation Plot 
  
  output$correlation_plot <- renderPlot({
    # Filter the data based on user inputs
    data <- filtered_global()  # Ensure this reactive function returns the filtered dataset
    
    # Validate that there is sufficient data to plot
    validate(
      need(nrow(data) > 0, "No data available for the selected filters")
    )
    
    # Create the correlation plot
    ggplot(data, aes(x = per_Capita, y = pct_GDP)) +
      geom_point(color = "blue", alpha = 0.7) +  # Scatter points
      geom_smooth(method = "lm", se = TRUE, color = "red") +  # Linear regression line with confidence interval
      labs(
        title = "Correlation: Per Capita Spend vs % of GDP",
        x = "Per Capita Spend",
        y = "% of GDP"
      ) +
      theme_minimal() +
      annotate(
        "text", x = max(data$per_Capita, na.rm = TRUE), 
        y = max(data$pct_GDP, na.rm = TRUE), 
        label = paste("Corr:", round(cor(data$per_Capita, data$pct_GDP, use = "complete.obs"), 2)),
        hjust = 1, vjust = 1, size = 4, color = "black"
      )
  })
  
  
  # Data Table 
  output$data_table <- renderDT({ 
    datatable(filtered_global(), 
              options = list(pageLength = 10, searching = TRUE, ordering = TRUE)) 
    }) } 
# Run the app 
shinyApp(ui, server)
  
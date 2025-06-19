library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(janitor)
library(ggrepel)
library(bslib)
library(plotly)
library(htmlwidgets)


# Set custom theme
my_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#0077b6",
  base_font = font_google("Poppins"),
  heading_font = font_google("Poppins"),
  font_scale = 1.1
)

# Load and preprocess data
temperature <- read_csv("temperature_at_select_stations_in_tamil_nadu_2020.csv") %>%
  clean_names() %>%
  rename(
    station = name_of_the_station,
    month = month,
    highest_max_temp = highest_maximum_temperature_in_celsius,
    mean_max_temp = mean_maximum_temperature_in_celsius,
    lowest_min_temp = lowest_minimum_temperature_in_celsius,
    mean_min_temp = mean_minimum_temperature_in_celsius,
    humidity_morning = percentage_of_humidity_at_08_30_hours,
    humidity_evening = percentage_of_humidity_at_17_30_hours
  ) %>%
  mutate(
    humidity_morning = as.numeric(humidity_morning),
    humidity_evening = as.numeric(humidity_evening),
    month_clean = str_replace_all(month, "'", ""),
    month_parsed = parse_date_time(month_clean, orders = c("my", "bY", "B'Y"))
  )

temperature_long <- temperature %>%
  pivot_longer(cols = c(mean_max_temp, mean_min_temp), names_to = "temperature_type", values_to = "temperature_c")

humidity_long <- temperature %>%
  pivot_longer(cols = c(humidity_morning, humidity_evening), names_to = "humidity_type", values_to = "humidity_pct")

ts_rainfall <- read_csv("time_series_data_rainfall_by_seasons_in_tamil_nadu_2020.csv") %>%
  clean_names() %>%
  mutate(year = as.numeric(str_sub(year, 1, 4)))

ts_rainfall_long <- ts_rainfall %>%
  pivot_longer(cols = ends_with("_in_mm"), names_to = "category", values_to = "rainfall") %>%
  separate(category, into = c("type", "season"), sep = "_rainfall_in_", extra = "merge") %>%
  mutate(season = str_replace_all(season, "_", " "), type = str_to_title(type))

rainfall_districts <- read_csv("rainfall_by_districts_2020.csv") %>%
  clean_names() %>%
  mutate(percentage_deviation = round(100 * (total_actual_rainfall_june_2018_to_may_2019_in_mm - total_normal_rainfall_june_2018_to_may_2019_in_mm) / total_normal_rainfall_june_2018_to_may_2019_in_mm, 1))

rainfall_districts_long <- rainfall_districts %>%
  pivot_longer(cols = ends_with("in_mm"), names_to = "season", values_to = "rainfall") %>%
  mutate(
    season = str_replace_all(season, "_", " "),
    season_type = case_when(
      str_detect(season, "actual") ~ "Actual",
      str_detect(season, "normal") ~ "Normal",
      TRUE ~ "Other"
    ),
    season_short = case_when(
      str_detect(season, "south west monsoon") ~ "SW Monsoon",
      str_detect(season, "north east monsoon") ~ "NE Monsoon",
      str_detect(season, "winter season") ~ "Winter",
      str_detect(season, "hot weather season") ~ "Hot Weather",
      str_detect(season, "total actual") ~ "Total Actual",
      str_detect(season, "total normal") ~ "Total Normal",
      TRUE ~ str_trunc(season, 20)
    )
  ) %>%
  mutate(season_short = factor(season_short, levels = c("SW Monsoon", "NE Monsoon", "Winter", "Hot Weather", "Total Actual", "Total Normal")))

rainfall_range <- read_csv("distribution_of_districts_by_range_of_rainfall_2020.csv") %>%
  clean_names()

actual_range <- rainfall_range %>%
  separate_rows(actual_rainfall_occurred, sep = ",\\s*") %>%
  count(range_of_rainfall, name = "actual_count")

normal_range <- rainfall_range %>%
  separate_rows(normal_rainfall_expected, sep = ",\\s*") %>%
  count(range_of_rainfall, name = "normal_count")

rainfall_range_long <- full_join(actual_range, normal_range, by = "range_of_rainfall") %>%
  pivot_longer(cols = c(actual_count, normal_count), names_to = "type", values_to = "count")

rainfall_range_long <- rainfall_range_long %>%
  mutate(districts = case_when(
    type == "actual_count" ~ rainfall_range$actual_rainfall_occurred[match(range_of_rainfall, rainfall_range$range_of_rainfall)],
    type == "normal_count" ~ rainfall_range$normal_rainfall_expected[match(range_of_rainfall, rainfall_range$range_of_rainfall)],
    TRUE ~ NA_character_
  ))

ui <- navbarPage(
  title = "Tamil Nadu Weather Dashboard",
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Poppins")),
  
  tabPanel("Temperature & Humidity",
             fluidPage(
               div(
                 style = "font-size: 15px; color: #333; margin-bottom: 10px;",
                 HTML("<h4 style='color: #2c3e50;'>Start Your Climate Journey Here 🌤️</h4>
        Welcome to the Tamil Nadu Weather Dashboard — your interactive window into the climate stories told by Tamil Nadu’s skies. Whether you're a curious citizen, a farmer planning ahead, or a student exploring data, this is your launchpad.<br><br>
        Each station whispers its unique tale:<br>
        The graphs below are interactive <strong>time series line plots</strong> — they track changes in temperature and humidity over time at selected weather stations.<br><br>
        <ul>
          <li><b>Red & Teal Lines:</b> Show the <em>mean maximum</em> and <em>mean minimum</em> temperatures respectively, plotted monthly.</li>
          <li><b>Golden & Blue Lines:</b> Represent <em>humidity in the evening</em> and <em>morning</em>, revealing daily atmospheric shifts.</li>
          <li><b>Hover</b> over each point to explore exact values; <b>download</b> the plots to use in your work or share insights.</li>
        <em>Tip:</em> Hover over data points to dive deeper, and download plots to share the story. Let the graphs guide you through patterns that shape our seasons, decisions, and daily life.")
               ),
             fluidRow(
               column(12,
                      wellPanel(
                        selectInput("station", "Station:", choices = unique(temperature$station)),
                        downloadButton("download_temp_plot", "Download Temperature Plot", class = "btn-primary"),
                        downloadButton("download_humidity_plot", "Download Humidity Plot", class = "btn-info")
                      )
               )
             ),
             fluidRow(
               column(6, plotlyOutput("temp_plot", height = "300px")),
               column(6, plotlyOutput("humidity_plot", height = "300px"))
             )
           )
  ),
  
  tabPanel("Seasonal Rainfall",
           fluidPage(
             div(
               style = "font-size: 15px; color: #333; margin-bottom: 10px;",
               HTML("<h4 style='color: #2c3e50;'>Unfolding the Rhythm of Rainfall 🌧️</h4>
          Every season tells a story through rain — nurturing crops, cooling cities, and shaping everyday life across Tamil Nadu.<br><br>
          This section uses <strong>multi-line time series plots</strong> to compare actual vs. normal rainfall trends by season and year.<br><br>
          <ul>
            <li><b>Color-coded Lines:</b> Each line pair shows the rainfall behavior of a particular season — actual vs. expected.</li>
            <li><b>Interactive Tips:</b> Use the checkboxes to explore how rainfall patterns differ across monsoon and dry seasons.</li>
            <li><b>Insights:</b> Notice deviations between normal and actual patterns — a powerful cue for understanding climate variation.</li>
          </ul>
          <em>Tip:</em> Select specific seasons or adjust the year range to tell your own climate story.")
             ),
             fluidRow(
               column(4,
                      wellPanel(
                        style = "background-color: #f7faff; border: 1px solid #d1e7f3; border-radius: 5px;",
                        sliderInput("year_range", "Year Range:", min = min(ts_rainfall$year), max = max(ts_rainfall$year), value = c(min(ts_rainfall$year), max(ts_rainfall$year)), sep = "")
                      )
               ),
               column(8,
                      fluidRow(
                        style = "background-color: #f7faff; border: 1px solid #d1e7f3; border-radius: 5px; padding: 10px;",
                        column(6, checkboxGroupInput("selected_actual", "Actual Rainfall Seasons:", choices = unique(ts_rainfall_long$season), selected = unique(ts_rainfall_long$season))),
                        column(6, checkboxGroupInput("selected_normal", "Normal Rainfall Seasons:", choices = unique(ts_rainfall_long$season), selected = unique(ts_rainfall_long$season)))
                      )
               )
             ),
             fluidRow(
               column(12, plotlyOutput("seasonal_plot", height = "400px"),
                      downloadButton("download_seasonal_plot", "Download Seasonal Rainfall Plot", class = "btn-primary")
               )
             )
           )
  ),
  
  tabPanel("District Rainfall",
           fluidPage(
             div(
               style = "font-size: 15px; color: #333; margin-bottom: 10px;",
               HTML("<h4 style='color: #2c3e50;'>Zoom Into Local Patterns 📍</h4>
          Every district experiences rainfall uniquely. In this section, you’ll explore <strong>clustered column charts</strong> that show <em>actual vs. normal rainfall</em> across the key seasons for your selected district.<br><br>
          <ul>
            <li><b>Grouped Bars:</b> Represent each season’s actual and normal rainfall side-by-side for easy comparison.</li>
            <li><b>Seasonal Breakdown:</b> Switch between SW Monsoon, NE Monsoon, Winter, and Hot Weather.</li>
            <li><b>District Selector:</b> Focus on your region of interest using the dropdown at the top.</li>
          </ul>
          <em>Tip:</em> Hover to see precise rainfall values, and use the download button to save the visualization for reports.")
             ),
             fluidRow(
               column(12,
                      selectInput("district", "District:", choices = unique(rainfall_districts_long$district)),
                      style = "background-color: #f7faff; border: 1px solid #d1e7f3; border-radius: 5px; padding: 10px;")
             )
           ),
           fluidRow(
             column(12, plotlyOutput("district_rain_plot", height = "500px"),
                    downloadButton("download_district_plot", "Download District Rainfall Plot", class = "btn-primary")
             )
           )
  ),

  
  tabPanel("Rainfall Distribution",
           fluidPage(
             div(
               style = "font-size: 15px; color: #333; margin-bottom: 10px;",
               HTML("<h4 style='color: #2c3e50;'>How Wet is Too Wet? 📊</h4>
          This section presents a <strong>bar chart of district counts</strong> grouped by rainfall range — offering a birds-eye view of how rainfall is distributed spatially across Tamil Nadu.<br><br>
          <ul>
            <li><b>Bar Height:</b> Indicates the number of districts falling within a particular rainfall range.</li>
            <li><b>Color Coding:</b> Separates actual vs. normal expected rainfall counts.</li>
            <li><b>Hover Interactivity:</b> Instantly see the list of districts for each bar in the plot and below it.</li>
          </ul>
          <em>Tip:</em> Use this section to identify extremes — districts with significantly more or less rainfall than expected.")
             ),
             fluidRow(
               column(12, plotlyOutput("range_plot", height = "400px"),
                      downloadButton("download_range_plot", "Download Range Plot", class = "btn-primary")
               )
             ),
             fluidRow(
               column(12, verbatimTextOutput("district_list"))
             ),
             hr(),
             fluidRow(
               column(12,
                      h4("References"),
                      tags$ul(
                        tags$li("Chennai, R. M. C. (2021, June 8). Regional Meteorological Centre. ",
                                tags$a(href = "https://www.data.gov.in/resource/temperature-select-stations-tamil-nadu-shb-2020", "Source")),
                        tags$li("India Meteorological Department. (2021, August 4). Time Series Data Rainfall by Seasons in Tamil Nadu : SHB 2020.",
                                tags$a(href = "https://www.data.gov.in/resource/rainfall-districts-shb-2020", "Source")),
                        tags$li("India Meteorological Department, Department of Economic Statistics. (2021, June 8). Rainfall by Districts : SHB 2020.",
                                tags$a(href = "https://www.data.gov.in/resource/rainfall-districts-shb-2020", "Source")),
                        tags$li("Regional Meteorological Centre. (2021, June 8). Districts by Range of Rainfall : SHB 2020.",
                                tags$a(href = "https://www.data.gov.in/resource/distribution-districts-range-rainfall-shb-2020", "Source"))
                      )
               )
             )
           )
  )
)
           
            
  


server <- function(input, output, session) {
  plot_cache <- reactiveValues()
  
  output$seasonal_plot <- renderPlotly({
    filtered_data <- ts_rainfall_long %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      filter((type == "Actual" & season %in% input$selected_actual) |
               (type == "Normal" & season %in% input$selected_normal))
    
    p <- ggplot(filtered_data, aes(x = year, y = rainfall, color = interaction(type, season), group = interaction(type, season), text = paste(type, season, ":", rainfall, "mm"))) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      theme_minimal(base_size = 11) +
      labs(x = "Year", y = "Rainfall (mm)", title = "Seasonal Rainfall Trends") +
      theme(legend.position = "bottom")
    plot_cache$seasonal <- p
    ggplotly(p, tooltip = "text")
  })
  
  output$district_rain_plot <- renderPlotly({
    df <- rainfall_districts_long %>% filter(district == input$district)
    p <- ggplot(df, aes(x = season_short, y = rainfall, fill = season_type, text = paste(season_type, season_short, ":", rainfall, "mm"))) +
      geom_col(position = position_dodge(width = 0.9)) +
      facet_wrap(~ season_type, scales = "free") +
      theme_minimal(base_size = 11) +
      labs(title = paste("District Rainfall by Season -", input$district), x = "Season", y = "Rainfall (mm)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plot_cache$district <- p
    ggplotly(p, tooltip = "text")
  })
  
  output$temp_plot <- renderPlotly({
    df <- temperature_long %>% filter(station == input$station)
    p <- ggplot(df, aes(x = month_parsed, y = temperature_c, color = temperature_type, group = temperature_type, text = paste(temperature_type, ":", temperature_c, "°C"))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c("mean_max_temp" = "#e76f51", "mean_min_temp" = "#2a9d8f")) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major = element_line(color = "#e0e0e0"), legend.position = "bottom") +
      labs(title = paste("Temperature Trends -", input$station), y = "°C", x = "Month")
    plot_cache$temp <- p
    ggplotly(p, tooltip = "text")
  })
  
  output$humidity_plot <- renderPlotly({
    df <- humidity_long %>% filter(station == input$station)
    p <- ggplot(df, aes(x = month_parsed, y = humidity_pct, color = humidity_type, group = humidity_type, text = paste(humidity_type, ":", humidity_pct, "%"))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c("humidity_morning" = "#219ebc", "humidity_evening" = "#ffb703")) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major = element_line(color = "#e0e0e0"), legend.position = "bottom") +
      labs(title = paste("Humidity Trends -", input$station), y = "% Humidity", x = "Month")
    plot_cache$humidity <- p
    ggplotly(p, tooltip = "text")
  })
  
  output$range_plot <- renderPlotly({
    p <- ggplot(rainfall_range_long, aes(x = range_of_rainfall, y = count, fill = type,
                                         text = paste0("Range: ", range_of_rainfall, "<br>Type: ", type, "<br>Districts: ", districts),
                                         key = districts)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("actual_count" = "#f4a261", "normal_count" = "#264653")) +
      theme_minimal(base_size = 11) +
      labs(title = "District Distribution by Rainfall Range", x = "Rainfall Range", y = "Number of Districts")
    plot_cache$range <- p
    ggplotly(p, tooltip = "text")
  })
  
  output$district_list <- renderText({
    hover <- event_data("plotly_hover")
    if (is.null(hover) || is.null(hover$key)) return("Hover over a bar to see the districts.")
    
    districts <- str_split(hover$key, ",\\s*")[[1]]
    paste("Districts:\n", paste("-", districts, collapse = "\n"))
  })
  
  output$download_temp_plot <- downloadHandler(
    filename = function() { paste0("temperature_plot_", input$station, ".png") },
    content = function(file) { ggsave(file, plot_cache$temp, width = 8, height = 4) }
  )
  
  output$download_humidity_plot <- downloadHandler(
    filename = function() { paste0("humidity_plot_", input$station, ".png") },
    content = function(file) { ggsave(file, plot_cache$humidity, width = 8, height = 4) }
  )
  
  output$download_seasonal_plot <- downloadHandler(
    filename = function() { "seasonal_rainfall_plot.png" },
    content = function(file) { ggsave(file, plot_cache$seasonal, width = 9, height = 5) }
  )
  
  output$download_district_plot <- downloadHandler(
    filename = function() { paste0("district_plot_", input$district, ".png") },
    content = function(file) { ggsave(file, plot_cache$district, width = 9, height = 5) }
  )
  
  output$download_range_plot <- downloadHandler(
    filename = function() { "range_distribution_plot.png" },
    content = function(file) { ggsave(file, plot_cache$range, width = 9, height = 5) }
  )
}

shinyApp(ui, server)

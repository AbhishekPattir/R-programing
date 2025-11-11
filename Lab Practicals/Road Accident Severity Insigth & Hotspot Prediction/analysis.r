install.packages("tidyverse")
install.packages("leaflet")

library(tidyverse)
library(leaflet)

# 2. LOAD DATA
file_path <- "cleaned_synthetic_traffic_accident_data.csv"

tryCatch({
  accidents_df <- read.csv(file_path)
}, error = function(e) {
  message("Error loading file. Make sure '", file_path, "' is in the correct directory.")
  message("Original error: ", e$message)
})


# 3. EXPLORATORY DATA ANALYSIS (EDA)

if (exists("accidents_df")) {
  
  message("--- Data Summary ---")
  # Get a statistical summary of the data
  print(summary(accidents_df))
  
  message("\n--- Data Structure ---")
  # Get the structure, data types, and column names
  str(accidents_df)
  

  # 4. VISUALIZATION: ACCIDENTS BY WEATHER CONDITION

  weather_counts <- accidents_df %>%
    count(Weather_Condition) %>%
    arrange(desc(n)) %>%
    # Use fct_reorder to sort the bars in the plot
    mutate(Weather_Condition = fct_reorder(Weather_Condition, n, .desc = TRUE))
  
  # Create the plot
  weather_plot <- ggplot(weather_counts, aes(x = Weather_Condition, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = "Traffic Accidents by Weather Condition",
      x = "Weather Condition",
      y = "Number of Accidents"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels
  
  # Save the plot
  ggsave("accidents_by_weather.png", plot = weather_plot, width = 10, height = 6)
  message("\nSaved 'accidents_by_weather.png'")
  

  # 5. VISUALIZATION: ACCIDENTS BY DAY OF WEEK

  day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  # Calculate counts for day of the week
  day_counts <- accidents_df %>%
    count(Day_of_Week) %>%
    # Convert Day_of_Week to a factor with the correct order
    mutate(Day_of_Week = factor(Day_of_Week, levels = day_order))
  
  # Create the plot
  day_plot <- ggplot(day_counts, aes(x = Day_of_Week, y = n)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    labs(
      title = "Traffic Accidents by Day of the Week",
      x = "Day of the Week",
      y = "Number of Accidents"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave("accidents_by_day.png", plot = day_plot, width = 10, height = 6)
  message("Saved 'accidents_by_day.png'")
  

  # 6. HOTSPOT MAPPING: VISUALIZING CLUSTERS
  accidents_df$Cluster <- as.factor(accidents_df$Cluster)
  
  # Get the number of unique clusters
  num_clusters <- n_distinct(accidents_df$Cluster)
  

  pal <- colorFactor(palette = "viridis", domain = accidents_df$Cluster)
  
  # Create the leaflet map
  hotspot_map <- leaflet(data = accidents_df) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%  # Add a base map
    addCircleMarkers(
      lng = ~Start_Lng,
      lat = ~Start_Lat,
      color = ~pal(Cluster),
      radius = 3,           # Size of the marker
      stroke = FALSE,       # No border
      fillOpacity = 0.7,
      # Add popup info
      popup = ~paste(
        "<b>Cluster:</b>", Cluster, "<br>",
        "<b>Weather:</b>", Weather_Condition, "<br>",
        "<b>Time:</b>", Start_Time
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~Cluster,
      title = "Accident Hotspots"
    )


print(hotspot_map)
  
library(htmlwidgets)
saveWidget(hotspot_map, file = "accident_hotspot_map.html")
message("Saved interactive map 'accident_hotspot_map.html'")
  
  
  # 7.RUNNING CLUSTERING

coords <- accidents_df %>% select(Start_Lat, Start_Lng)
  

set.seed(123) # for reproducible results
kmeans_result <- kmeans(coords, centers = 10, nstart = 25)
accidents_df$New_Cluster <- as.factor(kmeans_result$cluster)

  
  message("\n--- R Analysis Complete ---")
  
} else {
  message("Dataframe 'accidents_df' not found. Please check data loading step.")
}
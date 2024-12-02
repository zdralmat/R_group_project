#Matyas Zdralek use of copilot for dabugging and to help transforming graphs in to a web apps

toLoad <- c("stringr", "lubridate", "tibble", "ggplot2", "shiny", "tidyr", "dplyr", "ggrepel")

for (packageName in toLoad) { 
  if (!require(packageName, character.only = TRUE)) { 
    install.packages(packageName) 
    library(packageName, character.only = TRUE) 
  } }

netflixData <- read.csv("netflix_titles.csv")

nerflixData <- as_tibble(netflixData)

netflixData$date_added <- mdy(netflixData$date_added)

#it is impossible to fill in the NA no default will work and due to the fact that it is strings we can not do anything with it
#and omitting the rows with the NA will loose too much data


#too agresive
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  
  data %>% filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
}

#netflixData <- remove_outliers(netflixData, "release_year")

#can not get it to do what i wanted treshold 10 is closest to what i wanted
remove_outliers_zscore <- function(data, column, threshold = 10) {
  data <- data %>%
    mutate(z_score = (data[[column]] - mean(data[[column]], na.rm = TRUE)) / sd(data[[column]], na.rm = TRUE)) %>%
    filter(abs(z_score) <= threshold) %>%
    select(-z_score)
  
  return(data)
}


#netflixData <- remove_outliers_zscore(netflixData, "release_year")

remove_rare_values <- function(data, column, min_count) {
  value_counts <- data %>%
    group_by_at(column) %>%
    tally() %>%
    filter(n >= min_count)
  
  filtered_data <- data %>%
    filter(data[[column]] %in% value_counts[[column]])
  
  return(filtered_data)
}

netflixData <- remove_rare_values(netflixData, "release_year", 2)


netflixData$show_id <- as.numeric(gsub("s","",netflixData$show_id))
netflixData$cast <- strsplit(netflixData$cast, split = ", ")
netflixData$cast <- lapply(netflixData$cast, function(x) {
  if (length(x) == 0) { 
    return(NA) 
  } else { 
    x[x == ""] <- NA
    return(x) 
  } })  
netflixData$director <- strsplit(netflixData$director, split = ", ")
netflixData$director <- lapply(netflixData$director, function(x) {
  if (length(x) == 0) { 
    return(NA) 
    } else { 
      x[x == ""] <- NA
      return(x) 
      } })

netflixData$country <- strsplit(netflixData$country, split = ", ")
netflixData$country <- lapply(netflixData$country, function(x) {
  if (length(x) == 0) { 
    return(NA) 
  } else { 
    x[x == ""] <- NA
    return(x) 
  } })
 
netflixData$listed_in <- strsplit(netflixData$listed_in, split = ", ")

netflixDataTV <- netflixData[netflixData$type == "TV Show",]
netflixDataMovies <- netflixData[netflixData$type == "Movie",]


netflixDataMovies$duration <-as.numeric(gsub(" min","",netflixDataMovies$duration))

netflixDataTV$duration <- as.numeric(gsub(" Seasons?| Season", "", netflixDataTV$duration))



#Plotting

movies_count <- as.data.frame(table(netflixData$release_year))
colnames(movies_count) <- c("year", "count")  # Rename columns for clarity
movies_count$year <- as.numeric(as.character(movies_count$year))  # Ensure year is numeric

#Logartytmic plot of movies released in one year

ggplot(movies_count, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_log10() +  # Apply logarithmic scale to the y-axis
  labs(title = "Number of Movies Released Over the Years (Log Scale)",
       x = "Release Year",
       y = "Number of Movies (Logarithmic)") +
  theme_minimal()


ggplot(movies_count, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_smooth(method = "loess", color = "red", linewidth = 1) + # Add a smooth line 
  scale_y_log10() + # Apply logarithmic scale to the y-axis 
  labs(title = "Number of Movies Released Over the Years (Log Scale)", x = "Release Year", y = "Number of Movies (Logarithmic)") + theme_minimal()



first_year <- 2001 
last_year <- 2016 
ggplot(movies_count, aes(x = year, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_smooth(method = "loess", color = "red", size = 1) + # Add a smooth line 
  scale_y_log10() + # Apply logarithmic scale to the y-axis 
  scale_x_continuous(limits = c(first_year, last_year)) + # Set x-axis limits 
  labs(title = "Number of Movies Released Over the Years (Log Scale)", x = "Release Year", y = "Number of Movies (Logarithmic)") +
  theme_minimal()

#prep for hist by the director

director_counts <- netflixData %>%
  filter(!is.na(director)) %>%
  separate_rows(director, sep = ", ") %>%
  group_by(director) %>%
  summarise(num = n()) %>%
  ungroup()

director_counts <- director_counts %>%
  filter(director != "")

director_counts


works <- director_counts %>%
  group_by(num) %>%
  summarise(count = n()) %>%
  mutate(num = num) %>%
  select(num, count)
works

ggplot(works, aes(x = "", y = count, fill = factor(num))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Number of Works per Director", x = "", y = "", fill = "Number of Works") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())







# Movie vs TV show plot
type_count <- netflixData %>%
  count(netflixData$type)
type_count <- type_count %>%
  mutate(percentage = (n / sum(n)) * 100)

ggplot(type_count, aes(x = "", y = percentage, fill = `netflixData$type`)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +  # Converts to pie chart
  theme_void() +  # Removes background, grid, and labels
  ggtitle("Precentage of films and TV shows") +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5)) +  # Add labels to the chart
  scale_fill_manual(values = c("Movie" = "#1f77b4", "TV Show" = "#ff7f0e"))+
  labs(fill = "Legend")

# MOST POPULAR ACTORS ON NETFIX
#


# Assume netflixData is already loaded and has a 'cast' column

# Step 1: Expand the cast column into individual rows and clean the data
actors_df <- netflixData %>%
  filter(!is.na(cast)) %>%                             # Remove rows with missing cast data
  separate_rows(cast, sep = ",\\s*") %>%               # Split the 'cast' column into individual rows
  filter(str_detect(cast, "[A-Za-z]")) %>%             # Keep rows with at least one letter
  filter(nchar(cast) > 1)                              # Remove very short entries (e.g., single letters)

# Step 2: Count the occurrences of each actor
actor_counts <- actors_df %>%
  count(cast, sort = TRUE)                             # Count and sort actors by frequency

# Step 3: Visualize the top N actors (e.g., top 10)
top_n <- 10                                            # Adjust this number for more or fewer actors
top_actors <- actor_counts %>% top_n(n = top_n, wt = n)

# Create the plot
ggplot(top_actors, aes(x = reorder(cast, n), y = n, fill = cast)) +
  geom_col(show.legend = FALSE) +                      # Create a bar chart
  coord_flip() +                                       # Horizontal bars for readability
  labs(title = "Top Actors on Netflix",
       x = "Actors",
       y = "Number of Appearances") +
  theme_minimal()



# Assume netflixData is already loaded and has a 'cast' column

# Step 1: Expand the cast column into individual rows and clean the data
directors_df <- netflixData %>%
  filter(!is.na(director)) %>%                             # Remove rows with missing cast data
  separate_rows(director, sep = ",\\s*") %>%               # Split the 'cast' column into individual rows
  filter(str_detect(director, "[A-Za-z]")) %>%             # Keep rows with at least one letter
  filter(nchar(cast) > 1)                              # Remove very short entries (e.g., single letters)

# Step 2: Count the occurrences of each actor
directors_counts <- directors_df %>%
  count(director, sort = TRUE)                             # Count and sort actors by frequency

# Step 3: Visualize the top N actors (e.g., top 10)
top_n <- 10                                            # Adjust this number for more or fewer actors
top_directors <- directors_counts %>% top_n(n = top_n, wt = n)

# Create the plot
ggplot(top_directors, aes(x = reorder(director, n), y = n, fill = director)) +
  geom_col(show.legend = FALSE) +                      # Create a bar chart
  coord_flip() +                                       # Horizontal bars for readability
  labs(title = "Top Directors on Netflix",
       x = "Directors",
       y = "Movies/Shows directed") +
  theme_minimal()


#

# Assume netflixData has a 'country' column
# Step 1: Expand and clean the 'country' column
countries_df <- netflixData %>%
  filter(!is.na(country)) %>%                     
  separate_rows(country, sep = ",\\s*") %>%        
  mutate(country = str_trim(country)) %>%         
  filter(str_detect(country, "[A-Za-z]"))          

# Step 2: Count the occurrences of each country
country_counts <- countries_df %>%
  count(country, sort = TRUE)

# Step 3: Create "Other Countries" category
top_n <- 10  # Display top N countries
top_countries <- country_counts %>% 
  top_n(n = top_n, wt = n)                         # Extract top N countries

# Calculate the sum for "Other Countries"
other_sum <- sum(country_counts$n) - sum(top_countries$n)

# Add "Other Countries" row to the top_countries dataset
top_countries <- top_countries %>%
  add_row(country = "Other Countries", n = other_sum)

# Step 4: Plot with "Other Countries"
ggplot(top_countries, aes(x = reorder(country, n), y = n, fill = country)) +
  geom_col(show.legend = FALSE) +                 
  coord_flip() +                                   
  labs(title = "Top Countries on Netflix",
       x = "Country",
       y = "Number of Appearances") +
  theme_minimal()






#web app starts here

ui <- fluidPage(
  titlePanel("Netflix Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Movies Released Over the Years'",
        sliderInput("first_year", "First Year:", min = 1941, max = 2022, value = 1941),
        sliderInput("last_year", "Last Year:", min = 1941, max = 2022, value = 2022),
        actionButton("update", "Update Graph")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Number of Works per Director'",
        sliderInput("min_num", "Minimum Number of Works:", 
                    min = 1, max = 23, value = 1, step = 1, ticks = FALSE, animate = TRUE)
      ),
      conditionalPanel(
        condition = "input.tabs == 'Netflix Duration Visualization'",
        selectInput(
          inputId = "dataset", 
          label = "Select Dataset:", 
          choices = c("TV Shows" = "tv", "Movies" = "movies")
        )
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Movies Released Over the Years", plotOutput("moviesPlot")),
                  tabPanel("Number of Works per Director", plotOutput("pieChart")),
                  tabPanel("Netflix Duration Visualization", plotOutput("main_plot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  selected_years <- reactiveValues(first_year = 1941, last_year = 2022)
  
  observeEvent(input$update, {
    selected_years$first_year <- input$first_year
    selected_years$last_year <- input$last_year
  })
  
  observe({
    updateSliderInput(session, "last_year", min = input$first_year)
    updateSliderInput(session, "first_year", max = input$last_year)
  })
  
  output$moviesPlot <- renderPlot({
    movies_count <- netflixData %>%
      filter(release_year >= selected_years$first_year & release_year <= selected_years$last_year) %>%
      group_by(release_year) %>%
      summarise(count = n())
    
    ggplot(movies_count, aes(x = release_year, y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_smooth(method = "loess", formula = 'y ~ x', color = "red", size = 1) +
      scale_y_log10() +
      scale_x_continuous(limits = c(selected_years$first_year, selected_years$last_year)) +
      labs(title = "Number of Movies Released Over the Years (Log Scale)", x = "Release Year", y = "Number of Movies (Logarithmic)") +
      theme_minimal()
  })
  
  output$pieChart <- renderPlot({
    director_counts <- netflixData %>%
      filter(!is.na(director)) %>%
      separate_rows(director, sep = ", ") %>%
      group_by(director) %>%
      summarise(num = n()) %>%
      ungroup() %>%
      filter(director != "")
    
    works <- director_counts %>%
      group_by(num) %>%
      summarise(count = n()) %>%
      filter(num >= input$min_num) %>%
      mutate(num = num) %>%
      select(num, count)
    
    ggplot(works, aes(x = "", y = count, fill = factor(num))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Number of Works per Director", x = "", y = "", fill = "Number of Works") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks = element_blank())
  })
  
  output$main_plot <- renderPlot({
    # Choose dataset based on user input
    data <- if (input$dataset == "tv") {
      netflixDataTV
    } else {
      netflixDataMovies %>% filter(!is.na(duration))
    }
    
    # Ensure 'duration' column exists
    if (!"duration" %in% colnames(data)) {
      stop("Column 'duration' not found in the dataset.")
    }
    
    # Categorize durations
    if (input$dataset == "tv") {
      data$category <- ifelse(data$duration > 5, "+6", as.character(data$duration))
      legend_title <- "Duration in Series"
    } else {
      data$category <- cut(
        as.numeric(str_extract(data$duration, "\\d+")),
        breaks = c(-Inf, 50, 70, 90, 110, 130, 150, 170, 190, Inf),
        labels = c("0-50", "51-70", "71-90", "91-110", "111-130", "131-150", 
                   "151-170", "171-190", "191+"),
        include.lowest = TRUE
      )
      legend_title <- "Duration in Min"
    }
    
    # Count occurrences in each category and calculate percentages
    category_counts <- data %>%
      group_by(category) %>%
      summarise(count = n()) %>%
      mutate(percentage = round(count / sum(count) * 100, 1))
    
    # Donut chart with labels and legend
    ggplot(category_counts, aes(x = 2, y = count, fill = category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_label_repel(
        aes(label = paste0(category, "\n", percentage, "%")),
        position = position_stack(vjust = 0.5),
        size = 5,
        show.legend = FALSE,
        segment.color = "grey50"
      ) +
      labs(
        title = paste("Pie Chart of", input$dataset, "Durations"),
        x = NULL, y = NULL,
        fill = legend_title  # Dynamic legend title
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")
      ) +
      scale_fill_brewer(palette = "Set3") +
      xlim(1, 3)  # Donut chart effect
  })
}

shinyApp(ui = ui, server = server)
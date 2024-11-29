#Matyas Zdralek use of copilot for dabugging

toLoad <- c("stringr", "lubridate", "tibble", "ggplot2", "shiny", "tidyr", "dplyr")

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



                                        
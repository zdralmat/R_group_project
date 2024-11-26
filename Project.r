#Matyas Zdralek use of copilot for dabugging

toLoad <- c("stringr", "lubridate", "tibble", "ggplot2", "shiny", "tidyr")

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
netflixData$listed_in <- strsplit(netflixData$listed_in, split = ", ")

netflixDataTV <- netflixData[netflixData$type == "TV Show",]
netflixDataMovies <- netflixData[netflixData$type == "Movie",]


netflixDataMovies$duration <-as.numeric(gsub(" min","",netflixDataMovies$duration))

netflixDataTV$duration <- as.numeric(gsub(" Seasons?| Season", "", netflixDataTV$duration))


                                        
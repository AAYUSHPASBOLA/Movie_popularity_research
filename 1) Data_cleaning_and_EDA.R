# install.packages("outliers")

# Required libraries
library(tidyverse)
library(ggplot2) 
library(wordcloud)
library(GGally)
library(jsonlite)
library(outliers)

# Load the datasets
movies <- read.csv("tmdb_5000_movies.csv", stringsAsFactors = FALSE)
credits <- read.csv("tmdb_5000_credits.csv", stringsAsFactors = FALSE)

# Function to extract and clean JSON data for movie genres
extract_genres <- function(data) {
  data <- mutate(data, movie_genres = lapply(genres, function(x) {
    genre_names <- fromJSON(x)$name
    paste(genre_names, collapse = ", ")
  }))
  return(data)
}

# Function to extract and clean JSON data for movie keywords
extract_keywords <- function(data) {
  data <- mutate(data, movie_keywords = lapply(keywords, function(x) {
    keyword_names <- fromJSON(x)$name
    paste(keyword_names, collapse = ", ")
  }))
  return(data)
}

# Function to extract and clean JSON data for movie production_companies
extract_production_companies <- function(data) {
  data <- mutate(data, movie_production_companies = lapply(production_companies, function(x) {
    company_names <- fromJSON(x)$name
    paste(company_names, collapse = ", ")
  }))
  return(data)
}

# Function to extract and clean JSON data for movie production_countries
extract_production_countries <- function(data) {
  data <- mutate(data, movie_production_countries = lapply(production_countries, function(x) {
    country_names <- fromJSON(x)$name
    paste(country_names, collapse = ", ")
  }))
  return(data)
}

# Function to extract and clean JSON data for cast actors
extract_actors <- function(data) {
  data <- mutate(data, actors = lapply(cast, function(x) {
    actor_names <- fromJSON(x)$name
    paste(actor_names, collapse = ", ")
  }))
  return(data)
}

# Function to extract and clean JSON data for cast directors
extract_directors <- function(data) {
  data <- mutate(data, directors = lapply(crew, function(x) {
    # Filter crew members where the job is "Director"
    director_names <- fromJSON(x)$name[fromJSON(x)$job == "Director"]
    paste(director_names, collapse = ", ")
  }))
  return(data)
}

# Apply the functions to movies dataset
movies <- extract_genres(movies)
movies <- extract_keywords(movies)
movies <- extract_production_companies(movies)
movies <- extract_production_countries(movies)

# Apply the function to the credits dataset
credits <- extract_actors(credits)
credits <- extract_directors(credits)

#--------------------------------merging---------------------------------------
# Rename id column in movies to movie_id
colnames(movies)[4] ="movie_id"

# Merging the datasets based on movie ID
tmdb_data <- merge(movies, credits, by=c("movie_id", "title"))

# Replace zero values with NA
tmdb_data$budget[tmdb_data$budget == 0] <- NA
tmdb_data$revenue[tmdb_data$revenue == 0] <- NA

# Calculate the median for the "budget" and "revenue" columns
budget_median <- median(tmdb_data$budget, na.rm = TRUE)
revenue_median <- median(tmdb_data$revenue, na.rm = TRUE)

# Replace NA values with the respective medians
tmdb_data$budget[is.na(tmdb_data$budget)] <- budget_median
tmdb_data$revenue[is.na(tmdb_data$revenue)] <- revenue_median

# Remove NA values from the 'runtime' column
tmdb_data <- tmdb_data[!is.na(tmdb_data$runtime), ]

# Convert data types if necessary
# Example: Convert 'release_date' to Date type
tmdb_data$release_date <- as.Date(tmdb_data$release_date)

# Remove NA values from the 'release_date' column
tmdb_data <- tmdb_data[!is.na(tmdb_data$release_date), ]
#-------------------------------------------------------------------------------
# Function to remove outliers
remove_outliers <- function(df, cols) {
  # Calculate z-scores for each column
  z_scores <- sapply(df[cols], function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  
  # Keep rows where z-scores are within 3 standard deviations
  df_filtered <- df[rowSums(abs(z_scores) < 3, na.rm = TRUE) == length(cols), ]
  
  return(df_filtered)
}

# Columns to consider for outlier removal
cols_to_check <- c("budget", "popularity", "revenue", "runtime", "vote_average", "vote_count")

# Remove outliers
tmdb_data_cleaned <- remove_outliers(tmdb_data, cols_to_check)

# Additional cleaning steps (remove duplicates, handle missing values, etc.)
# Remove duplicates
tmdb_data_cleaned <- distinct(tmdb_data_cleaned)

# Check for missing values
missing_values <- colSums(is.na(tmdb_data_cleaned))
print(missing_values)

# Check for duplicates
duplicated_rows <- tmdb_data_cleaned[duplicated(tmdb_data_cleaned), ]
print(duplicated_rows)

tmdb_data_cleaned$year <- lubridate::year(tmdb_data_cleaned$release_date)

tmdb_data_cleaned$popularity_level <- cut(tmdb_data_cleaned$popularity,
                                          breaks = c(-Inf, quantile(tmdb_data_cleaned$popularity,
                                          c(1/3, 2/3)), Inf),
                                          labels = c("Low", "Medium", "High"))

#---------------------------------------------------------------------------------------
# Perform exploratory data analysis(EDA)
#---------------------------------------------------------------------------------------

# Word cloud of movie titles
wordcloud(tmdb_data_cleaned$title, min.freq = 7, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Split the 'genres' column into separate rows
movie_genre_split <- tmdb_data_cleaned %>%
  mutate(genres = strsplit(as.character(tmdb_data_cleaned$movie_genres), ", ")) %>%
  unnest(genres)

# Word cloud of movie genres
wordcloud(movie_genre_split$movie_genres, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
#---------------------------------------------------------------------------------------
# Correlation matrix
correlation_matrix <- cor(select(tmdb_data_cleaned, budget, revenue, popularity, runtime))
print(correlation_matrix)

#---------------------------------------------------------------------------------------
#GGally package to inspect correlation and distribution of some variables
relevant_columns <- c("budget", "revenue", "release_date", "vote_average", "vote_count", "runtime")
relevant_data <- tmdb_data_cleaned[, relevant_columns]
# Plot matrix using ggpairs
ggpairs(relevant_data)
#---------------------------------------------------------------------------------------
# Scatter plot of  runtime vs. vote average
ggplot(tmdb_data_cleaned, aes(x = vote_average, y = runtime)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = " Runtime vs. Vote Average .", x = "Vote Average", y = "Runtime (minutes)") +
  theme_minimal()
#---------------------------------------------------------------------------------------
# Distribution of vote average
ggplot(tmdb_data_cleaned, aes(x = vote_average)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Distribution of Vote Average", x = "Vote Average", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 10, by = 1))

#---------------------------------------------------------------------------------------
# Distribution of runtime
ggplot(tmdb_data_cleaned, aes(x = runtime)) +
  geom_histogram(binwidth = 10, fill = "red", color = "black") +
  labs(title = "Distribution of Runtime", x = "Runtime (minutes)", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 200, by = 20))

#---------------------------------------------------------------------------------------
# Plotting histogram of budget
ggplot(tmdb_data_cleaned, aes(x = budget)) +
  geom_histogram(binwidth = 10000000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Movie Budget", x = "Budget", y = "Frequency")

#---------------------------------------------------------------------------------------
# Number of movies released per year
movies_per_year <- tmdb_data_cleaned %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(count = n())

ggplot(movies_per_year, aes(x = year, y = count)) +
  geom_line() +
  labs(title = "Number of Movies Released per Year", x = "Year", y = "Number of Movies") +
  scale_x_continuous(breaks = seq(0, 2020, by = 2))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#---------------------------------------------------------------------------------------
# Average vote average per year
avg_vote_per_year <- tmdb_data_cleaned %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(avg_vote = mean(vote_average, na.rm = TRUE))

ggplot(avg_vote_per_year, aes(x = year, y = avg_vote)) +
  geom_line() +
  labs(title = "Average Vote Average per Year", x = "Year", y = "Average Vote Average")
#---------------------------------------------------------------------------------------
# Calculate average popularity by year
popularity_by_year <- tmdb_data_cleaned %>%
  group_by(year) %>%
  summarise(avg_popularity = mean(popularity, na.rm = TRUE))

# Plotting average popularity by year
ggplot(popularity_by_year, aes(x = year, y = avg_popularity)) +
  geom_line(color = "red") +
  geom_point(color = "blue") +
  labs(title = "Average Popularity of Movies by Year",
       x = "Year",
       y = "Average Popularity") +
  theme_minimal()
#---------------------------------------------------------------------------------------
# Scatter plot of budget vs. revenue
ggplot(tmdb_data_cleaned, aes(x = budget/1000000, y = revenue/1000000)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Budget vs. Revenue", x = "Budget (in millions)", y = "Revenue (in millions)")+
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 5))
#----------------------------------------------------------------------------------------

#selecting required columns for classification algorithms
tmdb_cols_required <- c("budget", "popularity", "vote_average", "vote_count", "revenue", "runtime", "year")

tmdb_cleaned_final <- tmdb_data_cleaned[tmdb_cols_required]

#colSums(tmdb_cleaned_final==0)

# Save cleaned dataset
# write.csv(tmdb_cleaned_final, "cleaned_tmdb_data_final.csv", row.names = FALSE)











#-------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------
# Average vote average per year
avg_vote_per_year <- tmdb_data_cleaned %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(avg_vote = mean(vote_average, na.rm = TRUE))

ggplot(avg_vote_per_year, aes(x = year, y = avg_vote)) +
  geom_line() +
  labs(title = "Average Vote Average per Year", x = "Year", y = "Average Vote Average")
#----------------------------------------------------------------------------------------
# Relationship between genre and average vote
# Calculate average vote for each genre
genre_vote_avg <- movie_genre_split %>%
  group_by(genres) %>%
  summarise(avg_vote = mean(vote_average))

# Plot the relationship between genre and average vote
ggplot(genre_vote_avg, aes(x = reorder(genres, avg_vote), y = avg_vote)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Vote by Genre", x = "Genre", y = "Average Vote") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#---------------------------------------------------------------------------------------
# EDA: Relationship between genre and revenue
genre_revenue <- tmdb_data_cleaned %>%
  mutate(revenue = ifelse(is.na(revenue), 0, revenue)) %>%
  separate_rows(movie_genres, sep = ",") %>%
  group_by(movie_genres) %>%
  summarise(avg_revenue = mean(revenue))

ggplot(genre_revenue, aes(x = reorder(movie_genres, avg_revenue), y = avg_revenue/1e6)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "Average Revenue by Genre (in millions)", x = "Genre", y = "Average Revenue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#---------------------------------------------------------------------------------------
# EDA: Relationship between genre and popularity
genre_popularity <- tmdb_data_cleaned %>%
  mutate(popularity = ifelse(is.na(popularity), 0, popularity)) %>%
  separate_rows(movie_genres, sep = ",") %>%
  group_by(movie_genres) %>%
  summarise(avg_popularity = mean(popularity))

ggplot(genre_popularity, aes(x = reorder(movie_genres, avg_popularity), y = avg_popularity)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Average Popularity by Genre", x = "Genre", y = "Average Popularity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# Scatter plot of vote_count vs. vote_average
ggplot(tmdb_data_cleaned, aes(x = vote_average, y = vote_count)) +
  geom_point(color = "blue") +
  labs(title = "Vote Count vs. Vote Average",
       x = "Vote Average",
       y = "Vote Count")
#---------------------------------------------------------------------------------------
# Box plot of vote average by popularity level
ggplot(tmdb_data_cleaned, aes(x = popularity_level, y = vote_average)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Vote Average by Popularity Level",
       x = "Popularity Level",
       y = "Vote Average")
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# coverting list type to string type for 
#movie_genres,movie_keywords, movie_production_companies, movie_production_countries, actors, directors
tmdb_data_cleaned <- tmdb_data_cleaned %>%
  mutate(
    movie_genres = sapply(tmdb_data_cleaned$movie_genres, paste, collapse = ","),
    movie_keywords = sapply(tmdb_data_cleaned$movie_keywords, paste, collapse = ","),
    movie_production_companies = sapply(tmdb_data_cleaned$movie_production_companies, paste, collapse = ","),
    movie_production_countries = sapply(tmdb_data_cleaned$movie_production_countries, paste, collapse = ","),
    actors = sapply(tmdb_data_cleaned$actors, paste, collapse = ","),
    directors = sapply(tmdb_data_cleaned$directors, paste, collapse = ",")
  )
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------

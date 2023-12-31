# import statements
library('rvest')
library(stringr)
library(ggplot2)
library(dplyr)

# scrapping data
## job title
scrape_title <- function(url) {

  # Read page URL
  page <- read_html(url)
  job_title <- page %>% html_nodes('.uo6mkd') %>% html_text()
  job_title <- gsub("\\*([^*]+)\\*|<[^<>]+>", "", job_title)
  job_title <- gsub("^/ ", "", job_title)
}


## location
scrape_location <- function(url) {

  # Read page URL
  page <- read_html(url)
  location <- page %>% html_nodes('.a1msqi6q .a1msqi6u ._1wkzzau0 .szurmz4 .a1msqi6m:nth-child(1) .lnocuo7') %>% html_text()
  location <- gsub("/.*", "", location)
  location <- gsub(" -.*", "", location)
}


## company name
scrape_co_name <- function(url) {

  # Read page URL
  page <- read_html(url)
  
  # Select all job elements
  job_elements <- html_nodes(page, ".uo6mkb")
  
  # Initialize vectors to store job titles and extracted company name
  extracted_name <- NULL
  
  # Loop through each job element
  for (i in 1:length(job_elements)) {
    
    # Extract the full company name text
    text <- html_text(html_nodes(job_elements[i], '._842p0a1'))
    
    if(length(text) == 0){
      text <- "Private Advertiser"
    }
    
    # Save name
    extracted_name[i] <- text
    
  }

  # Explicitly close the connection
  rm(page)

  return(extracted_name)
}


## date posted
# Helper function to process date information
process_date <- function(raw_date) {

  # Extract numeric value and time unit
  parts <- strsplit(raw_date, "\\s+")
  value <- as.numeric(sub("\\D+", "", parts[[1]][1]))
  unit <- substr(parts[[1]][1], nchar(parts[[1]][1]), nchar(parts[[1]][1]))

  if (unit == "h" | unit == "m" | unit == "s") {
    # If hours ago, return today's date
    return(format(Sys.Date(), format = "%Y-%m-%d"))
  } else if (unit == "d") {
    # If days ago, return date from (x) days ago
    return(format(Sys.Date() - lubridate::ddays(value), format = "%Y-%m-%d"))
  }

  # If no match, return NULL
  return(NULL)
}

# Function to scrape date
scrape_date <- function(url) {

  # Read page URL
  page <- read_html(url)
  # Select all job elements
  job_elements <- html_nodes(page, ".uo6mkb")

  # Initialize vectors to store job titles and extracted salaries
  extracted_date <- NULL

  # Loop through each job element
  for (i in 1:length(job_elements)) {
    # Extract the full salary text
    date_text <- html_text(html_nodes(job_elements[i], '.szurmz6 .lnocuo22.lnocuo7'))

    # Process the date information
    processed_date <- process_date(date_text)

    # Save processeed date
    extracted_date[i] <- processed_date

  }

  # Explicitly close the connection
  rm(page)

  # Return the extracted education levels
  return(extracted_date)

}


# salary
scrape_salary <- function(url) {

  # Read page URL
  page <- read_html(url)

  # Select all job elements
  job_elements <- html_nodes(page, ".uo6mkb")

  # Initialize vectors to store job titles and extracted salaries
  extracted_salary <- NULL

  # Loop through each job element
  for (i in 1:length(job_elements)) {

    # Check if salary information is present
    if (length(html_text(html_nodes(job_elements[i], '._16v7pfz3'))) == 0){

      extracted_salary[i] <- "NA"

    } else {

      # Extract the full salary text
      salary_text <- html_text(html_nodes(job_elements[i], '._16v7pfz3'))

      # Extract numeric values from the salary text
      salary_bounds <- strsplit(salary_text, " – | - ")

      # Convert the lower and upper bounds to numeric values
      lower_bound <- as.numeric(gsub("[^0-9]+", "", salary_bounds[[1]][1]))
      upper_bound <- as.numeric(gsub("[^0-9]+", "", salary_bounds[[1]][2]))

      # Extract the currency
      currency <- gsub("[0-9,]+", "", extracted_salary[i])

      # Check if the salary values represent a range
      if (!is.na(lower_bound) && !is.na(upper_bound)) {
        average_salary <- mean(c(lower_bound, upper_bound))
        extracted_salary[i] <- average_salary
      } else {
        extracted_salary[i] <- lower_bound
      }
    }
  }

  # Explicitly close the connection
  rm(page)

  # Return the extracted salaries
  return(extracted_salary)

}


## company size
scrape_co_size <- function(url) {

  all_company_size <- NULL

  # Read page URL
  page <- read_html(url)

  # Define empty list for all education levels
  all_company_size <- list()

  # Select all job links
  job_links <- page %>% html_nodes(".uo6mkd") %>% html_attr("href")

  # Loop through each job link
  for (job_link in job_links) {

    # Construct the full URL for the job article
    article_url <- paste0("https://www.jobstreet.com.my", job_link)

    # Add delay to prevent request limit error
    Sys.sleep(1)

    # Navigate to the article page
    article_page <- read_html(article_url)

    # Check if there is a link for the company
    company_title <- article_page %>% html_node(".lnocuod ._126xumx1") %>% html_attr("href")

    # Check if company title exists
    if(is.na(company_title)){

      all_company_size <- c(all_company_size, "Not Specified")

    } else {

      company_url <- paste0("https://www.jobstreet.com.my", company_title)

      # Add delay to prevent request limit error
      Sys.sleep(2)

      # Navigate to the article page
      company_page <- read_html(company_url)

      # Class containing education information
      size_elements <- company_page %>% html_nodes("._2q2j1u6v:nth-child(3) ._1athzic1")

      if(length(size_elements) == 0 || !grepl("employees", size_elements, ignore.case = TRUE)){
        size_elements <- company_page %>% html_nodes("._2q2j1u6v:nth-child(2) ._1athzic1")
      }
      
      if(length(size_elements) == 0 || !grepl("employees", size_elements, ignore.case = TRUE)){
        size_elements <- company_page %>% html_nodes("._2q2j1u6v:nth-child(1) ._1athzic1")
      }
      
      if(length(size_elements) == 0 || !grepl("employees", size_elements, ignore.case = TRUE)){
        size_elements <- company_page %>% html_nodes("._2q2j1u6v+ ._2q2j1u6v ._1athzic1")
      }
      

      # Extract text from education elements
      extracted_size <- html_text(size_elements)
      print(extracted_size)

      all_company_size <- c(all_company_size, extracted_size[1])
    }

    # Explicitly close the connection
    rm(article_page)

  }

  return(all_company_size)
}


## job type
scrape_job_type <- function(url) {

  # Read page URL
  page <- read_html(url)

  # Define empty list for all education levels
  job_types <- list()

  # Select all job links
  job_links <- page %>% html_nodes(".uo6mkd") %>% html_attr("href")

  # Loop through each job link
  for (job_link in job_links) {

    # Construct the full URL for the job article
    article_url <- paste0("https://www.jobstreet.com.my", job_link)

    # Add delay to prevent request limit error
    Sys.sleep(1)

    # Navigate to the article page
    article_page <- read_html(article_url)

    # Scrape all job types
    job_type <- article_page %>% html_nodes('.a1msqi6u:nth-child(3) .a1msqir+ .a1msqir') %>% html_text()

    # Save scrapped text
    job_types <- c(job_type, job_types)

    # Explicitly close the connection
    rm(article_page)
  }

  return(job_types)
}


## Classification
scrape_class <- function(url) {

  # Read page URL
  page <- read_html(url)

  #Scraping the classification section using CSS selector
  class <- page %>% html_nodes('.a1msqibu:nth-child(5)') %>% html_text()

  #Removing the brackets
  class <- gsub("[()]", "", class)

  # Explicitly close the connection
  rm(page)

  return(class)
}


## Ratings
scrape_ratings <- function(url) {

  # Read page URL
  page <- read_html(url)

  # Define empty list for all education levels
  ratings <- list()

  # Select all job links
  job_links <- page %>% html_nodes(".uo6mkd") %>% html_attr("href")

  # Loop through each job link
  for (job_link in job_links) {

    # Construct the full URL for the job article
    article_url <- paste0("https://www.jobstreet.com.my", job_link)

    # Add delay to prevent request limit error
    Sys.sleep(1)

    # Navigate to the article page
    article_page <- read_html(article_url)

    # Check if ratings are present
    if (length(html_text(html_nodes(article_page, '._1jcz3123'))) == 0){

      extracted_ratings <- "NA"

    } else {

      # Extract the ratings
      extracted_ratings <- html_text(html_nodes(article_page, '._1jcz3123'))
    }

    ratings <- c(ratings, extracted_ratings)

    # Explicitly close the connection
    rm(article_page)
  }

  # Return the extracted ratings
  return(ratings)

}


## education level (Cheryl)
scrape_edu_level <- function(url) {

  # Read page URL
  page <- read_html(url)

  # Define empty list for all education levels
  all_education_levels <- list()

  # Select all job links
  job_links <- page %>% html_nodes(".uo6mkd") %>% html_attr("href")

  # Loop through each job link
  for (job_link in job_links) {

    # Construct the full URL for the job article
    article_url <- paste0("https://www.jobstreet.com.my", job_link)

    # Add delay to prevent request limit error
    Sys.sleep(1)

    # Navigate to the article page
    article_page <- read_html(article_url)

    # Class containing education information
    education_elements <- article_page %>% html_nodes("._1pehz540 li, p~ p+ p")

    # Extract text from education elements
    extracted_education <- html_text(education_elements)

    # Keywords to check for in education text
    edu_keywords <- c("diploma", "advanced/higher/graduate diploma", "degree", "pre-u", "master", "phd")

    # Check for keywords in the education text
    matched_keywords <- edu_keywords[sapply(edu_keywords, function(keyword) any(grepl(keyword, extracted_education, ignore.case = TRUE)))]

    # If keywords are found, append to the parent list; otherwise, set education level to "Not Specified"
    if (length(matched_keywords) > 0) {
      all_education_levels <- c(all_education_levels, list(matched_keywords))
    } else {
      all_education_levels <- c(all_education_levels, list("Not Specified"))
    }

    # Explicitly close the connection
    rm(article_page)
  }

  # Return the extracted education levels
  return(all_education_levels)

}

## Main loop to scrape 4 pages of data
all_salaries <- NULL
all_education_levels <- NULL
job_title <- NULL
location <- NULL
date <- NULL
company_name <- NULL
class <- NULL
rating <- NULL
company_size <- NULL
job_type <- NULL

# Website URL
url <- 'https://www.jobstreet.com.my/jobs/in-Malaysia'

# Print message to indicate scrapping started
print("Scrapping webpages... (Might take up to 10 - 20 minutes)")

# Loop through pages to be scrapped
for (page_number in 1:2) {

  # Form page URL
  page_url <- paste0(url, "?page=", page_number)

  # Scrape job title
  job_title <- c(job_title, scrape_title(page_url))

  if(page_number == 2){
    print("finish scrapping job title (1/10)")
  }

  # Scrape location
  location <- c(location, scrape_location(page_url))

  if(page_number == 2){
    print("finish scrapping location (2/10)")
  }

  # Scrape company name
  company_name <- c(company_name, scrape_co_name(page_url))

  if(page_number == 2){
    print("finish scrapping company name (3/10)")
  }

  # Scrape salary
  all_salaries <- c(all_salaries, scrape_salary(page_url))

  if(page_number == 2){
    print("finish scrapping salary (4/10)")
  }

  # Scrape job type
  job_type <- c(job_type, scrape_job_type(page_url))

  if(page_number == 2){
    print("finish scrapping job type (5/10)")
  }

  # Scrape date posted
  date <- c(date, scrape_date(page_url))

  if(page_number == 2){
    print("finish scrapping date posted (6/10)")
  }

  # Scrape education level
  all_education_levels <- c(all_education_levels, scrape_edu_level(page_url))

  if(page_number == 2){
    print("finish scrapping education level (7/10)")
  }

  # Scrape company size
  company_size <- c(company_size, scrape_co_size(page_url))

  if(page_number == 2){
    print("finish scrapping company size (8/10)")
  }

  # Scrape classification
  class <- c(class, scrape_class(page_url))

  if(page_number == 2){
    print("finish scrapping class (9/10)")
  }

  # Scrape ratings
  rating <- c(rating, scrape_ratings(page_url))

  if(page_number == 2){
    print("finish scrapping rating (10/10)")
  }

}

print(job_title)

length_of_data <- length(all_salaries)

# Forming Data frame
data <- data.frame(
  Job_Title = rep("Not specified", length_of_data),
  Location = rep("Not specified", length_of_data),
  Company_Name = rep("Not specified", length_of_data),
  Salary = rep("Not specified", length_of_data),
  Job_Type = rep("Not specified", length_of_data),
  Date_Posted = rep("Not specified", length_of_data),
  Education_Level = rep("Not specified", length_of_data),
  Company_Size = rep("Not specified", length_of_data),
  Classification = rep("Not specified", length_of_data),
  Ratings = rep("Not specified", length_of_data)
)

# Populate the data frame with actual values
data$Salary <- all_salaries
data$Education_Level <- all_education_levels
data$Job_Title <- job_title
data$Location <- location
data$Ratings <- rating
data$Classification <- class
data$Date_Posted <- date
data$Company_Name <- company_name
data$Job_Type <- job_type
data$Company_Size <- company_size

# Add an index column
data$Index <- seq_along(all_salaries)

# Reorder columns to have the Index column first
data <- data[, c("Index", names(data)[-ncol(data)])]

# Convert list-type columns to characters
data[] <- lapply(data, function(x) if (is.list(x)) as.character(x) else x)

# Export data frame to CSV
write.csv(data, "job_data.csv", row.names = FALSE)

#==============================================================================================================#
# # Data Analysis
# job_data <- read.csv("job_data.csv")
# 
# ## Company Distribution
# company_counts <- table(job_data$Company_Name)
# company_counts_df <- data.frame(Company_Name = names(company_counts), count = as.numeric(company_counts))
# 
# # Select the top 5 companies
# top_5 <- head(company_counts_df[order(-company_counts_df$count), ], 5)
# 
# # Distribution of the top 5 companies w/ the most job postings
# Company_Distribution_top5 <- ggplot(top_5, aes(x = "", y = count, fill = Company_Name)) +
#   geom_col(color = "black") + scale_fill_brewer() +
#   coord_polar(theta = "y") +
#   theme(legend.text = element_text(size = 10),
#         axis.title = element_blank()) +
#   labs(fill = NULL) +
#   geom_text(aes(label = count), position = position_stack(vjust = 0.5)) + 
#   ggtitle("Top 5 Companies with the Most Job Postings")
# ggsave("Company_Distribution_top5.png", plot = Company_Distribution_top5, width = 11, height = 8)
# 
# Company_Distribution_all <- ggplot(company_counts_df, aes(x = "", y = count, fill = Company_Name)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y") +
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 10),
#         axis.title = element_blank()) +
#   labs(fill = NULL) +
#   ggtitle("Distribution of Job Postings Across Companies")
# ggsave("Company_Distribution_all.png", plot = Company_Distribution_all, width = 18, height = 12)
# 
# 
# 
# ## Job distribution by location
# ## geo bar graph
# states_data <- job_data
# states_data$States <- states_data$Location
# states_data$States <- gsub(".*,", "", states_data$States)
# states_data$States <- gsub("^ +| +$", "", states_data$States)
# states_data$States <- gsub("Shah Alam", "Selangor", states_data$States)
# states_data$States <- gsub("Klang", "Selangor", states_data$States)
# 
# # count df
# states_count <- table(states_data$States)
# states_count_df <- data.frame(States = names(states_count), Count = as.numeric(states_count))
# 
# # job distribution by location stack bar graph
# Job_Distribution_stacked <- ggplot(states_data, aes(x = States, fill = Job_Title)) +
#   geom_bar(position = "stack", width = 0.75) +
#   labs(title = "Job Distribution by States", x = "States", y = "Count") +
#   theme(axis.title.y = element_text(vjust = 2),
#         axis.title.x = element_text(vjust = -0.09)) +
#   theme(legend.position = "none")
# ggsave("Job_Distribution_Stacked.png", plot = Job_Distribution_stacked, width = 11, height = 8)
# 
# # job distribution by location (percentage)
# Job_Distribution_total <- ggplot(states_count_df, aes(x = reorder(States, -Count), y = Count, fill = Count)) +
#   geom_bar(stat = "identity", width = 0.75) +
#   labs(title = "Job Distribution by All States", x = "States", y = "Count") +
#   geom_text(aes(label = paste(as.character(round(100*Count/sum(Count), digits = 1)), "%")), vjust = -0.5) +
#   theme(axis.title.y = element_text(vjust = 2),
#         axis.title.x = element_text(vjust = -0.09)) +
#   theme(legend.position = "none")
# ggsave("Job_Distribution_Total.png", plot = Job_Distribution_total, width = 11, height = 8)
# 
# 
# ## Time Trend
# # Read CSV file for plot
# job_data <- read.csv("job_data.csv")
# 
# # Plot line graph
# time_trend_plot <- ggplot(job_data, aes(x = Date_Posted)) +
#   geom_bar(fill = "skyblue", color = "black") +
#   labs(title = "Job Posting Time Trend",
#        x = "Date",
#        y = "Number of Job Postings") +
#   theme_minimal()
# 
# ggsave("time_trend_histogram.png", time_trend_plot, width = 10, height = 6)
# 
# 
# ## Company size by frequency
# # Read CSV file for plot
# job_data <- read.csv("job_data.csv")
# 
# # Plot histogram
# company_size_frequency <- ggplot(job_data, aes(x = Company_Size)) +
#   geom_bar(fill = "skyblue", color = "black") +
#   labs(title = "Company Size Frequency",
#        x = "Company Size",
#        y = "Frequency") +
#   theme_minimal()
# 
# ggsave("company_size_by_frequency.png", company_size_frequency, width = 10, height = 6)
# 
#
#  # Classification by frequency
#  # Read CSV file
#  job_data <- read.csv("job_data.csv")
# 
#  # Plot histogram for classification frequency
#  experience_plot <- ggplot(job_data, aes(y = Classification)) +
#    geom_bar(fill = "skyblue", color = "black") +
#    labs(title = "Classification by Frequency",
#         y = "Classification",
#         x = "Frequency") +
#    theme_minimal()
# 
#  # Save the plot as a PNG file
#  ggsave("classification_histogram.png", experience_plot, width = 8, height = 6)
# 
# 
#  #Education level by frequency
#  # Read CSV file
#  job_data <- read.csv("job_data.csv")
# 
#  # Remove 'c()' from each entry if present
#  job_data$Education_Level <- gsub("c\\((.*)\\)", "\\1", job_data$Education_Level)
# 
#  # Convert each entry to a list of education levels
#  job_data$Education_Level <- sapply(strsplit(as.character(job_data$Education_Level), ", "), as.list)
# 
#  # If the education level is saved with double quotes, remove them
#  job_data$Education_Level <- sapply(job_data$Education_Level, function(levels) {
#    levels <- gsub('"', '', levels)
#    if (length(levels) == 1 && levels == "") {
#      return(list("Not Specified"))
#    } else {
#      return(levels)
#    }
#  }, simplify = FALSE)
# 
#  # Create a data frame with each education level as a separate row
#  education_data <- data.frame(
#    Education_Level = unlist(job_data$Education_Level),
#    stringsAsFactors = FALSE
#  )
# 
#  # Plot histogram for education level (horizontal)
#  education_plot <- ggplot(education_data, aes(y = Education_Level)) +
#    geom_bar(fill = "skyblue", color = "black", stat = "count") +
#    labs(title = "Education Level Distribution",
#         y = "Education Level",
#         x = "Frequency") +
#    theme_minimal() +
#    theme(axis.text.y = element_text(hjust = 0))
# 
#  # Save the plot as a PNG file
#  ggsave("education_level_histogram.png", education_plot, width = 10, height = 6)
# 
# 
#  ## Job type by frequency
#  # Read the CSV file
#  job_data <- read.csv("job_data.csv")
# 
#  # Plot histogram for Job Type by frequency
#  plot <- ggplot(job_data, aes(x = Job_Type)) +
#    geom_bar(fill = "skyblue", color = "black") +
#    labs(title = "Job Type Distribution", x = "Job Type", y = "Frequency") +
#    theme_minimal()
# 
#  # Save the plot as a PNG file
#  ggsave("job_type_histogram.png", plot, width = 8, height = 6, units = "in", dpi = 300)
# 
# 
#  ## Salary by job type
#  # Read CSV file
#  job_data <- read.csv("job_data.csv")
# 
#  # Install and load required library for plotting
#  library(ggplot2)
# 
#  # Replace "Not specified" with NA in Salary column
#  job_data$Salary[job_data$Salary == "Not specified"] <- NA
# 
#  # Convert Salary column to numeric, ignoring non-numeric entries
#  job_data$Salary <- as.numeric(gsub("[^0-9.]+", "", job_data$Salary))
# 
#  # Filter out rows with non-finite salary values
#  job_data <- job_data[is.finite(job_data$Salary), ]
# 
#  # Get unique job types
#  unique_job_types <- unique(job_data$Job_Type)
# 
#  # Create a list to store individual plots
#  boxplot_list <- list()
# 
#  # Create a grouped boxplot
#  grouped_boxplot <- ggplot(job_data, aes(x = Job_Type, y = Salary)) +
#    geom_boxplot() +
#    labs(title = "Grouped Boxplot of Salary by Job Type",
#         x = "Job Type",
#         y = "Salary (Monthly)") +
#    theme(axis.text.x = element_text(angle = 45, hjust = 1),
#          legend.position = "none")  # Remove legend to avoid redundancy
#  # Save the grouped boxplot as a PNG file
#  ggsave("grouped_salary_boxplot.png", grouped_boxplot, width = 12, height = 8, units = "in", dpi = 300)
# 
# 
# ## Ratings by Frequency
#  # Read the CSV file
#  job_data <- read.csv("job_data.csv")
# 
#  # 1. Handle Missing Values
#  job_data <- job_data[complete.cases(job_data$Ratings), ]
# 
#  # 2. Handle Infinite Values
#  job_data <- job_data[is.finite(job_data$Ratings), ]
# 
#  # 3. Check Data Type
#  job_data$Ratings <- as.numeric(job_data$Ratings)
# 
#  # Plot histogram for Ratings by frequency
#  ratings_plot <- ggplot(job_data, aes(x = Ratings)) +
#    geom_bar(fill = "yellow", color = "black", position = "identity") +
#    labs(title = "Total Ratings", x = "Ratings 1 to 5", y = "Frequency")
# 
#  # Save the plot as a PNG file
#  ggsave("Ratings_histogram.png", ratings_plot, width = 8, height = 6)

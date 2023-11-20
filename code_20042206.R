# import statements
library('rvest')
library(stringr)
<<<<<<< Updated upstream
=======
library(ggplot2)
library(dplyr)
>>>>>>> Stashed changes

# scrapping data

## job title (Gladys)
scrape_title <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  job_title <- page %>% html_nodes('.uo6mkd') %>% html_text()
  job_title <- gsub("/.*|\\(.*\\)| -.*| –.*| /.*", "", job_title)
}


## location (Gladys)
scrape_location <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  location <- page %>% html_nodes('.a1msqi6q .a1msqi6u ._1wkzzau0 .szurmz4 .a1msqi6m:nth-child(1) .lnocuo7') %>% html_text()
  location <- gsub("/.*", "", location)
<<<<<<< Updated upstream
  location <- gsub(" -.*", "", location)
=======
>>>>>>> Stashed changes
}


## company name (Gabriel)
scrape_co_name <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
}


## date posted (Gabriel)
scrape_date <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
}


## salary (Cheryl)
scrape_salary <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
  # Select all job elements
  job_elements <- html_nodes(page, ".rqoqz0")
  
  # Initialize vectors to store job titles and extracted salaries
  extracted_salary <- NULL
  
  # Loop through each job element
  for (i in 1:length(job_elements)) {
   
    # Check if salary information is present
    if (length(html_text(html_nodes(job_elements[i], '.y44q7ih+ .y44q7ih'))) == 0){
      
      extracted_salary[i] <- NA
      
    }else {
      
      extracted_salary[i] <- html_text(html_nodes(job_elements[i],'.y44q7ih+ .y44q7ih'))
      
      if (grepl("monthly", extracted_salary[i], ignore.case = TRUE)) {
        
        # Extract numeric values from the salary text
        salary_values <- as.numeric(gsub("[^0-9]+", "", extracted_salary[i]))
        
        # Calculate the average for salary ranges
        if (length(salary_values) == 2) {
          
          average_salary <- mean(salary_values)
          extracted_salary[i] <- paste(average_salary, "monthly")
          
        } else {
          
          # If there is only one value, use it as the average
          extracted_salary[i] <- paste(salary_values, "monthly")
          
        }
      }
    }
  }
  
  # Return the extracted salaries
  return(extracted_salary)
  
}
  

## company size (Bryan)
scrape_co_size <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
}


## job type (Bryan)
scrape_job_type <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
}


## working hour (Marcus)
scrape_working_hour <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
}


## experience level (Marcus)
scrape_exp_level <- function(url) {
  
  # Read page URL
  page <- read_html(url)

}


## education level (Cheryl)
scrape_edu_level <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
  # Define empty list for all education levels
  all_education_levels <- list()
  
  # Select all job links
  job_links <- page %>% html_nodes("#jobList article h1 a") %>% html_attr("href")
  
  # Loop through each job link
  for (job_link in job_links) {
    
    # Construct the full URL for the job article
    article_url <- paste0("https://www.jobstreet.com.my", job_link)  # Replace with the actual base URL
    
    # Add delay to prevent request limit error
    Sys.sleep(1)
    
    # Navigate to the article page
    article_page <- read_html(article_url)
    
    # Replace ".your-class" with the actual class containing education information
    education_element <- article_page %>% html_node(".pmwfa57:nth-child(2) .y44q7i1")
    
    # Extract text from the education element
    extracted_education <- html_text(education_element)
    
    # Split the extracted education text into a list of education levels
    edu_levels <- unlist(strsplit(extracted_education, ", "))
    
    # Append the list of education levels to the parent list
    all_education_levels <- c(all_education_levels, list(edu_levels))
    
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
url <- 'https://www.jobstreet.com.my/jobs/in-Malaysia'

print("Scrapping webpages... (Might take up to 3 - 5 minutes)")

for (page_number in 1:4) {
  page_url <- paste0(url, "?pg=", page_number)
  all_salaries <- c(all_salaries, scrape_salary(page_url))
  all_education_levels <- c(all_education_levels, scrape_edu_level(page_url))
  job_title <- c(job_title, scrape_title(page_url))
  location <- c(location, scrape_location(page_url))
}

# Print the results
print(length(all_salaries))
print(length(all_education_levels))
print(length(job_title))
print(length(location))

# Forming Data frame



# Data Analysis
<<<<<<< Updated upstream
## Company Distribution (Gladys) - Pie Charts

<<<<<<< Updated upstream
=======
  # Read CSV file
    job_data <- read.csv("job_data.csv")

  company_counts <- table(job_data$Company_Name)
  company_counts_df <- data.frame(Company_Name = names(company_counts), count = as.numeric(company_counts))

  # Select the top 5 companies
    top_5 <- head(company_counts_df[order(-company_counts_df$count), ], 5)
  
  # Distribution of the top 5 companies w/ the most job postings
    Company_Distribution_top5 <- ggplot(top_5, aes(x = "", y = count, fill = Company_Name)) +
      geom_col(color = "black") + scale_fill_brewer() +
      coord_polar(theta = "y") +
      theme(legend.text = element_text(size = 10),
            axis.title = element_blank()) +
      labs(fill = NULL) +
      geom_text(aes(label = count), position = position_stack(vjust = 0.5)) + 
      ggtitle("Top 5 Companies with Most Job Postings")
    ggsave("Company_Distribution_top5.png", plot = Company_Distribution_top5, width = 11, height = 8)
  
  
  # Distribution of All Companies
    Company_Distribution_all <- ggplot(company_counts_df, aes(x = "", y = count, fill = Company_Name)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 10),
            axis.title = element_blank()) +
      labs(fill = NULL) +
      ggtitle("Distribution of Job Postings Across All Companies")
    ggsave("Company_Distribution_all.png", plot = Company_Distribution_all, width = 22, height = 12)
=======
## Company Distribution (Gladys)
company_counts <- table(job_data$Company_Name)
company_counts_df <- data.frame(Company_Name = names(company_counts), count = as.numeric(company_counts))

# Select the top 5 companies
top_5 <- head(company_counts_df[order(-company_counts_df$count), ], 5)

# Distribution of the top 5 companies w/ the most job postings
Company_Distribution_top5 <- ggplot(top_5, aes(x = "", y = count, fill = Company_Name)) +
  geom_col(color = "black") + scale_fill_brewer() +
  coord_polar(theta = "y") +
  theme(legend.text = element_text(size = 10),
        axis.title = element_blank()) +
  labs(fill = NULL) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5)) + 
  ggtitle("Top 5 Companies with the Most Job Postings")
ggsave("Company_Distribution_top5.png", plot = Company_Distribution_top5, width = 11, height = 8)

Company_Distribution_all <- ggplot(company_counts_df, aes(x = "", y = count, fill = Company_Name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.title = element_blank()) +
  labs(fill = NULL) +
  ggtitle("Distribution of Job Postings Across Companies")
ggsave("Company_Distribution_all.png", plot = Company_Distribution_all, width = 18, height = 12)


>>>>>>> Stashed changes


>>>>>>> Stashed changes
## Time Trend (Gabriel)

## Company size by frequency (Bryan)

## Company size vs job post available duration (Gabriel)

## Experience level by frequency (Bryan)

## Education level by frequency (Bryan)

## Contract type vs average working hour (Marcus)

## Contract type by frequency (Cheryl)

## Salary by job category (Cheryl)

## Salary vs Experience level (Marcus)

## Job distribution by location (Gladys)
<<<<<<< Updated upstream
  #scraping data for states and cities
    geo_url <- 'https://www.wiki3.en-us.nina.az/List_of_cities_and_towns_in_Malaysia_by_population.html'
    geo_webpage <- read_html(geo_url)
    states <- geo_webpage %>% html_nodes('.flagicon div div') %>% html_text()
    geo_location <- geo_webpage %>% html_nodes('td:nth-child(2) a') %>% html_text()
    geo_location <- geo_location[1:60]
    my_states_cities <- data.frame(states, geo_location)
    my_states_cities$states <- gsub("^.{2}", "_", my_states_cities$states)
    my_states_cities$states <- gsub(" ", "_", my_states_cities$states)
    my_states_cities$states <- gsub("^_+|_+$", "", my_states_cities$states)
    my_states_cities$states <- gsub("_", " ", my_states_cities$states)
    my_states_cities$states <- gsub("Federal Territories", "Kuala Lumpur", my_states_cities$states)
  
  # Levels for states
    states_list <- c(as.list(unique(my_states_cities$states)), "Other Small Towns")
    states_vector <- as.character(states_list)
  
  # merging states into merge_geo_data
    # add state according to cities in geo_location
      merged_geo_data <- job_data %>% rename(geo_location = Location) %>% left_join(my_states_cities, by = "geo_location")
  
    # if statement to add state in merged_geo_data$states if geo_location = my_states_cities$states 
      for (geo_location in merged_geo_data$geo_location){
        if (geo_location %in% my_states_cities$states){
          row_number <- which(merged_geo_data$geo_location == geo_location)
          merged_geo_data$states[row_number] <- geo_location
        }
      }
      
    # data cleaning
      merged_geo_data$states[is.na(merged_geo_data$states)] <- "Other Small Towns"
      merged_geo_data$geo_location <- gsub("Melaka", "Malacca", merged_geo_data$geo_location)
      
    # adding levels into merged_geo_data$states 
      merged_geo_data_factor <- merged_geo_data
      merged_geo_data_factor$states <- factor(merged_geo_data$states, levels = states_vector)
  
  #stacked bar graph
    JD_count <- table(merged_geo_data$states)
    JD_count_df <- data.frame(States = names(JD_count), Count = as.numeric(JD_count))
  
    # job distribution by location stack bar graph
      Job_Distribution_stacked <- ggplot(merged_geo_data_factor, aes(x = states, fill = Job_Title)) +
        geom_bar(position = "stack", width = 0.75) +
        ylim(0, 30) +
        labs(title = "Job Distribution by States", x = "Location", y = "Count") +
        theme(axis.title.y = element_text(vjust = 2),
              axis.title.x = element_text(vjust = -0.09)) +
        theme(legend.position = "none")
      ggsave("Job_Distribution_Stacked.png", plot = Job_Distribution_stacked, width = 11, height = 8)
      
    
    # job distribution by location (percentage)
      Job_Distribution_total <- ggplot(JD_count_df, aes(x = reorder(States, -Count), y = Count, fill = Count)) +
        geom_bar(stat = "identity", width = 0.75) +
        ylim(0, 30) +
        labs(title = "Job Distribution by All States", x = "Location", y = "Count") +
        geom_text(aes(label = paste(as.character(round(100*Count/sum(Count), digits = 1)), "%")), vjust = -0.5) +
        theme(axis.title.y = element_text(vjust = 2),
              axis.title.x = element_text(vjust = -0.09)) +
        theme(legend.position = "none")
      ggsave("Job_Distribution_Total.png", plot = Job_Distribution_total, width = 11, height = 8)
      
  
=======
states_data <- job_data
states_data$States <- states_data$Location
states_data$States <- gsub(".*,", "", states_data$States)
states_data$States <- gsub("^ +| +$", "", states_data$States)
states_data$States <- gsub("Shah Alam", "Selangor", states_data$States)

# count df
states_count <- table(states_data$States)
states_count_df <- data.frame(States = names(states_count), Count = as.numeric(states_count))

# job distribution by location stack bar graph
Job_Distribution_stacked <- ggplot(states_data, aes(x = States, fill = Job_Title)) +
  geom_bar(position = "stack", width = 0.75) +
  labs(title = "Job Distribution by States", x = "States", y = "Count") +
  theme(axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = -0.09)) +
  theme(legend.position = "none")
ggsave("Job_Distribution_Stacked.png", plot = Job_Distribution_stacked, width = 11, height = 8)

# job distribution by location (percentage)
Job_Distribution_total <- ggplot(states_count_df, aes(x = reorder(States, -Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity", width = 0.75) +
  labs(title = "Job Distribution by All States", x = "States", y = "Count") +
  geom_text(aes(label = paste(as.character(round(100*Count/sum(Count), digits = 1)), "%")), vjust = -0.5) +
  theme(axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = -0.09)) +
  theme(legend.position = "none")
ggsave("Job_Distribution_Total.png", plot = Job_Distribution_total, width = 11, height = 8)





>>>>>>> Stashed changes

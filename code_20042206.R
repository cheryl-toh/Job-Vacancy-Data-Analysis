# import statements
library('rvest')
library(stringr)
library(ggplot2)
library(lubridate)

# scrapping data

## job title (Gladys)
scrape_title <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  job_title <- page %>% html_nodes('.im1gct2 .z1s6m00') %>% html_text()
  job_title <- gsub("/.*", "", job_title)
  job_title <- gsub("/.*", "", job_title)
  job_title <- gsub("\\(.*\\)", "", job_title)
  job_title <- gsub(" -.*", "", job_title)
  job_title <- gsub(" –.*", "", job_title)
  job_title <- gsub(" /.*", "", job_title)
}


## location (Gladys)
scrape_location <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  location <- page %>% html_nodes('._1hbhsw6ga._1hbhsw6fy') %>% html_node('.y44q7i3 .rqoqz4') %>% html_text()
  location <- gsub("/.*", "", location)
  location <- gsub(" -.*", "", location)
  location <- as.factor(location)
}


## company name (Gabriel)
scrape_co_name <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  company_names <- page %>%
    html_nodes(".a1msqi6q .lnocuoa :nth-child(2)") %>%
    html_text()
  rm(page)
  return(company_names)
}

## date posted (Gabriel)
# Helper function to process date information
process_date <- function(raw_date) {
  if (grepl("ago", raw_date)) {
    #Extract numeric value and time unit
   match <- regexec("(\\d+)\\s*(h|d)\\s*ago", date_str)
    
    if (length(match) > 1 && match[[2]][1] > 0) {
      value <- as.numeric(regmatches(date_str, match)[[1]][1])
      unit <- regmatches(date_str, match)[[1]][2]
      
      if (unit == "h") {
      # If hours ago, return today's date
        return(format(Sys.Date(), format = "%Y-%m-%d"))
      
      } else if ( unit == "d") 
      #If days ago, return date from (x) days ago
        return(format(Sys.Date() - ddays(value), format = "%Y-%m-%d")
        }
      
  } else if (grepl("Posted on ", raw_date)) {
    match <- regexec("(\\d+)\\s*(h|d)\\s*ago", date_str)
    
    if (length(match) > 1 && match[[2]][1] > 0) {
      value <- as.numeric(regmatches(date_str, match)[[1]][1])
      unit <- regmatches(date_str, match)[[1]][2]
      
      if (unit == "h") {
        # If hours ago, return today's date
        return(format(Sys.Date(), format = "%Y-%m-%d"))
        
      } else if ( unit == "d") 
        #If days ago, return date from (x) days ago
        return(format(Sys.Date() - ddays(value), format = "%Y-%m-%d")
    }
  }
}

scrape_date <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
  # Define empty list for all education levels
  all_dates <- list()
  
  # Select all job links
  job_links <- page %>% html_nodes(".uo6mkd") %>% html_attr("href")
  
  # Loop through each job link
  for (job_link in job_links) {
    
    # Construct the full URL for the job article
    article_url <- paste0("https://www.jobstreet.com.my", job_link)  # Replace with the actual base URL
    
    # Add delay to prevent request limit error
    Sys.sleep(1)
    
    # Navigate to the article page
    article_page <- read_html(article_url)
    
    # Check for the first class pattern
    date_element <- article_page %>% html_node(".szurmz6 .lnocuo22.lnocuo7")
    
    # If the first class pattern is not found, try the second class pattern
    if (length(date_element) == 0) {
      date_element <- article_page %>% html_node(".lnocuo22.lnocuoa")
    }
    
    extracted_date <- html_text(date_element)
    
    
    # Process the date information
    processed_date <- process_date(extracted_date)
    
    # Append the processed date to the list
    all_dates <- c(all_dates, processed_date)
    
    # Explicitly close the connection
    rm(article_page)
  }
  
  # Return the extracted education levels
  return(all_dates)
  
}

# salary (Cheryl)
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
      
      extracted_salary[i] <- "NA"
      
    } else {
      
      extracted_salary[i] <- html_text(html_nodes(job_elements[i],'.y44q7ih+ .y44q7ih'))
      
      # Extract numeric values from the salary text
      lower_bound <- as.numeric(gsub("[^0-9.]+", "", strsplit(extracted_salary[i], " - ")[[1]][1]))
      upper_bound <- as.numeric(gsub("[^0-9]+", "", strsplit(extracted_salary[i], " - ")[[1]][2]))
      
      # Convert the lower bound to a consistent format
      if (grepl("K", extracted_salary[i])) {
        lower_bound <- lower_bound * 1000
      }
      
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
  
  rm(page)
  
  # Return the extracted salaries
  return(extracted_salary)
  
}
  

## company size (Bryan)
scrape_co_size <- function(url) {
  
  all_company_size <- NULL
  
  # Read page URL
  page <- read_html(url)
  job_links <- page %>% html_nodes("#jobList article h1 a") %>% html_attr("href")

  for (job_link in job_links) {
    article_url <- paste0("https://www.jobstreet.com.my", job_link)  
    
    
    Sys.sleep(1)
    
    
    article_page <- read_html(article_url)
    
    
    company_size <- article_page %>% html_nodes('._1hbhsw64y+ ._5135gei .pmwfa57:nth-child(2) .y44q7i1') %>% html_text()
    
    if (!grepl("Employees", company_size)) {
      all_company_size <- c(all_company_size, "NA")
    } else {
      all_company_size <- c(all_company_size, company_size)
    }
    
    rm(article_page)
    
  }
  return(all_company_size)
}


## job type (Bryan)
scrape_job_type <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  job_type <- page %>% html_nodes('._1hbhsw67y~ ._1hbhsw652 ._1hbhsw6h') %>% html_text()
  # Explicitly close the connection
  rm(page)
  return(job_type)
  
}


## average processing time (Marcus)
scrape_ATP_levels <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
  # Define empty list for all APT levels
  all_APT_levels <- list()
  
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
    
    # Replace ".your-class" with the actual class containing APT information
    APT_element <- article_page %>% html_node("._1hbhsw64y+ ._5135gei .pmwfa57:nth-child(3) .y44q7i1")
    
    if (!grepl("days", APT_element)) {
      extracted_APT <- "NA"
    }else {
      # Extract text from the APT element
      extracted_APT <- html_text(APT_element)
    }
    
    APT_levels <- unlist(extracted_APT)
    
    # Append the list of APT levels to the parent list
    all_APT_levels <- c(all_APT_levels, list(APT_levels))
    
    # Explicitly close the connection
    rm(article_page)
  }
  
  return(all_APT_levels)
}

## experience level (Marcus)
scrape_exp_level <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  
  # Define empty list for all experience levels
  all_exp_levels <- list()
  
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
    
    # Replace ".your-class" with the actual class containing experience information
    exp_element <- article_page %>% html_node("._1hbhsw674~ ._1hbhsw674+ ._1hbhsw674 .pmwfa57:nth-child(3) .y44q7i1")
    
    # Extract text from the experience element
    extracted_exp <- html_text(exp_element)
    
    
    exp_levels <- unlist(extracted_exp)
    
    # Append the list of education levels to the parent list
    all_exp_levels <- c(all_exp_levels, list(exp_levels))
    
    # Explicitly close the connection
    rm(article_page)
  }
  
  return(all_exp_levels)
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
    article_url <- paste0("https://www.jobstreet.com.my", job_link) 
    
    # Add delay to prevent request limit error
    Sys.sleep(1)
    
    # Navigate to the article page
    article_page <- read_html(article_url)
    
    # Check if the element contains the text "Qualification"
    qualification_check <- article_page %>% html_node("._1hbhsw674~ ._1hbhsw674+ ._1hbhsw674 .pmwfa57:nth-child(2) .y44q7i3, ._5135gei ._5135gei:nth-child(1) .pmwfa57:nth-child(2) .y44q7i3") %>% html_text()

    if (!grepl("Qualification", qualification_check, ignore.case = TRUE)) {
      # If "Qualification" is not present, set education level to NA
      all_education_levels <- c(all_education_levels, list("Not Specified"))
    } else {
      # Class containing education information
      education_element <- article_page %>% html_node(".pmwfa57:nth-child(2) .y44q7i1")
      
      # Extract text from the education element
      extracted_education <- html_text(education_element)
      
      # Split the extracted education text into a list of education levels
      edu_levels <- unlist(strsplit(extracted_education, ", "))
      
      # Append the list of education levels to the parent list
      all_education_levels <- c(all_education_levels, list(edu_levels))
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
APT <- NULL
EXP_lvl <- NULL
company_size <- NULL
job_type <- NULL

url <- 'https://www.jobstreet.com.my/jobs/in-Malaysia'

print("Scrapping webpages... (Might take up to 5 - 10 minutes)")

for (page_number in 1:2) {
  page_url <- paste0(url, "?pg=", page_number)
  all_salaries <- c(all_salaries, scrape_salary(page_url))
  all_education_levels <- c(all_education_levels, scrape_edu_level(page_url))
  job_title <- c(job_title, scrape_title(page_url))
  location <- c(location, scrape_location(page_url))
  APT <- c(APT, scrape_ATP_levels(page_url))
  EXP_lvl <- c(EXP_lvl, scrape_exp_level(page_url))
  date <- c(date, scrape_date(page_url))
  company_name <- c(company_name, scrape_co_name(page_url))
  job_type <- c(job_type, scrape_job_type(page_url))
  company_size <- c(company_size, scrape_co_size(page_url))
}

length_of_data <- length(all_salaries)

# Forming Data frame
data <- data.frame(
  Job_Title = rep("Not specified", length_of_data),
  Location = rep("Not specified", length_of_data),
  Company_Name = rep("Not specified", length_of_data),
  Salary = rep("Not specified", length_of_data),
  Job_Type = rep("Not specified", length_of_data),
  Company_Size = rep("Not specified", length_of_data),
  Education_Level = rep("Not specified", length_of_data),
  Experience_Level = rep("Not specified", length_of_data),
  Date_Posted = rep("Not specified", length_of_data),
  APT = rep("Not specified", length_of_data)
)

# Populate the data frame with actual values
data$Salary <- all_salaries
data$Education_Level <- all_education_levels
data$Job_Title <- job_title
data$Location <- location
data$APT <- APT
data$Experience_Level <- EXP_lvl
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


# Data Analysis
## Company Distribution (Gladys)



## Time Trend (Gabriel)
# Read CSV file for plot
job_data <- read.csv("job_data.csv")

# Plot line graph
time_trend_plot <- ggplot(job_data, aes(x = Date_Posted)) +
  geom_bar(fill = "skyblue", color = "black") +

  labs(title = "Job Posting Time Trend",
       x = "Date",
       y = "Number of Job Postings") +
  theme_minimal()
              
ggsave("time_trend_line_graph.png", time_trend_plot, width = 10, height = 6)

## Company size by frequency (Gabriel)
# Read CSV file for plot
job_data <- read.csv("job_data.csv")

# Plot scatter plot
company_size_frequency <- ggplot(job_data, aes(x = Company_Size)) +
  geom_point(stat = "count", color = "blue") +
  labs(title = "Company Size Frequency",
       x = "Company Size",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Company Size by Frequency.png", company_size_frequency, width = 10, height = 6)

ggsave("time_trend_histogram.png", time_trend_plot, width = 10, height = 6)

## Company size by frequency (Gabriel)
# Read CSV file for plot
job_data <- read.csv("job_data.csv")

# Plot histogram
company_size_frequency <- ggplot(job_data, aes(x = Company_Size)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Company Size Frequency",
       x = "Company Size",
       y = "Frequency") +
  theme_minimal()

ggsave("company_by_size_frequency.png", company_size_frequency, width = 10, height = 6)


## Experience level by frequency (Bryan)
# Read CSV file
job_data <- read.csv("job_data.csv")

# Plot histogram for experience level frequency
experience_plot <- ggplot(job_data, aes(x = Experience_Level)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Experience Level Frequency",
       x = "Experience Level",
       y = "Frequency") +
  theme_minimal()

# Save the plot as a PNG file
ggsave("experience_level_histogram.png", experience_plot, width = 8, height = 6)



## Education level by frequency (Bryan)
library(tidyr)

# Read CSV file
job_data <- read.csv("job_data.csv")

# Remove 'c()' from each entry
job_data$Education_Level <- gsub("c\\((.*)\\)", "\\1", job_data$Education_Level)

# Convert each entry to a list of education levels
job_data$Education_Level <- sapply(strsplit(as.character(job_data$Education_Level), ", "), as.list)

# Create a data frame with each education level as a separate row
education_data <- data.frame(
  Education_Level = unlist(job_data$Education_Level),
  stringsAsFactors = FALSE
)

# Plot histogram for education level (horizontal)
education_plot <- ggplot(education_data, aes(y = Education_Level)) +
  geom_bar(fill = "skyblue", color = "black", stat = "count") +
  labs(title = "Education Level Distribution",
       y = "Education Level",
       x = "Frequency") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0))

# Save the plot as a PNG file
ggsave("education_level_histogram.png", education_plot, width = 10, height = 6)

## job type vs apt (Marcus)




## Job type by frequency (Cheryl)
# Read the CSV file
job_data <- read.csv("job_data.csv")

# Plot histogram for Job Type by frequency
plot <- ggplot(job_data, aes(x = Job_Type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Job Type Distribution", x = "Job Type", y = "Frequency") +
  theme_minimal()

# Save the plot as a PNG file
ggsave("job_type_histogram.png", plot, width = 8, height = 6, units = "in", dpi = 300)


## Salary by job type (Cheryl)
# Read CSV file
job_data <- read.csv("job_data.csv")

# Install and load required library for plotting
library(ggplot2)

# Replace "Not specified" with NA in Salary column
job_data$Salary[job_data$Salary == "Not specified"] <- NA

# Convert Salary column to numeric, ignoring non-numeric entries
job_data$Salary <- as.numeric(gsub("[^0-9.]+", "", job_data$Salary))

# Filter out rows with non-finite salary values
job_data <- job_data[is.finite(job_data$Salary), ]

# Get unique job types
unique_job_types <- unique(job_data$Job_Type)

# Create a list to store individual plots
boxplot_list <- list()

# Create boxplot for salary for each job type
for (job_type in unique_job_types) {
  plot_data <- subset(job_data, Job_Type == job_type)
  
  boxplot <- ggplot(plot_data, aes(x = Job_Type, y = Salary)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of Salary for", job_type),
         x = "Job Type",
         y = "Salary (Monthly)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  boxplot_list[[job_type]] <- boxplot
}

# Save each plot as a PNG file
for (job_type in unique_job_types) {
  ggsave(paste0("salary_by_", gsub(" ", "_", tolower(job_type)), ".png"), boxplot_list[[job_type]], width = 8, height = 6, units = "in", dpi = 300)
}

## Salary vs Experience level (Marcus)

## Job distribution by location (Gladys)




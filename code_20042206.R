# import statements
library('rvest')
library(stringr)

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
  
  #TESTING average processing time  
  website <- read_html('https://www.jobstreet.com.my/jobs/in-Malaysia')

  APT_data_html <- html_nodes(website,'._1hbhsw64y+ ._5135gei .pmwfa57:nth-child(2) .y44q7i1')

  APT_data <- html_text(APT_data_html)

  print(head(APT_data))

  #git part
  APT <- page %>% html_nodes('._1hbhsw64y+ ._5135gei .pmwfa57:nth-child(2) .y44q7i1') %>% html_text()
  APT <- gsub("days", "", APT)
  APT <- as.numeric(APT)
  
  return(APT)
}


## experience level (Marcus)
scrape_exp_level <- function(url) {
  
  # Read page URL
  page <- read_html(url)
  Exp_level <- page %>% html_nodes('._1hbhsw674~ ._1hbhsw674+ ._1hbhsw674 .pmwfa57:nth-child(3) .y44q7i1') %>% html_text()
  Exp_level <- gsub("years", "", Exp_level)
  Exp_level <- as.numeric(Exp_level)
  
  return(Exp_level)
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
APT <- NULL
EXP_lvl <- NULL

url <- 'https://www.jobstreet.com.my/jobs/in-Malaysia'

print("Scrapping webpages... (Might take up to 3 - 5 minutes)")

for (page_number in 1:4) {
  page_url <- paste0(url, "?pg=", page_number)
  #all_salaries <- c(all_salaries, scrape_salary(page_url))
  #all_education_levels <- c(all_education_levels, scrape_edu_level(page_url))
  #job_title <- c(job_title, scrape_title(page_url))
  #location <- c(location, scrape_location(page_url))
  APT <- c(APT, scrape_working_hour(page_url))
  EXP_lvl <- c(EXP_lvl, scrape_exp_level(page_url))
}

# Print the results
#print(length(all_salaries))
#print(length(all_education_levels))
#print(length(job_title))
#print(length(location))
print(length(APT))
print(length(EXP_lvl))

# Forming Data frame



# Data Analysis
## Company Distribution (Gladys)

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




# import statements
library('rvest')

# scrapping data

url <- 'https://www.jobstreet.com.my/jobs/in-Malaysia'

# Function to scrape data from a page
scrape_page <- function(url) {
  
  ## read page url
  page <- read_html(url)
  
  ## job title (Gladys)

  ## location (Gladys)
  
  ## company name (Gabriel)
  
  ## date posted (Gabriel)
  
  ## date due (Gabriel)
  
  ## salary (Cheryl)
  selected_salary_elements <- html_nodes(page, ".y44q7ih+ .y44q7ih")
  extracted_salary <- html_text(selected_salary_elements)
  print(extracted_salary)
  
  ## company size (Bryan)
  
  ## contract type (Bryan)
  
  ## working hour (Marcus)
  
  ## experience level (Marcus)
  
  ## education level (Cheryl)
  #selected_edu_elements <- page %>% html_node("#jobList article h1 a")
  #extracted_link <- selected_edu_elements %>% html_attr("href")
  #print(extracted_link)
  
}




## main loop to scrap 4 pages of data
for (page_number in 1:4) {
  
  page_url <- paste0(url, "?pg=", page_number)
  page_data <- scrape_page(page_url)
  
}

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




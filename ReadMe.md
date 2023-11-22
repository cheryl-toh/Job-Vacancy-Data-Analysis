# Job Market Analysis with Web Scraping in R

## Overview
This project involves web scraping job data from [JobStreet](https://www.jobstreet.com.my/jobs/in-Malaysia) using R programming. The primary goals were to extract information on job titles, locations, company details, salaries, and more. The extracted data was then analyzed and visualized to gain insights into the job market.

## Web Scraping Functions
Several functions have been implemented to scrape specific information:

- scrape_title: Extracts job titles.
- scrape_location: Retrieves job locations.
- scrape_co_name: Scrapes company names.
- scrape_salary: Extracts salary information.
- scrape_job_type: Retrieves job types.
- scrape_date: Extracts date posted.
- scrape_co_size: Scrapes company sizes.
- scrape_class: Retrieves job classifications.
- scrape_ratings: Extracts ratings.
- scrape_edu_level: Scrapes education levels.

## Data Analysis **(Uncomment from Line 512 for Graphs)**
The subsequent section involves data analysis and visualization using ggplot2. Graphs include:

- Company Distribution
- Job Distribution by Location
- Time Trend of Job Postings
- Company Size by Frequency
- Classification by Frequency
- Education Level Distribution
- Job Type Distribution
- Salary by Job Type
- Ratings by Frequency
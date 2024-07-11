# Load libraries
library(httr)
library(jsonlite)
library(lubridate) #used for calculating the date parameters
library(dplyr) #used for dedup; grammer of data manipulation, part of tidyverse 
library(data.table)

fetch_api_data <- function(start_date){
  
  options(encoding = "UTF-8")
  
  # Set your Figshare API key
  api_key <- Sys.getenv("API_KEY")
  
  # Set the Figshare API request URL
  endpoint1 <- "https://api.figshare.com/v2/account/institution/articles?"
  
  #today_date <- Sys.Date()
  #start_date <- today_date %m-% months(3)
  published_since <- paste0("page_size=1000&published_since=", start_date)
  keywords <- c("race", "disability", "discrimination", "colonialism", "racism", "impairment", "dyscalculia", "equality", "equity", "EDI", "LGBT", "LGBT+", "queer", "lesbian", "disabled", "fibromyalgia", "autism", "neurodiversity", "gay", "transgender", "Mental health", "schizophrenia", "PTSD", "ableism", "epilepsy", "stroke", "anxiety", "depression", "syndrome", "colonialism", "feminism", "LGBTQ+", "gender", "intersectionality", "racial", "feminism", "BAME", "whiteness")
  
  combined_df <- data.frame()
  deduplicated_df <- data.frame()
  number <- 0
  
  for (keyword in keywords){
    # Construct the complete URL for the article ID search
    full_url <- paste0(endpoint1, published_since, "&search_for=", '"', keyword, '"')
    encoded_url <- URLencode(full_url)
    
    # Get the data to create the article IDs
    response <- GET(url = encoded_url, add_headers(Authorization = paste("token", api_key)))
    
    # Create a JSON file from the data (I think), and put the article ids into an article ids 'column'
    articles_data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
    article_ids <- articles_data$id
    
    #Set the Figshare API request URL
    endpoint2 <- "https://api.figshare.com/v2/articles/"
    
    #Use article IDs to get article citation
    for (article_id in article_ids) {
      full_url_citation <- paste0(endpoint2, article_id)
      
      # Get the article citation datadata
      response <- GET(full_url_citation)
      # Create a JSON file
      citation_data <- fromJSON(content(response, "text", encoding = "UTF-8"), flatten = TRUE)
      #put the citation into a dataframe
      citation_df <-data.frame(Citation = citation_data$citation, URL = citation_data$figshare_url)
      combined_df <- rbind(combined_df, citation_df)
    }
    number <- number + 1
    print(number)
  }
  
  #remove duplicates
  deduplicated_df <- distinct(data.frame(combined_df), .keep_all = TRUE)
  assign("deduplicated_df", deduplicated_df, envir = .GlobalEnv)
}
# Coursework How Has Vehicle Restrictions in Sheffield Impacted the Air Quality
library(httr) # GET function 
library(purrr) # 
library(tibble) # 

# creating a function to fetch one page
fetch_page <- function(page_num) {
  result <- GET( # get the results from OpenAQ
    "https://api.openaq.org/v3/sensors/2508/measurements/daily", # 3525 = Birmingham, 2508 = Sheffield Devonshire Green
    query = list( #setting the query parameters 
      limit = 1000,
      page = page_num #use the page number passed into the function
    ),
    add_headers("X-API-Key" = "MY_OPENAQ_API")
  )
  content(result, "parsed") # returns the results from a string to a integer (JSON -> structured R list)
}

# First request to get metadata
first_page <- fetch_page(1) #get the first 1000 daily measurements
total_found <- first_page$meta$found #tells the total records available for query
total_pages <- ceiling(total_found / 1000) #loop through all the pages and collect the full data

# Fetch all pages
all_data <- map(1:total_pages, fetch_page) # using the purrr command to loop through all the data found, calling the function to easily get the API 

# Flatten all results from all pages
all_results <- map(all_data, "results") %>% flatten() # loops through all_data and extracts the results. Then flattens the data into one list per page

str(all_results[[1]]$summary) #checking the data, debugging 

# Create tibble from flattened results
df <- tibble( #with the tidyverse package, working with tabular data (any data in a table), easier 
  datetime_from_utc = map_chr(all_results, ~ .x$period$datetimeFrom$utc %||% NA_character_), #flattens all the results, extracts every result from DateTime if data is missing it returns as N/A rather than crashing
  datetime_to_utc = map_chr(all_results, ~ .x$period$datetimeTo$utc %||% NA_character_), # ~ introduces the formula and .x refers to the current element being processed (shorter for function(x))
  parameter = map_chr(all_results, ~ .x$parameter$name %||% NA_character_),
  units = map_chr(all_results, ~ .x$parameter$units %||% NA_character_),
  value = map_dbl(all_results, ~ .x$value %||% NA_real_),
  min = map_dbl(all_results, ~ .x$summary$min %||% NA_real_),
  q02 = map_dbl(all_results, ~ .x$summary$q02 %||% NA_real_),
  q25 = map_dbl(all_results, ~ .x$summary$q25 %||% NA_real_),
  median = map_dbl(all_results, ~ .x$summary$median %||% NA_real_),
  q75 = map_dbl(all_results, ~ .x$summary$q75 %||% NA_real_),
  q98 = map_dbl(all_results, ~ .x$summary$q98 %||% NA_real_),
  max = map_dbl(all_results, ~ .x$summary$max %||% NA_real_),
  avg = map_dbl(all_results, ~ .x$summary$avg %||% NA_real_),
  sd = map_dbl(all_results, ~ .x$summary$sd %||% NA_real_)
)

write.csv(df, "anothercleantestsheff.csv", row.names = FALSE)

#Google Flights API interactions
library(httr)
library(dplyr)
library(jsonlite)
library(lubridate)
KEY = 'AIzaSyAE-ngDIn5Dau4aNCzf2FqbVeIH9iHQB9o'
DEFAULT_ORIGIN = "CPT"


choose_destination <- function(){
  #Dataset loading
  dest_data <- read_csv("Airports.csv")
  random_row <- sample_n(dest_data,1,replace = TRUE)
  #Get the airport code
  airport <- random_row$CODE
  return(airport)
}
prepare_body <- function(){
  #Get JSON object
  body <- fromJSON("request.json")
  todays_date <- today(tzone = "CET")
  flight_date <- todays_date + days(7)
  body$request$slice$origin <- DEFAULT_ORIGIN
  body$request$slice$destination <- choose_destination()
  body$request$slice$date <- flight_date
  body$request$solutions <- 1
  return(body)
}
produce_request <- function(){
  body <- prepare_body()
  url <- sprintf("https://www.googleapis.com/qpxExpress/v1/trips/search?key=%s",KEY)
  jsonBody <- toJSON(body)
  request <- POST(url,body=body, encode = "json",verbose())
  status <- http_status(request)
  print(request)
  if(status$category == "Success"){
    data <- content(request, as = "text", encoding = "UTF-8")
    flights_data <- fromJSON(data)
    price <- flights_data$trips$tripOption$saleTotal
    return(price)
  }
}
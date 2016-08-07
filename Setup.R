#!/usr/bin/Rscript
library(twitteR)
library(jsonlite)
library(dplyr)
#Google Flights API interactions
library(httr)
library(lubridate)

KEY = 'AIzaSyAE-ngDIn5Dau4aNCzf2FqbVeIH9iHQB9o'
DEFAULT_ORIGIN = "CPT"

prepare_data <- function(){
  airports <-  read.csv("Airports.csv")
  airports <- airports %>% rename(City = Location)
  #Strip the airport name and add it as a city
  airports$City <- gsub(",.*","",airports$City)
  return(airports)
}

choose_destination <- function(){
  #Dataset loading
  dest_data <- read.csv("Airports.csv")
  random_row <- sample_n(dest_data,1,replace = TRUE)
  #Get the airport code
  airport <- random_row$CODE
  return(airport)
}
prepare_body <- function(origin = DEFAULT_ORIGIN){
  #Get JSON object
  body <- fromJSON("request.json")
  todays_date <- today(tzone = "CET")
  flight_date <- todays_date + days(7)
  body$request$slice$origin <- origin
  body$request$slice$destination <- choose_destination()
  body$request$slice$date <- flight_date
  body$request$solutions <- 1
  return(body)
}
produce_request <- function(){
  body <- prepare_body()
  url <- sprintf("https://www.googleapis.com/qpxExpress/v1/trips/search?key=%s",KEY)
  jsonBody <- toJSON(body)
  request <- POST(url,body=body,encode="json",verbose())
  status <- http_status(request)
  if(status$category == "Success"){
    data <- content(request, as="text", encoding="UTF-8")
    flights_obj <- fromJSON(data)
    return(flights_obj)
  }
}

##CODE
options(httr_oauth_cache=T)
api_key = "0dNKgfWBE8bKAcBfL8xyzhiog"
api_secret = "1BrIkmUDhTAA6LR0FjKz00lr9gl8fSEbHdqCTbz2lD0A9BzEWn"
access_token = "760930056487182336-dZIccBHYOKQiSNEBS48nVQYiEH5sWgR"
access_secret = "TqxhqyjUdKmEUQ8lZcuhdVSnpBLFlsmBfFtJEasGOPmXx"
setup_twitter_oauth(api_key, api_secret, access_token, access_secret)
#Message Templates to fill
message1 <- "Feeling Spontaneous? don't miss this deal! Fly to %s, for just %s once of a lifetime opportunity #travel #fun"
message2 <- "Time to travel! Your bot found a great deal to: %s. You got it just %s, #travel #botislife"
message3  <- "Getting the hell out of town!  Go to %s for just %s next week, time to rock'n roll #experiences #olympics"
messages <- list(message1,message2,message3)
#Get our data
airports_data <- prepare_data()
#Connect to Google Flights API
response <- produce_request()
airport <- last(response$trips$data$airport$code)
price <- response$trips$tripOption$saleTotal
city_frame <- airports_data %>% filter(CODE == airport)
city <- city_frame$City

#Tweet it!
chosen_message <- sample(messages,1)
final_message <- sprintf((chosen_message)[[1]],city,price)
#TODO: Keep choosing tweet until has the right length
tweet(final_message)

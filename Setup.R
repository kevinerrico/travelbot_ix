library(twitteR)
library(jsonlite)
library(dplyr)
#Google Flights API interactions
library(httr)
library(lubridate)

KEY = 'AIzaSyAE-ngDIn5Dau4aNCzf2FqbVeIH9iHQB9o'
DEFAULT_ORIGIN = "CPT"
TRAVEL_1=c("TripAdvisor","travelchannel","lonelyplanet")
TRAVEL_2=c("fodorstravel","GuardianTravel","nytimestravel")
TRAVEL_3=c("TravelMagazine","GoogleTravel","frugaltraveler")

all_handles = list(TRAVEL_1,TRAVEL_2,TRAVEL_3)
prepare_data <- function(){
  airports <-  read.csv("Airports.csv")
  airports <- airports %>% rename(City = Location)
  #Strip the airport name and add it as a city
  airports$City <- gsub(",.*","",airports$City)
  return(airports)
}

choose_destination <- function(){
  #Dataset loading
  dest_data <- read_csv("Airports.csv")
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

'# Expects a word that is in upper case letter
'#
lower_case <- function(word){
  parsed <- strsplit(word, split=" ")
  #Check if we have one or two words
  if(length(parsed) > 1){
    final <- strsplit(word," ")[[1]]
    final <- paste(substring(word,1,1),tolower(substring(word,2)),sep="")
    return(final)
  }
  else{
    final <- paste(substring(word,1,1),tolower(substring(word,2)),sep="")
    return(final)
  }
}

'# Function that mines three of the most popular travel sites for hashtags
# to use in our publications 
#'
#'
#'
find_hashtags <- function(handles){
  #Get the last 500 tweets from each user
  first_tweets = userTimeline(handles[[1]][1],n=500)
  second_tweets = userTimeline(handles[[1]][2],n=500)
  third_tweets = userTimeline(handles[[1]][3],n=500)
  #Put them as dataframes
  first_df = twListToDF(first_tweets)
  second_df = twListToDF(second_tweets)
  third_df = twListToDF(third_tweets)
  
  #Find the hashtags that they use 
  first_hashtags = str_extract_all(first_df,"#\\w+")
  second_hashtags = str_extract_all(second_df,"#\\w+")
  third_hashtags = str_extract_all(third_df,"#\\w+")
  #Put them all in a vector
  first_final = unlist(first_hashtags)
  second_final = unlist(second_hashtags)
  third_final = unlist(third_hashtags)
  #Take the two most used tweets of each account
  first_freq = tail(sort(table(first_final)),2)
  second_freq = tail(sort(table(second_final)),2)
  third_freq = tail(sort(table(third_final)),2)
  #Put them together in a vector
  all_hashtags <- names(c(first_freq,second_freq,third_freq))
  return(all_hashtags)
}

produce_tweet <- function(city,price){
  #Choose a random message template
  chosen_message <- sample(messages,1)
  #Choose a set of handles to parse for hashtags
  handles <- as.vector(sample(all_handles,1))
  #Get a vector with hastags
  hashtags <-find_hashtags(handles)
  chosen_hastags <- sample(hashtags,2)
  #Get the most trendy hashtags from on
  final_message <- sprintf((chosen_message)[[1]],city,price,chosen_hastags[1],chosen_hastags[2])
  i <- 0
  if(length(final_message) > 139){
    print("too long")
    while(length(final_message) < 140 || i > 10){
      print("too long")
      chosen_hastags <- sample(hashtags,2)
      #Repeat the loop at max ten times
      final_message <- sprintf((chosen_message)[[1]],city,price,chosen_hastags[1],chosen_hastags[2])
      i <- i + 1
    }
    tweet(final_message)
  }
  else{
    tweet(final_message) 
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
message1 <- "Feeling Spontaneous? don't miss this deal! Fly to %s, for just %s once of a lifetime opportunity #CapeTown %s %s"
message2 <- "Time to travel! Your bot found a great deal to: %s. You got it just %s, #CapeTown %s %s"
message3  <- "Getting the hell out of town!  Go to %s for just %s next week, time to rock'n roll #CapeTown %s %s"
messages <- list(message1,message2,message3)
#Get our data
airports_data <- prepare_data()
#Connect to Google Flights API
response <- produce_request()
airport <- last(response$trips$data$airport$code)
price <- response$trips$tripOption$saleTotal
city_frame <- airports_data %>% filter(CODE == airport)
city <- city_frame$City
city <- lower_case(city)

#Tweet it!
produce_tweet(city,price)
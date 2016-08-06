library(twitteR)
library(readr)
library(jsonlite)
library(dplyr)
options(httr_oauth_cache=T)
api_key = "0dNKgfWBE8bKAcBfL8xyzhiog"
api_secret = "1BrIkmUDhTAA6LR0FjKz00lr9gl8fSEbHdqCTbz2lD0A9BzEWn"
access_token = "760930056487182336-dZIccBHYOKQiSNEBS48nVQYiEH5sWgR"
access_secret = "TqxhqyjUdKmEUQ8lZcuhdVSnpBLFlsmBfFtJEasGOPmXx"
setup_twitter_oauth(api_key, api_secret, access_token, access_secret)
#Message Templates to fill
message1 <- "Feeling Spontaneous? don't miss this deal! Fly to %s, for just %s once of a lifetime opportunity #travel#fun"
message2 <- "Time to travel! Your bot found a great deal to: %s. You got it just %s, #travel#botislife"
message3  <- "Getting the hell out of town!  Go to %s,%s for just %s next week, time to rock'n roll #experiences#olympics"
messages <- list(message1,message2,message3)
#Get our data
airports_data <- prepare_data()
#Connect to Google Flights API
response <- produce_request()
json.on <- toJSON(response)
airport <- last(response$trips$data$airport$code)
price <- response$trips$tripOption$saleTotal
city_frame <- airports_data %>% filter(CODE == airport)
city <- city_frame$City

#Tweet it!
chosen_message <- sample(messages,1)
final_message <- sprintf((chosen_message)[[1]],city,price)
#TODO: Keep choosing tweet until has the right length
tweet(final_message)

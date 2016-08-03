library(rvest)
html <- read_html("http://brilliantmaps.com/top-100-tourist-destinations/")
destinations_data<- html %>% html_nodes("table") %>% .[[1]] %>% html_table(trim=TRUE)
View(destinations_data)

parse_airports <- function(destinations_data){
  #Find all the rows with common city names
  #Replace airpiort column comma separated of all the airport codes
  #Delete any repeated entries
  #return dataframe 
}
airports<-read_csv("World Airports Airport Code, City name and Country.csv")

destinations_data <- merge(destinations_data, airports, by =  "City")

#if(!Unique(destinations_data$City)) then merge(destinations_data$CODE, sep = ",")
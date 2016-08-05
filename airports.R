library(rvest)
library(readr)
html <- read_html("http://brilliantmaps.com/top-100-tourist-destinations/")
destinations_data<- html %>% html_nodes("table") %>% .[[1]] %>% html_table(trim=TRUE)
View(destinations_data)

parse_airports <- function(destinations_data){
  #Find all the rows with common city names
  unique_cities <- unique(destinations_data$City)
  countries <- unique(destinations_data$Country)
  ranks <- unique(destinations_data$Rank)
  tourism <- unique(destinations_data$`Tourists (Millions)`)
  #Replace airpiort column comma separated of all the airport codes
  airport_codes <- sapply(unique_cities,subset,destinations_data,unique_cities)
  #Delete any repeated entries
  #return dataframe 
}
airports_code<-read_csv("World Airports Airport Code, City name and Country.csv")

destinations_data <- merge(destinations_data, airports_code, by = c("City","Country"))
airport <- subset(destinations_data, City == 'Amman', drop=FALSE)
#if(!Unique(destinations_data$City)) then merge(destinations_data$CODE, sep = ",")


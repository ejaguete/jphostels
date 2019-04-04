# Meta
# This script webscrapes from HostelWorld for information about Japan hostels
# Rewritten for SAIT BI assignment and for my peace of mind theres minimal comments i want to cry
#
# Original script author:  koki25ando @ Github
# Github: https://github.com/koki25ando/Hostel-Data-Scraping/blob/master/hostel.R
# Modified/rewritten by ciralili


# *************************************
# *             SCRIPT                *
# *************************************

# install and load packages as needed
pkgNames <- c("magrittr","dplyr","tidyverse","rvest","reshape2")
lapply(pkgNames, require, character.only = TRUE)

#debug toggle
debugOn <- FALSE

# -------------------------------------- 
# VAR: homepage for japan hostels
# -------------------------------------- 
jpHostelLink <- "https://www.hostelworld.com/hostels/Japan"
jpMainPg <- 
  read_html(jpHostelLink)

# --------------------------------------
# scrape city names from jpHostelLink
# -------------------------------------- 

# step through html to get dirty text with spaces
# stores: 1d vector, len 1 (string) of city names
mainPgCityName <- 
  jpMainPg %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("h2") %>% 
  html_text()

if(debugOn==TRUE) {
  print("mainPgCityName contents")
  print(mainPgCityName)
}

# -------------------------------------- 
# scrape city links from jpHostelLink
# -------------------------------------- 

# stores: 1d vector len n of city links
mainPgCityLink <- 
  jpMainPg %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("h2") %>% 
  html_nodes("a") %>% 
  html_attr("href")

if(debugOn==TRUE) {
  print("mainPgCityLink contents")
  print(mainPgCityLink)
}

# -------------------------------------- 
# scrape number of hostels in a city from jpHostelLink
# -------------------------------------- 

# stores: 1d vector len 1 of number of hostels
mainPgCityHostelNum <- 
  jpMainPg %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("span.propnumber") %>% 
  html_text()

if(debugOn==TRUE) {
  print("cityHostelNum contents")
  print(cityHostelNum)
}

# -------------------------------------- 
# VAR: store city names, links, hostel num vars in a dataframe
# -------------------------------------- 
mainPgDF <- data.frame(mainPgCityName,mainPgCityLink, mainPgCityHostelNum)
write.csv(mainPgDF,"mainPgDF.csv")

# -------------------------------------- 
# get information from each page of each city
# --------------------------------------

# function to retrieve url of pagination links per city
# param: url of city link
# returns: pagination url in city link
linkGet <- function (url) {
  as.character(url) %>% 
    read_html() %>% 
    html_nodes("ul.pagination") %>% 
    html_nodes("li.pagination-number") %>% 
    html_nodes("a") %>% 
    html_attr("href")
}

cityPgLinks <- apply(data.frame(mainPgDF$mainPgCityLink), 1, linkGet)
cityPgLinks <- 
  cityPgLinks %>% 
  unlist()
cityPgLinks <- data.frame(cityPgLinks)
cityPgLinks$City <- c("Tokyo", "Tokyo", "Tokyo", "Tokyo", "Tokyo",
                    "Kyoto", "Kyoto", "Kyoto", "Kyoto", 
                    "Osaka", "Osaka", "Osaka", "Osaka",
                    "Hiroshima", "Fukuoka")
names(cityPgLinks) <- c("Link", "City")

if(debugOn==TRUE) {
  print("cityPgLinks contents")
  print(cityPgLinks)
}

# -------------------------------------- 
# get information from each hostel
# --------------------------------------

hostelScraping <- function (url) {
  
  # retrieve page link
  page <- read_html(as.character(url))  
  
  #retrieve array of hostel names
  hostelName <- 
    page %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("h2") %>% 
    html_text()
  
  #retrieve array of hostel links per hostel
  hostelLink <- 
    page %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("h2") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # Rating summary score
  #retrieve array of hostel overall ratings
  rating.summary <- 
    page %>% 
    html_nodes("div.inner-wrap") %>% 
    html_nodes("div.page-contents") %>% 
    html_nodes("div.contentbackground") %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.fabresult-details-rating") %>% 
    html_nodes("div.hwta-rating-container") %>% 
    html_text()
  rating.summary <- 
    rating.summary %>% 
    unlist()
  
  rating.summary <- data.frame(rating.summary)
  rating.summary$rating.summary <- as.character(rating.summary$rating.summary) 
  rating.summary$rating.summary <- 
    rating.summary$rating.summary %>% 
    str_remove_all("\n")
  rating.summary$rating.summary <- 
    rating.summary$rating.summary %>% 
    str_remove("\\s")
  
  rating.summary$rating.summary <- 
    rating.summary$rating.summary %>% 
    str_remove("                                      ")
  
  rating.summary$rating.summary <- 
    rating.summary$rating.summary %>% 
    str_sub(1, 5)
  
  rating.summary$rating.summary <- as.numeric(rating.summary$rating.summary)
  
  overall.score <-
    page %>%
    html_nodes("div.row") %>%
    html_nodes("div.resultcontainer") %>%
    html_nodes("div#fabResultsContainer") %>%
    html_nodes("div.fabresult") %>%
    html_nodes("div.resultheader") %>%
    html_nodes("div.fabresult-details-rating") %>%
    html_nodes("div.hwta-rating-container") %>%
    html_nodes("div.hwta-rating-summary") %>%
    html_nodes("a.hwta-rating-score") %>%
    html_text()
  
  rating_band <-
    page %>%
    html_nodes("div.row") %>%
    html_nodes("div.resultcontainer") %>%
    html_nodes("div#fabResultsContainer") %>%
    html_nodes("div.fabresult") %>%
    html_nodes("div.resultheader") %>%
    html_nodes("div.fabresult-details-rating") %>%
    html_nodes("div.hwta-rating-container") %>%
    html_nodes("div.hwta-rating-summary") %>%
    html_nodes("div.hwta-rating-info") %>%
    html_nodes("span") %>%
    html_text()

   review_num <-
    page %>%
    html_nodes("div.row") %>%
    html_nodes("div.resultcontainer") %>%
    html_nodes("div#fabResultsContainer") %>%
    html_nodes("div.fabresult") %>%
    html_nodes("div.resultheader") %>%
    html_nodes("div.fabresult-details-rating") %>%
    html_nodes("div.hwta-rating-container") %>%
    html_nodes("div.hwta-rating-summary") %>%
    html_nodes("div.hwta-rating-info") %>%
    html_nodes("a.hwta-rating-counter") %>%
    html_text()

  price.from <- 
    page %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("div.fabresult-prices") %>% 
    html_nodes("span.price") %>% 
    html_nodes("a") %>% 
    html_text()
  
  location <- 
    page %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("div.addressline") %>% 
    html_text()
  
  data.frame(hostelName, hostelLink, 
             #overall.score, rating_band, review_num, 
             price.from, location, rating.summary)
}

scrapeResult <- apply(data.frame(cityPgLinks$Link), 1, hostelScraping)
scrapeResult <- do.call(rbind.data.frame, scrapeResult)

# Clean dataset
scrapeResult$location <- 
  as.character(scrapeResult$location) %>% 
  str_remove("  - Show on Map\n        ")
scrapeResult$location <- 
  scrapeResult$location %>% 
  str_remove("\n             ")


scrapeResult <- 
  scrapeResult %>% 
  mutate(City = as.character(hostelLink))
scrapeResult$City <- 
  scrapeResult$City %>% 
  str_remove("https://www.hostelworld.com/hosteldetails.php/")
scrapeResult <- 
  scrapeResult %>% 
  separate(City, sep = "/",
           into = c("Name", "City", "num")) %>% 
  select(hostelName, City, price.from, location, rating.summary, hostelLink)

#export hostel list csv
write.csv(scrapeResult,"Hostel_list.csv")

print("finished scraping ty")

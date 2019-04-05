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
# used to scrape top 5 cities & their links
jpHostelHomePg <- read_html("https://www.hostelworld.com/hostels/Japan")

# --------------------------------------
# scrape city names from jpHostelLink
# -------------------------------------- 

# stores: 1d vector, len n of city names
homePgCityNames <- 
  jpHostelHomePg %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("h2") %>% 
  html_text()

if(debugOn==TRUE) {
  print("homePgCityNames contents")
  print(homePgCityNames)
}

# -------------------------------------- 
# scrape city links from jpHostelLink
# -------------------------------------- 

# stores: 1d vector, len n of city links
homePgCityLinks <- 
  jpHostelHomePg %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("h2") %>% 
  html_nodes("a") %>% 
  html_attr("href")

if(debugOn==TRUE) {
  print("homePgCityLink contents")
  print(homePgCityLink)
}

# -------------------------------------- 
# scrape number of hostels in each city from jpHostelLink
# -------------------------------------- 

# stores: 1d vector len 1 of number of hostels
homePgCityNumHostels <- 
  jpHostelHomePg %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("span.propnumber") %>% 
  html_text()

if(debugOn==TRUE) {
  print("mainPgCitynumHostels contents")
  print(mainPgCitynumHostels)
}

# -------------------------------------- 
# VAR: store city names, links, hostel num vars in a dataframe
# -------------------------------------- 
homePgDF <- data.frame(homePgCityNames, homePgCityLinks, homePgCityNumHostels)
write.csv(homePgDF,"homePgDF.csv")

# -------------------------------------- 
# get information from each page of each city
# --------------------------------------

# function to retrieve url of pagination links per city
# param: txt url of city link e.g. Tokyo, Osaka, Hiroshima
# returns: pagination url txt in city link = pg1,2,3... for Tokyo
pgLinkGet <- function (url) {
  as.character(url) %>% 
    read_html() %>% 
    html_nodes("ul.pagination") %>% 
    html_nodes("li.pagination-number") %>% 
    html_nodes("a") %>% 
    html_attr("href")
}

# insert column for city name corresponding to link
# eg pg1,2,3 for Tokyo hostels has city = 'Tokyo'
cityPgLinks <- apply(data.frame(homePgDF$homePgCityLink), 1, pgLinkGet)
# cityPgLinks <- 
#   cityPgLinks %>% 
#   unlist()
cityPgLinksDF <- data.frame(cityPgLinks %>% unlist())
cityPgLinksDF$City <- c("Tokyo", "Tokyo", "Tokyo", "Tokyo", "Tokyo",
                    "Kyoto", "Kyoto", "Kyoto", "Kyoto", 
                    "Osaka", "Osaka", "Osaka", "Osaka",
                    "Hiroshima", "Fukuoka")
# column labels
names(cityPgLinksDF) <- c("Link", "City")

if(debugOn==TRUE) {
  print("cityPgLinksDF contents")
  print(cityPgLinksDF)
}
write.csv(cityPgLinksDF,"cityPgLinksDF.csv")

# -------------------------------------- 
# get name & link from each hostel
# --------------------------------------

# new scrape function cause idk how the old one works
# param url: 
scrapeHostelLink <- function(url) {
  #convert to url to scrape
  pg <- read_html(as.character(url))
  
  # scrape hostel name from url
  hostelName <-
    pg %>%
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("h2") %>% 
    html_text()
  
  #scrape hostel page link
  hostelLink <- 
    pg %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("h2") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # hostelOverallRating <-
  #   hostelLink %>%
  #   html_nodes("div.row") %>%
  #   html_nodes("section.small-12") %>%
  #   #html_attr("name=ms-rating") %>%
  #   html_nodes("div.ms-rating-summary-block") %>%
  #   html_nodes("div.rating-summary") %>%
  #   html_nodes("div.score") %>%
  #   html.text()

  # return dataframe object containing the scraped attributes
  data.frame(hostelName,hostelLink)
}

#print(scrapeHostelLink(cityPgLinksDF$Link[1]))


scrapeHostelInfo <- function(url) {
  #convert to url to scrape
  pg <- read_html(as.character(url))
}

# for each link in cityPgLinksDF, apply function scrapeHostelPg
scrapedData <- apply(data.frame(cityPgLinksDF$Link),1,scrapeHostelLink)
scrapedData <- do.call(rbind.data.frame, scrapedData)
write.csv(scrapedData,"scrapedData.csv")

print(scrapedData$hostelLink[1]) #print ONE LINK IT WORKS!!!
# -------------------------------------- 
# STOP HERE IGNORE STUFF AFTER THIS
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
  
  #Rating summary score
  #retrieve array of hostel overall ratings
  ratingOverall <- 
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
  
  #cleaning overall rating
  #split into atomic cells
  ratingOverall <- 
    ratingOverall %>% 
    unlist()
  ratingOverallDF <- data.frame(ratingOverall)
  ratingOverallDF$ratingOverall <- as.character(ratingOverallDF$ratingOverall) 
  #remove all newlines
  ratingOverallDF$ratingOverall <- 
    ratingOverallDF$ratingOverall %>% 
    str_remove_all("\n")
  #remove all spaces
  ratingOverallDF$ratingOverall <- 
    ratingOverallDF$ratingOverall %>% 
    str_remove("\\s")
  #remove all spaces?
  ratingOverallDF$ratingOverall <- 
    ratingOverallDF$ratingOverall %>% 
    str_remove("                                      ")
  #extract first 5?? characters of string
  ratingOverallDF$ratingOverall <- 
    ratingOverallDF$ratingOverall %>% 
    str_sub(1, 5)
  #convert to numeric
  ratingOverallDF$ratingOverall <- as.numeric(ratingOverallDF$ratingOverall)
  
  # rating_band <-
  #   page %>%
  #   html_nodes("div.row") %>%
  #   html_nodes("div.resultcontainer") %>%
  #   html_nodes("div#fabResultsContainer") %>%
  #   html_nodes("div.fabresult") %>%
  #   html_nodes("div.resultheader") %>%
  #   html_nodes("div.fabresult-details-rating") %>%
  #   html_nodes("div.hwta-rating-container") %>%
  #   html_nodes("div.hwta-rating-summary") %>%
  #   html_nodes("div.hwta-rating-info") %>%
  #   html_nodes("span") %>%
  #   html_text()

   # review_num <-
   #  page %>%
   #  html_nodes("div.row") %>%
   #  html_nodes("div.resultcontainer") %>%
   #  html_nodes("div#fabResultsContainer") %>%
   #  html_nodes("div.fabresult") %>%
   #  html_nodes("div.resultheader") %>%
   #  html_nodes("div.fabresult-details-rating") %>%
   #  html_nodes("div.hwta-rating-container") %>%
   #  html_nodes("div.hwta-rating-summary") %>%
   #  html_nodes("div.hwta-rating-info") %>%
   #  html_nodes("a.hwta-rating-counter") %>%
   #  html_text()

  minPrice <- 
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
             minPrice, location, ratingOverall)
}

#step through each pagination link and scrape for hostel info
scrapeResult <- apply(data.frame(cityPgLinksDF$Link), 1, hostelScraping)
write.csv(scrapeResult,"scrapeResult.csv")

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
  select(hostelName, City, minPrice, location, ratingOverall, hostelLink)

#export hostel list csv
write.csv(scrapeResult,"Hostel_list.csv")

print("finished scraping ty")

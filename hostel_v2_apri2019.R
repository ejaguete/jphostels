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
homePage <- read_html("https://www.hostelworld.com/hostels/Japan")

# --------------------------------------
# VAR: scrape city names from jpHostelLink
# -------------------------------------- 
homePageCityNames <- 
  homePage %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("h2") %>% 
  html_text()

if(debugOn==TRUE) {
  print("homePageCityNames contents")
  print(homePageCityNames)
}

# -------------------------------------- 
# VAR: scrape city links from jpHostelLink
# -------------------------------------- 
homePageCityLinks <- 
  homePage %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("h2") %>% 
  html_nodes("a") %>% 
  html_attr("href")

if(debugOn==TRUE) {
  print("homePgCityLink contents")
  print(homePgCityLinks)
}

# -------------------------------------- 
# VAR: scrape number of hostels in each city from jpHostelLink
# -------------------------------------- 
homePageCityNumHostels <- 
  homePage %>% 
  html_nodes("div.contentbackground") %>% 
  html_nodes("div.small-12") %>% 
  html_nodes("div.citysection") %>% 
  html_nodes("div.cityresults_details") %>% 
  html_nodes("span.propnumber") %>% 
  html_text()

if(debugOn==TRUE) {
  print("homePageCityNumHostels contents")
  print(homePageCityNumHostels)
}

# -------------------------------------- 
# VAR: store city names, links, hostel num vars in a dataframe
# -------------------------------------- 
homePageDF <- data.frame(homePageCityNames, homePageCityLinks, homePageCityNumHostels)
write.csv(homePageDF,"homePageDF.csv")

# -------------------------------------- 
# FUNC: retrieve pagination links for each city
# param: txt url of city link e.g. Tokyo, Osaka, Hiroshima
# returns: pagination url txt in city link = pg1,2,3... for Tokyo
# -------------------------------------- 
getPaginationLink <- function (url) {
  as.character(url) %>% 
    read_html() %>% 
    html_nodes("ul.pagination") %>% 
    html_nodes("li.pagination-number") %>% 
    html_nodes("a") %>% 
    html_attr("href")
}

# -------------------------------------- 
# VAR: store pagination links & add corresponding city
# eg pg1,2,3 for Tokyo hostels has city = 'Tokyo'
# -------------------------------------- 
cityPageLinks <- apply(data.frame(homePageDF$homePageCityLink), 1, getPaginationLink)
cityPageLinksDF <- data.frame(cityPgLinks %>% unlist())
cityPageLinksDF$City <- c("Tokyo", "Tokyo", "Tokyo", "Tokyo", "Tokyo",
                    "Kyoto", "Kyoto", "Kyoto", "Kyoto", 
                    "Osaka", "Osaka", "Osaka", "Osaka",
                    "Hiroshima", "Fukuoka")
# column labels
names(cityPageLinksDF) <- c("Link", "City")

if(debugOn==TRUE) {
  print("cityPgLinksDF contents")
  print(cityPgLinksDF)
}
write.csv(cityPageLinksDF,"cityPageLinksDF.csv")

# -------------------------------------- 
# FUNC: for each pagination link, scrape hostel names and their links
# param: url of pagin. link
# returns: DF containing names and links of hostels
# -------------------------------------- 
scrapeHostelLink <- function(url) {
  #convert to url to scrape
  link <- read_html(as.character(url))
  
  # scrape hostel name from url
  hostelName <-
    link %>%
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("h2") %>% 
    html_text()
  
  #scrape hostel page link
  hostelLink <- 
    link %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("h2") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  minPrice <- 
    link %>% 
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
    link %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("div.addressline") %>% 
    html_text()
  location <- as.character(location) %>% str_remove("- Show on Map")
  location <- location %>% str_remove_all("\\n")

  # return dataframe object containing the scraped attributes
  data.frame(hostelName,hostelLink, minPrice, location)
}

# -------------------------------------- 
# FUNC: scrape info from individual hostel pages
# -------------------------------------- 
scrapeHostelInfo <- function(url) {
  link <- read_html(as.character(url))
  
  name <- 
    link %>% 
    html_nodes("div.ms-content") %>% 
    html_nodes('[name=ms-hero]') %>% 
    html_nodes("div.jumbotron") %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.small-12") %>% 
    html_nodes("div.content") %>% 
    html_nodes("h1") %>% 
    html_text()
  
  ratingOverall <-
    link %>%
    html_nodes("div.row") %>%
    html_nodes("section.small-12") %>%
    html_nodes("div.ms-rating-summary-block") %>%
    html_nodes("div.rating-summary") %>%
    html_nodes("div.score") %>%
    html_text()
  #clean out white spaces
  ratingOverall <- ratingOverall %>% str_remove_all("\\n")
  ratingOverall <- ratingOverall %>% str_remove_all("\\s")

   ratingKeyword <-
     link %>%
     html_nodes("div.row") %>%
     html_nodes("section.small-12") %>%
     html_nodes("div.ms-rating-summary-block") %>%
     html_nodes("div.rating-summary") %>%
     html_nodes("div.info") %>%
     html_nodes("p.keyword") %>%
     html_text()

   totalReviews <-
     link %>%
     html_nodes("div.row") %>%
     html_nodes("section.small-12") %>%
     html_nodes("div.ms-rating-summary-block") %>%
     html_nodes("div.rating-summary") %>%
     html_nodes("div.info") %>%
     html_nodes("a.counter") %>%
     html_nodes("span") %>%
     html_text()
   
   ratingBreakdown <-
     link %>%
     html_nodes("div.row") %>%
     html_nodes("section.small-12") %>%
     html_nodes("ul.rating-breakdown") %>%
     html_nodes("li.small-12") %>%
     html_nodes("p.rating-label") %>%
     html_text()
   
   data.frame(name, ratingOverall, ratingKeyword, totalReviews, ratingBreakdown)
}

# -------------------------------------- 
# VAR: for each link in cityPgLinksDF, apply function scrapeHostelPg
# stored in a dataframe
# -------------------------------------- 
# for each link in cityPgLinksDF, apply function scrapeHostelPg
scrapedData <- apply(data.frame(cityPgLinksDF$Link),1,scrapeHostelLink)
scrapedData <- do.call(rbind.data.frame, scrapedData)
write.csv(scrapedData,"scrapedData.csv")

print(scrapeHostelInfo(scrapedData$hostelLink[1]))
write.csv(scrapeHostelInfo(scrapedData$hostelLink[1]),
          "scrapedHotelInfo.csv")


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

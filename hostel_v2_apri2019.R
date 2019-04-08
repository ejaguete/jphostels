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
names(homePageDF) <- c("City", "Link", "Num Hostels")
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
cityPageLinks <- apply(data.frame(homePageDF$Link), 1, getPaginationLink)
cityPageLinksDF <- data.frame(cityPageLinks %>% unlist())
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
#   as well as min price and location from city centre
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
    html_text(trim=TRUE)
  
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
  
  #scrape summary rating of hostel
  ratingOverall <- 
    link %>% 
    html_nodes("div.inner-wrap") %>% 
    html_nodes("div.page-contents") %>% 
    html_nodes("div.contentbackground") %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.fabresult-details-rating") %>% 
    html_nodes("div.hwta-rating-container") %>% 
    html_text(trim=TRUE)
  #clean
  ratingOverall <- ratingOverall %>%  unlist()
  ratingOverall <- data.frame(ratingOverall)
  ratingOverall$ratingOverall <- as.character(ratingOverall$ratingOverall)
  ratingOverall$ratingOverall <- ratingOverall$ratingOverall %>% 
    str_remove_all("\n") %>%
    str_remove("\\s") %>%
    #str_remove("                                      ") %>%
    str_sub(1, 5)
  ratingOverall$ratingOverall <- as.numeric(ratingOverall$ratingOverall)
  
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
    html_text(trim=TRUE)
  
  location <- 
    link %>% 
    html_nodes("div.row") %>% 
    html_nodes("div.resultcontainer") %>% 
    html_nodes("div#fabResultsContainer") %>% 
    html_nodes("div.fabresult") %>% 
    html_nodes("div.resultheader") %>% 
    html_nodes("div.addressline") %>% 
    html_text(trim=TRUE)
  location <- as.character(location) %>% str_remove("- Show on Map")
  location <- location %>% str_remove_all("\\n")

  # return dataframe object containing the scraped attributes
  data.frame(hostelName,hostelLink, ratingOverall, minPrice, location)
}

# -------------------------------------- 
# VAR: for each link in cityPgLinksDF, apply function scrapeHostelPg
# stored in a dataframe
# -------------------------------------- 
scrapedLinks <- apply(data.frame(cityPageLinksDF$Link),1,scrapeHostelLink)
scrapedLinksDF <- do.call(rbind.data.frame, scrapedLinks)
names(scrapedLinksDF) <- c("Name", "Link", "OverallRating", "StartingPrice", "DistanceFromCityCentre")

write.csv(scrapedLinksDF,"scrapedLinks.csv")

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
    html_text(trim=TRUE)

  ratingOverall <-
    link %>%
    html_nodes("div.row") %>%
    html_nodes("section.small-12") %>%
    html_nodes("div.ms-rating-summary-block") %>%
    html_nodes("div.rating-summary") %>%
    html_nodes("div.score") %>%
    html_text(trim=TRUE)

   ratingKeyword <-
     link %>%
     html_nodes("div.row") %>%
     html_nodes("section.small-12") %>%
     html_nodes("div.ms-rating-summary-block") %>%
     html_nodes("div.rating-summary") %>%
     html_nodes("div.info") %>%
     html_nodes("p.keyword") %>%
     html_text(trim=TRUE)
   
   totalReviews <-
     link %>%
     html_nodes("div.row") %>%
     html_nodes("section.small-12") %>%
     html_nodes("div.ms-rating-summary-block") %>%
     html_nodes("div.rating-summary") %>%
     html_nodes("div.info") %>%
     html_nodes("a.counter") %>%
     html_nodes("span") %>%
     html_text(trim=TRUE)

   rbk <-
     link %>%
     html_nodes("div.row") %>%
     html_nodes("section.small-12") %>%
     html_nodes("ul.rating-breakdown") %>%
     html_nodes("li.small-12") %>%
     html_nodes("p.rating-label") %>%
     html_text(trim=TRUE) #%>% replace(!nzchar(.),NA)
   
  if(length(rbk)>0) {
    temp <- data.frame(rbk) # put into temp df
    temp$rbk <- as.character(temp$rbk) # convert everything to text
    temp$rbk <- temp$rbk %>% str_remove("\\.") %>% str_to_lower() %>% str_remove_all(" ") # make values a long string (will fix later)
    temp$rbk <- temp$rbk %>% colsplit("(?<=\\p{L})(?=[\\d+$])", c("Type", "Score")) # make 2 columns, type and score, then populate with values
    ratingBreakdown <- data.frame(temp) # make a new df containing those columns
    ratingBreakdown <- ratingBreakdown$rbk # df has a wrapper around it so get rid of the wrapper
    ratingBreakdown$Score <- ratingBreakdown$Score/10 # ratings had no decimals so converted them eg 95 -> 9.5
    ratingBreakdown <- ratingBreakdown %>% spread(Type, Score) # pivot the table
  } else {
    ratingBreakdown <- data.frame(atmosphere=NA, cleanliness=NA,facilities=NA, location=NA,security=NA, staff=NA, valueformoney=NA) 
  }
   df <- data.frame(name, ratingOverall, ratingKeyword, totalReviews, ratingBreakdown)
   print(df) # so i can see its doing things :B
   return(df)
}

# filter out hostels with no rating
filteredHostels <- scrapedLinksDF %>% filter(scrapedLinksDF$OverallRating > 0.0)
write.csv(filteredHostels, "filteredHostels.csv")

hostelDataset <- apply(data.frame(filteredHostels$Link),1,scrapeHostelInfo)
hostelDataset <- do.call(rbind.data.frame, hostelDataset)

write.csv(hostelDataset, "hostelDataset.csv")

print("finished scraping ty")

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
for(i in cityPgLinks){
  print(getIndex(i))
}
# cityPgLinks <- 
#   link.list.for.each.city %>% 
#   unlist()
# cityPgLinks <- data.frame(cityPgLinks)

# if(debugOn==TRUE) {
#   print("cityPgLinks contents")
#   print(cityPgLinks)
# }


# cityPgLinks <- 
#   cityPageLinks %>% 
#   unlist()
# link.list <- data.frame(cityPgLinks)
print("finished scraping ty")

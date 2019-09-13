#' A web scraping function
#'
#' This function allows you to scrape data from Global Affairs Canada softwood lumber Monthly Export Reports (Canada-US)
#' @param
#' @keywords exports
#' @export
#' @examples
#' jan_2018 <- getgac("2018","01")



getgac <- function(year,month){

  if (!require(rvest)) install.packages('rvest')
  if (!require(tidyverse)) install.packages('tidyverse')
  library(rvest)
  library(tidyverse)

  url <- paste("https://www.eics-scei.gc.ca/report-rapport/SWL%20monthly%20Exports%20Report_",year, month,".htm", sep="")
  webpage <- read_html(url)

  data <- webpage %>%
    html_nodes("span") %>%
    html_text %>%
    as.data.frame()%>%
    "["(.,24:45,)%>%
    as.data.frame()

  evens <- seq(1,45,2)
  odds <- seq(2,44,2)

  Volume <- data.frame(data[odds,])
  Volume <- Volume[complete.cases(Volume),]
  Volume <- as.data.frame(Volume)


  Region <- data.frame(data[evens,])
  Region <- Region[complete.cases(Region),]
  Region <- as.data.frame(Region)

  merge.data <- data.frame(Region, Volume)
  merge.data$Year <- year
  merge.data$Month <- month

  merge.data$Volume <- as.numeric(gsub(",", "", as.character(merge.data$Volume)))

  return(merge.data[-11,])

}

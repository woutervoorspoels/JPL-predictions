#this function gets relevant data from season 2015-2016
#that is, team information, identification codes and scores
pullDataJPL2015_2016 <- function(){
# pull data from wikipedia by using "rvest" 
## LOAD THE PACKAGES ####
library(rvest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

## LOAD THE DATA ####
JPL2015 = read_html("https://nl.wikipedia.org/wiki/Eerste_klasse_2015-16_(voetbal_België)")
help(html_nodes)

###### teams and information
teams <- JPL2015 %>%
  html_nodes("table") %>%
  .[[3]]%>%
  html_table(fill=T)
teamInfo <- JPL2015 %>%
  html_nodes("table") %>%
  .[[4]]%>%
  html_table(fill=T)

champ_table <- JPL2015 %>%
  html_nodes("table") %>%
  .[[6]]%>%
  html_table(fill=T) 
champ_table <- champ_table[,3:11]

POI_table <- JPL2015 %>%
  html_nodes("table") %>%
  .[[7]]%>%
  html_table(fill=T)
POI_table <- POI_table[,3:11]
 
###### results 
source(file="extract_results.R")
champ_results <- extract_results(JPL2015,5,16)
POI_results <-extract_results(JPL2015,8,6)
POIIA_results <- extract_results(JPL2015,10,4)
POIIB_results <- extract_results(JPL2015,12,4)
source(file="linkTables.R")
allgames <- rbind(champ_results,POI_results,POIIA_results,POIIB_results)
out <- linktables(allgames,teams)
allgames <- out[[1]]
teams <- out[[2]]

return(list(teams=teams,allGames=allgames))

}

### this function pulls data up to current time ###

pullDataJPL2016_2017 <- function(){

rm(list=ls())
## LOAD THE PACKAGES ####
library(rvest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
source('pullDataJPL2015_2016.R')
## LOAD THE DATA ####

## the data for JPL 2015-2016, all games and team information in a list
JPL2015 = pullDataJPL2015_2016()

## extract basic information for 2016-2017
JPL2016 = read_html("https://nl.wikipedia.org/wiki/Eerste_klasse_A_2016-17_(voetbal_België)")
teams <- JPL2016 %>%
  html_nodes("table") %>%
  .[[4]]%>%
  html_table(fill=T)
teamInfo <- JPL2016 %>%
  html_nodes("table") %>%
  .[[5]]%>%
  html_table(fill=T)
champ_table <- JPL2016 %>%
  html_nodes("table") %>%
  .[[7]]%>%
  html_table(fill=T) 
champ_table <- champ_table[,3:11]

## extract games up till current time
source(file="extract_results.R")
champ_results <- extract_results(JPL2016,6,16)

## pull all coming games at present time
calendar <- read_html("https://www.voetbalkrant.com/nl/nieuws/lees/2016-06-08/de-volledige-kalender-2016-2017-van-de-jupiler-pro-league")

program <- calendar %>%
  html_nodes("table") %>%
  .[[2]]%>%
  html_table(fill=T) 
colnames(program) <- c('date','time', 'rest','game','score') #rename columns
comingGames <- select(program,date,time,game,score)%>% #select columns
  separate(date,c('day','month','year'),sep="/")%>% #split date
  separate(game,c('home','away'),sep=" - ") %>% #split teams in home and away
  separate(score,c('homegoals','awaygoals'),sep="-")%>% #split scores
  mutate(homegoals=as.numeric(homegoals),awaygoals=as.numeric(awaygoals))%>% #adjust to numeric
  separate(day,c('day'),sep=" ") %>%  filter(day!="Speeldag")%>% # ditch some rows
  mutate(day=as.numeric(day),month=as.numeric(month),year=as.numeric(year))%>% 
  unite(date,year,month,day,sep="-") %>% mutate(date=as.Date(date))%>%
  filter(!is.na(date),date>=Sys.Date()) # filter all coming games


#### we need some uniformization of names #################################
allgames <- champ_results
allteams <- merge(JPL2015[[1]],teams, all.x=T,all.y=T,by="Plaats") %>%
arrange(ID) %>% select(ID,IDCODE,3:4,7,1) %>% mutate(IDCODE=as.character(IDCODE))
allteams[17,] <- c(17,"EUP", filter(teams,Plaats=="Eupen")%>%select(2:3,1,Plaats))
nname <- unique(comingGames$home)
code <- c('W-B','EUP','KVK', 'KVM', 'M-P','GNT',
          'AND','STA', 'CHA', 'CLU','STV', 'WES',
          'ZWA','GNK','LOK', 'KVO')

identificationTable <- data.frame(clubname=nname,IDCODE=code)
allteams <- left_join(allteams,identificationTable) %>% select(1:4,7,6)%>%
  mutate(clubname=as.character(clubname))
allteams$clubname[3] <- 'OH leuven'
colnames(allteams)[3:4] <- c('stamnummer', 'club')
comingGames <- left_join(comingGames,allteams,by= c("home" = "clubname")) %>%
  select(1:6,IDCODE,ID)%>%rename(clubnamehome=home,clubnameaway=away) %>%
  rename(home=IDCODE,homeID=ID)%>%
  left_join(allteams, by = c("clubnameaway" = "clubname"))%>%
  rename(away=IDCODE,awayID=ID)%>%
  select(1:2,home,away,homeID,awayID,homegoals,awaygoals)

## some additional changes because people are stupid
finishedGames <- allgames
finishedGames$home[finishedGames$home=="MOE"] <- 'M-P'
finishedGames$away[finishedGames$away=="MOE"] <- 'M-P'
finishedGames$home[finishedGames$home=="KOR"] <- 'KVK'
finishedGames$away[finishedGames$away=="KOR"] <- 'KVK'
finishedGames$home[finishedGames$home=="MEC"] <- 'KVM'
finishedGames$away[finishedGames$away=="MEC"] <- 'KVM'
finishedGames$home[finishedGames$home=="WAA"] <- 'W-B'
finishedGames$away[finishedGames$away=="WAA"] <- 'W-B'
finishedGames$home[finishedGames$home=="OOS"] <- "KVO"
finishedGames$away[finishedGames$away=="OOS"] <- "KVO"
finishedGames$home[finishedGames$home=="ZUL"] <- "ZWA"
finishedGames$away[finishedGames$away=="ZUL"] <- "ZWA"


## extra information
teamInfo <- left_join(teamInfo,allteams,by = "Plaats")%>%
  select(clubname,ID,IDCODE,Plaats,Stadion:Aanvoerder)


return(list(finishedGames=finishedGames,comingGames=comingGames,allTeams=allteams,teamInfo=teamInfo,identificationTable=identificationTable))
}

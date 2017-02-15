#### this function links teams to the different codes and a number

## link different tables ...
linktables <- function(allgames,teams){

labels1 <- unique(allgames$away)
labels2 <- unique(allgames$home)
labels3 <- unique(teams$Club)

# consistent order of teams
nr1 <- c(2,12,4,11,1,5,10,6,16,3,7,13,9,14,8,15)
#nr2 <- c(12,4,11,1,5,10,6,16,3,7,13,9,14,8,15,2,2,11,15,16,10,12)

altnames1 <- data.frame(nr1,labels1)
#altnames2 <- data.frame(nr2,labels2)
locations <- data.frame(cbind(teams$'# kaart', teams$Plaats))
colnames(locations) <- c('ID','Plaats')
identificationTable <- left_join(teams, altnames1, by = c('# kaart'= 'nr1'))
colnames(identificationTable)[1] <- 'ID'
colnames(identificationTable)[6] <- 'IDCODE'

## find correct IDnr
#for (i in 1: dim(allgames)[1]){
#  index <- which(altnames2$labels2%in%allgames[i,1])
#        allgames$homeID[i] <- altnames2$nr2[index]
#}

allgames <- left_join(allgames,altnames1, by = c('home' = "labels1"))
colnames(allgames)[6] <- "homeID"
allgames <- left_join(allgames,altnames1, by = c('away' = "labels1"))
colnames(allgames)[7] <- "awayID"

### uniformize the labels in home and away, clean up

allgames <- allgames[,-1]
allgames$home <- altnames1$labels1[allgames$homeID]
allgames <- select(allgames,home,away,homeID,awayID,2:4)

### TODO: get an identification table up and raunning
### TODO: how to add temporal information (round nr, game ID?)
return(list(allgames,identificationTable))
}
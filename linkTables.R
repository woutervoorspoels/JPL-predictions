#### this function links teams to the different codes and a number

## link different tables ...
linktables <- function(allgames,teams){

labels1 <- sort(unique(allgames$away),decreasing=FALSE)
labels2 <- sort(unique(allgames$home),decreasing=FALSE)
labels3 <- unique(teams$Club)

idnrs <- c(2,3,4,11,1,5,6,7,8,9,10,13,12,14,15,16)

# consistent order of teams
#nr1 <- c(2,12,4,11,1,5,10,6,16,3,7,13,9,14,8,15)
#nr2 <- c(12,4,11,1,5,10,6,16,3,7,13,9,14,8,15,2,2,11,15,16,10,12)

altnames1 <- data.frame(IDCODE=labels1,ID=idnrs)
#altnames2 <- data.frame(nr2,labels2)
locations <- data.frame(cbind(teams$'# kaart', teams$Plaats))
colnames(locations) <- c('ID','Plaats')
identificationTable <- left_join(teams, altnames1, by = c("# kaart" = "ID"))
colnames(identificationTable)[1] <- 'ID'
colnames(identificationTable)[6] <- 'IDCODE'

allgames <- left_join(allgames,altnames1, by = c('home' = "IDCODE"))
colnames(allgames)[6] <- "homeID"
allgames <- left_join(allgames,altnames1, by = c('away' = "IDCODE"))
colnames(allgames)[7] <- "awayID"

### uniformize the labels in home and away, clean up
allgames <- select(allgames,home,away,homeID,awayID,3:5)

return(list(allgames,identificationTable))
}
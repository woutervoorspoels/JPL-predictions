####################################################
#### Anayze jupiler pro league for 2016 - 2017 #####
####################################################
rm(list=ls())
## GET STARTED:
### pull data from previous season, and all games untill current t
### data from previous season will be used for priors
### data from the current season has finished games and coming games
source('pullDataJPL2015_2016.R')
source('pullDataJPL2016-2017.R')
JPL2015 <- pullDataJPL2015_2016()
JPL2016 <- pullDataJPL2016_2017()
## score data is automatically pulled from wikipedia pages (they should be up to date)
## both finished games and coming games are relative to current time
## NOTE: this code is not robust to changes in webpage layout


# I. GET PRIORS
##########################

## last season's posterior is today's prior
source('generate_priors_from2015-2016.R')
prior_abilities <- generate_abilities()
prior_abilities <- left_join(JPL2016$allTeams,
                             prior_abilities, by=c("ID" = "teamID")) %>%
  select(ID,defense:sd.defense)
## and add a non-comittal prior for the new team in the league
prior_abilities[17,2:5] <- c(mean(prior_abilities$attack,na.rm=T),1,
                             mean(prior_abilities$defense,na.rm=T),1)

# II. PREDICT COMING GAMES
##########################
require(rjags)
require(runjags)

## first, on the basis of the current season, non committal priors

## data file, including games to be predicted
inputData <- left_join(rbind(JPL2016$finishedGames[,1:4],JPL2016$comingGames[,c(3,4,7,8)]),
                  JPL2016$allTeams[,1:2], by = c("home" = "IDCODE"))%>%
                    rename(homeID=ID)%>%
  left_join(JPL2016$allTeams[,1:2], by = c("away" = "IDCODE"))%>%
  rename(awayID=ID)%>%select(1:2,5:6,3:4)

dataList <- list(
  nGames = length(inputData$home),
  nTeams = 17, ## note that OHL is still in the estimates.
  T1 = inputData$homeID,
  T2 = inputData$awayID,
  X1=inputData$homegoals,
  X2=inputData$awaygoals
)

initsList <- function(){ 
  Tattack = rgamma(dataList$nTeams,1,1) #attack parameter
  Tdefense= rgamma(dataList$nTeams,1,1) # defense paramter
  gamma = rgamma(dataList$nTeams,1,1) # home advantage parameter
  return(list(Tattack=Tattack,Tdefense=Tdefense,gamma=gamma))
}

nchains <- 3
nadapt <- 1000
nburnin <- 10000
nsample <- 10000
nthin <- 1

runJagsout <- run.jags( method = "parallel",
                        model = "jplclassic.txt",
                        monitor = c("Tattack","Tdefense", "gamma", "delta"),
                        data = dataList,
                        inits = initsList,
                        n.chains = nchains,
                        thin = nthin,
                        adapt = nadapt,
                        burnin = nburnin,
                        sample = nsample,
                        summarise=FALSE
)

#summary(runJagsout)
codaSamples = as.mcmc.list(runJagsout)
gelman.diag(codaSamples)
allSamples<-combine.mcmc(codaSamples)

abilities <- matrix(colMeans(allSamples)[1:34],17,2)
sds <- matrix(apply(allSamples[,1:34],2,sd),17,2)
homeA <- as.numeric(colMeans(allSamples)[35:51])
abilities <- data.frame(cbind(seq(1,17),abilities,sds))
colnames(abilities) <- c("teamID","attack","defense","sd.attack","sd.defense")
abilities <- left_join(abilities, JPL2015$teams, by = c('teamID' = 'ID')) %>%
  select(teamID,IDCODE,defense,attack,sd.attack,sd.defense)
abilities$IDCODE <- as.character(abilities$IDCODE)
abilities$IDCODE[17] <- "EUP"
abilities$homeAdvantage <- homeA
abilities



# III. MAKE DECISIONS
#########################
## e.g. which teams will be in PO I?
## e.g. predict the ladder at autumn
## e.g. ...

### get results of games and compare

colnames(allSamples)

outcomes <- colMeans(allSamples)[,52:290]







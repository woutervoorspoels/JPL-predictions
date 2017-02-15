### this function generates priors for attack and defense parameters
### in the classical model of ladder prediction
### on the basis of all games played in the previous season ('15-'16)
### note: this really just is a tailored function...

generate_abilities <- function(...){
#### Analyze all games played in the JPL season 14-15 
rm(list=ls())
#source('get_data_easy_tryout.R')
source('pullDataJPL2015_2016.R')
source('models.R')
## I. analyze
require(rjags)
require(runjags)

dataList <- list(
  nGames = dim(JPL2015$allGames)[1],
  nTeams = dim(JPL2015$teams)[1],
  X1 = JPL2015$allGames$homegoals,
  X2 = JPL2015$allGames$awaygoals,
  T1 = JPL2015$allGames$homeID,
  T2 = JPL2015$allGames$awayID
)

initsList <- function(){ 
  Tattack = rgamma(dataList$nTeams,1,1) #attack parameter
  Tdefense= rgamma(dataList$nTeams,1,1) # defense paramter
  gamma = rgamma(1,1,1) # home advantage parameter
  return(list(Tattack=Tattack,Tdefense=Tdefense,gamma=gamma))
}


runJagsout <- run.jags( method = "parallel",
                        model = "jplclassic.txt",
                        monitor = c("Tattack","Tdefense", "gamma", "delta"),
                        data = dataList,
                        inits = initsList,
                        n.chains = 3,
                        thin = 10,
                        adapt = 10000,
                        burnin = 10000,
                        sample = 10000,
                        summarise=FALSE
)

#summary(runJagsout)
codaSamples = as.mcmc.list(runJagsout)
gelman.diag(codaSamples)
allSamples<-combine.mcmc(codaSamples)

abilities <- matrix(colMeans(allSamples)[1:32],16,2)
sds <- matrix(apply(allSamples[,1:32],2,sd),16,2)
abilities <- data.frame(cbind(seq(1,16),abilities,sds))
colnames(abilities) <- c("teamID","attack","defense","sd.attack","sd.defense")
abilities <- left_join(abilities, JPL2015$teams, by = c('teamID' = 'ID')) %>%
  select(teamID,IDCODE,defense,attack,sd.attack,sd.defense)
abilities

return(abilities)

}
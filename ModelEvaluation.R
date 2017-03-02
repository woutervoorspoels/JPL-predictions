#### validate model ####
rm(list=ls())
require(rjags)
require(runjags)
source('pullDataJPL2016-2017.R')
JPL2016 <- pullDataJPL2016_2017()



## first, on the basis of the current season, non committal priors

## data file: all games played
inputData <- left_join(rbind(JPL2016$finishedGames[,1:4],JPL2016$comingGames[,c(3,4,7,8)]),
                       JPL2016$allTeams[,1:2], by = c("home" = "IDCODE"))%>%
  rename(homeID=ID)%>%
  left_join(JPL2016$allTeams[,1:2], by = c("away" = "IDCODE"))%>%
  rename(awayID=ID)%>%select(1:2,5:6,3:4)%>%
  filter(!is.na(homegoals))

## sample and predict
niter <- 10
wspec <- rep(NA,niter) # p(home win given a predicted home win)
lspec <- rep(NA,niter) # p(home loss given predicted loss)
wsens <- rep(NA,niter) # p(predicted home win given a win)
lsens <- rep(NA,niter) # p(predicted home loss given a loss)
tab <- array(data=NA,c(3,2,niter))

for (i in 1:niter){
  temp <- evaluate(inputData,16)
  wspec[i] <- temp$wspec
  lspec[i] <- temp$lspec
  wsens[i] <- temp$wsens
  lsens[i] <- temp$lsens
}
#####


### evaluation function
evaluate <- function(x,testsize){
  #### construct training and test set
  ngames <- dim(x)[1]
  ro <- sample(ngames)
  training <- ro[1:(ngames-testsize)]
  test <- ro[(ngames-testsize+1):ngames]
  modeldata <- x
  modeldata[test,5:6] <- NA
  
  ## run analysis
  dataList <- list(
    nGames = length(modeldata$home),
    nTeams = 17, ## note that OHL is still in the estimates.
    T1 = modeldata$homeID,
    T2 = modeldata$awayID,
    X1=modeldata$homegoals,
    X2=modeldata$awaygoals
  )
  
  initsList <- function(){ 
    Tattack = rgamma(dataList$nTeams,1,1) #attack parameter
    Tdefense= rgamma(dataList$nTeams,1,1) # defense paramter
    gamma = rgamma(dataList$nTeams,1,1) # home advantage parameter
    return(list(Tattack=Tattack,Tdefense=Tdefense,gamma=gamma))
  }
  
  nchains <- 2
  nadapt <- 100
  nburnin <- 1000
  nsample <- 10000
  nthin <- 2
  
  runJagsout <- run.jags( method = "parallel",
                          model = "jplclassic.txt",
                          monitor = c("delta"),
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
  #gelman.diag(codaSamples)
  allSamples<-combine.mcmc(codaSamples)
  
  colnames(allSamples)
  homep <- apply(allSamples,2,function(x) mean(x>0))
  
  obspred <- cbind(inputData,homep) %>%
    mutate(delta=homegoals-awaygoals)
  obspred$obs <- ifelse(obspred$delta>0,1,-1)
  obspred$obs[obspred$delta==0] <- 0
  
  obspred$model <- ifelse(obspred$homep>0.6,1,-1)
  #evaltraining$model[evaltraining$homep<.55 & evaltraining$homep>.45] <- 0
  eval <- table(obspred$obs[test],obspred$model[test])
  
  if(dim(eval)[1]==3 & dim(eval)[2]==2){
    wsp <- eval[3,2]/sum(eval[,2])
    lsp <- eval[1,1]/sum(eval[,1])
    
    wse <- eval[3,2]/sum(eval[3,])
    lse <- eval[1,1]/sum(eval[1,])
  }
  
  return(list(wspec=wsp,lspec=lsp,wsens=wse,lsens=lse, tabular=eval))
}
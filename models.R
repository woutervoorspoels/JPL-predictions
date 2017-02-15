# the simplest model with ability

modelString = "

model{  

#data
for (i in 1:nGames ){

#calculate scoring probability for each team
pi1[i] <- exp(Tability[ T1[i] ] - Tability[ T2[i] ]) / (1 + exp(Tability[ T1[i] ] - Tability[ T2[i] ]) )
pi2[i] <- exp(Tability[ T2[i] ] - Tability[ T1[i] ]) / (1 + exp(Tability[ T2[i] ] - Tability[ T1[i] ]) )

#combine to arrive at two lambdas
#first determine the number of chances per team
n1[i] <- 7
n2[i] <- 7

#resulting expected goals
X1[i] ~ dbin(pi1[i],n1[i])
X2[i] ~ dbin(pi2[i],n2[i])
X1pred[i] ~ dbin(pi1[i],n1[i])
X2pred[i] ~ dbin(pi2[i],n2[i])
delta[i] <- X1pred[i] - X2pred[i] 


}

# a prior on all team parameters
# in this model, i try to estimate three parameters for each team

for (j in 1:nTeams){
Tability[j] ~ dnorm(0,1)
}

}
#end model
"
writeLines(modelString, con="jplability.txt")


#########################################################

# a simple model with attack and defence

modelString = "

model{  

#data
for (i in 1:nGames ){

#calculate scoring probability for each team
x1[i,1] <- Tattack[ T1[i] ]
x1[i,2] <- Tdefence[ T1[i] ]
x2[i,1] <- Tattack[ T2[i] ]
x2[i,2] <- Tdefence[ T2[i] ]
#mid[i,1] <- Tmid[ T1[i] ]
#mid[i,2] <- Tmid[ T2[i] ]

#combine to arrive at two lambdas
#first determine the number of chances per team
n1[i] <- 7
n2[i] <- 7

pi1[i] <- exp(x1[i,1] - x2[i,2]) / (1 + exp(x1[i,1] - x2[i,2]) ) 
pi2[i] <- exp(x2[i,1] - x1[i,2]) / (1 + exp(x2[i,1] - x1[i,2]) ) 

#resulting expected goals
X1[i] ~ dbin(pi1[i],n1[i])
X2[i] ~ dbin(pi2[i],n2[i])
X1pred[i] ~ dbin(pi1[i],n1[i])
X2pred[i] ~ dbin(pi2[i],n2[i])
delta[i] <- X1pred[i] - X2pred[i] 


}

# a prior on all team parameters
# in this model, i try to estimate three parameters for each team

for (j in 1:nTeams){
Tattack[j] ~ dnorm(0,1)
Tdefence[j] ~ dnorm(0,1)
#Tmid[j]  ~ dnorm(0,1) 
}

}
#end model
"


writeLines(modelString, con="jplattackdefense.txt")

######################################################
# the classic model 

modelString = "

model{  

#data
for (i in 1:nGames ){

#calculate scoring probability for each team
x1[i,1] <- Tattack[ T1[i] ]
x1[i,2] <- Tdefense[ T1[i] ]
x2[i,1] <- Tattack[ T2[i] ]
x2[i,2] <- Tdefense[ T2[i] ]


#combine to arrive at two lambdas

lambda1[i] <- gamma*x1[i,1]*x2[i,2]
lambda2[i] <- x2[i,1]*x1[i,2]

#resulting expected goals
X1[i] ~ dpois(lambda1[i])
X2[i] ~ dpois(lambda2[i])
X1pred[i] ~ dpois(lambda1[i])
X2pred[i] ~ dpois(lambda2[i])
delta[i] <- X1pred[i] - X2pred[i]

}

# a prior on all team parameters
# in this model, i try to estimate two parameters for each team

for (j in 1:nTeams){
Tattack[j] ~ dgamma(1,1) #check these priors!!
Tdefense[j] ~ dgamma(1,1) #check these priors!!
}
gamma~dgamma(1,1)

}
#end model
"
writeLines(modelString, con="jplclassic.txt")

#####################################################

#this is a model with attack, defense and midfield

modelString = "

model{  

#data
for (i in 1:nGames ){

#calculate scoring probability for each team
x1[i,1] <- Tattack[ T1[i] ]
x1[i,2] <- Tdefence[ T1[i] ]
x2[i,1] <- Tattack[ T2[i] ]
x2[i,2] <- Tdefence[ T2[i] ]
mid[i,1] <- Tmid[ T1[i] ]
mid[i,2] <- Tmid[ T2[i] ]

#combine to arrive at two lambdas
#first determine the number of chances per team
lambda1[i] <- MAX*(exp(mid[i,1])/(exp(mid[i,1]) + exp(mid[i,2])))
lambda2[i] <- MAX*(exp(mid[i,2])/(exp(mid[i,1]) + exp(mid[i,2])))

n1[i] ~ dpois(lambda1[i])
n2[i] ~ dpois(lambda2[i])

pi1[i] <- (exp(x1[i,1] - x2[i,2]) / (1 + exp(x1[i,1] - x2[i,2]) )) * n1[i]
pi2[i] <- (exp(x2[i,1] - x1[i,2]) / (1 + exp(x2[i,1] - x1[i,2]) )) * n2[i]

#resulting expected goals
X1[i] ~ dpois(pi1[i])
X2[i] ~ dpois(pi2[i])
#X1pred[i] ~ dbin(pi1[i],n1[i])
#X2pred[i] ~ dbin(pi2[i],n2[i])
#delta[i] <- X1pred[i] - X2pred[i] 


}

# a prior on all team parameters
# in this model, i try to estimate three parameters for each team

for (j in 1:nTeams){
Tattack[j] ~ dnorm(0,1)
Tdefence[j] ~ dnorm(0,1)
Tmid[j]  ~ dgamma(1,1) 

}

MAX ~dunif(2,15)

}
#end model
"


writeLines(modelString, con="jplatdefmid.txt")


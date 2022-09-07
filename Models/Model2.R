#define global parameters: N = number of individuals; generations = maximum lifetime of individual or number of generations of individuals in population; mu = mutation rate; delta = probability game ends on next round

mimic.PD <- function(N=100, generations=1000, mu=.01, delta=.001){

  #define payoffs, c = cooperate; d = defect; payoff refers to ego, which is the first player in the given sequences
  
  cc <- .1
  cd <- -.1
  dc <- .2
  dd <- 0
  
  #define cost of being punished and punishing
  
  cost <- -.4
  punish <- -.1
  
  #define degree of fluctuation that will adjust PTR after reproduction
  
  f <- seq(-.4,.4,.1)
  
  #create first population of N individuals with matching sampled traits 
  #individual-level parameters: PTR = potential to reproduce; CAE = conditional action expression; types = individual types
  #individual-level parameter variables: CAE: c = cooperative; d = free-rider (defects); types: h = honest; m = mimic; mimicry = mimicry score; sensitivity = sensitivity score
  
  individual <- 1:N
  
  CAE <- factor(c("c","d"))
  
  types <- factor(c("h","m"))
  
  population <- data.frame(individual=1:N, PTR=round(abs(rnorm(N,mean=.5,sd=.2)), digits=2), strategy=factor("d",levels=c("c","d")), type=sample(types, N, replace=TRUE), mimicry=NA, sensitivity=NA)
  
  #add mimicry and sensitivity scores, and strategies — mimic default strategy in gen 1 is defect; note that honest signallers still have mimicry score but do not use it
  
  population$strategy[population$type=="h"] <- sample(CAE, length(population$strategy[population$type=="h"]), replace=TRUE)
  population$mimicry <- round(abs(rnorm(N,mean=.5,sd=.2)), digits=2)
  population$sensitivity <- round(abs(rnorm(N,mean=.5,sd=.2)), digits=2)
  
  #ensure no individual-level parameter values are below 0 or above 1
  
  population$PTR[population$PTR>1] <- 1
  population$PTR[population$PTR<0] <- 0
  population$mimicry[population$mimicry>1] <- 1
  population$mimicry[population$mimicry<0] <- 0
  population$sensitivity[population$sensitivity>1] <- 1
  population$sensitivity[population$sensitivity<0] <- 0
  
  #create dataframes for output with columns for strategy, frequencies of strategy types, and corresponding generations; second df for types
  
  output.CAE <- data.frame(strategy=rep(CAE, generations), Freq=rep(NA, 2*generations), generation=rep(1:generations, each=2))
  output.types <- data.frame(type=rep(types, generations), Freq=rep(NA, 2*generations), generation=rep(1:generations, each=2))
  
  #create cooperation % output dataframe
  
  coop.output <-  data.frame(cooperation=NA, generation=1:generations)
  
  #determine the strategy and type frequencies from the first population
  
  Freq.pop.CAE <- as.data.frame(table(population$strategy))
  names(Freq.pop.CAE)[1] <- "strategy"
  
  Freq.pop.type <- as.data.frame(table(population$type))
  names(Freq.pop.type)[1] <- "type"
  
  #slot first population trait frequencies into outputs
  
  output.CAE[1:length(CAE), 2] <- Freq.pop.CAE[1:length(CAE), 2]
  
  output.types[1:length(types), 2] <- Freq.pop.type[1:length(types), 2]
  
  #determine average PTR, mimicry, and sensitivity scores, slot into outputs
  
  output.parameters <- data.frame(PTR=rep(NA, generations), mimicry=rep(NA, generations), sensitivity=rep(NA, generations), generations=1:generations)
  
  output.parameters[1,1:3] <- c(mean(population$PTR), mean(population$mimicry[population$type=="m"]), mean(population$sensitivity[population$type=="h"]))
  
  #determine reproduction over generations
  
  for (generations in 1:generations) {
    
    #determine whether game ends
    
    if(sample(1:2, 1, prob=c(delta,1-delta))==1){
      
      break
      
    }
    
    #play out PD over generations
      
    #randomize population order
    
    population <- population[sample(1:nrow(population)),]
    
    #split population into 2 groups to match dyads
    
    group1 <- population[1:round(N/2),]
    group2 <- population[(round((N/2)+1):N),]

    #if mimic has equal or lower mimicry to twice partner's sensitivity, switch to cooperate

    group1$strategy[group1$mimicry<=2*group2$sensitivity] <- "c"
    group2$strategy[group2$mimicry<=2*group1$sensitivity] <- "c"
    
    #store individual IDs of conditional defectors
    
    conditional.defectors <- c(group1[c(which(group1$mimicry<=group2$sensitivity)),1],group2[c(which(group2$mimicry<=group1$sensitivity)),1])
    conditional.defectors <- sort(conditional.defectors)
    
    #match dyads to determine strategies; format is: focal individual strategy, partner strategy, partner type
    
    group1$payoff <- paste0(group1$strategy,group2$strategy,group2$type)
    group2$payoff <- paste0(group2$strategy,group1$strategy,group1$type)
    
    #regroup population, order by individual
    
    population <- rbind(group1,group2)
    population <- population[order(population$individual),]
      
    #determine payoffs from PD, note that conditional cooperators do not cooperate with honest defectors and punish both honest and mimic defectors
    
    population$PTR[which(population$payoff=="cch")] <- population$PTR[which(population$payoff=="cch")]+cc
    population$PTR[which(population$payoff=="ccm")] <- population$PTR[which(population$payoff=="ccm")]+cc
    
    population$PTR[which(population$payoff=="cdh")] <- population$PTR[which(population$payoff=="cdh")]+dd
    population$PTR[which(population$payoff=="cdm")] <- population$PTR[which(population$payoff=="cdm")]+cd
    
    population$PTR[which(population$payoff=="dch")] <- population$PTR[which(population$payoff=="dch")]+dd
    population$PTR[which(population$payoff=="dcm")] <- population$PTR[which(population$payoff=="dcm")]+dc
    
    population$PTR[which(population$payoff=="ddh")] <- population$PTR[which(population$payoff=="ddh")]+dd
    population$PTR[which(population$payoff=="ddm")] <- population$PTR[which(population$payoff=="ddm")]+dd
    
    #cooperating individuals punish defectors, whether honest or mimic
    
    population$PTR[which(population$payoff=="cdh")] <- population$PTR[which(population$payoff=="cdh")]+punish
    population$PTR[which(population$payoff=="cdm")] <- population$PTR[which(population$payoff=="cdm")]+punish
    
    population$PTR[which(population$payoff=="dch")] <- population$PTR[which(population$payoff=="dch")]+cost
    population$PTR[which(population$payoff=="dcm")] <- population$PTR[which(population$payoff=="dcm")]+cost
    
    #determine % of cooperation in population, slot into cooperation output
    
    coop <- length(which(population$payoff=="cch"))+length(which(population$payoff=="ccm"))+length(which(population$payoff=="cdm"))
    coop.output[(generations-1),1] <- coop
    
    #revert conditional mimics who failed sensitivity test to strategy d
    
    population[c(conditional.defectors),3] <- "d"
    
    #ensure no PTR values are below 0 or above 1
    
    population$PTR[population$PTR>1] <- 1
    population$PTR[population$PTR<0] <- 0

    #reproduce population based on PTR
    
    population <- cbind(individual, population[sample(1:nrow(population), N, replace=TRUE, prob=population$PTR),2:6])
    
    #adjust PTR, mimicry, and sensitivity by f in next generation
    
    population$PTR <- population$PTR+sample(f,nrow(population),replace=TRUE)
    population$mimicry <- population$mimicry+sample(f,nrow(population),replace=TRUE)
    population$sensitivity <- population$sensitivity+sample(f,nrow(population),replace=TRUE)
    
    #ensure no individual parameter values are below 0 or above 1
    
    population$PTR[population$PTR>1] <- 1
    population$PTR[population$PTR<0] <- 0
    population$mimicry[population$mimicry>1] <- 1
    population$mimicry[population$mimicry<0] <- 0
    population$sensitivity[population$sensitivity>1] <- 1
    population$sensitivity[population$sensitivity<0] <- 0
    
    #mutate strategy and type with probability mu
    
    for(i in 1:N){
      
      if(sample(1:2, 1, prob=c(mu,1-mu))==1){
        
        ifelse(population$strategy[i]=="c",population$strategy[i] <- "d",population$strategy[i] <- "c")

      }
      
      if(sample(1:2, 1, prob=c(mu,1-mu))==1){
        
        ifelse(population$type[i]=="h",population$type[i] <- "m",population$type[i] <- "h")
        
      }
      
    }
    
    #determine the strategy and type frequencies from the population
    
    Freq.pop.CAE <- as.data.frame(table(population$strategy))
    names(Freq.pop.CAE)[1] <- "strategy"
    
    Freq.pop.type <- as.data.frame(table(population$type))
    names(Freq.pop.type)[1] <- "type"
    
    #slot new generation data into outputs
    
    if(any(is.na(output.CAE))){
      
      output.CAE[match(NA, output.CAE[, 2]):(match(NA, output.CAE[, 2])+(length(CAE))-1), 2] <- Freq.pop.CAE[1:(length(CAE)), 2]
      output.types[match(NA, output.types[, 2]):(match(NA, output.types[, 2])+(length(types))-1), 2] <- Freq.pop.type[1:(length(types)), 2]
      output.parameters[generations,1:3] <- c(mean(population$PTR), mean(population$mimicry[population$type=="m"]), mean(population$sensitivity))
      
    }
    
  }
    
#remove NAs
  
if(any(is.na(output.CAE))){
  
output.CAE <- output.CAE[-c(match(NA, output.CAE[,2]):nrow(output.CAE)),]

}
  
if(any(is.na(output.types))){
    
output.types <- output.types[-c(match(NA, output.types[,2]):nrow(output.types)),]
    
}
  
if(any(is.na(coop.output))){
    
coop.output <- coop.output[-c(match(NA, coop.output[,1]):nrow(coop.output)),]
    
}
  
#record final parameters

output.parameters[max(generations),1:3] <- c(mean(population$PTR), mean(population$mimicry[population$type=="m"]), mean(population$sensitivity))
  
#print

list(output.CAE,output.types,output.parameters,coop.output)
  
}

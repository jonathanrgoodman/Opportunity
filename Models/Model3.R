#define global parameters: N = number of individuals; generations = maximum lifetime of individual or number of generations of individuals in population; mu = mutation rate; delta = probability game ends on next round

mimic.novel.PD <- function(N=100, generations=1000, mu=.01, delta=.001){
  
  #define payoffs, c = cooperate; d = defect; payoff refers to ego, which is the first player in the given sequences
  
  cc <- .1
  cd <- -.1
  dc <- .2
  dd <- 0
  
  #define cost of being punished (cost) and punishing (punish)
  
  cost <- -.4
  punish <- -.1
  
  #define degree of fluctuation that will adjust PTR after reproduction
  
  f <- seq(-.4,.4,.1)
  
  #define probability of discovery of mutant addendum to strategy, s, by mimics; s = silent (covert) defection; all actions are initially overt
  
  mu.s <- .0001
  
  #create first population of N individuals with matching sampled traits 
  #individual-level parameters: PTR = potential to reproduce; CAE = conditional action expression; types = individual types
  #individual-level parameter variables: CAE: c = cooperative; d = free-rider (defects); types: h = honest; m = mimic; mimicry = mimicry score; sensitivity = sensitivity score
  
  individual <- 1:N
  
  CAE <- factor(c("c","d"))
  
  types <- factor(c("h","m"))
  
  #individual-level parameter of whether actions are overt or silent; all strategies are initially overt
  
  appearance <- factor(c("o","s"))
  
  #generate first population
  
  population <- data.frame(individual=1:N, PTR=round(abs(rnorm(N,mean=.5,sd=.2)), digits=2), strategy=factor("d",levels=c("c","d")), type=sample(types, N, replace=TRUE), mimicry=NA, sensitivity=NA, appearance=factor("o", levels=c("o","s")))
  
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
  
  #create dataframes for output with columns for strategy, frequencies of strategy types, and corresponding generations; second df for types; third df for appearance
  
  output.CAE <- data.frame(strategy=rep(CAE, generations), Freq=rep(NA, 2*generations), generation=rep(1:generations, each=2))
  output.types <- data.frame(type=rep(types, generations), Freq=rep(NA, 2*generations), generation=rep(1:generations, each=2))
  output.appearance <- data.frame(appearance=rep(appearance, generations), Freq=rep(NA, 2*generations), generation=rep(1:generations, each=2))
  
  #create cooperation % output dataframe
  
  coop.output <-  data.frame(cooperation=NA, generation=1:generations)
  
  #determine the strategy, type, and appearance frequencies from the first population
  
  Freq.pop.CAE <- as.data.frame(table(population$strategy))
  names(Freq.pop.CAE)[1] <- "strategy"
  
  Freq.pop.type <- as.data.frame(table(population$type))
  names(Freq.pop.type)[1] <- "type"
  
  Freq.pop.appearance <- as.data.frame(table(population$appearance))
  names(Freq.pop.type)[1] <- "appearance"
  
  #slot first population trait frequencies into outputs
  
  output.CAE[1:length(CAE), 2] <- Freq.pop.CAE[1:length(CAE), 2]
  
  output.types[1:length(types), 2] <- Freq.pop.type[1:length(types), 2]
  
  output.appearance[1:length(appearance), 2] <- Freq.pop.appearance[1:length(appearance), 2]
  
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

    #if overt mimic has equal or lower mimicry to twice partner's sensitivity, switch to cooperate; covert mimics require better mimicry only

    group1$strategy[group1$mimicry <= 2*group2$sensitivity & group1$appearance=="o"] <- "c"
    group2$strategy[group2$mimicry <= 2*group1$sensitivity & group2$appearance=="o"] <- "c"
    
    group1$strategy[group1$mimicry <= group2$sensitivity & group1$appearance=="s"] <- "c"
    group2$strategy[group2$mimicry <= group1$sensitivity & group2$appearance=="s"] <- "c"
    
    #store individual IDs of conditional defectors
    
    conditional.defectors <- c(group1[c(which(group1$mimicry<=group2$sensitivity)),1],group2[c(which(group2$mimicry<=group1$sensitivity)),1])
    conditional.defectors <- sort(conditional.defectors)
    
    #match dyads to determine strategies; format is: focal individual (ego) strategy, ego appearance, partner strategy, partner type, partner appearance
    
    group1$payoff <- paste0(group1$strategy,group1$appearance,group2$strategy,group2$type,group2$appearance)
    group2$payoff <- paste0(group2$strategy,group2$appearance,group1$strategy,group1$type,group1$appearance)
    
    #regroup population, order by individual
    
    population <- rbind(group1,group2)
    population <- population[order(population$individual),]
      
    #determine payoffs from PD, note that conditional cooperators do not cooperate with honest defectors and punish both honest and mimic defectors
    
    #strategies to be interpreted as follows: CAE1, CAE2, type2, appearance2, where 1 = ego and 2 = partner
    
    #s, allows high-mimicry defectors to defect without being noticed; sd-players defect against one-another and defectors; h cannot be s
    
    #payoffs to be determined by dyad trait match as translated into relationship: dscmo = "ego silently defects against mimic who cooperates overtly"
    
    population$PTR[which(population$payoff=="cocho")] <- population$PTR[which(population$payoff=="cocho")]+cc
    population$PTR[which(population$payoff=="cscho")] <- population$PTR[which(population$payoff=="cscho")]+cc
    
    population$PTR[which(population$payoff=="cocmo")] <- population$PTR[which(population$payoff=="cocmo")]+cc
    population$PTR[which(population$payoff=="cscmo")] <- population$PTR[which(population$payoff=="cscmo")]+cc
    
    population$PTR[which(population$payoff=="cocms")] <- population$PTR[which(population$payoff=="cocms")]+cc
    population$PTR[which(population$payoff=="cscms")] <- population$PTR[which(population$payoff=="cscms")]+cc
    
    population$PTR[which(population$payoff=="codho")] <- population$PTR[which(population$payoff=="codho")]+dd
    population$PTR[which(population$payoff=="csdho")] <- population$PTR[which(population$payoff=="csdho")]+dd
    
    population$PTR[which(population$payoff=="codmo")] <- population$PTR[which(population$payoff=="codmo")]+cd
    population$PTR[which(population$payoff=="csdmo")] <- population$PTR[which(population$payoff=="csdmo")]+cd
    
    population$PTR[which(population$payoff=="codms")] <- population$PTR[which(population$payoff=="codms")]+cd
    population$PTR[which(population$payoff=="csdms")] <- population$PTR[which(population$payoff=="csdms")]+cd
    
    population$PTR[which(population$payoff=="docho")] <- population$PTR[which(population$payoff=="docho")]+dd
    population$PTR[which(population$payoff=="dscho")] <- population$PTR[which(population$payoff=="dscho")]+dc
    
    population$PTR[which(population$payoff=="docmo")] <- population$PTR[which(population$payoff=="docmo")]+dc
    population$PTR[which(population$payoff=="dscmo")] <- population$PTR[which(population$payoff=="dscmo")]+dc
    
    population$PTR[which(population$payoff=="docms")] <- population$PTR[which(population$payoff=="docms")]+dc
    population$PTR[which(population$payoff=="dscms")] <- population$PTR[which(population$payoff=="dscms")]+dc
    
    population$PTR[which(population$payoff=="dodho")] <- population$PTR[which(population$payoff=="dodho")]+dd
    population$PTR[which(population$payoff=="dsdho")] <- population$PTR[which(population$payoff=="dsdho")]+dd
    
    population$PTR[which(population$payoff=="dodmo")] <- population$PTR[which(population$payoff=="dodmo")]+dd
    population$PTR[which(population$payoff=="dsdmo")] <- population$PTR[which(population$payoff=="dsdmo")]+dd
    
    population$PTR[which(population$payoff=="dodms")] <- population$PTR[which(population$payoff=="dodms")]+dd
    population$PTR[which(population$payoff=="dsdms")] <- population$PTR[which(population$payoff=="dsdms")]+dd
    
    #determine % of cooperation in population, slot into cooperation output
    
    coop <- length(which(population$payoff=="cocho"))+
            length(which(population$payoff=="cscho"))+
            length(which(population$payoff=="cocmo"))+
            length(which(population$payoff=="cscmo"))+
            length(which(population$payoff=="cocms"))+
            length(which(population$payoff=="cscms"))+
            length(which(population$payoff=="codmo"))+
            length(which(population$payoff=="csdmo"))+
            length(which(population$payoff=="codms"))+
            length(which(population$payoff=="csdms"))
    
    coop.output[(generations-1),1] <- coop
    
    #cheated cooperators punish honest defectors and mimic defectors who do not have appearance = s or who do not have higher mimicry score than cooperator's sensitivity
    
    #cheated cooperators can include mimics who switch to cooperate; these still punish to avoid second-order perception of failure to punish
    
    population$PTR[which(population$payoff=="codho")] <- population$PTR[which(population$payoff=="codho")]+punish
    population$PTR[which(population$payoff=="csdho")] <- population$PTR[which(population$payoff=="csdho")]+punish
    
    population$PTR[which(population$payoff=="codmo")] <- population$PTR[which(population$payoff=="codmo")]+punish
    population$PTR[which(population$payoff=="csdmo")] <- population$PTR[which(population$payoff=="csdmo")]+punish
    
    population$PTR[which(population$payoff=="dsdmo")] <- population$PTR[which(population$payoff=="dsdmo")]+punish
    
    population$PTR[which(population$payoff=="docho")] <- population$PTR[which(population$payoff=="docho")]+cost
    population$PTR[which(population$payoff=="dochs")] <- population$PTR[which(population$payoff=="dochs")]+cost
    
    population$PTR[which(population$payoff=="docmo")] <- population$PTR[which(population$payoff=="docmo")]+cost
    population$PTR[which(population$payoff=="docms")] <- population$PTR[which(population$payoff=="docms")]+cost
    
    #revert conditional mimics who failed sensitivity test to strategy d
    
    population[c(conditional.defectors),3] <- "d"
    
    #ensure no PTR values are below 0 or above 1
    
    population$PTR[population$PTR>1] <- 1
    population$PTR[population$PTR<0] <- 0

    #reproduce population based on PTR
    
    population <- cbind(individual, population[sample(1:nrow(population), N, replace=TRUE, prob=population$PTR),2:7])
    
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
      
      if(sample(1:2, 1, prob=c(mu,1-mu))==1 & population$appearance[i]=="o"){
        
        ifelse(population$type[i]=="h",population$type[i] <- "m",population$type[i] <- "h")
        
      }
      
    }
    
    #mutate appearance of mimics with probability mu.s
    
    for(i in 1:N){
      
      if(length(which(population$type=="m")) > 0){
        
      if(sample(1:2, 1, prob=c(mu.s,1-mu.s))==1 & population$type[i]=="m"){
        
        ifelse(population$appearance[i]=="o",population$appearance[i] <- "s",population$appearance[i]=="o")
        
      }
        
      }
      
    }
    
    #mutate appearance of individuals from s to o with probability mu.s
    
    for(i in 1:N){
      
      if(length(which(population$appearance=="s")) > 0){
        
        if(sample(1:2, 1, prob=c(mu.s,1-mu.s))==1 & population$appearance[i]=="s"){
          
          population$appearance[i] <- "o"
          
        }
        
      }
      
    }
    
    
    #determine the strategy, type, and appearance frequencies from the population
    
    Freq.pop.CAE <- as.data.frame(table(population$strategy))
    names(Freq.pop.CAE)[1] <- "strategy"
    
    Freq.pop.type <- as.data.frame(table(population$type))
    names(Freq.pop.type)[1] <- "type"
    
    Freq.pop.appearance <- as.data.frame(table(population$appearance))
    names(Freq.pop.appearance)[1] <- "appearance"
    
    #slot new generation data into outputs
    
    if(any(is.na(output.CAE))){
      
      output.CAE[match(NA, output.CAE[, 2]):(match(NA, output.CAE[, 2])+(length(CAE))-1), 2] <- Freq.pop.CAE[1:(length(CAE)), 2]
      output.types[match(NA, output.types[, 2]):(match(NA, output.types[, 2])+(length(types))-1), 2] <- Freq.pop.type[1:(length(types)), 2]
      output.appearance[match(NA, output.appearance[, 2]):(match(NA, output.appearance[, 2])+(length(appearance))-1), 2] <- Freq.pop.appearance[1:(length(appearance)), 2]
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

if(any(is.na(output.appearance))){
  
  output.appearance <- output.appearance[-c(match(NA, output.appearance[,2]):nrow(output.appearance)),]
  
}
  
if(any(is.na(coop.output))){
    
   coop.output <- coop.output[-c(match(NA, coop.output[,1]):nrow(coop.output)),]
    
}
  
#record final parameters

output.parameters[max(generations),1:3] <- c(mean(population$PTR), mean(population$mimicry[population$type=="m"]), mean(population$sensitivity))
  
#print

list(output.CAE,output.types,output.appearance,output.parameters,coop.output)
  
}

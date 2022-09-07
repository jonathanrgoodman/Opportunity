#define global parameters: N = number of individuals; generations = maximum lifetime of individual or number of generations of individuals in population; mu = mutation rate; delta = probability game ends on next round

basic.PD <- function(N=100, generations=1000, mu=.01, delta=.001){
  
  #define payoffs, c = cooperate; d = defect; payoff refers to ego, which is the first player in the given sequences
  
  cc <- .1
  cd <- -.1
  dc <- .2
  dd <- 0
  
  #define degree of fluctuation that will adjust PTR after reproduction
  
  f <- seq(-.4,.4,.1)
  
  #create first population of N individuals with matching sampled traits, individual-level parameters: PTR = potential to reproduce; CAE = conditional action expression; c = cooperative; d = free-rider (defects)
  
  individual <- 1:N
  
  CAE <- factor(c("c","d"))
  
  population <- data.frame(individual=1:N, PTR=round(abs(rnorm(N,mean=.5,sd=.2)), digits=2), strategy=sample(CAE, N, replace=TRUE))
  
  #ensure no PTR values are below 0 or above 1
  
  population$PTR[population$PTR>1] <- 1
  population$PTR[population$PTR<0] <- 0
  
  #create dataframe for output with columns for strategy, frequencies of strategy types, and corresponding generations
  
  output <- data.frame(strategy=rep(CAE, generations), Freq=rep(NA, 2*generations), generation=rep(1:generations, each=2))
  
  coop.output <-  data.frame(cooperation=NA, generation=1:generations)
  
  #determine the strategy frequencies from the first population
  
  Freq.pop <- as.data.frame(table(population$strategy))
  names(Freq.pop)[1] <- "strategy"
  
  #slot first population trait frequencies into outputs
  
  output[1:length(CAE), 2] <- Freq.pop[1:length(CAE), 2]
  
  #determine reproduction over generations
  
  for (generations in 2:generations) {
    
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
    
    #match dyads to determine strategies
    
    group1$payoff <- paste(group1$strategy,group2$strategy)
    group2$payoff <- paste(group2$strategy,group1$strategy)
    
    #regroup population, order by individual
    
    population <- rbind(group1,group2)
    population <- population[order(population$individual),]
      
    #determine payoffs from PD
    
    population$PTR[which(population$payoff=="c c")] <- population$PTR[which(population$payoff=="c c")]+cc
    population$PTR[which(population$payoff=="c d")] <- population$PTR[which(population$payoff=="c d")]+cd
    population$PTR[which(population$payoff=="d c")] <- population$PTR[which(population$payoff=="d c")]+dc
    population$PTR[which(population$payoff=="d d")] <- population$PTR[which(population$payoff=="d d")]+dd
    
    #determine % of cooperation in population, slot into cooperation output
    
    coop <- length(substr(population$payoff, 1, 1)[substr(population$payoff, 1, 1)=="c"])
    coop.output[(generations-1),1] <- coop
    
    #ensure no PTR values are below 0 or above 1
    
    population$PTR[population$PTR>1] <- 1
    population$PTR[population$PTR<0] <- 0

    #reproduce population based on PTR
    
    population <- cbind(individual, population[sample(1:nrow(population), N, replace=TRUE, prob=population$PTR),2:3])
    
    #adjust PTR by f in next generation
    
    population$PTR <- population$PTR+sample(f,nrow(population),replace=TRUE)
    
    #ensure no PTR values are below 0 or above 1
    
    population$PTR[population$PTR>1] <- 1
    population$PTR[population$PTR<0] <- 0
    
    #mutate strategy with probability mu
    
    for(i in 1:N){
      
      if(sample(1:2, 1, prob=c(mu,1-mu))==1){
        
        ifelse(population$strategy[i]=="c",population$strategy[i] <- "d",population$strategy[i] <- "c")

      }
      
    }
    
    #determine the strategy frequencies from the population
    
    Freq.pop <- as.data.frame(table(population$strategy))
    names(Freq.pop)[1] <- "strategy"
    
    #slot into output
    
    output[match(NA, output[, 2]):(match(NA, output[, 2])+(length(CAE))-1), 2] <- Freq.pop[1:(length(CAE)), 2]
    
  }
    
#remove NAs and print
  
if(any(is.na(output))){
  
  output <- output[-c(match(NA, output[,2]):nrow(output)),]

}
  
if(any(is.na(coop.output))){
    
  coop.output <- coop.output[-c(match(NA, coop.output[,1]):nrow(coop.output)),]
    
  }
  
list(output,coop.output)
  
}

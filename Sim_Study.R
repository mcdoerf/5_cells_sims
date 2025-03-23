

set.seed(2389)


####################Setting important parameters.

nsims<-10000  ##Number of simulations we want to run.
Ntot<-10000  ##Total size of the population
N_Flu<-300  ##The number diseased

psymp_flu<-0.5 #Probability of being symptomatic given that the individual has the Flu.
psymp_healthy<-0.1 #Probability of being symptomatic given that the individual is healthy.

p1_symp<-0.25  #Probability of being selected into stream 1 (non-anchor stream) given symptomatic.
p1_nonsymp<-0.05 #Probability of being selected into stream 1 given non-symptomatic.

p2<-0.2 #Probability of being selected into stream 2 (anchor stream).



##################Generating population.

population<-data.frame(  #Generating population
  id=1:Ntot,
  flu=c(rep(1, N_Flu), rep(0, Ntot-N_Flu))
  )



for (i in 1:Ntot){     ###Generating symptomatic status
  
population$symptomatic[i]<-ifelse(
  population$flu[i]==1,
  rbinom(1, 1, psymp_flu),
  rbinom(1,1, psymp_healthy))

}



#################Simulations

sim_study<-function(nsims, population, p1_symp, p1_nonsymp, p2){

##Simulating sampling nsims number of times.
datasets <- replicate(nsims, simulate_data(population=population, p1_symp=p1_symp, p1_nonsymp=p1_nonsymp, p2=p2), simplify = FALSE)

#Making datasets into a dataframe.
datasets<-as.data.frame(datasets)

#Taking transpose for simplification.
datasets<-t(datasets)

rownames(datasets)<-NULL

colnames(datasets)<-c("n1", "n2", "n3", "n4", "n5", "n6", "n7")


###We calculate the MLE as well as the estimated standard error for each simulation.
MLEs <- t(apply(datasets, 1, MLE_5))

##Let's compare the empirical standard deviation of the MLE estimates to the mean of Lin's FPC corrected standard error estimates.

sd<-sd(MLEs[,1])  ##Empirical standard deviation
se<-mean(MLEs[,2]) ##Mean of Lin's FPC corrected standard error estimates.


paste("Empirical standard deviation =", sd, "FPC Corrected standard deviation=", se)

}


sim_study(nsims=nsims, population=population, p1_symp=p1_symp, p1_nonsymp=p1_nonsymp, p2=p2)
sim_study(nsims=1000, population=population, p1_symp=0.3, p1_nonsymp=0.1, p2=0.05)
sim_study(nsims=1000, population=population, p1_symp=0.3, p1_nonsymp=0.1, p2=0.1)
sim_study(nsims=1000, population=population, p1_symp=0.3, p1_nonsymp=0.1, p2=0.2)
sim_study(nsims=1000, population=population, p1_symp=0.3, p1_nonsymp=0.1, p2=0.5)
sim_study(nsims=1000, population=population, p1_symp=0.3, p1_nonsymp=0.1, p2=0.6)


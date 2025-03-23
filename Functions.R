#############Important Functions



###This function creates our population
##Ntot is total number of people in the population.  N_flu is the number of flu cases.  psymp_flu is the probability of
##being symptomatic, given that the individual has the flu.  psymp_healthy is the probability of begin symptomatic given the
##individual does not have the flu.

population_generator<-function(Ntot, N_flu, psymp_flu, psymp_healthy){
population<-data.frame(  #Generating population
  id=1:Ntot,
  flu=c(rep(1, N_flu), rep(0, Ntot-N_flu))
)




for (i in 1:Ntot){     ###Generating symptomatic status
  
  population$symptomatic[i]<-ifelse(
    population$flu[i]==1,
    rbinom(1, 1, psymp_flu),
    rbinom(1,1, psymp_healthy))
  
}

return(population)

}
##This function takes our population, and samples from it (implements streams 1 and 2).  Then, returns a vector of the counts
##n1, n2, n3, n4 n5, n6, and n7.

simulate_data<-function(population, p1_symp, p1_nonsymp, p2){
  
  sim_pop<-population
  
  sim_pop$stream1 <- ifelse(
    population$symptomatic == 1,
      rbinom(Ntot, 1, p1_symp),
      rbinom(Ntot, 1, p1_nonsymp)
  )
  
  sim_pop$stream2<-rbinom(Ntot, 1, p2)
  
  n1<-sum(sim_pop$stream1==1 & sim_pop$stream2==1 & sim_pop$flu==0)
  n2<-sum(sim_pop$stream1==1 & sim_pop$stream2==1 & sim_pop$flu==1)
  n3<-sum(sim_pop$stream1==1 & sim_pop$stream2==0 & sim_pop$flu==0)
  n4<-sum(sim_pop$stream1==1 & sim_pop$stream2==0 & sim_pop$flu==1)
  n5<-sum(sim_pop$stream1==0 & sim_pop$stream2==1 & sim_pop$flu==0)
  n6<-sum(sim_pop$stream1==0 & sim_pop$stream2==1 & sim_pop$flu==1)
  n7<-sum(sim_pop$stream1==0 & sim_pop$stream2==0)
  
return(c(n1, n2, n3, n4, n5, n6, n7))
  
}


####MLEs based on 5 cell count table.
###Data consists of (n1+n5, n2, n4, n6, n3+n7)
###N_MLE_SE is Lin's estimated standard error with FPC corrections for the 5 cell case.
MLE_5<-function(data){
  
n15<-data[1]+data[5]

n2<-data[2]

n4<-data[4]

n6<-data[6]

n37<-data[3]+data[7]


#If a cell count is 0, I set it to 1 to avoid division by 0 problems when calculating the FPC corrected standard error.

if (n15==0){
  n15=1
}

if (n2==0){
  n2=1
}


if (n4==0){
  n4=1
}


if (n6==0){
  n6=1
}


if (n37==0){
  n37=1
}





n11 = n2; n10 = n4; n01 = n6

nc = n2+n4+n6

Ntot<-n15+n2+n4+n6+n37

theta = (n2+n4)/Ntot

psi_star = (n15+n6)/(n15+n6+n37)


pi_all = theta + (n6/psi_star)/Ntot


## N estimate and SE

N_MLE = Ntot*pi_all

var_part1 = (n11*n10)/((n11+n10)*Ntot*psi_star)*theta *0.5/(n11-0.5)

var_part2 = (n01*(N_MLE-nc)*n01/psi_star)/((N_MLE-n11-n10)*Ntot^2*psi_star) *0.5/(n01-0.5)

N_MLE_se = Ntot*sqrt(var_part1 + var_part2) # variance approximation



return(c(mle = N_MLE, mle_se = N_MLE_se))
}



###Function for conducting simulation study.  Returns the true number of flu patients, the mean estimated number based on the 
#5 cell MLE estimate, the standard deviation of the MLE estimates, and the mean of Lin's FPC corrected standard deviation estimate.


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
  
  N_hat<-mean(MLEs[,1]) #The mean of the MLE estimates
  sd<-sd(MLEs[,1])  ##Empirical standard deviation
  se<-mean(MLEs[,2]) ##Mean of Lin's FPC corrected standard error estimates.
  
  
  paste("Number of Flue Patients", sum(population$flu), "Mean Estimated Number of Flu Cases", N_hat, "Empirical standard deviation ="
        , sd, "FPC Corrected standard error=", se)
  
}


#############Important Functions


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


MLE_5(simulate_data(population, p1_symp, p1_nonsymp, p2))

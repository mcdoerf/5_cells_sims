

set.seed(2389)


####################Setting important parameters.

nsims<-1000  ##Number of simulations we want to run.
Ntot<-10000  ##Total size of the population
N_flu<-300  ##The number diseased

psymp_flu<-0.5 #Probability of being symptomatic given that the individual has the Flu.
psymp_healthy<-0.1 #Probability of being symptomatic given that the individual is healthy.

p1_symp<-0.25  #Probability of being selected into stream 1 (non-anchor stream) given symptomatic.
p1_nonsymp<-0.05 #Probability of being selected into stream 1 given non-symptomatic.

p2<-0.2 #Probability of being selected into stream 2 (anchor stream).



##################Generating populations.  We are going to consider 6 different scenarios: Nflu=50, Nflu=100, Nflu=300, Nflu=700, Nflu=1000, Nflu=5000

population1<-population_generator(Ntot=10000, N_flu=100, psymp_flu=psymp_flu, psymp_healthy=psymp_healthy)
population2<-population_generator(Ntot=10000, N_flu=250, psymp_flu=psymp_flu, psymp_healthy=psymp_healthy)
population3<-population_generator(Ntot=10000, N_flu=500, psymp_flu=psymp_flu, psymp_healthy=psymp_healthy)
population4<-population_generator(Ntot=10000, N_flu=1000, psymp_flu=psymp_flu, psymp_healthy=psymp_healthy)
population5<-population_generator(Ntot=10000, N_flu=2500, psymp_flu=psymp_flu, psymp_healthy=psymp_healthy)
population6<-population_generator(Ntot=10000, N_flu=5000, psymp_flu=psymp_flu, psymp_healthy=psymp_healthy)




#################Simulations.  We let p2 be 0.5, 0.1, 0.25, 0.5.
 ##Letting p2 vary.
sim_study(nsims=1000, population=population1, p1_symp=0.3, p1_nonsymp=0.1, p2=0.05)
sim_study(nsims=1000, population=population1, p1_symp=0.3, p1_nonsymp=0.1, p2=0.1)
sim_study(nsims=1000, population=population1, p1_symp=0.3, p1_nonsymp=0.1, p2=0.25)
sim_study(nsims=1000, population=population1, p1_symp=0.3, p1_nonsymp=0.1, p2=0.5)

  ##Letting p2 vary.
sim_study(nsims=1000, population=population2, p1_symp=0.3, p1_nonsymp=0.1, p2=0.05)
sim_study(nsims=1000, population=population2, p1_symp=0.3, p1_nonsymp=0.1, p2=0.1)
sim_study(nsims=1000, population=population2, p1_symp=0.3, p1_nonsymp=0.1, p2=0.25)
sim_study(nsims=1000, population=population2, p1_symp=0.3, p1_nonsymp=0.1, p2=0.5)


 ##Letting p2 vary.
sim_study(nsims=1000, population=population3, p1_symp=0.3, p1_nonsymp=0.1, p2=0.05)
sim_study(nsims=1000, population=population3, p1_symp=0.3, p1_nonsymp=0.1, p2=0.1)
sim_study(nsims=1000, population=population3, p1_symp=0.3, p1_nonsymp=0.1, p2=0.25)
sim_study(nsims=1000, population=population3, p1_symp=0.3, p1_nonsymp=0.1, p2=0.5)


  ##Letting p2 vary.
sim_study(nsims=1000, population=population4, p1_symp=0.3, p1_nonsymp=0.1, p2=0.05)
sim_study(nsims=1000, population=population4, p1_symp=0.3, p1_nonsymp=0.1, p2=0.1)
sim_study(nsims=1000, population=population4, p1_symp=0.3, p1_nonsymp=0.1, p2=0.25)
sim_study(nsims=1000, population=population4, p1_symp=0.3, p1_nonsymp=0.1, p2=0.5)


 ##Letting p2 vary.
sim_study(nsims=1000, population=population5, p1_symp=0.3, p1_nonsymp=0.1, p2=0.05)
sim_study(nsims=1000, population=population5, p1_symp=0.3, p1_nonsymp=0.1, p2=0.1)
sim_study(nsims=1000, population=population5, p1_symp=0.3, p1_nonsymp=0.1, p2=0.25)
sim_study(nsims=1000, population=population5, p1_symp=0.3, p1_nonsymp=0.1, p2=0.5)


  ##Letting p2 vary.
sim_study(nsims=1000, population=population6, p1_symp=0.3, p1_nonsymp=0.1, p2=0.05)
sim_study(nsims=1000, population=population6, p1_symp=0.3, p1_nonsymp=0.1, p2=0.1)
sim_study(nsims=1000, population=population6, p1_symp=0.3, p1_nonsymp=0.1, p2=0.25)
sim_study(nsims=1000, population=population6, p1_symp=0.3, p1_nonsymp=0.1, p2=0.5)




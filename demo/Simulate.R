require(distr)

sim <- new("Simulation",
           seed = setRNG(),
           distribution = Norm(mean = 0, sd = 1),
           filename="sim_01",
           runs = 1000,
           samplesize = 30)
#generate an object of class Simulation
#(ideal) situation:  x_i~i.i.d. N(0,1) 

contsim <- new("Contsimulation",
               seed = setRNG(),
               distribution.id = Norm(mean = 0, sd = 1),
               distribution.c = Norm(mean = 0, sd = 9),
               rate = 0.1,
               filename="contsim_01",
               runs = 1000,
               samplesize = 30)

#generate an object of class Contsimulation

#fill the data-slots
simulate(sim)
simulate(contsim)



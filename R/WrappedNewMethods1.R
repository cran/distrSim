Simulation <- function(filename = NULL, runs = 100, samplesize = 10, seed = setRNG(), distribution = Norm())
  new("Simulation", filename = filename, runs = runs, samplesize = samplesize, seed = seed, distribution = distribution)

Contsimulation <- function(filename = NULL, runs = 100, samplesize = 10, seed = setRNG(), distribution.id = Norm(), 
                  distribution.c = Norm(sd =3), rate = 0.1)
  new("Contsimulation",filename = filename, runs = runs, samplesize = samplesize, seed = seed,
      distribution.id = distribution.id, distribution.c = distribution.c, rate)

Dataclass <- function(filename = NULL, Data) new("Dataclass", filename = filename, Data = Data)

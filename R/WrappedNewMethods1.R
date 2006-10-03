Simulation <- function(filename = NULL, samplesize = 10, runs = 100,  seed = setRNG(), distribution = Norm())
  new("Simulation", filename = filename, runs = runs, samplesize = samplesize, seed = seed, distribution = distribution)

Contsimulation <- function(filename = NULL, samplesize = 10, runs = 100, seed = setRNG(), distribution.id = Norm(), 
                  distribution.c = Norm(sd =3), rate = 0.1)
  new("Contsimulation",filename = filename, runs = runs, samplesize = samplesize, seed = seed,
      distribution.id = distribution.id, distribution.c = distribution.c, rate)

Dataclass <- function(filename = NULL, Data, name = "Data-Set") 
{if(!is(Data,"array") & !is(Data,"vector"))
    stop("generating an object of class \"Dataclasss\" requires data of type \"array\" or \"vector\"")

 runs0 <- 1
 obsDim0 <- 1
 dimnames0 <- NULL
 rnames <- NULL
 dnames <- NULL
 snames <- NULL
 if(is(Data,"array") & !is.na(dim(Data)[3]))
    { runs0 <- dim(Data)[3]   
      if(!is.null(dimnames(Data))) 
          rnames <- dimnames(Data)[[3]]
    } 
 if(!is.null(dim(Data)))
    { obsDim0 <- dim(Data)[2]   
      samplesize0 <- dim(Data)[1] 
      if(!is.null(dimnames(Data))) 
          {dnames <- dimnames(Data)[[2]]
           snames <- dimnames(Data)[[1]]
           dimnames0 <- list(snames,dnames,rnames)
          }
    }   
 else
    { samplesize0 <- length(Data)
      if(!is.null(names(Data)))  
          {snames <- names(Data)
           dimnames0 <- list(snames,dnames,rnames)
           }
    }
      
 Data0 <- array(data=Data, dim=c(samplesize0,obsDim0,runs0),dimnames=dimnames0)       
 new("Dataclass", filename = filename, Data = Data0, runs = runs0, 
     obsDim = obsDim0, samplesize = samplesize0, name = name)
}

 
 
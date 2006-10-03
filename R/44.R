################################
##
## Class: Contsimulation
##
################################

setClass("Contsimulation",
         representation("Dataclass",
                        ind = "MatrixorNULLorVector",
                        rate = "numeric",
 ##new 03-10-06:
                        Data.id = "ArrayorNULLorVector",
                        Data.c =  "ArrayorNULLorVector",
                        distribution.c = "Distribution",
                        distribution.id = "Distribution",
 ###old:        Data.id = "vectororNULL",
 ###old:        Data.c = "vectororNULL",
 ###old:        distribution.c = "UnivariateDistribution"
 ###old:        distribution.id = "UnivariateDistribution"
                        seed = "list"),
         contains = "Dataclass")            


setMethod("initialize", "Contsimulation",
          function(.Object,
                   filename = NULL,
                   runs = 100,
                   samplesize = 10,
                   seed = setRNG(),
                   distribution.id = Norm(),
                   distribution.c = Norm(sd = 3),
                   rate = 0.1) {
            .Object@Data <- NULL
            .Object@Data.id <- NULL
            .Object@Data.c <- NULL
### new 031006:            
            .Object@version <- "1.8"
            .Object@obsDim <- dim(distribution.id)
###
            .Object@filename <- filename
            .Object@runs <- runs
            .Object@samplesize <- samplesize        
            .Object@seed <- seed        
            .Object@distribution.id <- distribution.id
            .Object@distribution.c <- distribution.c
            .Object@rate <- rate
            
            validObject(.Object)
            .Object
          })

## Access methods
if(!isGeneric("ind")) setGeneric("ind", function(object) standardGeneric("ind"))
if(!isGeneric("Data.id")) setGeneric("Data.id", function(object) standardGeneric("Data.id"))
if(!isGeneric("Data.c")) setGeneric("Data.c", function(object) standardGeneric("Data.c"))
if(!isGeneric("rate")) setGeneric("rate", function(object) standardGeneric("rate"))
if(!isGeneric("distribution.c")) setGeneric("distribution.c", function(object) standardGeneric("distribution.c"))
if(!isGeneric("distribution.id")) setGeneric("distribution.id", function(object) standardGeneric("distribution.id"))
if(!isGeneric("seed")) setGeneric("seed", function(object) standardGeneric("seed"))
setMethod("ind", "Contsimulation", function(object) object@ind)
setMethod("Data.id", "Contsimulation", function(object) object@Data.id)
setMethod("Data.c", "Contsimulation", function(object) object@Data.c)
setMethod("rate", "Contsimulation", function(object) object@rate)
setMethod("distribution.c", "Contsimulation", function(object) object@distribution.c)
setMethod("distribution.id", "Contsimulation", function(object) object@distribution.id)
setMethod("seed", "Contsimulation", function(object) object@seed)
## Replace Methoden
if(!isGeneric("rate<-")) setGeneric("rate<-", function(object, value) standardGeneric("rate<-"))
if(!isGeneric("distribution.c<-")) setGeneric("distribution.c<-", function(object, value) standardGeneric("distribution.c<-"))
if(!isGeneric("distribution.id<-")) setGeneric("distribution.id<-", function(object, value) standardGeneric("distribution.id<-"))
if(!isGeneric("seed<-")) setGeneric("seed<-", function(object, value) standardGeneric("seed<-"))
setReplaceMethod("rate", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = distribution.id(object),
                                 distribution.c = distribution.c(object),
                                 rate = value,
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("distribution.c", "Contsimulation",
                 function(object, value){
                   if(all(identical(dim(distribution.id(object)),dim(value))))
                              d.id <- distribution.id(object)
                   else       d.id <- value
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = d.id,
                                 distribution.c = value,
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("distribution.id", "Contsimulation",
                 function(object, value){
                   if(all(identical(dim(distribution.c(object)),dim(value))))
                              d.c <- distribution.c(object)
                   else       d.c <- value
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = value,
                                 distribution.c = d.c,
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("seed", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = value,
                                 distribution.id = distribution.id(object),
                                 distribution.c = distribution.c(object),
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = samplesize(object))                                              
                   object})
setReplaceMethod("runs", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = distribution.id(object),
                                 distribution.c = distribution.c(object),
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = value,
                                 samplesize = samplesize(object))                                              
                   object})
setReplaceMethod("samplesize", "Contsimulation",
                 function(object, value){
                   object <- new("Contsimulation",
                                 seed = seed(object),
                                 distribution.id = distribution.id(object),
                                 distribution.c = distribution.c(object),
                                 rate = rate(object),
                                 filename = filename(object),
                                 runs = runs(object),
                                 samplesize = value)
                   object
                 })

setReplaceMethod("Data", "Contsimulation",
                 function(object, value){
                   stop("This slot should not be altered")
                   object
                 })

validContsimulation <- function(object){
  if(!identical(floor(samplesize(object)), samplesize(object)))
    stop("samplesize has to be a positive integer")      
  if(samplesize(object) <= 0)
    stop("samplesize has to be a positive integer")
  if(!identical(floor(runs(object)), runs(object)))
    stop("runs has to be a positive integer")      
  if(runs(object) <= 0)
    stop("runs has to be a positive integer")      
  if(rate(object) < 0)
    stop("rate has to be in [0,1]")
  if(rate(object) > 1)
    stop("rate has to be in [0,1]")
  if(dim(distribution.id(object))!=dim(distribution.c(object)))
    stop("Dimensions of ideal and contaminated distribution must coincide")
  else return(TRUE)
}

setValidity("Contsimulation", validContsimulation)




## Save method
setMethod("savedata", "Contsimulation", function(object,...){
  if(is.null(filename(object))) stop("This simulation has to be given a filename before it can be saved to harddisk")
  
  name <- as.character(substitute(object))
  
  eval.parent(parse(text=paste(name,"@Data <- NULL",sep=""))) 
  eval.parent(parse(text=paste(name,"@Data.id <- NULL",sep=""))) 
  eval.parent(parse(text=paste(name,"@Data.c <- NULL",sep="")))
  eval.parent(parse(text=paste(name,"@ind <- NULL",sep="")))     
  eval.parent(substitute(save(object, file = filename(object))))
})

## Simulation method

setMethod("simulate",signature(object="Contsimulation"),
          function(object, nsim=-1, seed=-1, ...){
            if(!is.null(Data(object)))
              return(invisible())
            if(!(seed==-1))
                stop("Seed of an object of class Simulation is changed by the replacement method seed(<object>,<value>)!")
            if(!(nsim==-1))
                stop("Sample size of an object of class Simulation is changed by the replacement method samplesize(<object>,<value>)!")

            setRNG(seed(object)) 

###new:031006:
            data.raw <- r(distribution.id(object))(object@runs*object@samplesize)

            Data.id.raw <- aperm(array( t(data.raw),
                                    c(object@obsDim, object@samplesize, object@runs)
                                   ), perm = c(2,1,3))

            data.raw <- r(distribution.c(object))(object@runs*object@samplesize)
            Data.c.raw <- aperm(array( t(data.raw),
                                   c(object@obsDim, object@samplesize, object@runs)
                                  ), perm = c(2,1,3))
            Ind.raw <- matrix(rbinom(object@runs*object@samplesize,1,object@rate),
                               object@samplesize,object@runs)
            
            Indx <- array(Ind.raw,c(samplesize(object),runs(object),obsDim(object)))
            x.id <- aperm(aperm(Data.id.raw, perm = c(1,3,2))*(1-Indx), perm = c(1,3,2))
            x.c  <- aperm(aperm(Data.c.raw,  perm = c(1,3,2))* Indx,    perm = c(1,3,2))

            Data.raw <- x.id + x.c
            eval.parent(substitute(object@Data.id <- Data.id.raw)) 
            eval.parent(substitute(object@Data.c <- Data.c.raw))
            eval.parent(substitute(object@ind <- Ind.raw))
            eval.parent(substitute(object@Data <- Data.raw))
### old:
###            eval.parent(substitute(object@Data.id <- matrix(r(distribution.id(object))(object@runs*object@samplesize),
###                                                     object@runs,object@samplesize))) 
###            eval.parent(substitute(object@Data.c <- matrix(r(distribution.c(object))(object@runs*object@samplesize),
###                                                     object@runs,object@samplesize))) 
###            eval.parent(substitute(object@ind <- matrix(rbinom(object@runs*object@samplesize,1,object@rate),
###                                                     object@runs,object@samplesize)))
###            eval.parent(substitute(object@Data <- (1-object@ind)*object@Data.id+object@ind*object@Data.c))
            
            return(invisible())
          })


###Plot

setMethod("plot","Contsimulation", 
                    function(x,y=NULL, obs0=1:samplesize(x), dims0=1:obsDim(x), runs0=1:runs(x), ...){

            dots <- list(...)
            if(is.null(Data(x)))
               stop("No Data found -> simulate first")
            
            if(any(Data(x) == 0)) return("Warning: plot won't work properly")
            
            
            lobs0 <- min(getdistrSimOption("MaxNumberofPlottedObs"), length(obs0))           
            lrun0 <- min(getdistrSimOption("MaxNumberofPlottedRuns"), length(runs0))           
            ldim0 <- min(getdistrSimOption("MaxNumberofPlottedObsDims"), length(dims0))           
            if((lrun0<length(runs0))|(ldim0<length(dims0))|(lobs0<length(obs0)))   
                warning(paste("your data set is too big; only ", lobs0,  "x", ldim0, 
                               "x", lrun0, "observations x dimensions x runs are plotted"))

            x.id <- array(aperm(aperm(Data(x),c(1,3,2))*array(1-ind(x),c(samplesize(x),runs(x),obsDim(x))),c(1,3,2)),
                           c(lobs0,ldim0,lrun0))
            x.id[x.id == 0] <- Inf
            
            x.c <-  array(aperm(aperm(Data(x),c(1,3,2))*array(ind(x),c(samplesize(x),runs(x),obsDim(x))),c(1,3,2)),
                           c(lobs0,ldim0,lrun0))
            x.c[x.c == 0] <- Inf
            
      #      get(getOption("device"))()
            oldwarn <- getOption("warn")
            oldpar <- par()$mfrow
            options("warn" = -1)
            par(mfrow=c(1,lrun0))
            
            y0<-1:lobs0
            dots[["x"]] <- y0

            #if(lrun0==1)
            #   {matplot(y0,x.id[,dims0[1:ldim0]],ylim = 2*range(Data.id(x)),x 
            #            xlab=gettextf("observation-index"),ylab=gettextf("data"),type="p",cex=1.3,pch="*", 
            #            col=(colors()[grep("blue",colors())])[65:1]
            #            )
            #
            #    if(any(x.c != Inf)) 
            #        matpoints(y0,x.c[,dims0[1:ldim0]], type="p",pch="x",cex=0.8,col=colors()[grep("red",colors())])
            #   }        
            #else
            #  {            
            
            ## catch ylims given in ...
            ylim0<-matrix(2*range(Data.id(x)),2,lrun0)
            ##  wylim <- FALSE ### is ylim specified? changed: ylim has to be set by default...
            if("ylim" %in% names(dots)) 
                { wylim <- TRUE
                  oldwarn <- getOption("warn"); options("warn" = -1)
                  ylim1 <- as.matrix(dots[["ylim"]])
                  c1 <- ncol(ylim1); c2 <- ldim0%/%c1; c3 <- ldim0%%c1
                  if(c2>0)
                     ylim0[,1:(c2*c1)] <- ylim1
                  if(c3>0)
                     ylim0[,c2*c1+(1:c3)]<- ylim1[,1:c3]
                  options("warn" = oldwarn) }  
            
            dots["xlab"] <- gettextf("observation-index")
            dots["ylab"] <- gettextf("data")
            dots["type"] <- "p"
            
            cex.id0 <- rep(1.3,ldim0,length=ldim0) 
            if("cex.id" %in% names(dots) )
                cex.id0 <- rep(unlist(dots["cex.id"]),ldim0,length=ldim0) 
            
            cex.c0 <- rep(0.8,ldim0,length=ldim0) 
            if("cex.c" %in% names(dots) )
                cex.c0 <- rep(unlist(dots["cex.c"]),ldim0,length=ldim0) 

            pch.id0 <- rep("*",ldim0,length=ldim0) 
            if("pch.id" %in% names(dots) )
                pch.id0 <- rep(unlist(dots["pch.id"]),ldim0,length=ldim0) 

            pch.c0 <- rep("x",ldim0,length=ldim0) 
            if("pch.c" %in% names(dots) )
                pch.c0 <- rep(unlist(dots["pch.c"]),ldim0,length=ldim0) 

            col.id0 <- rep((colors()[grep("blue",colors())])[65:1],ldim0,length=ldim0) 
            if("col.id" %in% names(dots))
                col.id0 <- rep(unlist(dots["col.id"]),ldim0,length=ldim0) 
            
            col.c0 <- rep((colors()[grep("red",colors())]),ldim0,length=ldim0) 
            if("col.c" %in% names(dots))
                col.c0 <- rep(unlist(dots["col.c"]),ldim0,length=ldim0) 


            for( i in 1: lrun0)
                   { ### if(wylim) 
                     dots[["ylim"]] <- ylim0[,i]
                     dots[["y"]] <- x.id[,dims0[1:ldim0],runs0[i]]
                     dots[["cex"]] <- cex.id0
                     dots[["pch"]] <- pch.id0
                     dots[["col"]] <- col.id0
                     do.call("matplot", args=dots)
                   
                    if(any(x.c[,dims0[1:ldim0],runs0[i]] != Inf)) 
                       { dots[["cex"]] <- cex.c0
                         dots[["pch"]] <- pch.c0
                         dots[["col"]] <- col.c0
                         dots[["y"]] <- x.c[,dims0[1:ldim0],runs0[i]]
                         do.call("matpoints", args=dots)                                              
                       }   
                   }                  
            #   }        
            
            par(mfrow=oldpar)
            options("warn" = oldwarn)
          })

###summary

setMethod("summary","Contsimulation",
          function(object,...){
            if(is.null(Data(object)))
              stop("No Data found -> simulate first")
            
            cat(gettextf("name of simulation: %s\n",filename(object)))
            cat(gettextf("rate of contamination: %f\n",rate(object)))
            cat(gettextf("real Data:\n"))
            summary(as(object,"Dataclass"), dims0=1:obsDim(object), runs0=1:runs(object), ..., NOT.A.SIMULATION=FALSE)            
          })

###print

setMethod("print","Contsimulation",
          function(x,...){
            cat(gettextf("filename of Simulation: %s\n",filename(x)))
            print(as(x,"Dataclass"), ..., NOT.A.SIMULATION=FALSE)                        
            cat(gettextf("rate of contamination: %f\n",rate(x)))
            
            distr.id <-distribution.id(x)
            distr.c <- distribution.c(x)            

            cat(gettextf("ideal distribution:\n"))
            print(distr.id)
            cat(gettextf("contaminating distribution:\n"))
            print(distr.c)
          })


setMethod("show", "Contsimulation",
          function(object)print(object))


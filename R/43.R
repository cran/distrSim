################################
##
## Class: Simulation
##
################################

### changed from version 1.8 on:
## ith observation in ith line of datamatrix/array
## jth item/dimension of each observation in jth column of datamatrix/array
## kth run/time of each observation in kth slide of datamatrix/array

## ++old
## +ith run in ith line of datamatrix
## +jth samples of each run in jth column of datamatrix


setClass("Simulation",
         representation("Dataclass",
                        seed = "list",
 ##new 03-10-06:
                        distribution = "Distribution"
 ###old:        distribution = "UnivariateDistribution"
         ),
         contains="Dataclass")

setMethod("initialize", "Simulation",
          function(.Object, filename = NULL, runs = 100, samplesize = 10, seed = setRNG(), distribution = Norm()) {
            .Object@filename <- filename
            .Object@Data <- NULL
            .Object@runs <- runs
 ##new 03-10-06:
            .Object@version <- "1.8"
            .Object@obsDim <- dim(distribution)
 ##end (new)
            .Object@samplesize <- samplesize        
            .Object@seed <- seed        
            .Object@distribution <- distribution        
            validObject(.Object)
            .Object
          })



## Access Methods
if(!isGeneric("seed")) setGeneric("seed", function(object) standardGeneric("seed"))
if(!isGeneric("distribution")) setGeneric("distribution", function(object) standardGeneric("distribution"))
setMethod("seed", "Simulation", function(object) object@seed)
setMethod("distribution", "Simulation", function(object) object@distribution)
## Replace Methoden
if(!isGeneric("seed<-")) setGeneric("seed<-", function(object, value) standardGeneric("seed<-"))
if(!isGeneric("distribution<-")) setGeneric("distribution<-", function(object, value) standardGeneric("distribution<-"))
setReplaceMethod("distribution", "Simulation",
                 function(object, value){
                   object <- new("Simulation",
                     seed = seed(object),
                     distribution = value,
                     filename = filename(object),
                     obsDim = dim(value),
                     runs = runs(object),
                     samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("seed", "Simulation",
                 function(object, value){
                   object <- new("Simulation",
                     seed = value,
                     distribution = distribution(object),
                     filename = filename(object),
                     runs = runs(object),
                     samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("runs", "Simulation",
                 function(object, value){
                   object <- new("Simulation",
                     seed = seed(object),
                     distribution = distribution(object),
                     filename = filename(object),
                     runs = value,
                     samplesize = samplesize(object))                                              
                   object
                 })
setReplaceMethod("samplesize", "Simulation",
                 function(object, value){
                   object <- new("Simulation",
                     seed = seed(object),
                     distribution = distribution(object),
                     filename = filename(object),
                     runs = runs(object),
                     samplesize = value)
                   object
                 })

setReplaceMethod("Data", "Simulation", function(object, value){ stop("This slot should not be altered"); object})



validSimulation <- function(object){
  if(!identical(floor(samplesize(object)), samplesize(object)))
    stop("samplesize has to be a positive integer")      
  if(samplesize(object) <= 0)
    stop("samplesize has to be a positive integer")
  if(!identical(floor(runs(object)), runs(object)))
    stop("runs has to be a positive integer")      
  if(runs(object) <= 0)
    stop("runs has to be a positive integer")      
  else return(TRUE)
}

setValidity("Simulation", validSimulation)


## Simulation method
if(!isGeneric("simulate")) setGeneric("simulate",function(object, nsim=-1, seed=-1, ...)standardGeneric("simulate"))
#if(!isGeneric("simulate")) setGeneric("simulate",function(object, ...)standardGeneric("simulate"))

#### changed for compatibility with stats 04-10-05 P.R.
setMethod("simulate", signature(object = "Simulation"),
          function(object, nsim=-1, seed=-1, ...){
#           function(object, ...){
            if(!is.null(Data(object)))
              return(invisible())
            if(!(seed==-1))
                stop("Seed of an object of class Simulation is changed by the replacement method seed(<object>,<value>)!")
            if(!(nsim==-1))
                stop("Sample size of an object of class Simulation is changed by the replacement method samplesize(<object>,<value>)!")
            setRNG(seed(object)) 
###new:031006:
                        ### later: something like: if(!is(distribution(object), "TSDistribution") {...}
                        ####                       else 'call r(.) with two arguments...'
            Data0<-aperm(array( t(r(distribution(object))(object@runs*object@samplesize)),
                                                  c(object@obsDim, object@samplesize, object@runs)), 
                         perm=c(2,1,3))
            eval.parent(substitute(object@Data<-Data0)) 

###old:            eval.parent(substitute(object@Data<-matrix(r(distribution(object))(object@runs*object@samplesize),
###                        nrow=object@runs,ncol=object@samplesize))) 
            return(invisible())
          })

setMethod("savedata", "Simulation", function(object,...){
  if(is.null(filename(object))) stop("This simulation has to be given a filename before it can be saved to harddisk")
  
  name <- as.character(substitute(object))
  
  eval.parent(parse(text=paste(name,"@Data <- NULL",sep=""))) 
  eval.parent(substitute(save(object, file = filename(object))))
})

##setMethod("plot","Simulation",
##          function(x,y=NULL,...){
##            if(is.null(Data(x)))
##             stop("No Data found -> simulate first")
##            
##            y0<-1:runs(x)
##            matplot(y0,Data(x),xlab="run-index",ylab="data",type="p",pch="*",col="blue")
##          })

## changed w.r.t <1.8            

setMethod("plot","Simulation", 
                    function(x,y=NULL, obs0=1:samplesize(x), dims0=1:obsDim(x), runs0=1:runs(x), ...){

            if(is.null(Data(x)))
               stop("No Data found -> simulate first")
  
           plot(as(x,"Dataclass"),y=NULL, obs0=obs0, dims0=dims0, runs0=runs0, ...)            
          })



setMethod("summary","Simulation",
          function(object,...){
            if(is.null(Data(object)))
              stop("No Data found -> simulate first")
            
            cat(gettextf("filename of simulation: %s\n",filename(object)))
            summary(as(object,"Dataclass"), dims0=1:obsDim(object), runs0=1:runs(object), ..., NOT.A.SIMULATION=FALSE)            
          })


setMethod("print","Simulation",
          function(x,...){
            if(!is.null(filename(x))) cat(gettextf("filename of Simulation: %s\n",filename(x)))
            cat(gettextf("seed of Simulation: %s\n",seed(x)))
            print(as(x,"Dataclass"), ..., NOT.A.SIMULATION=FALSE)                        
            cat(gettextf("Distribution:\n"))
            print(distribution(x))
          })

setMethod("show", "Simulation",
          function(object)print(object))


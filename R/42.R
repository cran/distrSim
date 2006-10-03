#########
## about Dataclass: slot Data
##
### changed from version 1.8 on:
## ith observation in ith line of datamatrix/array
## jth item/dimension of each observation in jth column of datamatrix/array
## kth run/time of each observation in kth slide of datamatrix/array

## ++old
## +ith run in ith line of datamatrix
## +jth samples of each run in jth column of datamatrix


################################
##
## Class: Dataclass
##
################################

setClassUnion("ArrayorNULLorVector",c("array", "NULL","vector"))
setClassUnion("MatrixorNULLorVector",c("matrix", "NULL","vector"))

setClass("Dataclass",
         representation(filename = "vectororNULL",
#old:                        Data = "vectororNULL",
                        Data = "ArrayorNULLorVector",
                        obsDim ="numeric",   ### new v.1.8
                        runs = "numeric",
                        samplesize = "numeric",
                        name = "character", ### new v.1.8
                        version = "character" ### new v.1.8
                        ),
         prototype=list(filename = "Data-set", Data = NULL, 
                        runs = 1, samplesize = 1, 
                        obsDim=1, version="1.8", name="Data-Set"))

## Access methods
if(!isGeneric("filename")) setGeneric("filename", function(object) standardGeneric("filename"))
if(!isGeneric("Data")) setGeneric("Data", function(object) standardGeneric("Data"))
if(!isGeneric("runs")) setGeneric("runs", function(object) standardGeneric("runs"))
if(!isGeneric("samplesize")) setGeneric("samplesize", function(object) standardGeneric("samplesize")) 
if(!isGeneric("obsDim")) setGeneric("obsDim", function(object) standardGeneric("obsDim")) ### new v.1.8
if(!isGeneric("name")) setGeneric("name", function(object) standardGeneric("name"))### new v.1.8
if(!isGeneric("getVersion")) setGeneric("getVersion", function(object) standardGeneric("getVersion"))### new v.1.8

setMethod("filename", "Dataclass", function(object) object@filename)
setMethod("Data", "Dataclass", function(object) object@Data)
setMethod("runs", "Dataclass", function(object) object@runs)
setMethod("samplesize", "Dataclass", function(object) object@samplesize)
setMethod("obsDim", "Dataclass", function(object) object@obsDim)### new v.1.8
setMethod("name", "Dataclass", function(object) object@name)### new v.1.8
setMethod("getVersion", "Dataclass", function(object) object@version)### new v.1.8

## Replacement methods
if(!isGeneric("filename<-")) setGeneric("filename<-", function(object, value) standardGeneric("filename<-"))
if(!isGeneric("Data<-")) setGeneric("Data<-", function(object, value) standardGeneric("Data<-"))
if(!isGeneric("name<-")) setGeneric("name<-", function(object, value) standardGeneric("name<-")) ### new v.1.8

### not be set for Dataclass itself without generating a new object:

if(!isGeneric("runs<-")) setGeneric("runs<-", function(object, value) standardGeneric("runs<-"))
if(!isGeneric("samplesize<-")) setGeneric("samplesize<-", function(object, value) standardGeneric("samplesize<-"))


setReplaceMethod("name", "Dataclass", function(object, value){ object@name <- value; object}) ### new 1.8
setReplaceMethod("filename", "Dataclass", function(object, value){ object@filename <- value; object})
setReplaceMethod("Data", "Dataclass",
                 function(object, value){
                   datdim <- dim(value)
                   object <- new("Dataclass",
                                 filename = filename(object),
                                 Data = value, samplesize=datdim[1],obsDim=datdim[2],runs=datdim[3])
                   object
                 })

###produces difficulties in coercing...:

## Initialize methods
#setMethod("initialize", "Dataclass",
#           function(.Object, filename = NULL, Data=.Object@Data, runs=1, obsDim=1) {
#            .Object@filename <- filename
#            .Object@Data <- Data
###changed in 1.8:
#            .Object@version <- "1.8"
#            .Object@runs <- 1
#            .Object@obsDim <- ncol(as.matrix(Data))
#            .Object@samplesize <- nrow(as.matrix(Data))
### old:
#            .Object@runs <- nrow(as.matrix(Data))
#            .Object@samplesize <- ncol(as.matrix(Data))
#            .Object
#          })

### instead generating function ...

### specific generation methods for #runs>1 from version 1.9 on

### simple version management

if(!isGeneric("isOldVersion")) setGeneric("isOldVersion", function(object) standardGeneric("isOldVersion"))
setMethod("isOldVersion", "Dataclass", function(object) {
           if(is(try(slot(object,"version"),silent=TRUE),"try-error"))
              { mywarning <- function(){paste("Object \"", myobject, "\" was defined under a deprecated version of ",
                                        "class \"DataClass\" (or subclasses). Try conv2NewVersion()...", sep="", collapse="")}
                body(mywarning) <- substitute({paste("Object \"", myobject, "\" was defined under a deprecated version of ",
                                        "class \"DataClass\" (or subclasses). Try conv2NewVersion()...", sep="", collapse="")},
                                        list(myobject = object))
                warning(mywarning) 
                return(TRUE)
                }                        
            else return(FALSE)     })

if(!isGeneric("conv2NewVersion")) setGeneric("conv2NewVersion", function(object) standardGeneric("conv2NewVersion"))

setMethod("conv2NewVersion", "Dataclass", function(object) {
           myobj <- new("Dataclass", Data=aperm(array(object@Data,c(object@runs,object@samplesize,1)),perm=c(2,3,1)), 
                         filename=object@filename)
           eval.parent(substitute(object<- myobj)) 
           })

## Save method
if(!isGeneric("savedata")) setGeneric("savedata", function(object,...) standardGeneric("savedata"))

setMethod("savedata", "Dataclass", function(object,...){

  name0 <- as.character(match.call(call=sys.call(-1))$object)

  if(is.null(filename(object))) stop("This Dataclass object has to be given a filename before it can be saved to harddisk")
            
  eval.parent(parse(text=paste("save(", name0,", file = \"",filename(object),"\")",sep="")))
  
  namecomment <- paste(name0,".comment",sep="")
  filenamecomment <- paste(filename(object),".comment",sep="")
  
  eval.parent(parse(text=paste(namecomment," <- ",name0,sep=""))) 
  eval.parent(parse(text=paste(namecomment,"@Data <- NULL",sep=""))) 
  eval.parent(parse(text=paste("save(",namecomment,", file = \"",filenamecomment,"\")",sep=""))) 
  eval.parent(parse(text=paste("rm(",namecomment,")",sep="")))
})

## Load Method for comments
cload <- function(filename){
  eval.parent(parse(text=paste("load(\"",filename,".comment\")", sep = "")))
}


## plot method

## changed w.r.t <1.8            
##setMethod("plot","Dataclass",
##          function(x,y=NULL,...){
##old:             y0<-1:runs(x)
##                 matplot(y0,Data(x),xlab="Runindex",ylab="data",type="p",pch="*",col="blue")
##          })

setMethod("plot","Dataclass", 
          function(x,y=NULL, obs0=1:samplesize(x), dims0=1:obsDim(x), runs0=1:runs(x), ...){

            dots <- list(...)

            lobs0 <- min(getdistrSimOption("MaxNumberofPlottedObs"), length(obs0))           
            lrun0 <- min(getdistrSimOption("MaxNumberofPlottedRuns"), length(runs0))           
            ldim0 <- min(getdistrSimOption("MaxNumberofPlottedObsDims"), length(dims0))           
            if((lrun0<length(runs0))|(ldim0<length(dims0))|(lobs0<length(obs0)))   
                warning(paste("your data set is too big; only ", lobs0,  "x", ldim0, 
                               "x", lrun0, "observations x dimensions x runs are plotted"))

#            get(getOption("device"))()
            oldwarn <- getOption("warn")
            oldpar <- par()$mfrow
            options("warn" = -1)
            par(mfrow=c(1,lrun0))
            
            y0<-1:length(obs0)
            dots[["x"]] <- y0
#            if(lrun0==1)
#               matplot(y0,Data(x)[,dims0[1:ldim0]],
#                       xlab=gettext("observation-index"),ylab=gettext("data"),type="p",pch="*")
#            else
#              {

            wylim <- FALSE ## with ylim-Argument
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
            
            cex0 <- rep(1.3,ldim0,length=ldim0) 
            if("cex" %in% names(dots) ) 
                cex0 <- rep(unlist(dots["cex"]),ldim0,length=ldim0) 
            
            pch0 <- rep("*",ldim0,length=ldim0) 
            if("pch" %in% names(dots) ) 
                pch0 <- rep(unlist(dots["pch"]),ldim0,length=ldim0) 

            col0 <- rep((colors()[grep("blue",colors())])[65:1],ldim0,length=ldim0) 
            if("col" %in% names(dots) )
                col0 <- rep(unlist(dots["col"]),ldim0,length=ldim0) 

            dots[["cex"]] <- cex0
            dots[["pch"]] <- pch0
            dots[["col"]] <- col0
            
            for( i in 1: lrun0)
                   { if (wylim) dots[["ylim"]] <- ylim0[,i]
                     dots[["y"]] <- x[,dims0[1:ldim0],runs0[i]]
                     
                     do.call("matplot", args=dots)
                   
                    }                  
            #   }        

            
            par(mfrow=oldpar)
            options("warn" = oldwarn)
            
          })

##summary

setMethod("summary","Dataclass",
          function(object, dims0=1:obsDim(object), runs0=1:runs(object), ..., NOT.A.SIMULATION=TRUE){
            if(NOT.A.SIMULATION) 
               {cat(gettextf("name of Dataclass: %s\n",name(object)))
                cat(gettextf("filename of Dataclass: %s\n",filename(object)))}
            cat(gettextf("dimension of the observations: %d\n",obsDim(object)))
            cat(gettextf("number of runs: %d\n",runs(object)))
            cat(gettextf("size of sample: %d\n",samplesize(object)))
            ISOLD <- isOldVersion(object)
            if(is.null(Data(object))) simulate(object)
 
            lrun0=min(getdistrSimOption("MaxNumberofSummarizedRuns"), length(runs0))           
            ldim0=min(getdistrSimOption("MaxNumberofSummarizedObsDims"), length(dims0))           
            
            z0<-runs0[1:lrun0]
            y0<-dims0[1:ldim0]
            x0<-Data(object)[,y0,z0,drop=FALSE]
            apply(x0,c(2,3),summary)
          })


setMethod("print", "Dataclass",
          function(x, ..., NOT.A.SIMULATION=TRUE){
            if(NOT.A.SIMULATION) 
               {cat(gettextf("name of Dataclass: %s\n",name(x)))
                cat(gettextf("filename of Dataclass: %s\n",filename(x)))
               }
            cat(gettextf("number of runs: %d\n",runs(x)))
            cat(gettextf("dimension of the observations: %d\n",obsDim(x)))
            cat(gettextf("size of sample: %d\n",samplesize(x)))
            if(!isOldVersion(x))
                cat(gettextf("object was generated by version: %s\n",getVersion(x)))
          })

setMethod("show", "Dataclass",
          function(object)print(object))

.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE) 
    require(setRNG)
}

setClassUnion("vectororNULL", c("vector","NULL"))

.distrSimoptions <- list(
                      MaxNumberofPlottedObs = 4000,
                      MaxNumberofPlottedObsDims = 6,
                      MaxNumberofPlottedRuns = 6,
                      MaxNumberofSummarizedObsDims = 6,
                      MaxNumberofSummarizedRuns = 6)
  

.onAttach <- function(library, pkg)
{
  unlockBinding(".distrSimoptions", asNamespace("distrSim"))
# next lines are taken from Valentin Todorov's package "rrcov"
#    ver <- read.dcf(file.path(library, pkg, "DESCRIPTION"), "Version")
#    ver <- as.character(ver)
#    title <- read.dcf(file.path(library, pkg, "DESCRIPTION"), "Title")
#    title <- as.character(title)
#    if((!getOption("StartupBanner")=="off")||is.null(getOption("StartupBanner"))) 
#       message(paste(title, " (version ", ver, ")\n", sep = ""))
#    msga <- gettext("For more information see ?\"distrTEst\", NEWS(\"distrTEst\"), and \n")
#    msgb <- gettext("    http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf .\n")
#    if((getOption("StartupBanner")=="complete")||is.null(getOption("StartupBanner"))) 
#       message(msga,msgb,sep=""); 
buildStartupMessage(pkg="distrSim", packageHelp=TRUE, library=library, 
               #     MANUAL="http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf",
                    VIGNETTE=gettext("Package \"distrDoc\" provides a vignette to this package as well as\nto several related packages; try vignette(\"distr\")."))
###
  invisible()
}


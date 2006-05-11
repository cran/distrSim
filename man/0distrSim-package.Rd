\name{distrSim-package}
\alias{distrSim-package}
\alias{distrSim}
\docType{package}
\title{
distrSim -- (S4-) classes for simulations based on package distr
}
\description{
\pkg{distrSim} is to provide a conceptual treatment of simulations by means of S4 classes.
The package is based on our package \pkg{distr}, hence uses distribution classes 
as introduced there to describe the distributions under which simulations are performed.
}
\details{
\tabular{ll}{
Package: \tab distrSim\cr
Version: \tab 1.7\cr
Date: \tab 2006-05-11\cr
Depends: \tab R(>= 2.2.0), methods, graphics, setRNG(>= 2006.2-1), distr(>=
1.7)\cr
Imports: \tab stats\cr
SaveImage: \tab no\cr
LazyLoad: \tab yes\cr
License: \tab GPL (version 2 or later)\cr
URL: \tab http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/\cr
Packaged: \tab Thu May 11 10:16:03 2006; Peter\cr
Built: \tab R 2.4.0dev; i386-pc-mingw32; 2006-05-11 10:20:11; windows\cr
}
}
\section{Classes}{

\preformatted{

"Dataclass"
      slots: [<name>(<class>)] 
      filename(vectororNULL), Data(vectororNULL), runs(numeric), samplesize(numeric)
|>"Simulation"
      (additional) slots: [<name>(<class>)] 
      seed(list), distribution(UnivariateDistribution)
|>"Contsimulation"
      (additional) slots: [<name>(<class>)] 
      seed(list), ind(vectororNULL), rate(numeric),
      distribution.id(UnivariateDistribution),
      distribution.c(UnivariateDistribution),
      Data.id(vectororNULL),
      Data.c(vectororNULL)
}}

\section{Methods}{
\preformatted{
savedata                Methods to save the data slot 
                       (for Simulation/Contsimulation)
simulate                Methods to fill the data slot 
                       (for Simulation/Contsimulation)
plot                    Methods for Function plot 
                       (for Simulation/Contsimulation)
print-methods           Methods for Function print 
                       (for Simulation/Contsimulation)
summary-methods         Methods for Function summary 
                       (for Simulation/Contsimulation)
}}
\section{Functions}{
\preformatted{
cload                   loads just the comment slot 
                       (for Simulation/Contsimulation)
}}

\section{Slot accessors / -replacement functions}{
All slots are inspected / modified by corresponding 
accessors / -replacement functions, e.g. \code{rate(X)} for
an object of class \code{"Contsimulation"}
}
}
\author{
  Thomas Stabla \email{statho3@web.de},\cr 
  Florian Camphausen \email{fcampi@gmx.de},\cr
  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de},\cr 
  Matthias Kohl \email{Matthias.Kohl@stamats.de}\cr
  \emph{Maintainer:}  Peter Ruckdeschel \email{Peter.Ruckdeschel@uni-bayreuth.de}
  }

\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting \code{options("StartupBanner"="off")}
somewhere before loading this package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  \code{options("StartupBanner"="complete")} 
the complete start-up banner is displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.

The same can be achieved by wrapping the \code{library} or \code{require}  call into
either \code{suppressStartupMessages()} or \code{onlytypeStartupMessages(.,atypes="version")}. 
}


\references{
a more detailed manual for \pkg{distr}, \pkg{distrSim}, \pkg{distrTEst}, and \pkg{distrEx} may be downloaded from  
\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distr.pdf}

a homepage to this package is available under\cr
\url{http://www.uni-bayreuth.de/departments/math/org/mathe7/DISTR/distrSim.html}
}
\section{Start-up-Banner}{
You may suppress the start-up banner/message completely by setting \code{options("StartupBanner"="off")}
somewhere before loading this package by \code{library} or \code{require} in your R-code / R-session.

If option \code{"StartupBanner"} is not defined (default) or setting    
\code{options("StartupBanner"=NULL)} or  \code{options("StartupBanner"="complete")} 
the complete start-up banner is displayed.

For any other value of option \code{"StartupBanner"} (i.e., not in \code{c(NULL,"off","complete")})
only the version information is displayed.}

\keyword{ package }
\seealso{
\code{\link[distr]{distr}} \code{\link[setRNG]{setRNG}} 
}

.onLoad <- function(lib, pkg){
    require("methods", character = TRUE, quietly = TRUE) 
}

setClassUnion("vectororNULL", c("vector","NULL"))

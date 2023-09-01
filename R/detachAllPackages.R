#' detachAllPackages
#'
#' Function to detach all loaded packages
#' 

detachAllPackages <- function() {
  basic.packages.blank <- c(    
    "stats",    
    "graphics",    
    "grDevices",    
    "utils",   
    "datasets",  
    "methods",    
    "base"    
  )    
  basic.packages <- paste("package:", basic.packages.blank, sep = "")   
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]   
  package.list <- setdiff(package.list, basic.packages)   
  if (length(package.list) > 0) {   
    for (package in package.list) {   
      detach(package, character.only = TRUE)   
    }   
  }    
  cat("All packages detached")
}


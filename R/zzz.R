.onLoad <- function(lib, pkg) {
       setCores(1)
       packageStartupMessage("\nUse the function setCores() to change the number of CPU cores.\n")
}

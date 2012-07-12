.onLoad <- function(lib, pkg) {
       nn <- setCores(1L)
       if (nn > 0L) packageStartupMessage("\nUse the function setCores() to change the number of CPU cores.")
}

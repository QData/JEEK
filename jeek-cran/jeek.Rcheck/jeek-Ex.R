pkgname <- "jeek"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "jeek-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('jeek')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("jeek-package")
### * jeek-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: jeek-package
### Title: A Fast and Scalable Joint Estimator for Integrating Additional
###   Knowledge in Learning Multiple Related Sparse Gaussian Graphical
###   Models
### Aliases: jeek-package
### Keywords: package

### ** Examples

## Not run: 
##D data(exampleData)
##D result = jeek(X = exampleData, 0.3, covType = "cov", parallel = TRUE)
##D plot.jeek(results)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("jeek-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("jeek")
### * jeek

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: jeek
### Title: A Fast and Scalable Joint Estimator for Integrating Additional
###   Knowledge in Learning Multiple Related Sparse Gaussian Graphical
###   Models
### Aliases: jeek

### ** Examples

## Not run: 
##D data(exampleData)
##D result = jeek(X = exampleData, 0.3, covType = "cov", parallel = TRUE)
##D plot.jeek(results)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("jeek", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.jeek")
### * plot.jeek

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.jeek
### Title: Plot jeek result specified by user input
### Aliases: plot.jeek

### ** Examples

## Not run: 
##D data(exampleData)
##D result = jeek(X = exampleData, 0.3, covType = "cov", parallel = TRUE)
##D plot.jeek(result)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.jeek", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("returngraph")
### * returngraph

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: returngraph
### Title: return igraph object from jeek result specified by user input
### Aliases: returngraph

### ** Examples

## Not run: 
##D data(exampleData)
##D result = jeek(X = exampleData, 0.3, covType = "cov", parallel = TRUE)
##D graph = returngraph(result)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("returngraph", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

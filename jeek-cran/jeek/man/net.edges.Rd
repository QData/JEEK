\name{net.edges}
\alias{net.edges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the edges of each graph in the input list of multiple graphs
}
\description{
List every estimated edge in the form of pair of connected nodes for each graph in the input list of multiple graphs.
}
\usage{
net.edges(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
An input list of multiple graphs. Each graph is represented as a pXp matrix. (For example, the result of the SIMULE algorithm: a list of pXp matrices in which each matrix represents an estimated sparse inverse covariance matrix.)
}
}

\value{
edges, a length K list, each element of the list represents an igraph.es object which is the detail of all pairs of connected nodes of each graph in the input list of multiple graphs.
}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi. A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in Learning Multiple Related Sparse Gaussian Graphical Models. ICML 2018
}
\author{
Beilun Wang}

\examples{
\dontrun{
## load an example multi-task dataset with K=2 tasks, p=100 features, and n=200 samples per task:
data(exampleData)
##run jeek
result = jeek(X = exampleData , 0.05, covType = "cov", parallel = FALSE)
## get edges list:
net.edges(result$Graphs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

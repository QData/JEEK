\name{net.neighbors}
\alias{net.neighbors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get neighbors of a node in each graph in the input list of multiple graphs
}
\description{
For each graph in the input list of multiple graphs, returns the name of neighbor nodes connected to a given node.
}
\usage{
net.neighbors(theta, index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
  An input list of multiple graphs. Each graph is represented as a pXp matrix. (For example, the result of the SIMULE algorithm: a list of pXp matrices in which each matrix represents an estimated sparse inverse covariance matrix.)
}
  \item{index}{
The row number of the node to be investigated.
}
}

\value{
neighbors, a length K list. Each element in the list is a vector including row names of the neighbor nodes for the index node in each graph in the input list of multiple graphs.
}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi. A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in Learning Multiple Related Sparse Gaussian Graphical Models. ICML 2018
}
\author{
Beilun Wang}

\examples{
data(cancer)
X = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"),]),
as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"),]))
##run jeek
result = jeek(X, 0.05, covType = "cov", parallel = FALSE)
## get neighbors of node 10:
net.neighbors(result$Graphs,index=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

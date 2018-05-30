\name{net.degree}
\alias{net.degree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the degree of every node of each graph in the input list of multiple graphs.
}
\description{
Lists the degree of every node of each graph in the input list of multiple graphs.
}
\usage{
net.degree(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
An input list of multiple graphs. Each graph is represented as a pXp matrix. (For example, the result of the SIMULE algorithm: a list of pXp matrices in which each matrix represents an estimated sparse inverse covariance matrix.)
}
}

\value{
Degrees, in the format of a list of length p vectors represents the degree of all p nodes of each graph in the input list of multiple graphs.}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi. A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in Learning Multiple Related Sparse Gaussian Graphical Models. ICML 2018
}
\author{
Beilun Wang}


\examples{
\dontrun{
## load an examplar multi-task dataset with K=2 tasks, p=100 features, and n=200 samples per task:
data(exampleData)
##run jeek
result = jeek(X = exampleData , 0.05, covType = "cov", parallel = FALSE)
## get degree list:
net.degree(result$Graphs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

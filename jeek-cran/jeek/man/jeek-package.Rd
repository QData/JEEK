\name{jeek-package}
\alias{jeek-package}
\docType{package}
\title{A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in Learning Multiple Related Sparse Gaussian Graphical Models}
\description{
This is an R implementation of a Fast and Scalable Joint Estimator for Integrating Additional Knowledge in Learning Multiple Related Sparse Gaussian Graphical Models (JEEK).The JEEK algorithm can be used to fast estimate multiple related precision matrices in a large-scale. For instance, it can identify multiple gene networks from multi-context gene expression datasets. By performing data-driven network inference from high-dimensional and heterogenous data sets, this tool can help users effectively translate aggregated data into knowledge that take the form of graphs among entities. Please run demo(jeekDemo) to learn the basic functions provided by this package. For further details, please read the original paper: Beilun Wang, Arshdeep Sekhon, Yanjun Qi (2018).
}
\details{
\tabular{ll}{
Package: \tab jeek\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2018-06-15\cr
License: \tab GPL (>= 2)\cr
}
We consider the problem of including additional knowledge in estimating sparse Gaussian graphical models (sGGMs) from aggregated samples, arising often in bioinformatics and neuroimaging applications. Previous joint sGGM estimators either fail to use existing knowledge or cannot scale-up to many tasks (large \eqn{K}) under a high-dimensional (large \eqn{p}) situation.  In this paper, we propose a novel Joint Elementary Estimator incorporating additional Knowledge (JEEK) to infer multiple related sparse Gaussian Graphical models from
large-scale heterogeneous data. Using domain knowledge as weights, we design a novel hybrid norm as the minimization objective to enforce the superposition of two weighted sparsity constraints, one on the shared interactions and the other on the task-specific structural patterns. This enables JEEK to elegantly consider various forms of existing knowledge based on the domain at hand and avoid the need to design knowledge-specific optimization. JEEK is solved through a fast and entry-wise parallelizable solution that largely improves the computational efficiency of the state-of-the-art  \eqn{O(p^5K^4)} to \eqn{O(p^2K^4)}. We conduct a rigorous statistical analysis showing that JEEK achieves the same  convergence rate \eqn{O(\log(Kp)/n_{tot})} as the state-of-the-art estimators that are much harder to compute.
Empirically, on multiple synthetic datasets and one real-world data from neuroscience, JEEK outperforms the speed of the state-of-arts significantly while achieving the same level of prediction accuracy.

}
\author{
Beilun Wang

Maintainer: Beilun Wang - bw4mw at virginia dot edu
}
\references{
Beilun Wang, Arshdeep Sekhon, Yanjun Qi. A Fast and Scalable Joint Estimator for Integrating Additional Knowledge in Learning Multiple Related Sparse Gaussian Graphical Models. <arXiv:1806.00548>
}
\keyword{ package }
\examples{
data(cancer)
X = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"),]),
as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"),]))
jeek(X, 0.05, covType = "cov", parallel = FALSE)
}

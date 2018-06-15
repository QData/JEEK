pause <- function() {}

### load the jeek library
library(jeek)

pause <- function() {}

### load the cancer data
data(cancer)
X = list(as.matrix(cancer[[1]][which(cancer[[2]] == "not"),]), as.matrix(cancer[[1]][which(cancer[[2]] == "pcr"),]))

### run the simule
results = jeek(X, 0.05,  covType = "cov", parallel = TRUE)
results

pause()

### plot the estimated graphs by simule
plot.jeek(results)


pause()


pause()

### plot the estimated subgraphs that is about a specific node
plot.jeek(results, type="neighbor", index=15)


### load the  example data (larger p)

data(exampleData)
exampleData

pause()

### run simule algorithm to estimate two sparse precision matrices from the exampleData

results = jeek(X = exampleData , 0.05, covType = "cov", parallel = FALSE)
results


### Output the top-10 hubs in each identified graph.

print("Output the top-10 hubs in each identified graph.")
hub = net.hubs(results$Graphs)
hub

pause()

### Output the degrees of nodes in each identified graph.

print("Output the degrees of nodes in each identified graph.")
degree = net.degree(results$Graphs)
degree

pause()

### Output the list of edges in each identified graph.

print("Output the list of edges in each identified graph.")
edges = net.edges(results$Graphs)
edges

pause()

### Output the list of edges in each identified graph.

print("Output the list of edges in each identified graph.")
edges = net.edges(results$Graphs)
edges

pause()

### Output the neighbors of 50th node in each identified graph.

print("Output the neighbors of 50th node in each identified graph.")
neighbors = net.neighbors(results$Graphs,index=50)
neighbors

pause()

### plot the estimated graphs by simule
plot.jeek(results)


pause()

### plot the estimated subgraphs that is about a specific node
plot.jeek(results, type="neighbor", index=50)

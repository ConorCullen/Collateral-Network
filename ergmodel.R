install.packages('statnet')
library(statnet)
install.packages('ergm')
library(ergm)

mat <- read.csv(file="/Users/conorbarrycullen/Documents/CDO/CM/sociomat.csv", header=FALSE, sep=",",stringsAsFactors = FALSE)
net <- as.network(x = mat, # the network object
                                     directed = FALSE, # specify whether the network is directed
                                     loops = FALSE, # do we allow self ties (should not allow them)
                                     matrix.type = "adjacency" # the type of input
                                     )

plot_nets = function(n)
        plot(n
                     , displaylabels = FALSE,
                     , vertex.cex = degree(n, cmode = 'indegree') / 2 + 1
                     , vertex.col = 'blue'
                     , vertex.sides = ifelse(n %v% 'cloisterville', 4, 50)
                     )

model1 <- ergm(net~edges) 
print(summary(model1))
g.sim <- simulate(model1,nsim=4,burnin=1000,interval=1000)
par(mfrow = c(2, 2))
invisible(lapply(g.sim, plot_nets))
model2 <- ergm(net~triangles) 
print(summary(model2))





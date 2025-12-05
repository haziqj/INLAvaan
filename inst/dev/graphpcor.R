
library(graphpcor)

g1 <- graphpcor(x1 ~ x2 + x3)
plot(g1)

vcov(g1, theta = c(1, 1))

prec(g1, theta = 1:5)

G1 <- matrix(c(1,1,1, 1,1,0, 1,0,1),3)
G1
g2 <- graphpcor(G1)

library(INLA)
cmodel <- cgeneric_graphpcor(g1, base = c(-1, 2), lambda = 3, useINLAprecomp = FALSE)
class(cmodel) <- 'cgeneric'

prec(g1, theta = c(-1, 1))
INLAtools::prec(cmodel, theta = c(-1,1))

INLAtools::prior(cmodel, theta = c(1, 2))
INLAtools::prior(cmodel, theta = cbind(c(1, 2), c(1,1)))


th2 <- cbind(rnorm(1000), rnorm(1000))
vcov(g1, theta = th2[1,])

pth2 <- apply(th2, 1, function(th) vcov(g1, theta = th)[c(2,3,6)])

par(mfrow = c(2,2))
hist(pth2[1,])
hist(pth2[2,])
hist(pth2[3,])

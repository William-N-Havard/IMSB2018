## ---- message = FALSE, fig.width = 27, fig.heigth = 12, fig.align = "center"----
library(tidyverse)
set.seed(1234567)
sample(x = c(0, 1), size = 50, replace = TRUE, prob = c(0.4,0.6)) %>%
        data.frame %>%
        mutate(x = seq_along(.), y = cumsum(.) / seq_along(.) ) %>%
        ggplot(aes(x = x, y = y), log = "y") +  geom_line(lwd = 1, col = "steelBlue") +
        geom_hline(yintercept = 0.5, lty = 3) +
        xlab("nombre de lancers") + ylab("proportion de faces") +
        ylim(0, 1) + theme_bw(base_size = 20)

## ----echo = TRUE, eval= TRUE, fig.width= 25, fig.height= 8, fig.align= "center"----
NbrTrial     = 100
N            = 10
nbrSuccess   = 7
theta        = seq(from = 0, to = 1, by = 1/NbrTrial)
likelihood   = (theta^(nbrSuccess)) * (1 - theta)^(N - nbrSuccess) 
data.frame(theta, likelihood) %>%
ggplot(aes(x = theta, y = likelihood)) + 
    geom_area(color="orangered", fill = "orangered", alpha =.4) +
    theme_bw(base_size = 20) + theme(legend.position="none",
          text = element_text(size=30))

## ------------------------------------------------------------------------
a     = 2
b     = 3
theta = seq( from= 0 , to= 1 , length.out= 7 )
dbeta(theta,a,b)
beta(a,b)

## ---- echo = FALSE, fig.width = 27, fig.heigth = 13, fig.align = "center"----
library(tidyverse)
# we build a dataframe with all the data
theta = seq(from = 0, to = 1, by = 0.01)
data <- data.frame(
    theta,
    prior_a_eq_b = dbeta(theta, 1, 1),
    prior_a_gt_b = dbeta(theta, 5, 2),
    prior_a_lt_b = dbeta(theta, 2, 5)
)
# ggplot is initialize with the dataFrame (the only one we use here) 
ggplot(data)  +
    # we plot the curves with their specific colors
    geom_line(aes(x = theta, y = prior_a_eq_b / sum(prior_a_eq_b), color = "a = 1, b = 1"), lwd = 1) + 
    geom_line(aes(x = theta, y = prior_a_gt_b / sum(prior_a_gt_b), color = "a = 5, b = 2"), lwd = 1) + 
    geom_line(aes(x = theta, y = prior_a_lt_b / sum(prior_a_lt_b), color = "a = 2, b = 5"), lwd = 1) + 
    # we specify the Y-axis label  
    ylab("Prior distribution") +
    # change title of legend and colors
    scale_colour_manual(name = "Parameters", values = c("steelBlue", "orangered","green")) +
    theme_bw(base_size = 30) +
    theme(legend.justification = c(0, 1),
        legend.position = c(0.47, .95)) 

## ---- echo = FALSE, fig.width = 27, fig.heigth = 13, fig.align = "center"----
library(tidyverse)
# we build a dataframe with all the data
theta = seq(from = 0, to = 1, by = 0.01)
data <- data.frame(
    theta,
    prior_unif = dbeta(theta, 1, 1),
    prior_small = dbeta(theta, 5, 5),
    prior_large = dbeta(theta, 50, 50)
)
# ggplot is initialize with the dataFrame (the only one we use here) 
ggplot(data)  +
    # we plot the curves with their specific colors
    geom_line(aes(x = theta, y = prior_unif / sum(prior_unif), color = "a = 1, b = 1"), lwd = 1) + 
    geom_line(aes(x = theta, y = prior_small / sum(prior_small), color = "a = 5, b = 5"), lwd = 1) + 
    geom_line(aes(x = theta, y = prior_large / sum(prior_large), color = "a = 50, b = 50"), lwd = 1) + 
    # we specify the Y-axis label  
    ylab("Prior distribution") +
    # change title of legend and colors
    scale_colour_manual(name = "Parameters", values = c("steelBlue", "orangered","green")) +
    theme_bw(base_size = 30) +
    theme(legend.justification = c(0, 1),
        legend.position = c(0.17, .95)) 

## ---- echo = FALSE, fig.width = 27, fig.heigth = 13, fig.align = "center"----
library(tidyverse)
# we build a dataframe with all the data
theta = seq(from = 0, to = 1, by = 0.01)
W <- 0.65
K1 <- 25
K2 <- 10
data <- data.frame(
    theta,
    prior_KappaL = dbeta(theta, W*(K1-2) + 1, (1-W)*(K1-2) + 1),
    prior_KappaS = dbeta(theta, W*(K2-2) + 1, (1-W)*(K2-2) + 1)
)
# ggplot is initialize with the dataFrame (the only one we use here) 
ggplot(data)  +
    # we plot the curves with their specific colors
    geom_line(aes(x = theta, y = prior_KappaL / sum(prior_KappaL), color = "w = 0.65, K = 25"), lwd = 1) + 
    geom_line(aes(x = theta, y = prior_KappaS / sum(prior_KappaS), color = "w = 0.65, k = 10"), lwd = 1) + 
    # we specify the Y-axis label  
    ylab("Prior distribution") +
    # change title of legend and colors
    scale_colour_manual(name = "Parameters", values = c("steelBlue", "orangered")) +
    theme_bw(base_size = 30) +
    theme(legend.justification = c(0, 1),
        legend.position = c(0.17, .95)) 

## ---- echo = FALSE, fig.width = 27, fig.heigth = 13, fig.align = "center"----
library(tidyverse)
# we build a dataframe with all the data
theta = seq(from = 0, to = 1, by = 0.01)
M <- .8
K1 <- 22
K2 <- 7
data <- data.frame(
    theta,
    prior_KappaL = dbeta(theta, M*K1, (1-M)*K1),
    prior_KappaS = dbeta(theta, M*K2, (1-M)*K2)
)
# ggplot is initialize with the dataFrame (the only one we use here) 
ggplot(data)  +
    # we plot the curves with their specific colors
    geom_line(aes(x = theta, y = prior_KappaL / sum(prior_KappaL), color = "m = 0.8, K = 22"), lwd = 1) + 
    geom_line(aes(x = theta, y = prior_KappaS / sum(prior_KappaS), color = "m = 0.8, k = 7"), lwd = 1) + 
    # we specify the Y-axis label  
    ylab("Prior distribution") +
    # change title of legend and colors
    scale_colour_manual(name = "Parameters", values = c("steelBlue", "orangered")) +
    theme_bw(base_size = 30) +
    theme(legend.justification = c(0, 1),
        legend.position = c(0.17, .95)) 

## ---- echo = FALSE, fig.width = 27, fig.heigth = 13, fig.align = "center"----
library(tidyverse)
# we build a dataframe with all the data
theta = seq(from = 0, to = 1, by = 0.01)
M <- .8
S1 <- 0.15
S2 <- 0.10
data <- data.frame(
    theta,
    prior_SL = dbeta(theta, M*(M*(1-M)/S1^2-1), (1-M)*(M*(1-M)/S1^2-1)),
    prior_SS = dbeta(theta, M*(M*(1-M)/S2^2-1), (1-M)*(M*(1-M)/S2^2-1))
)
# ggplot is initialize with the dataFrame (the only one we use here) 
ggplot(data)  +
    # we plot the curves with their specific colors
    geom_line(aes(x = theta, y = prior_SL / sum(prior_SL), color = "mu = 0.8, Sigma = 0.15"), lwd = 1) + 
    geom_line(aes(x = theta, y = prior_SS / sum(prior_SS), color = "mu = 0.8, Sigma = 0.10"), lwd = 1) + 
    # we specify the Y-axis label  
    ylab("Prior distribution") +
    # change title of legend and colors
    scale_colour_manual(name = "Parameters", values = c("steelBlue", "orangered")) +
    theme_bw(base_size = 30) +
    theme(legend.justification = c(0, 1),
        legend.position = c(0.17, .95)) 

## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 8----
source("./Script/IMSB_binomial.R")
thetaSize    = 100
theta        = seq(from = 0, to = 1, by = 1/thetaSize)
a            = 4
b            = 16
pTheta.prior = prior.AB (theta, a, b, normalize = TRUE)
N            = 10       # Specify the total number of flips, denoted N.
z            = 6
pTheta.likelihood  = likelihood.param(N, z, theta, normalize = TRUE)
pTheta.posterior   = pTheta.prior*pTheta.likelihood / sum(pTheta.prior*pTheta.likelihood)
display(theta, pTheta.posterior, funcType = "posterior", plotType = "Points")
display(theta, pTheta.prior, funcType = "prior", plotType = "Points", add = TRUE)
display(theta, pTheta.likelihood, funcType = "likelihood", plotType = "Points", add = TRUE)
legend(.87, .035, c("prior","likelihood","posterior"), cex=2, 
       col=c(color.prior, color.likelihood, color.posterior), pch=15, lty=1:2)

## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 8----
source("./Script/IMSB_binomial.R")
thetaSize    = 100
theta        = seq(from = 0, to = 1, by = 1/thetaSize)
a            = 4
b            = 16
pTheta.prior = prior.AB (theta, a, b, normalize = TRUE)
N            = 20       # Specify the total number of flips, denoted N.
z            = 12
pTheta.likelihood  = likelihood.param(N, z, theta, normalize = TRUE)
pTheta.posterior   = pTheta.prior*pTheta.likelihood / sum(pTheta.prior*pTheta.likelihood)
display(theta, pTheta.posterior, funcType = "posterior", plotType = "Points")
display(theta, pTheta.prior, funcType = "prior", plotType = "Points", add = TRUE)
display(theta, pTheta.likelihood, funcType = "likelihood", plotType = "Points", add = TRUE)
legend(.87, .055, c("prior","likelihood","posterior"), cex=2, 
       col=c(color.prior, color.likelihood, color.posterior), pch=15, lty=1:2)


## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 8----
source("./Script/IMSB_binomial.R")
thetaSize    = 100
theta        = seq(from = 0, to = 1, by = 1/thetaSize)
a            = 4
b            = 16
pTheta.prior = prior.AB (theta, a, b, normalize = TRUE)
N            = 40       # Specify the total number of flips, denoted N.
z            = 24
pTheta.likelihood  = likelihood.param(N, z, theta, normalize = TRUE)
pTheta.posterior   = pTheta.prior*pTheta.likelihood / sum(pTheta.prior*pTheta.likelihood)
display(theta, pTheta.posterior, funcType = "posterior", plotType = "Points")
display(theta, pTheta.prior, funcType = "prior", plotType = "Points", add = TRUE)
display(theta, pTheta.likelihood, funcType = "likelihood", plotType = "Points", add = TRUE)
legend(.87, .055, c("prior","likelihood","posterior"), cex=2, 
       col=c(color.prior, color.likelihood, color.posterior), pch=15, lty=1:2)


## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 12----
source("./Script/IMSB_binomial.R")
thetaSize    = 100
theta        = seq(from = 0, to = 1, by = 1/thetaSize)
a            = 3
b            = 17
pTheta.prior = prior.AB (theta, a, b, normalize = TRUE)
N            = 10       # Specify the total number of flips, denoted N.
z            = 8
pTheta.likelihood  = likelihood.param(N, z, theta, normalize = TRUE)
pTheta.posterior   = pTheta.prior*pTheta.likelihood / sum(pTheta.prior*pTheta.likelihood)
display(theta, pTheta.prior, funcType = "prior", plotType = "Points")
display(theta, pTheta.posterior, funcType = "posterior", plotType = "Points", add = TRUE)
display(theta, pTheta.likelihood, funcType = "likelihood", tittle = "Likelihood function", plotType = "Points", add = TRUE)
legend(.8, .055, c("prior","likelihood","posterior"), cex=2, 
       col=c(color.prior, color.likelihood, color.posterior), pch=15, lty=1:2)


## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 8----
thetaSize = 30    
theta     = seq(from = 0, to = 1, by = 1/thetaSize)
pTheta    = rep(1, thetaSize+1)
display(theta, pTheta, funcType = "none", plotType = "Bars")


## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 8----
thetaSize    = 30    
theta        = seq(from = 0, to = 1, by = 1/thetaSize)
pTheta.prior = prior.AB (theta, 3, 7, normalize = TRUE)
display(theta, pTheta.prior, funcType = "prior", plotType = "Bars")

## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 8----
thetaSize         = 30    
theta             = seq(from = 0, to = 1, by = 1/thetaSize)
pTheta.prior      = prior.AB (theta, 3, 7, normalize = TRUE)
pTheta.likelihood = likelihood.param (20, 12, theta, normalize = TRUE)
display(theta, pTheta.likelihood, funcType = "likelihood", plotType = "Bars")
display(theta-.005, pTheta.prior, funcType = "prior", plotType = "Bars", add = TRUE)
legend(0, .12, c("prior","likelihood"), cex=2, 
       col=c(color.prior, color.likelihood), pch=15, lty=1:2)

## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 8----
thetaSize         = 30    
theta             = seq(from = 0, to = 1, by = 1/thetaSize)
pTheta.prior      = prior.AB (theta, 3, 7, normalize = TRUE)
pTheta.likelihood = likelihood.param (20, 12, theta, normalize = TRUE)
pTheta.posterior  = pTheta.prior*pTheta.likelihood / sum(pTheta.prior*pTheta.likelihood)

display(theta+.005, pTheta.posterior, funcType = "posterior", plotType = "Bars")
display(theta, pTheta.likelihood, funcType = "likelihood", plotType = "Bars", add = TRUE)
display(theta-.005, pTheta.prior, funcType = "prior", plotType = "Bars", add = TRUE)
legend(0, .15, c("prior","likelihood","posterior"), cex=2, 
       col=c(color.prior, color.likelihood, color.posterior), pch=15, lty=1:2)

## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25, fig.height= 8----
thetaSize         = 100    
theta             = seq(from = 0, to = 1, by = 1/thetaSize)
pTheta.prior      = prior.AB (theta, 3, 7, normalize = TRUE)
pTheta.likelihood = likelihood.param (20, 12, theta, normalize = TRUE)
pTheta.posterior  = pTheta.prior*pTheta.likelihood / sum(pTheta.prior*pTheta.likelihood)
display(theta+.009, pTheta.posterior, funcType = "posterior", plotType = "Bars")
display(theta, pTheta.likelihood, funcType = "likelihood", plotType = "Bars", add = TRUE)
display(theta-.009, pTheta.prior, funcType = "prior", plotType = "Bars", add = TRUE)
legend(0, .04, c("prior","likelihood","posterior"), cex=2, 
       col=c(color.prior, color.likelihood, color.posterior), pch=15, lty=1:2)

## ---- eval=FALSE, echo=TRUE, fig.width= 25, fig.align="center"-----------
## trajLength     = 250 # nbr of measure
## theta          = 1:7
## ptheta         = theta
## trajectory     = sample(theta, prob = ptheta, size = trajLength, replace = TRUE)

## ---- eval=TRUE, echo=FALSE, fig.width= 25, fig.height = 5, fig.align="center"----
source("./Script/IMSB_binomial.R")

set.seed(1789)
trajLength     = 100 # nbr of measure                    
theta          = 1:7
ptheta         = theta
trajectory     = sample(theta, prob = ptheta, size = trajLength, replace = TRUE)

layout(matrix(1:2, ncol= 2), widths = c(.75, .25))
plot(trajectory, main="distribution postérieure basé sur 100 tirages" ,
     ylab=bquote(theta) , xlim=c(0,trajLength) , xlab="itérations" ,
     type="o" , pch=20 , col= color.posterior , cex.lab=2, cex.main= 3, cex.axis= 2 )
barplot(table(trajectory), col= color.posterior, horiz= TRUE, axes = FALSE, axisnames = FALSE)

## ---- eval=TRUE, echo=FALSE, fig.width= 25, fig.height = 5, fig.align="center"----
source("./Script/IMSB_binomial.R")

set.seed(1789)
trajLength     = 1000 # nbr of measure                    
theta          = 1:7
ptheta         = theta
trajectory     = sample(theta, prob = ptheta, size = trajLength, replace = TRUE)

layout(matrix(1:2, ncol= 2), widths = c(.75, .25))
plot(trajectory, main="distribution postérieure basé sur 5000 tirages" ,
     ylab=bquote(theta) , xlim=c(0,trajLength) , xlab="itérations" ,
     type="o" , pch=20 , col= color.posterior , cex.lab=2, cex.main= 3, cex.axis= 2 )
barplot(table(trajectory), col= color.posterior, horiz= TRUE, axes = FALSE, axisnames = FALSE)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## p_grid    =  seq( from=0 , to=1 , length.out=1000 )
## a = b     = 1
## N         = 9
## z         = 6
## posterior = dbeta(p_grid, z+a-1, N-z+b-1)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## p_grid      = seq( from=0 , to=1 , length.out = 1000 )
## prior       = rep( 1 , 1000 )
## likelihood  = dbinom( 6 , size=8 , prob=p_grid )
## posterior   = likelihood * prior
## posterior   = posterior / sum(posterior)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## samples = sample( p_grid , prob = posterior , size = 1e4 , replace = TRUE )

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## library(rethinking)
## chainmode( samples , adj=0.01 ) # pour le mode ou MAP
## mean( samples )                 # pour la moyenne
## median( samples )               # pour la médiane

## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25------------
library(rethinking)
set.seed(1234567)
p_grid     = seq( from=0 , to=1 , length.out=1000 )
prior      = rep(1,1000)
likelihood = dbinom( 3 , size=10 , prob=p_grid )
posterior  = likelihood * prior
posterior  = posterior / sum(posterior)
samples    = sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
mode      = chainmode( samples , adj=0.01 )
mean       = mean( samples )
median     = median( samples )

plot( p_grid, posterior, main="Distribution Postérieure" ,
      xlim=c(0,1) , xlab=bquote(theta) , ylab = "densité",
      type="l", cex.lab=2, cex.main= 3, cex.axis= 2)
abline(v = mode, col= "Blue")
abline(v = mean, col= "Orange")
abline(v = median, col= "ForestGreen")
text(mode-.005, .00, "mode", adj = c(-.1, -.1), srt = 90, cex=1.5)
text(mean-.005, .002, "moyenne", adj = c(-.1, -.1), srt = 90, cex=1.5)
text(median-.005, .001, "médiane", adj = c(-.1, -.1), srt = 90, cex=1.5)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## library(rethinking)
## chainmode( samples , adj=0.01 ) # pour le mode ou MAP
## mean( samples )                 # pour la moyenne
## median( samples )               # pour la médiane

## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25------------
library(rethinking)
p_grid     = seq( from=0 , to=1 , length.out=1000 )
prior      = rep(1,1000)
likelihood = dbinom( 3 , size=3 , prob=p_grid )
posterior  = likelihood * prior
posterior  = posterior / sum(posterior)
samples    = sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
mode1      = chainmode( samples , adj=0.01 )
mean       = mean( samples )
median     = median( samples )

plot( p_grid, posterior, main="Distribution Postérieure" ,
      xlim=c(0,1) , xlab=bquote(theta) , ylab = "densité",
      type="l", cex.lab=2, cex.main= 3, cex.axis= 2)
abline(v = mode1, col= "Blue")
abline(v = mean, col= "Orange")
abline(v = median, col= "ForestGreen")
text(mode1-.005, .00, "mode", adj = c(-.1, -.1), srt = 90, cex=1.5)
text(mean-.005, .002, "moyenne", adj = c(-.1, -.1), srt = 90, cex=1.5)
text(median-.005, .001, "médiane", adj = c(-.1, -.1), srt = 90, cex=1.5)


## ----eval=FALSE, echo=TRUE-----------------------------------------------
## # pour la moyenne
## sapply( p_grid , function(d) sum( posterior*( d - p_grid )^2 ) )
## # pour la médiane
## sapply( p_grid , function(d) sum( posterior*abs( d - p_grid ) ) )

## ----eval=TRUE, echo=FALSE, fig.align="center", fig.width= 25------------
library(rethinking)
p_grid     = seq( from=0 , to=1 , length.out=1000 )
prior      = rep(1,1000)
likelihood = dbinom( 3 , size=3 , prob=p_grid )
posterior  = likelihood * prior
posterior  = posterior / sum(posterior)
samples    = sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
mode      = chainmode( samples , adj=0.01 )
mean       = mean( samples )
median     = median( samples )

loss2      = sapply( p_grid , function(d) sum( posterior*( d - p_grid )^2 ) )
minLoss2   = p_grid[ which.min(loss2) ]
plot(p_grid, loss2, main="Fonction de perte" ,
      xlim=c(0,1) , xlab="décision" , ylab = "taux de perte attendu",
      type="l" , col= "Orange" , cex.lab=2, cex.main= 3, cex.axis= 2)
points(minLoss2, loss2[ which.min(loss2) ], pch=19, col = "Red")

loss    = sapply( p_grid , function(d) sum( posterior*abs( d - p_grid ) ) )
minLoss = p_grid[ which.min(loss) ]
lines(p_grid, loss, main="Fonction de perte" ,
    xlim=c(0,1) , xlab="décision" , ylab = "taux de perte attendu",
     type="l" , col= "ForestGreen" , cex.lab=2, cex.main= 3, cex.axis= 2)
points(minLoss, loss[ which.min(loss) ], pch=19, col = "Red")

abline(v = mode, col= "Blue")
abline(v = mean, col= "Orange")
abline(v = median, col= "ForestGreen")
text(mode-.005, .2, "mode", adj = c(-.1, -.1), srt = 90, cex=1.25)
text(mean-.005, .4, "moyenne", adj = c(-.1, -.1), srt = 90, cex=1.25)
text(median-.005, .3, "médiane", adj = c(-.1, -.1), srt = 90, cex=1.25)

legend(0, .2, c("absolue","carré"), cex=1.25, 
       col=c("Orange","ForestGreen"), pch=15, lty=1:2)

## ----eval=TRUE, echo=TRUE------------------------------------------------
library(coda)
library(BEST)
set.seed(1234567)
iterMax = 10000
p_grid  = seq( from=0 , to=1 , length.out=iterMax )
pTheta  = dbeta( p_grid, 3, 10)
massVec = pTheta/sum(pTheta)
samples = sample( p_grid , size=1e4 , replace=TRUE , prob=pTheta )

#plotPost(samples, credMass = 0.85)
hdi( samples , credMass = 0.89 )

## ----eval=TRUE, echo=FALSE, fig.width= 10, fig.height= 10----------------
library(BEST)
library(coda)
set.seed(1234567)
iterMax = 10000
p_grid  = seq( from=0 , to=1 , length.out=iterMax )
pTheta  = dbeta( p_grid, 3, 10)
massVec = pTheta/sum(pTheta)
samples = sample( p_grid , size=1e4 , replace=TRUE , prob=pTheta )

plotPost(samples,  credMass = 0.89, cex = 3)

## ----eval=T, echo=TRUE---------------------------------------------------
library(rethinking)
set.seed(1234567)
iterMax = 10000
p_grid  = seq( from=0 , to=1 , length.out=iterMax )
pTheta  = dbeta( p_grid, 3, 10)
massVec = pTheta / sum(pTheta)
samples = sample( p_grid , size = 1e4 , replace = TRUE , prob = pTheta )

# utilise le package CODA pour calculer HPDI
HPDI( samples, prob = 0.89)

## ----eval=TRUE, echo=FALSE, fig.height = 10, fig.width = 10--------------
library(BEST)
library(coda)
set.seed(1234567)
terMax = 10000
p_grid  = seq( from=0 , to=1 , length.out=iterMax )
pTheta  = dbeta( seq(0, 1, length=iterMax), 3, 4)
massVec = pTheta/sum(pTheta)
samples = sample( p_grid , size=1e4 , replace=TRUE , prob=pTheta )

plotPost(samples,  ROPE=c(-0.1,0.1), cex = 3)

## ----eval=F, echo=T------------------------------------------------------
## w <- rbinom(n = 10e4, size = 9, prob = 0.6)

## ----eval=F, echo=T------------------------------------------------------
## w <- rbinom(n = 10e4, size = 9, prob = samples)


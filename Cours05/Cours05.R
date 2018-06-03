## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(cache = TRUE)
library(tidyverse)
prior.color <- "steelBlue"
likelihood.color <- "orangered"
posterior.color <- "magenta4"

## ---- eval=TRUE, echo=FALSE, fig.width= 25, fig.align="center"-----------
source("../Cours02/Script/IMSB_binomial.R")

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

## ---- eval=TRUE, echo=FALSE, fig.width= 25, fig.align="center"-----------
source("../Cours02/DBDA2Eprograms/DBDA2E-utilities.R")
set.seed(1789)

nextPosition = function(currentPosition){
        # flip coin to generate proposal
        proposal = currentPosition + sample( c(-1,1) , size=1 )
        # now make sure he loops around the archipelago
        if ( proposal < 1 ) proposal = 1
        if ( proposal > 7 ) proposal = 7
        # move?
        prob_move = min(1.0, proposal/currentPosition)
        result = ifelse( runif(1) < prob_move , proposal , currentPosition )
        return(result)
}

positionLength = 250 # nbr of measure                    
trajectory     = rep( 0 , positionLength )
trajectory[1]  = 1

for (ii in 1:(positionLength-1)) {
        trajectory[ii+1]= nextPosition(trajectory[ii])
}

idxToPlot = 1:positionLength
layout(matrix(1:2, ncol= 2), widths = c(.75, .25))
# plot histogram
plot( idxToPlot , trajectory[idxToPlot] , main="Méthode Metropolis algorithm" ,
      ylab= bquote(theta) , xlim= c(0, positionLength) , xlab= "itérations" ,
      type= "o" , pch= 20 , col= prior.color , cex.lab= 2, cex.main= 3, cex.axis= 2 )
# barplot
barplot(table(trajectory[idxToPlot]), col= prior.color, horiz= TRUE, axes = FALSE, axisnames = FALSE)

## ---- eval=TRUE, echo=FALSE, fig.width= 25, fig.align="center"-----------
source("../Cours02/Script/IMSB_binomial.R")

set.seed(1789)
trajLength     = 250 # nbr of measure                    
theta          = 1:7
ptheta         = theta
trajectory     = sample(theta, prob = ptheta, size = trajLength, replace = TRUE)

layout(matrix(1:2, ncol= 2), widths = c(.75, .25))
plot(trajectory, main="Méthode Monte Carlo" ,
     ylab=bquote(theta) , xlim=c(0,trajLength) , xlab="itérations" ,
     type="o" , pch=20 , col= prior.color , cex.lab=2, cex.main= 3, cex.axis= 2 )
barplot(table(trajectory), col= prior.color, horiz= TRUE, axes = FALSE, axisnames = FALSE)

## ----eval=TRUE, echo=FALSE, fig.width = 10, fig.height= 15, fig.align="center"----
source("../Cours02/DBDA2Eprograms/DBDA2E-utilities.R")

# Specify the data, to be used in the likelihood function.
myData = c(rep(0,6),rep(1,14))

# Define the Bernoulli likelihood function, p(D|theta).
# The argument theta could be a vector, not just a scalar.
likelihood = function( theta , data ) {
  z = sum( data )
  N = length( data )
  pDataGivenTheta = theta^z * (1-theta)^(N-z)
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The likelihood for theta > 1 or for theta < 0 is zero:
  pDataGivenTheta[ theta > 1 | theta < 0 ] = 0
  return( pDataGivenTheta )
}

# Define the prior density function. 
prior = function( theta ) {
  pTheta = dbeta( theta , 1 , 1 )
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The prior for theta > 1 or for theta < 0 is zero:
  pTheta[ theta > 1 | theta < 0 ] = 0
  return( pTheta )
}

# Define the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.
targetRelProb = function( theta , data ) {
  targetRelProb =  likelihood( theta , data ) * prior( theta )
  return( targetRelProb )
}

# Specify the length of the trajectory, i.e., the number of jumps to try:
trajLength = 50000 # arbitrary large number
# Initialize the vector that will store the results:
trajectory = rep( 0 , trajLength )
# Specify where to start the trajectory:
trajectory[1] = 0.01 # arbitrary value
# Specify the burn-in period:
burnIn = ceiling( 0.0 * trajLength ) # arbitrary number, less than trajLength
# Initialize accepted, rejected counters, just to monitor performance:
nAccepted = 0
nRejected = 0

# Now generate the random walk. The 't' index is time or trial in the walk.
# Specify seed to reproduce same random walk:
set.seed(47405)
# Specify standard deviation of proposal distribution:
proposalSD = c(0.02,0.2,2.0)[2]
for ( t in 1:(trajLength-1) ) {
	currentPosition = trajectory[t]
	# Use the proposal distribution to generate a proposed jump.
	proposedJump = rnorm( 1 , mean=0 , sd=proposalSD )
	# Compute the probability of accepting the proposed jump.
	probAccept = min( 1,
		targetRelProb( currentPosition + proposedJump , myData )
		/ targetRelProb( currentPosition , myData ) )
	# Generate a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump.
	if ( runif(1) < probAccept ) {
		# accept the proposed jump
		trajectory[ t+1 ] = currentPosition + proposedJump
		# increment the accepted counter, just to monitor performance
		if ( t > burnIn ) { nAccepted = nAccepted + 1 }
	} else {
		# reject the proposed jump, stay at current position
		trajectory[ t+1 ] = currentPosition
		# increment the rejected counter, just to monitor performance
		if ( t > burnIn ) { nRejected = nRejected + 1 }
	}
}

# Extract the post-burnIn portion of the trajectory.
acceptedTraj = trajectory[ (burnIn+1) : length(trajectory) ]

# End of Metropolis algorithm.

#-----------------------------------------------------------------------
# Display the chain.

#openGraph(width=4,height=8)
layout( matrix(1:3,nrow=3) )
par(mar=c(3,4,2,1),mgp=c(2,0.7,0))

# Posterior histogram:
paramInfo = plotPost( acceptedTraj , xlim=c(0,1) , xlab=bquote(theta) , 
                      cex=3.0, cex.main= 2.5, col=posterior.color,
                      main=bquote( list( "Prpsl.SD" == .(proposalSD) ,
                      "Eff.Sz." == .(round(effectiveSize(acceptedTraj),1)) ) ) )

# Trajectory, a.k.a. trace plot, end of chain:
idxToPlot = (trajLength-100):trajLength
plot( trajectory[idxToPlot] , idxToPlot , main="End of Chain" ,
      xlab=bquote(theta) , xlim=c(0,1) , ylab="Step in Chain" ,
      type="o" , pch=20 , col=posterior.color , cex.lab=3. )
# Display proposal SD and acceptance ratio in the plot.
text( 0.0 , trajLength , adj=c(0.0,1.1) , cex=3. ,
      labels = bquote( frac(N[acc],N[pro]) == 
                       .(signif( nAccepted/length(acceptedTraj) , 3 ))))

# Trajectory, a.k.a. trace plot, beginning of chain:
idxToPlot = 1:100
plot( trajectory[idxToPlot] , idxToPlot , main="Beginning of Chain" ,
      xlab=bquote(theta) , xlim=c(0,1) , ylab="Step in Chain" ,
      type="o" , pch=20 , col=posterior.color , cex.lab=3. )
# Indicate burn in limit (might not be visible if not in range):
if ( burnIn > 0 ) {
  abline(h=burnIn,lty="dotted")
  text( 0.5 , burnIn+1 , "Burn In" , adj=c(0.5,1.1) )
}

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## library('rjags')
## # cr&ation du modèle
## N <- 100
## x <- 1:N
## epsilon <- rnorm(N, 0, 1)
## y <- x + epsilon
## 
## jags <- jags.model('./Script/example.jags',
##                    data = list('x' = x,
##                                'y' = y,
##                                'N' = N),
##                    n.chains = 4,
##                    n.adapt = 1000)
## samples <- coda.samples(jags,
##                         c('a', 'b'),
##                         1000)
## plot(samples)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## model #fichier './Script/example.jags'
## {
## 	for (i in 1:N)
## 	{
## 		y[i] ~ dnorm(y.hat[i], tau)
## 		y.hat[i] <- a + b * x[i]
## 	}
## 	a ~ dnorm(0, .0001)
## 	b ~ dnorm(0, .0001)
## 	tau <- pow(sigma, -2)
## 	sigma ~ dunif(0, 100)
## }

## ----eval=TRUE, echo=FALSE, fig.width = 12, fig.height= 12, fig.align="center"----
library('rjags')

N <- 100
x <- 1:N
epsilon <- rnorm(N, 0, 1)
y <- x + epsilon

jags <- jags.model('./Script/example.jags',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 1000)
samples <- coda.samples(jags,
                        c('a', 'b'),
                        1000)
#png('./Cours5/Script/plot_2.png')
plot(samples)


## ----eval=FALSE, echo=TRUE-----------------------------------------------
## library('rjags')
## # cr&ation du modèle
## N <- 100
## x <- 1:N
## epsilon <- rnorm(N, 0, 1)
## y <- x + epsilon
## 
## jags <- jags.model('./Script/example.jags',
##                    data = list('x' = x,
##                                'y' = y,
##                                'N' = N),
##                    n.chains = 4,
##                    n.adapt = 1000)
## samples <- coda.samples(jags,
##                         c('a', 'b'),
##                         1000)
## gelman.plot(samples)

## ----eval=TRUE, echo=FALSE, fig.width = 12, fig.height= 12, fig.align="center"----
jags <- jags.model('./Script/example.jags',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 1000)
samples <- coda.samples(jags,
                        c('a', 'b'),
                        1000)                        
gelman.plot(samples)

## ----eval=FALSE, echo=TRUE, fig.width = 12, fig.height= 12, fig.align="center"----
## library('rjags')
## source("../Cours02/DBDA2Eprograms/DBDA2E-utilities.R")
## 
## N <- 100
## x <- 1:N
## epsilon <- rnorm(N, 0, 1)
## y <- x + epsilon
## 
## jags <- jags.model('./Script/example.jags',
##                    data = list('x' = x,
##                                'y' = y,
##                                'N' = N),
##                    n.chains = 4,
##                    n.adapt = 1000)
## samples <- coda.samples(jags,
##                         c('a', 'b'),
##                         1000)
## diagMCMC( codaObject=samples )

## ----eval=TRUE, echo=FALSE, fig.width = 12, fig.height= 12, fig.align="center"----
library('rjags')
source("../Cours02/DBDA2Eprograms/DBDA2E-utilities.R")

N <- 100
x <- 1:N
epsilon <- rnorm(N, 0, 1)
y <- x + epsilon

jags <- jags.model('./Script/example.jags',
                   data = list('x' = x,
                               'y' = y,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 1000)
samples <- coda.samples(jags,
                        c('a', 'b'),
                        1000)
diagMCMC( codaObject=samples )


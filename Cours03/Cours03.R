## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(cache = TRUE)

## ----echo = TRUE---------------------------------------------------------
library(rethinking)
library(tidyverse)

data(Howell1)
d <- Howell1
str(d)

## ----echo = TRUE---------------------------------------------------------
d2 <- d %>% filter(age >= 18)
head(d2)

## ----echo = TRUE, fig.align = "center"-----------------------------------
d2 %>%
    ggplot(aes(x = height) ) +
    geom_histogram(bins = 10, col = "white") +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
rnorm(10000, 10, 1) %>%
    data.frame(value = .) %>%
    ggplot(aes(x = value) ) +
    geom_histogram(col = "white") +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 6----
ggplot(data.frame(x = c(100, 250) ), aes(x) ) +
        stat_function(
                fun = dnorm, args = list(mean = 178, sd = 20),
                col = "steelblue", lwd = 2) +
        theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 6----
ggplot(data.frame(x = c(-10, 60) ), aes(x) ) +
        stat_function(
                fun = dunif, args = list(0, 50),
                col = "steelblue", lwd = 2) +
        theme_bw(base_size = 20)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## library(ks)
## sample_mu <- rnorm(1e4, 178, 20) # prior on mu
## sample_sigma <- runif(1e4, 0, 50) # prior on sigma
## prior <- data.frame(cbind(sample_mu, sample_sigma) ) # multivariate prior
## H.scv <- Hscv(x = prior, verbose = TRUE)
## fhat_prior <- kde(x = prior, H = H.scv, compute.cont = TRUE)
## plot(
##     fhat_prior, display = "persp", col = "steelblue", border = NA,
##     xlab = "\nmu", ylab = "\nsigma", zlab = "\n\np(mu, sigma)",
##     shade = 0.8, phi = 30, ticktype = "detailed",
##     cex.lab = 1.2, family = "Helvetica")

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
sample_mu <- rnorm(1000, 178, 20)
sample_sigma <- runif(1000, 0, 50)

rnorm(1000, sample_mu, sample_sigma) %>%
    data.frame(x = .) %>%
    ggplot(aes(x) ) +
    geom_histogram() +
    xlab(expression(y[i]) ) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mu_exemple <- 151.23
sigma_exemple <- 23.42

d2$height[34] # one observation

## ----eval = TRUE, echo = FALSE, fig.align = "center"---------------------
ggplot(data.frame(x = c(50, 250) ), aes(x) ) +
    stat_function(
        fun = dnorm, args = list(mu_exemple, sigma_exemple), lwd = 2) +
    geom_segment(
        aes(
            x = d2$height[34],
            xend = d2$height[34],
            y = 0,
            yend = dnorm(d2$height[34], mu_exemple,sigma_exemple) ),
        color = "black", size = 1, linetype = 2) +
    geom_point(
        data = d2,
        aes(x = d2$height[34], y = dnorm(d2$height[34], mu_exemple,sigma_exemple) ),
        size = 4) +
    xlab("height") +
    ylab("density") +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
dnorm(d2$height[34], mu_exemple, sigma_exemple)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
normal_likelihood <- function(x, mu, sigma) {
    
    a1 <- (1 / (sigma * sqrt(2 * pi) ) )
    a2 <- exp( (- 1 / 2) * ( (x - mu) / sigma)^2 )
    a1 * a2

}

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
normal_likelihood(d2$height[34], mu_exemple, sigma_exemple)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
# defines a grid of possible values for mu and sigma
mu.list <- seq(from = 140, to = 160, length.out = 200)
sigma.list <- seq(from = 4, to = 9, length.out = 200)

post <- expand.grid(mu = mu.list, sigma = sigma.list)

# computes log-likelihood (of each observed height)
post$LL <-
    sapply(
        1:nrow(post),
        function(i) sum(dnorm(
            d2$height,
            mean = post$mu[i],
            sd = post$sigma[i],
            log = TRUE)
            )
        )

# computes unnormalised posterior probability
post$prod <-
        post$LL +
        dnorm(post$mu, 178, 20, log = TRUE) +
        dunif(post$sigma, 0, 50, log = TRUE)

post$prob <- exp(post$prod - max(post$prod) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 16, fig.height = 10----
library(viridis)

ggplot(
    data.frame(sample.mu,sample.sigma),
    aes(x = sample.mu, y = sample.sigma)
    ) + 
    stat_density_2d(
        geom = "raster", aes(fill = ..density..),
        contour = FALSE, show.legend = FALSE) +
    geom_vline(xintercept = mean(sample.mu), lty = 2) +
    geom_hline(yintercept = mean(sample.sigma), lty = 2) +
    scale_fill_viridis(na.value = "black") +
    coord_cartesian(xlim = c(min(sample.mu),max(sample.mu) ),
            ylim = c(min(sample.sigma),max(sample.sigma) ) ) +
    scale_x_continuous(expand = c(0, 0) ) +
    scale_y_continuous(expand = c(0, 0) ) +
    labs(x = expression(mu), y = expression(sigma) ) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
BEST::plotPost(sample.mu, breaks = 40)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
BEST::plotPost(sample.sigma, breaks = 40)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod1 <- map(alist( # alist ≠ list
        height ~ dnorm(mu, sigma),
        mu ~ dnorm(178, 20),
        sigma ~ dunif(0, 50) ),
        data = d2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod1, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod2 <- map(alist(
        height ~ dnorm(mu, sigma),
        mu ~ dnorm(178, 0.1),
        sigma ~ dunif(0, 50) ),
        data = d2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod2, prob = 0.95)

## ----eval = TRUE, echo = FALSE, fig.align = "center"---------------------
library(viridis)
library(MASS)

# Get density of points in 2 dimensions.
# @param x A numeric vector.
# @param y A numeric vector.
# @param n Create a square n by n grid to compute density.
# @return The density within each square.

get_density <- function(x, y, n = 100) {
    
    dens <- MASS::kde2d(x = x, y = y, n = n)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
    
}

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10, fig.height = 8----
post <-
    extract.samples(mod1, n = 1e4) %>%
    mutate(density = get_density(mu, sigma, n = 1e2) )

ggplot(post, aes(x = mu, y = sigma, color = density) ) +
    geom_point(alpha = 0.5, show.legend = FALSE) +
    theme_bw(base_size = 20) + 
    viridis::scale_color_viridis()

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
head(post)
precis(post[, 1:2], prob = 0.95)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## H.scv <- Hscv(post[, 1:2])
## fhat_post <- kde(x = post[, 1:2], H = H.scv, compute.cont = TRUE)
## 
## plot(
##     fhat_post, display = "persp", col = "purple", border = NA,
##     xlab = "\nmu", ylab = "\nsigma", zlab = "\np(mu, sigma)",
##     shade = 0.8, phi = 30, ticktype = "detailed",
##     cex.lab = 1.2, family = "Helvetica")

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
sample.mu <- post$mu
sample.sigma <- post$sigma

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 16, fig.height = 10----
library(viridis)

data.frame(sample.mu,sample.sigma) %>%
    ggplot(aes(x = sample.mu, y = sample.sigma) ) + 
    stat_density_2d(
        geom = "raster",
        aes(fill = ..density..),
        contour = FALSE, show.legend = FALSE
        ) +
    geom_vline(xintercept = mean(sample.mu), lty = 2) +
    geom_hline(yintercept = mean(sample.sigma), lty = 2) +
    scale_fill_viridis(na.value = "black") +
    coord_cartesian(
        xlim = c(min(sample.mu), max(sample.mu) ),
        ylim = c(min(sample.sigma), max(sample.sigma) )
        ) +
    scale_x_continuous(expand = c(0, 0) ) +
    scale_y_continuous(expand = c(0, 0) ) +
    labs(x = expression(mu), y = expression(sigma) ) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
d2 %>%
    ggplot(aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod3 <- lm(height ~ weight, data = d2)
precis(mod3, prob = 0.95)

## ----eval = TRUE, echo = FALSE, fig.align = "center"---------------------
d2 %>%
    ggplot(aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE, col = "black") +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod3 <- map(alist(
        height ~ dnorm(mu, sigma),
        mu <-  a + b * weight,
        a ~ dnorm(178, 20),
        b ~ dnorm(0, 10),
        sigma ~ dunif(0, 50) ),
        data = d2,
        start = list(a = mean(d2$height), sigma = sd(d2$height) ) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod3, prob = 0.95, corr = TRUE)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
d2$weight.c <- d2$weight - mean(d2$weight)

mod4 <- map(alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * weight.c,
        a ~ dnorm(178, 20),
        b ~ dnorm(0, 10),
        sigma ~ dunif(0, 50) ),
        data = d2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod4, prob = 0.95, corr = TRUE)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
d2 %>%
    ggplot(aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    geom_abline(intercept = coef(mod3)["a"], slope = coef(mod3)["b"]) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
weight.seq <- seq(from = 25, to = 70, by = 1)
mu <- link(mod3, data = data.frame(weight = weight.seq), refresh = 0)

mu.HPDI <- t(apply(mu, 2, HPDI, prob = 0.95) )

ggplot(data = d2, aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    geom_abline(intercept = coef(mod3)["a"], slope = coef(mod3)["b"]) +
    geom_ribbon(
        data = data.frame(mu.HPDI), inherit.aes = FALSE,
        aes(x = weight.seq, ymin = mu.HPDI[, 1], ymax = mu.HPDI[, 2] ), alpha = 0.2) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
post <- extract.samples(mod3)
mu.link <- function(weight) post$a + post$b * weight # << custom link function
mu <- sapply(weight.seq, mu.link)
mu.HPDI <- t(apply(mu, 2, HPDI, prob = 0.95) )

ggplot(data = d2, aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    geom_abline(intercept = coef(mod3)["a"], slope = coef(mod3)["b"]) +
    geom_ribbon(
        data = data.frame(mu.HPDI), inherit.aes = FALSE,
        aes(x = weight.seq, ymin = mu.HPDI[, 1], ymax = mu.HPDI[, 2] ), alpha = 0.2) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mu.mean <- apply(mu, 2, mean)
sim.height <- sim(mod3, data = list(weight = weight.seq), refresh = 0, n = 1e3)
height.HPDI <- t(apply(sim.height, 2, HPDI, prob = 0.95) )

## ----eval = TRUE, echo = FALSE, fig.align = "center"---------------------
ggplot(data = d2, aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    geom_abline(intercept = coef(mod3)["a"], slope = coef(mod3)["b"]) +
    geom_ribbon(
        data = data.frame(mu.HPDI), inherit.aes = FALSE,
        aes(x = weight.seq, ymin = mu.HPDI[, 1], ymax = mu.HPDI[, 2] ), alpha = 0.2) +
    geom_ribbon(
        data = data.frame(height.HPDI), inherit.aes = FALSE,
        aes(x = weight.seq, ymin = height.HPDI[, 1], ymax = height.HPDI[, 2] ), alpha = 0.1) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 6, fig.height = 6----
d %>% # on utilise d au lieu de d2
    ggplot(aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 6, fig.height = 6----
d <- d %>% mutate(weight.s = (weight - mean(weight) ) / sd(weight) )

d %>%
    ggplot(aes(x = weight.s, y = height) ) +
    geom_point(size = 2) +
    theme_bw(base_size = 20)

c(mean(d$weight.s), sd(d$weight.s) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
d$weight.s2 <- d$weight.s^2

mod5 <- rethinking::map(alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b1 * weight.s + b2 * weight.s2,
        a ~ dnorm(156, 100),
        b1 ~ dnorm(0, 10),
        b2 ~ dnorm(0, 10),
        sigma ~ dunif(0, 50) ),
        data = d)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod5, prob = 0.95, corr = TRUE)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
pred_dat <- list(weight.s = weight.seq, weight.s2 = weight.seq^2)
mu <- link(mod5, data = pred_dat, refresh = 0)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- t(apply(mu, 2, HPDI, prob = 0.95) )
sim.height <- sim(mod5, data = pred_dat, refresh = 0)
height.HPDI <- t(apply(sim.height, 2, HPDI, prob = 0.95) )

## ----eval = TRUE, echo = FALSE, fig.align = "center"---------------------
ggplot(data = d, aes(x = weight.s, y = height) ) +
    geom_point(size = 2) +
    geom_line(
        data = data.frame(mu.mean),
        aes(x = weight.seq, y = mu.mean),
        size = 1) +
    geom_ribbon(
        data = data.frame(mu.HPDI),
        inherit.aes = FALSE,
        aes(x = weight.seq, ymin = mu.HPDI[, 1], ymax = mu.HPDI[, 2] ), alpha = 0.2) +
    geom_ribbon(
        data = data.frame(height.HPDI),
        inherit.aes = FALSE,
        aes(x = weight.seq, ymin = height.HPDI[, 1], ymax = height.HPDI[, 2] ),
        alpha = 0.1) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
post <- extract.samples(mod3)
beta <- post$b
sigma <- post$sigma

f1 <- beta^2 * var(d2$weight)
rho <- f1 / (f1 + sigma^2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
BEST::plotPost(rho, showMode = TRUE)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
summary(lm(height ~ weight, data = d2) )$r.squared

## ------------------------------------------------------------------------
data(Howell1)

d <- 
    Howell1 %>%
    filter(age < 18) %>%
    mutate(weight.s = scale(weight) %>% as.numeric)

mod6 <- rethinking::map(alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * weight,
        a ~ dnorm(160, 100),
        b ~ dnorm(0, 10),
        sigma ~ dunif(0, 50) ),
        data = d)

precis(mod6, prob = 0.89)

## ---- fig.align = "center"-----------------------------------------------
weight.seq <- seq(from = 5, to = 45, length.out = 100)
post <- extract.samples(mod6)

# simulate mu then compute mean and hpdi
mu.link <- function(weight) post$a + post$b * weight # custom link function
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- t(apply(mu, 2, HPDI, prob = 0.89) )

# simulate heights then compute hpdi
sim.height <- sim(mod6, data = list(weight = weight.seq), refresh = 0, n = 1e3)
height.HPDI <- t(apply(sim.height, 2, HPDI, prob = 0.89) )

## ---- fig.align = "center"-----------------------------------------------
ggplot(data = d, aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    geom_abline(intercept = coef(mod6)["a"], slope = coef(mod6)["b"]) +
    geom_ribbon(
        data = data.frame(mu.HPDI), inherit.aes = FALSE,
        aes(x = weight.seq, ymin = mu.HPDI[, 1], ymax = mu.HPDI[, 2] ), alpha = 0.2) +
    geom_ribbon(
        data = data.frame(height.HPDI), inherit.aes = FALSE,
        aes(x = weight.seq, ymin = height.HPDI[, 1], ymax = height.HPDI[, 2] ), alpha = 0.1) +
    theme_bw(base_size = 20)

## ------------------------------------------------------------------------
d <- Howell1

mod7 <- rethinking::map(alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * log(weight),
        a ~ dnorm(160, 50),
        b ~ dnorm(0, 10),
        sigma ~ dunif(0, 50) ),
        data = d)

precis(mod7, prob = 0.95)

## ---- fig.align = "center"-----------------------------------------------
weight.seq <- seq(from = 5, to = 70, length.out = 100)
post <- extract.samples(mod7)

# simulate mu then compute mean and hpdi
mu.link <- function(weight) post$a + post$b * log(weight) # custom link function
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- t(apply(mu, 2, HPDI, prob = 0.89) )

# simulate heights then compute hpdi
sim.height <-
    sapply(
        weight.seq,
        function(weight)
            rnorm(
                n = nrow(post),
                mean = post$a + post$b * log(weight),
                sd = post$sigma)
    )

height.HPDI <- t(apply(sim.height, 2, HPDI, prob = 0.89) )

## ---- fig.align = "center"-----------------------------------------------
ggplot(data = d, aes(x = weight, y = height) ) +
    geom_point(size = 2) +
    geom_line(data = data.frame(weight = weight.seq, height = mu.mean) ) +
    geom_ribbon(
        data = data.frame(mu.HPDI), inherit.aes = FALSE,
        aes(x = weight.seq, ymin = mu.HPDI[, 1], ymax = mu.HPDI[, 2] ), alpha = 0.2) +
    geom_ribbon(
        data = data.frame(height.HPDI), inherit.aes = FALSE,
        aes(x = weight.seq, ymin = height.HPDI[, 1], ymax = height.HPDI[, 2] ), alpha = 0.1) +
    theme_bw(base_size = 20)


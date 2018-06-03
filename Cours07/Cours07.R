## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(cache = TRUE)
library(tidyverse)
set.seed(671)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
men <- rnorm(100, 175, 10) # 100 tailles d'hommes
women <- rnorm(100, 170, 10) # 100 tailles de femmes

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
t.test(men, women)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
nSims <- 1e4 # number of simulations
t <- rep(NA, nSims) # initialising an empty vector

for (i in 1:nSims) {
    
    men2 <- rnorm(100, 170, 10)
    women2 <- rnorm(100, 170, 10)
    t[i] <- t.test(men2, women2)$statistic
    
}

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
t <- replicate(nSims, t.test(rnorm(100, 170, 10), rnorm(100, 170, 10) )$statistic)

## ----eval = TRUE, echo = TRUE, fig.align = "center", message = FALSE-----
data.frame(t = t) %>%
    ggplot(aes(x = t) ) +
    geom_histogram() +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
data.frame(x = c(-5, 5) ) %>%
    ggplot(aes(x = x) ) +
    stat_function(fun = dt, args = list(df = t.test(men, women)$parameter), size = 1.5) +
    theme_bw(base_size = 20)

abs(qt(0.05 / 2, df = t.test(men, women)$parameter) ) # two-sided critical t-value

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
alpha <- .05
abs(qt(alpha / 2, df = t.test(men, women)$parameter) ) # two-sided critical t-value

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 9, fig.height = 9----
data.frame(t = c(-5, 5) ) %>%
    ggplot(aes(x = t) ) +
    stat_function(fun = dt, args = list(df = t.test(men, women)$parameter), size = 1.5) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(-5, qt(0.025, df = t.test(men, women)$parameter) ),
        geom = "area", alpha = 0.5
        ) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(qt(0.975, df = t.test(men, women)$parameter), 5),
        geom = "area", alpha = 0.5
        ) +
    theme_bw(base_size = 20) + ylab("density")

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
tobs <- t.test(men, women)$statistic # observed t-value
tobs %>% as.numeric

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 9, fig.height = 9----
data.frame(t = c(-5, 5) ) %>%
    ggplot(aes(x = t) ) +
    stat_function(fun = dt, args = list(df = t.test(men, women)$parameter), size = 1.5) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(-5, qt(0.025, df = t.test(men, women)$parameter) ),
        geom = "area", alpha = 0.5
            ) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(qt(0.975, df = t.test(men, women)$parameter), 5),
        geom = "area", alpha = 0.5
        ) +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(-5, - tobs),
        geom = "area") +
    stat_function(
        fun = dt, args = list(df = t.test(men, women)$parameter),
        xlim = c(tobs, 5),
        geom = "area") +
    theme_bw(base_size = 20) + ylab("density")

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
t.test(men, women)$p.value

tvalue <- abs(t.test(men, women)$statistic)
df <- t.test(men, women)$parameter

2 * integrate(dt, tvalue, Inf, df = df)$value

2 * (1 - pt(abs(t.test(men, women)$statistic), t.test(men, women)$parameter) )

## ---- echo = FALSE, fig.align = "center", fig.width = 12, fig.height = 6----
data.frame(x = seq(from = 0, to = 1, length.out = 1e2) ) %>%
    mutate(M1 = dbeta(x, 6, 10), M2 = dbeta(x, 20, 12) ) %>%
    gather(prior, value, M1:M2) %>%
    ggplot(aes(x = x, y = value, fill = prior) ) +
    geom_area(alpha = 0.75, position = "identity") +
    scale_fill_manual(values = c("#016392", "#c72e29") ) +
    xlab(expression(paste("probability of heads ", theta) ) ) +
    ylab("density") +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE--------------------------------------------
library(BayesFactor)
ttestBF(men, women)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
data.frame(x = c(-10, 10) ) %>%
    ggplot(aes(x = x) ) +
    stat_function(
        fun = dcauchy,
        args = list(location = 0, scale = sqrt(2) / 2), size = 1.5) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 9, fig.height = 7----
data.frame(x = c(-20, 20) ) %>%
    ggplot(aes(x = x) ) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 2), size = 1.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 5), size = 1.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 10), size = 1.5) +
    theme_bw(base_size = 20) +
    xlab(expression(theta) ) +
    ylab("")

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 9, fig.height = 7, message = FALSE----
ppnames <- c("afarensis","africanus","habilis","boisei",
        "rudolfensis","ergaster","sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)

d <- data.frame(species = ppnames, brain = brainvolcc, mass = masskg)

d %>%
    ggplot(aes(x = mass, y = brain, label = species) ) +
    geom_point() +
    ggrepel::geom_label_repel(hjust = 0, nudge_y = 50, size = 5) +
    theme_bw(base_size = 20) + xlim(30, 70)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod1.1 <- lm(brain ~ mass, data = d)
(var(d$brain) - var(residuals(mod1.1) ) ) / var(d$brain)

mod1.2 <- lm(brain ~ mass + I(mass^2), data = d)
(var(d$brain) - var(residuals(mod1.2) ) ) / var(d$brain)

mod1.3 <- lm(brain ~ mass + I(mass^2) + I(mass^3), data = d)
(var(d$brain) - var(residuals(mod1.3) ) ) / var(d$brain)

## ----eval = TRUE, echo = FALSE, fig.align = "center"---------------------
mod1.4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), data = d)

mod1.5 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
    I(mass^5), data = d)

mod1.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) +
    I(mass^5) + I(mass^6), data = d)

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 20, fig.height = 12----
library(gridExtra)
library(ggplot2)

p <- list()

for (i in 1:6) {
    
    p[[i]] <-
        ggplot(data = d, aes(x = mass, y = brain) ) +
        geom_point() +
        theme_bw(base_size = 20) +
        ylim(-400, 2000) +
        ggtitle(bquote(R^'2'~'='~.(round(summary(get(paste0("mod1.",i) ) )$r.squared, 2) ) ) ) +
        geom_line(
            data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
                mutate(pred = predict(get(paste0("mod1.",i) ), newdata = .) ), aes(x = mass, y = pred) ) +
        geom_hline(yintercept = 0, linetype = 2)
    
}

do.call(grid.arrange, p)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod1.7 <- lm(brain ~ 1, data = d)

## ----eval = TRUE, echo = FALSE, fig.align = "center"---------------------
ggplot(data = d, aes(x = mass, y = brain) ) +
        geom_point() +
        theme_bw(base_size = 20) +
        ylim(-400, 2000) +
        ggtitle(bquote(R^'2'~'='~.(round(summary(mod1.7)$r.squared, 2) ) ) ) +
        geom_line(
            data = data.frame(mass = seq(min(d$mass), max(d$mass), length.out = 100) ) %>%
                mutate(pred = predict(mod1.7, newdata = .) ), aes(x = mass, y = pred) ) +
        geom_hline(yintercept = 0, linetype = 2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
p <- c(0.3, 0.7)
- sum(p * log(p) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
p <- c(0.01, 0.99)
- sum(p * log(p) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
p <- c(0.3, 0.7)
q <- c(0.25, 0.75)

sum(p * log(p / q) )
sum(q * log(q / p) )

## ----eval = FALSE, echo = TRUE-------------------------------------------
## sum(p * (log(q) ) )

## ----eval = TRUE, echo = TRUE--------------------------------------------
- sum (p * (log(q) - log(p) ) )

## ----echo = FALSE, fig.align = "center"----------------------------------
knitr::include_graphics("mind_blowing.jpg")

## ----echo = FALSE, fig.align = "center"----------------------------------
knitr::include_graphics("KL_distance.png")

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
d$mass.s <- scale(d$mass)
mod1.8 <- lm(brain ~ mass.s, data = d)

-2 * logLik(mod1.8) # compute deviance

## ----eval = TRUE, echo = TRUE--------------------------------------------
# extracting model's coefficients

alpha <- coef(mod1.8)[1]
beta <- coef(mod1.8)[2]

# computing the log-likelihood

ll <- sum(dnorm(
    d$brain,
    mean = alpha + beta * d$mass.,
    sd = sd(residuals(mod1.8) ),
    log = TRUE)
    )

# computing the deviance

(-2) * ll

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 12, fig.height = 8----
data.frame(x = c(-3, 3) ) %>%
    ggplot(aes(x = x) ) +
    stat_function(
        fun = dnorm, args = list(mean = 0, sd = 0.25),
        size = 1.5, linetype = 1) +
    stat_function(
        fun = dnorm, args = list(mean = 0, sd = 0.5),
        size = 1.5, linetype = 2) +
    stat_function(
        fun = dnorm, args = list(mean = 0, sd = 1),
        size = 1.5, linetype = 3) +
    theme_bw(base_size = 20) +
    xlab(expression(theta) ) +
    ylab("density")

## ----echo = FALSE, fig.align = "center", out.width = "400px"-------------
knitr::include_graphics("mind_blowing2.gif")

## ----eval = TRUE, echo = TRUE--------------------------------------------
library(rethinking)
data(cars)

m <- rethinking::map(alist(
        dist ~ dnorm(mu, sigma),
        mu <- a + b * speed,
        a ~ dnorm(0, 100),
        b ~ dnorm(0, 10),
        sigma ~ dunif(0, 30) ),
        data = cars
    )

post <- extract.samples(m, n = 1e3)

head(post)

## ----eval = TRUE, echo = TRUE--------------------------------------------
ll <- sapply(
    1:nrow(post),
    function(s) {
        
        mu <- post$a[s] + post$b[s] * cars$speed
        dnorm(cars$dist, mu, post$sigma[s], log = TRUE)
        
        }
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
lppd <- sapply(1:nrow(cars), function(i) log_sum_exp(ll[i, ]) - log(nrow(post) ) )

sum(lppd)

## ----eval = TRUE, echo = TRUE--------------------------------------------
pWAIC <- sapply(1:nrow(cars), function(i) var(ll[i, ]) )

## ----eval = TRUE, echo = TRUE--------------------------------------------
(WAIC <- (-2) * (sum(lppd) - sum(pWAIC) ) )

## ----eval = TRUE, echo = TRUE--------------------------------------------
data(milk)

d <- milk[complete.cases(milk), ] # removing NAs
d$neocortex <- d$neocortex.perc / 100 # rescaling explanatory variable

head(d)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g) )

mod2.1 <- rethinking::map(alist(
        kcal.per.g ~ dnorm(a, exp(log.sigma) ) ),
        data = d,
        start = list(a = a.start, log.sigma = sigma.start) )

mod2.2 <- rethinking::map(alist(
        kcal.per.g ~ dnorm(mu, exp(log.sigma) ),
        mu <- a + bn * neocortex),
        data = d,
        start = list(a = a.start, bn = 0, log.sigma = sigma.start) )

mod2.3 <- rethinking::map(alist(
        kcal.per.g ~ dnorm(mu, exp(log.sigma) ),
        mu <- a + bm * log(mass) ),
        data = d,
        start = list(a = a.start, bm = 0, log.sigma = sigma.start) )

mod2.4 <- rethinking::map(alist(
        kcal.per.g ~ dnorm(mu, exp(log.sigma) ),
        mu <- a + bn * neocortex + bm * log(mass) ),
        data = d,
        start = list(a = a.start, bn = 0, bm = 0, log.sigma = sigma.start) )

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10------
(milk.models <- compare(mod2.1, mod2.2, mod2.3, mod2.4) )
plot(milk.models, SE = TRUE, dSE = TRUE)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10------
coeftab(mod2.1, mod2.2, mod2.3, mod2.4)
plot(coeftab(mod2.1, mod2.2, mod2.3, mod2.4) )

## ----eval = TRUE, echo = TRUE, results = "hide", fig.align = "center"----
nc.seq <- seq(from = 0.5, to = 0.8, length.out = 30)

d.predict <- list(
    kcal.per.g = rep(0, 30), # empty outcome
    neocortex = nc.seq,      # sequence of neocortex
    mass = rep(4.5, 30)      # average mass
    )

milk.ensemble <- ensemble(mod2.1, mod2.2, mod2.3, mod2.4, data = d.predict, refresh = 0)
mu <- apply(milk.ensemble$link, 2, mean)
mu.HPDI <- t(apply(milk.ensemble$link, 2, HPDI) )

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 10, fig.height = 8----
d %>%
    ggplot(aes(x = neocortex, y = kcal.per.g) ) +
    geom_point() +
    geom_line(data = data.frame(nc.seq = nc.seq, mu = mu), aes(x = nc.seq, y = mu) ) +
    geom_ribbon(
        data = data.frame(mu.HPDI = mu.HPDI, nc.seq = nc.seq),
        aes(x = nc.seq, ymin = mu.HPDI[, 1], ymax = mu.HPDI[, 2]),
        inherit.aes = FALSE, alpha = 0.2
        ) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
data(Howell1)

d <-
    Howell1 %>%
    mutate(age = scale(age) )

set.seed(1000)
i <- sample(1:nrow(d), size = nrow(d) / 2)

d1 <- d[i, ] # training sample
d2 <- d[-i, ] # test sample

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
alpha.start <- mean(d1$height)
sigma.start <- sd(d1$height)

mod3.1 <- rethinking::map(alist(
        height ~ dnorm(mean = mu, sd = sigma),
        mu <- alpha + beta.1*age,
        c(alpha, beta.1) ~ dnorm(mean = 0, sd = 100),
        sigma ~ dunif(min = 0, max = 50) ),
        data = d1,
        start = list(alpha = alpha.start) )

mod3.2 <- rethinking::map(alist(
        height ~ dnorm(mean = mu, sd = sigma),
        mu <- alpha + beta.1*age + beta.2*age^2,
        c(alpha, beta.1, beta.2) ~ dnorm(mean = 0, sd = 100),
        sigma ~ dunif(min = 0, max = 50) ),
        data = d1,
        start = list(alpha = alpha.start) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod3.3 <- rethinking::map(alist(
        height ~ dnorm(mean = mu, sd = sigma),
        mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3,
        c(alpha, beta.1, beta.2, beta.3) ~ dnorm(mean = 0, sd = 100),
        sigma ~ dunif(min = 0, max = 50) ),
        data = d1,
        start = list(alpha = alpha.start) )

mod3.4 <- rethinking::map(alist(
        height ~ dnorm(mean = mu, sd = sigma),
        mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 + beta.4*age^4,
        c(alpha, beta.1, beta.2, beta.3, beta.4) ~ dnorm(mean = 0, sd = 100),
        sigma ~ dunif(min = 0, max = 50) ),
        data = d1,
        start = list(alpha = alpha.start) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod3.5 <- rethinking::map(alist(
        height ~ dnorm(mean = mu, sd = sigma),
        mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 + 
                beta.4*age^4 + beta.5*age^5,
        c(alpha, beta.1, beta.2, beta.3, beta.4, beta.5) ~ dnorm(mean = 0, sd = 100),
        sigma ~ dunif(min = 0, max = 50) ),
        data = d1,
        start = list(alpha = alpha.start) )

mod3.6 <- rethinking::map(alist(
        height ~ dnorm(mean = mu, sd = sigma),
        mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 +
                beta.4*age^4 +
                beta.5*age^5 + beta.6*age^6,
        c(alpha, beta.1, beta.2, beta.3, beta.4, beta.5, beta.6) ~dnorm(mean = 0, sd = 100),
        sigma ~ dunif(min = 0, max = 50) ),
        data = d1,
        start = list(alpha = alpha.start) )

## ----eval = TRUE, echo = TRUE--------------------------------------------
compare(mod3.1, mod3.2, mod3.3, mod3.4, mod3.5, mod3.6)

## ----eval = TRUE, echo = TRUE--------------------------------------------
n.trials <- 1e4
age.seq <- seq(from = -2, to = 3.5, length.out = 58)
prediction.data <- data.frame(age = age.seq)

computeMu <- function(model, data, n.trials) {
    
    mu <- link(fit = model, data = data, n = n.trials)
    return(mu)

}

computeMuMean <- function(mu) {
    
    mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
    return(mu.mean)
    
}

computeMuHPDI <- function(mu) {
    
    mu.HPDI <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = 0.97)
    return(mu.HPDI)
    
}

## ----eval = TRUE, echo = TRUE--------------------------------------------
simulateHeights <- function(model, prediction.data) {
    
    simulated.heights <- sim(fit = model, data = prediction.data)
    return(simulated.heights)
    
}

plotResults <- function(model, prediction.data, original.data, n.trials) {
    
    mu <- computeMu(model, prediction.data, n.trials)
    mu.mean <- computeMuMean(mu)
    mu.HPDI <- computeMuHPDI(mu)
    simulated.heights <- simulateHeights(model = model, prediction.data = prediction.data)
    simulated.heights.HPDI <- apply(X = simulated.heights, MARGIN = 2, FUN = HPDI)
    plot(height ~ age, data = original.data, col = "steelblue", pch = 16)
    lines(x = prediction.data$age, y = mu.mean, lty = 2)
    lines(x = prediction.data$age, y = mu.HPDI[1, ], lty = 2)
    lines(x = prediction.data$age, y = mu.HPDI[2, ], lty = 2)
    shade(object = simulated.heights.HPDI, lim = prediction.data$age)
    
}

## ----eval = FALSE, echo = TRUE-------------------------------------------
## plotResults(
##     model = mod3.1, prediction.data = prediction.data,
##     original.data = d1, n.trials = n.trials
##     )
## 
## plotResults(
##     model = mod3.2, prediction.data = prediction.data,
##     original.data = d1, n.trials = n.trials
##     )
## 
## plotResults(
##     model = mod3.3, prediction.data = prediction.data,
##     original.data = d1, n.trials = n.trials
##     )
## 
## plotResults(
##     model = mod3.4, prediction.data = prediction.data,
##     original.data = d1, n.trials = n.trials
##     )
## 
## plotResults(
##     model = mod3.5, prediction.data = prediction.data,
##     original.data = d1, n.trials = n.trials
##     )
## 
## plotResults(
##     model = mod3.6, prediction.data = prediction.data,
##     original.data = d1, n.trials = n.trials
##     )

## ----eval = TRUE, echo = TRUE, results = "hide", fig.align = "center", fig.height = 5----
h.ensemble <- ensemble(mod3.4, mod3.5, mod3.6, data = list(age = age.seq) )
mu.mean <- apply(h.ensemble$link, 2, mean)
mu.ci <- t(apply(h.ensemble$link, 2, HPDI) )
height.ci <- t(apply(h.ensemble$sim, 2, HPDI) )

ggplot(data = d1, aes(x = as.numeric(age), y = height) ) +
    geom_point(size = 2) +
    geom_line(data = data.frame(age = age.seq, height = mu.mean) ) +
    geom_ribbon(
        data = data.frame(mu.ci), inherit.aes = FALSE,
        aes(x = age.seq, ymin = mu.ci[, 1], ymax = mu.ci[, 2]), alpha = 0.2) +
    geom_ribbon(
        data = data.frame(height.ci), inherit.aes = FALSE,
        aes(x = age.seq, ymin = height.ci[, 1], ymax = height.ci[, 2]), alpha = 0.1) +
    theme_bw(base_size = 20) + xlab("age") + xlim(-2, 3) + ylim(60, 190)

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
# model 1
coefs <- coef(mod3.1)
mu <- coefs["alpha"] + coefs["beta.1"] * d2$age
log.likelihood <- sum(dnorm(x = d2$height, mean = mu, sd = coefs["sigma"], log = TRUE) )
dev.mod3.1 <- -2 * log.likelihood

# model 2
coefs <- coef(mod3.2)
mu <- coefs["alpha"] + coefs["beta.1"] * d2$age + coefs["beta.2"] * (d2$age)^2
log.likelihood <- sum(dnorm(x = d2$height, mean = mu, sd = coefs["sigma"], log = TRUE) )
dev.mod3.2 <- -2 * log.likelihood

# model 3
coefs <- coef(mod3.3)
mu <- coefs["alpha"] + coefs["beta.1"] * d2$age + coefs["beta.2"] * (d2$age)^2 + coefs["beta.3"] * (d2$age)^3
log.likelihood <- sum(dnorm(x = d2$height, mean = mu, sd = coefs["sigma"], log = TRUE) )
dev.mod3.3 <- -2 * log.likelihood

## ----eval = TRUE, echo = TRUE, results = "hide", fig.align = "center"----
# model 4
coefs <- coef(mod3.4)
mu <- coefs["alpha"] + coefs["beta.1"]*d2$age + coefs["beta.2"]*(d2$age)^2 + coefs["beta.3"]*(d2$age)^3 + coefs["beta.4"]*(d2$age)^4
log.likelihood <- sum(dnorm(x = d2$height, mean = mu, sd = coefs["sigma"], log = TRUE) )
dev.mod3.4 <- -2 * log.likelihood

# model 5
coefs <- coef(mod3.5)
mu <- coefs["alpha"] + coefs["beta.1"]*d2$age + coefs["beta.2"]*(d2$age)^2 + coefs["beta.3"]*(d2$age)^3 + coefs["beta.4"]*(d2$age)^4 + coefs["beta.5"]*(d2$age)^5
log.likelihood <- sum(dnorm(x = d2$height, mean = mu, sd = coefs["sigma"], log = TRUE) )
dev.mod3.5 <- -2 * log.likelihood

# model 6
coefs <- coef(mod3.6)
mu <- coefs["alpha"] + coefs["beta.1"]*d2$age + coefs["beta.2"]*(d2$age)^2 + coefs["beta.3"]*(d2$age)^3 + coefs["beta.4"]*(d2$age)^4 + coefs["beta.5"]*(d2$age)^5 + coefs["beta.6"]*(d2$age)^6
log.likelihood <- sum(dnorm(x = d2$height, mean = mu, sd = coefs["sigma"], log = TRUE) )
dev.mod3.6 <- -2 * log.likelihood

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10, fig.height = 6----
deviances <- c(dev.mod3.1,dev.mod3.2,dev.mod3.3,dev.mod3.4,dev.mod3.5,dev.mod3.6)
comparison <- compare(mod3.1,mod3.2,mod3.3,mod3.4,mod3.5,mod3.6)
comparison <- as.data.frame(comparison@output)
comparison <- comparison[order(rownames(comparison) ), ]
waics <- comparison$WAIC

data.frame(deviance = deviances, waic = waics) %>%
    gather(type, value) %>%
    mutate(x = rep(1:6, 2) ) %>%
    ggplot(aes(x = x, y = value, colour = type) ) +
    scale_colour_grey() +
    geom_point(size = 2) +
    scale_x_continuous(breaks = 1:6) +
    theme_bw(base_size = 20) + xlab("model") + ylab("Déviance/WAIC")


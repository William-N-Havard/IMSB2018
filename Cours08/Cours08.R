## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(cache = TRUE)
library(tidyverse)
set.seed(666)

## ----eval = TRUE, echo = TRUE--------------------------------------------
d <- read.csv("robot.csv")
head(d, 15)

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
library(rethinking)

mod1 <- map2stan(alist(
    wait ~ dnorm(mu, sigma),
    mu <- a,
    a ~ dnorm(5, 10),
    sigma ~ dcauchy(0, 2) ),
    data = d,
    warmup = 2000, iter = 5000,
    chains = 2, cores = parallel::detectCores()
    )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
ggplot(data = data.frame(x = c(0, 10) ), aes(x = x) ) +
    stat_function(
        fun = dcauchy,
        args = list(location = 0, scale = 2), size = 1.5
        ) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "right", fig.width = 16, fig.height = 6----
plot(mod1)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod1, prob = 0.95)

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
mod2 <- map2stan(alist(
    wait ~ dnorm(mu, sigma),
    mu <- a_cafe[cafe],
    a_cafe[cafe] ~ dnorm(5, 10),
    sigma ~ dcauchy(0, 2) ),
    data = d,
    warmup = 2000, iter = 5000,
    chains = 2, cores = parallel::detectCores()
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
precis(mod2, prob = 0.95, depth = 2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
y1 <- rnorm(1e4, 10, 1)
y2 <- 10 + rnorm(1e4, 0, 1)

dens(y1)
dens(y2, add = TRUE)

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
mod3 <- map2stan(alist(
    wait ~ dnorm(mu, sigma),
    mu <- a_cafe[cafe],
    a_cafe[cafe] ~ dnorm(a, sigma_cafe),
    a ~ dnorm(5, 10),
    sigma ~ dcauchy(0, 2),
    sigma_cafe ~ dcauchy(0, 2) ),
    data = d,
    warmup = 2000, iter = 5000,
    chains = 2, cores = parallel::detectCores() )

## ----echo = FALSE, fig.align = "center", fig.width = 15, fig.height = 10----
post <- extract.samples(mod3)

d %>%
    group_by(cafe) %>%
    summarise(raw = mean(wait) ) %>%
    mutate(estimate = apply(post$a_cafe, 2, mean) ) %>%
    gather(type, raw, raw:estimate) %>%
    ggplot(aes(x = cafe, y = raw, color = type) ) +
    geom_point(size = 5, show.legend = TRUE) +
    geom_hline(yintercept = mean(post$a), linetype = 2) +
    scale_color_manual(values = rev(wesanderson::wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_x_continuous(name = "café", breaks = 1:20) +
    theme_bw(base_size = 20) +
    ylab("temps d'attente (en minutes)") +
    theme(legend.title = element_blank() )

## ----eval = TRUE, echo = TRUE--------------------------------------------
compare(mod1, mod2, mod3)

## ----eval = TRUE, echo = TRUE--------------------------------------------
precis(mod1, prob = 0.95)
precis(mod3, prob = 0.95)

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
d2 <- read.csv("robot_inequal.csv")

mod4 <- map2stan(alist(
    wait ~ dnorm(mu, sigma),
    mu <- a_cafe[cafe],
    a_cafe[cafe] ~ dnorm(a, sigma_cafe),
    a ~ dnorm(5, 10),
    sigma ~ dcauchy(0, 2),
    sigma_cafe ~ dcauchy(0, 2) ),
    data = d2,
    warmup = 2000, iter = 5000,
    chains = 2, cores = parallel::detectCores() )

## ----echo = FALSE, fig.align = "center", fig.width = 16, fig.height = 10----
post <- extract.samples(mod4)

d2 %>%
    group_by(cafe) %>%
    summarise(raw = mean(wait) ) %>%
    mutate(estimate = apply(post$a_cafe, 2, mean) ) %>%
    mutate(visits = rep(c(6, 12, 24, 48), each = 5) ) %>%
    gather(type, raw, raw:estimate) %>%
    ggplot(aes(x = cafe, y = raw, color = type) ) +
    geom_point(size = 5, show.legend = TRUE) +
    geom_hline(yintercept = mean(post$a), linetype = 2) +
    scale_color_manual(values = rev(wesanderson::wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_x_continuous(name = "café", breaks = 1:20) +
    geom_vline(xintercept = c(5.5, 10.5, 15.5) ) +
    theme_bw(base_size = 20) +
    ylab("temps d'attente (en minutes)") +
    theme(legend.title = element_blank() )

## ----eval = TRUE, echo = TRUE--------------------------------------------
sigma_a <- 1
sigma_b <- 0.75
rho <- 0.7
cov_ab <- sigma_a * sigma_b * rho
(Sigma1 <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol = 2) )

## ----eval = TRUE, echo = TRUE--------------------------------------------
(sigmas <- c(sigma_a, sigma_b) ) # standard deviations
(Rho <- matrix(c(1, rho, rho, 1), nrow = 2) ) # correlation matrix
(Sigma2 <- diag(sigmas) %*% Rho %*% diag(sigmas) )

## ---- echo = FALSE, fig.align = "center", fig.width = 8, fig.height = 8, cache = TRUE----
nsims <- 1e6

data.frame(
    zeta05 = rethinking::rlkjcorr(nsims, K = 2, eta = 0.5)[, 1, 2],
    zeta1 = rethinking::rlkjcorr(nsims, K = 2, eta = 1)[, 1, 2],
    zeta5 = rethinking::rlkjcorr(nsims, K = 2, eta = 5)[, 1, 2],
    zeta50 = rethinking::rlkjcorr(nsims, K = 2, eta = 10)[, 1, 2] ) %>%
    gather(shape, y, zeta05:zeta50) %>%
    ggplot(aes(x = y, linetype = shape) ) +
    geom_line(stat = "density", position = "identity", size = 0.8, alpha = 1) +
    xlab(expression(rho) ) +
    ylab("density") +
    theme_bw(base_size = 20) +
    scale_linetype_manual(
        values = c("dotted", "dotdash", "dashed", "solid"),
        labels = c(
            expression(paste(zeta, " = ", "0.5") ),
            expression(paste(zeta, " = ", "1") ),
            expression(paste(zeta, " = ", "10") ),
            expression(paste(zeta, " = ", "50") ) ) ) +
    theme(
        legend.text.align = 0,
        legend.position = c(0.75, 0.8),
        legend.background = element_rect(size = 0.5, colour = "black")
        )

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
mod5 <- map2stan(alist(
        wait ~ dnorm(mu, sigma),
        mu <- a_cafe[cafe] + b_cafe[cafe] * afternoon,
        c(a_cafe, b_cafe)[cafe] ~ dmvnorm2(c(a, b), sigma_cafe, Rho),
        a ~ dnorm(0, 10),
        b ~ dnorm(0, 10),
        sigma_cafe ~ dcauchy(0, 2),
        sigma ~ dcauchy(0, 2),
        Rho ~ dlkjcorr(2) ),
        data = d,
        iter = 5000, warmup = 2000,
        chains = 2, cores = parallel::detectCores() )

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10, fig.height = 8----
post <- extract.samples(mod5)
R <- rlkjcorr(6000, K = 2, eta = 2)

data.frame(prior = R[, 1, 2], posterior = post$Rho[, 1, 2]) %>%
    gather(type, value, prior:posterior) %>%
    ggplot(aes(x = value, color = type, fill = type) ) +
    geom_histogram(position = "identity", alpha = 0.2) +
    theme_bw(base_size = 20) +
    xlab("correlation")

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 14, fig.height = 11----
a1 <- sapply(1:20, function(i) mean(d$wait[d$cafe == i & d$afternoon == 0]) )
b1 <- sapply(1:20, function(i) mean(d$wait[d$cafe == i & d$afternoon == 1]) ) - a1

no_pooling <-
    data.frame(intercept = a1, slope = b1) %>%
    mutate(model = "no pooling")

partial_pooling <-
    data.frame(
        intercept = apply(post$a_cafe, 2, mean),
        slope = apply(post$b_cafe, 2, mean)
        ) %>%
    mutate(model = "partial pooling")

shrinkage <- bind_rows(no_pooling, partial_pooling)

mu <- c(mean(post$a), mean(post$b) )
rho <- mean(post$Rho[, 1, 2] )
sda <- mean(post$sigma_cafe[, 1])
sdb <- mean(post$sigma_cafe[, 2])
cov_ab <- sda * sdb * rho
sigma <- matrix(c(sda^2, cov_ab, cov_ab, sdb^2), ncol = 2)

###########################################################################################
# Helper function to make ellipse, credits to Tristan Mahr
# https://tjmahr.github.io/plotting-partial-pooling-in-mixed-effects-models/
##################################################################################

library(ellipse)

make_ellipse <- function(cov_mat, center, level) {
    
    ellipse(cov_mat, centre = center, level = level) %>%
        as.data.frame() %>%
        add_column(level = level)
    
}

levels <- c(.1, .3, .5, .7)

df_ellipse <-
    levels %>%
    purrr::map_df(~ make_ellipse(sigma, mu, level = .x) ) %>% 
    rename(intercept = x, slope = y)

shrinkage %>%
    mutate(id = rep(1:20, 2) ) %>%
    ggplot(aes(x = intercept, y = slope, color = model) ) +
    scale_color_manual(values = wesanderson::wes_palette(n = 2, name = "Chevalier1") ) +
    geom_point(size = 5, show.legend = FALSE) +
    # connecting lines
    geom_path(
        aes(group = id, color = NULL),
        arrow = arrow(length = unit(.015, "npc"), type = "closed"), 
        show.legend = FALSE
        ) +
    # ellipses
    geom_path(
        aes(group = level, color = NULL),
        data = df_ellipse,
        linetype = "dashed", color = "grey40", alpha = 0.8
        ) +
    labs(x = "intercept", y = "slope") +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10, fig.height = 10----
plot(precis(mod5, depth = 2) )

## ----eval = TRUE, echo = TRUE--------------------------------------------
compare(mod1, mod2, mod3, mod5)

## ----eval = TRUE, echo = TRUE, warning = FALSE---------------------------
precis(mod1, prob = 0.95)
precis(mod3, prob = 0.95)
precis(mod5, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
library(lme4)

data(sleepstudy)
head(sleepstudy, 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 14, fig.height = 8----
sleepstudy %>%
    ggplot(aes(x = Days, y = Reaction) ) +
    geom_smooth(method = "lm", colour = "black") +
    geom_point() +
    facet_wrap(~Subject, nrow = 2) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
fmod0 <- lm(Reaction ~ Days, sleepstudy)
fmod1 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
fmod2 <- lmer(Reaction ~ Days + (1 + Days|Subject), sleepstudy)

anova(fmod1, fmod2)

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
mod6 <- map2stan(alist(
        Reaction ~ dnorm(mu,sigma),
        mu <- a + b_Days * Days,
        a ~ dnorm(200, 100),
        b_Days ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 10) ),
        data = sleepstudy,
        iter = 5000, warmup = 2000,
        chains = 2, cores = parallel::detectCores() )

## ----eval = TRUE, echo = TRUE--------------------------------------------
precis(mod6, prob = 0.95)

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
mod7 <- map2stan(alist(
        Reaction ~ dnorm(mu,sigma),
        mu <- a_Subject[Subject] + b_Days * Days,
        a_Subject[Subject] ~ dnorm(a, sigma_Subject),
        a ~ dnorm(200, 100),
        b_Days ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 10),
        sigma_Subject ~ dcauchy(0, 10) ),
        data = sleepstudy,
        iter = 5000, warmup = 2000,
        chains = 2, cores = parallel::detectCores() )

## ----eval = TRUE, echo = TRUE--------------------------------------------
precis(mod7, prob = 0.95)

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
mod8 <- map2stan(alist(
        Reaction ~ dnorm(mu, sigma),
        mu <- a_Subject[Subject] + b_Subject[Subject] * Days,
        c(a_Subject, b_Subject)[Subject] ~ dmvnorm2(c(a, b), sigma_Subject, Rho),
        a ~ dnorm(200, 100),
        b ~ dnorm(0, 10),
        sigma_Subject ~ dcauchy(0, 2),
        sigma ~ dcauchy(0, 2),
        Rho ~ dlkjcorr(2) ),
        data = sleepstudy,
        iter = 5000, warmup = 2000,
        chains = 2, cores = parallel::detectCores() )

## ----eval = TRUE, echo = TRUE--------------------------------------------
precis(mod8, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
compare(mod6, mod7, mod8)


## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(cache = TRUE)
set.seed(666)

## ---- eval = FALSE, echo = TRUE------------------------------------------
## data {
##   int<lower=0> J; // number of schools
##   real y[J]; // estimated treatment effects
##   real<lower=0> sigma[J]; // s.e. of effect estimates
## }
## 
## parameters {
##   real mu;
##   real<lower=0> tau;
##   real eta[J];
## }
## 
## transformed parameters {
##   real theta[J];
##   for (j in 1:J)
##     theta[j] = mu + tau * eta[j];
## }
## 
## model {
##   target += normal_lpdf(eta | 0, 1);
##   target += normal_lpdf(y | theta, sigma);
## }

## ----eval = FALSE, echo = TRUE-------------------------------------------
## brm(y ~ x + (1|subject) + (1|item), data = d, family = gaussian() )

## ----eval = TRUE, echo = TRUE, message = FALSE, results = "hide"---------
library(brms)
data(mtcars)

mod0 <- brm(mpg ~ vs, data = mtcars)

## ----eval = TRUE, echo = TRUE--------------------------------------------
summary(mod0)

## ----eval = TRUE, echo = TRUE--------------------------------------------
library(lme4)
data(sleepstudy)

get_prior(Reaction ~ Days + (1 + Days|Subject), sleepstudy)

## ----eval = TRUE, echo = TRUE--------------------------------------------
prior1 <- c(
    prior(normal(200, 10), class = Intercept),
    prior(normal(0, 10), class = b, coef = Days),
    prior(cauchy(0, 10), class = sigma),
    prior(lkj(2), class = cor)
    )

## ----eval = TRUE, echo = TRUE, results = "hide"--------------------------
mod1 <- brm(
    Reaction ~ Days + (1 + Days | Subject),
    data = sleepstudy,
    family = gaussian(),
    prior = prior1,
    warmup = 2000, iter = 1e4,
    chains = 2
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
library(broom)
tidy(mod1, parameters = c("^b_", "^sd_", "sigma"), prob = 0.95)

## ----echo = FALSE, fig.align = "center", out.width = "500px"-------------
knitr::include_graphics("easy.gif")

## ----eval = FALSE, echo = TRUE-------------------------------------------
## Reaction ~ Days + (1 + Days | Subject)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days | Subject)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days | Subject)
## c(Reaction, Memory) ~ 1 + Days + (1 + Days | Subject)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## c(Reaction, Memory) ~ 0 + Days + (1 + Days | Subject)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## c(Reaction, Memory) ~ Days + (1 | Subject)
## c(Reaction, Memory) ~ Days + (Days | Subject)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days || Subject)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## brm(Reaction ~ 1 + Days + (1 + Days | Subject), family = lognormal() )

## ----eval = TRUE, echo = TRUE--------------------------------------------
library(tidyverse)

data <- read.csv("absenteeism.csv")
data %>% sample_frac %>% head(10)

## ----eval = TRUE, echo = TRUE--------------------------------------------
prior2 <- c(
    prior(normal(0, 10), class = Intercept, coef = ""),
    prior(cauchy(0, 10), class = sd),
    prior(normal(0, 10), class = b),
    prior(lkj(2), class = cor)
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
mod2 <- brm(
    presence | trials(total) ~ 1 + reminder + (1 + reminder|researcher), 
    family = binomial(link = "logit"),
    prior = prior2,
    data = data,
    sample_prior = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.95)
    )

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 12, fig.height = 8----
mod2 %>%
    plot(
        combo = c("hist", "trace"), widths = c(1, 1.5),
        theme = theme_bw(base_size = 16)
        )

## ----eval = TRUE, echo = TRUE--------------------------------------------
tidy(mod2, parameters = c("^b_", "^sd_"), prob = 0.95)

## ----eval = TRUE, echo = TRUE--------------------------------------------
a <- fixef(mod2)[1] # extracting the intercept
exp(a) / (1 + exp(a) ) # equivalent to plogis(a)

## ----eval = TRUE, echo = TRUE--------------------------------------------
fixef(mod2)[2, c(1, 3, 4)] %>% exp

## ---- echo = FALSE, eval = TRUE, message = FALSE, warning = FALSE, fig.align = "center", fig.width = 12, fig.height = 8----
library(tidybayes)
library(modelr)

data %>%
    group_by(researcher, total) %>%
    data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
    add_fitted_samples(mod2, newdata = ., n = 100, scale = "linear") %>%
    mutate(estimate = plogis(estimate) ) %>%
    group_by(reminder, .iteration) %>%
    summarise(estimate = mean(estimate) ) %>%
    ggplot(aes(x = reminder, y = estimate, group = .iteration) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .iteration), size = 0.5, alpha = 0.1) +
    theme_bw(base_size = 20)

## ---- echo = TRUE, eval = TRUE, message = FALSE, warning = FALSE, fig.align = "center", fig.width = 16, fig.height = 8----
data %>%
    group_by(researcher, total) %>%
    data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
    add_fitted_samples(mod2, newdata = ., n = 100, scale = "linear") %>%
    mutate(estimate = plogis(estimate) ) %>%
    ggplot(aes(x = reminder, y = estimate, group = .iteration) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .iteration), size = 0.5, alpha = 0.1) +
    facet_wrap(~researcher, nrow = 2) +
    theme_bw(base_size = 20)

## ---- echo = TRUE--------------------------------------------------------
(hyp1 <- hypothesis(mod2, "reminder = 0") )
1 / hyp1$hypothesis$Evid.Ratio

## ---- echo = TRUE, fig.align = "center", fig.width = 12.5, fig.height = 10----
plot(hyp1, theme = theme_bw(base_size = 20) )

## ---- echo = TRUE, fig.align = "center", fig.width = 16, fig.height = 8----
data.frame(prior = hyp1$prior_samples$H1, posterior = hyp1$samples$H1) %>%
    gather(type, value) %>%
    ggplot(aes(x = value) ) +
    geom_histogram(bins = 50, alpha = 0.8) +
    geom_vline(xintercept = 0, lty = 2, size = 1) +
    facet_wrap(~type, scales = "free") +
    xlab(expression(beta[reminder]) ) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE--------------------------------------------
prior3 <- c(
    prior(normal(0, 10), class = Intercept, coef = ""),
    prior(cauchy(0, 10), class = sd),
    prior(lkj(2), class = cor) )

mod2 <- brm(presence | trials(total) ~ 1 + reminder + (1 + reminder|researcher), 
    family = binomial(link = "logit"),
    prior = prior2,
    data = data,
    # this line is important for bridgesampling
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.95) )

mod3 <- brm(presence | trials(total) ~ 1 + (1 + reminder|researcher), 
    family = binomial(link = "logit"),
    prior = prior3,
    data = data,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.95) )

## ----eval = TRUE, echo = TRUE--------------------------------------------
bayes_factor(mod2, mod3)

## ---- echo = TRUE, eval = TRUE-------------------------------------------
WAIC(mod2, mod3)
LOO(mod2, mod3)

## ---- echo = TRUE, fig.align = "center", fig.width = 16, fig.height = 8----
data %>%
    ggplot(aes(x = presence / total) ) +
    geom_density(fill = "grey20") +
    theme_bw(base_size = 20)

## ---- echo = TRUE, fig.align = "center", fig.width = 16, fig.height = 8----
pp_check(mod2, nsamples = 1e2) + theme_bw(base_size = 20)

## ---- echo = TRUE, fig.align = "center", fig.width = 12.5, fig.height = 10----
pp_check(mod2, nsamples = 1e3, type = "stat_2d") + theme_bw(base_size = 20)

## ----eval = FALSE, echo = TRUE-------------------------------------------
## mod2 <- brm(
##     presence | trials(total) ~ 1 + reminder + (1 + reminder|researcher),
##     family = binomial(link = "logit"),
##     prior = prior2,
##     data = data,
##     warmup = 2000, iter = 1e4,
##     cores = parallel::detectCores(), # using all availables cores
##     control = list(adapt_delta = 0.95) # adjusting the delta step size
##     )

## ----eval = TRUE, echo = TRUE--------------------------------------------
d <- read.csv("meta.csv")
head(d, 15)

## ----echo = FALSE, fig.align = "center", out.width = "1200px"------------
knitr::include_graphics("meta_structure.png")

## ----eval = TRUE, echo = TRUE--------------------------------------------
prior4 <- c(
    prior(normal(0, 1), coef = intercept),
    prior(cauchy(0, 1), class = sd)
    )

mod4 <- brm(
    yi | se(sqrt(vi) ) ~ 0 + intercept + (1|study) + (1|experiment),
    data = d,
    prior = prior4,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = .99)
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
summary(mod4)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 14, fig.height = 10----
mod4 %>% plot(combo = c("hist", "trace"), theme = theme_bw(base_size = 20) )

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 12, fig.height = 12----
source("fplot2.R")
fplot2(d, mod4, level = 0.95)

## ----eval = TRUE, echo = TRUE--------------------------------------------
d <- read.csv("popular.csv")
head(d, 10)

## ----echo = FALSE, fig.align = "center", out.width = "500px"-------------
knitr::include_graphics("cat.gif")

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10------
d %>%
    ggplot(aes(x = popular) ) +
    geom_histogram() +
    facet_wrap(~sex) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10------
d %>%
    ggplot(aes(x = texp, y = popular) ) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", colour = "black") +
    facet_wrap(~sex) +
    theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE--------------------------------------------
library(magrittr)

d %<>%
    mutate(
        # contrast-coding gender
        sex = ifelse(sex == "boy", -0.5, 0.5),
        # centering and standardising teacher experience
        texp = scale(texp) %>% as.numeric
        )

prior5 <- c(
    prior(normal(5, 2.5), class = Intercept),
    prior(cauchy(0, 10), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

mod5 <- brm(
    popular ~ 1 + (1 | school),
    data = d,
    prior = prior5,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
prior6 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

mod6 <- brm(
    popular ~ 1 + texp + (1 | school),
    data = d,
    prior = prior6,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
prior7 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(cauchy(0, 10), class = sigma),
    prior(lkj(2), class = cor)
    )

mod7 <- brm(
    popular ~ 1 + sex + texp + (1 + sex | school),
    data = d,
    prior = prior7,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
mod8 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 + sex | school),
    data = d,
    prior = prior7,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )

## ----eval = TRUE, echo = TRUE--------------------------------------------
WAIC(mod5, mod6, mod7, mod8)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 12, fig.height = 6----
pp_check(mod8, nsamples = 1e2) + theme_bw(base_size = 20)

## ----eval = TRUE, echo = TRUE--------------------------------------------
mod9 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 | school),
    data = d,
    prior = prior6,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )

prior10 <- c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sd)
    )

mod10 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 | school),
    data = d,
    family = cumulative("logit"),
    prior = prior10,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.99, max_treedepth = 15)
    )

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 12, fig.height = 6----
WAIC(mod9, mod10)
pp_check(mod10, type = "dens_overlay", nsamples = 1e2) + theme_bw(base_size = 20)


## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(cache=TRUE)

## ---- echo = T-----------------------------------------------------------
# L'ordre est important pour utiliser la fonction map de rethinking...
library(tidyverse)
library(rethinking)
data(WaffleDivorce)
df1 <- WaffleDivorce

kable(df1[1:10, 1:9], caption = "Données sur la population américaine par état")

## ----eval = T, echo = T, fig.width = 25, fig.heigth = 12, fig.align = "center"----
df1$Marriage.s <- 
    (df1$Marriage - mean(df1$Marriage) ) / sd(df1$Marriage) 

ggplot(df1, aes(x = Marriage.s, y = Divorce) ) +
geom_point(size = 2) +
geom_smooth(method = "lm", se = FALSE) +
theme_bw(base_size = 25)

## ----eval = T, echo = T, fig.width = 25, fig.heigth = 12, fig.align = "center"----
df1$MedianAgeMarriage.s <- 
        (df1$MedianAgeMarriage - mean(df1$MedianAgeMarriage) ) / sd(df1$MedianAgeMarriage)

ggplot(df1, aes(x = MedianAgeMarriage.s, y = Divorce) ) +
geom_point(size = 2) +
geom_smooth(method = "lm", se = FALSE) +
theme_bw(base_size = 25)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod1 <- map(alist(
        Divorce ~ dnorm(mu, sigma),
        mu <- a + bA * Marriage.s,
        a ~ dnorm(10, 10),
        bA ~ dnorm(0 , 1),
        sigma ~ dunif(0, 10) ),
        data = df1)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod1, prob = 0.95, corr = TRUE)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod2 <- map(alist(
        Divorce ~ dnorm(mu, sigma),
        mu <- a + bA * MedianAgeMarriage.s,
        a ~ dnorm(10, 10),
        bA ~ dnorm(0 , 1),
        sigma ~ dunif(0, 10) ),
        data = df1)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod2, prob = 0.95, corr = TRUE)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod3 <- map(alist(
        Divorce ~ dnorm(mu, sigma),
        mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s,
        a ~ dnorm(10, 10),
        bR ~ dnorm(0, 1),
        bA ~ dnorm(0, 1),
        sigma ~ dunif(0 ,10) ),
        data = df1)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 10, fig.height = 5----
precis(mod3)

## ----eval = FALSE, echo = TRUE, fig.align = "center", fig.width = 20, fig.height = 10----
## source("postcheck2.R")
## postcheck2(mod3, window = nrow(df1) )

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 20, fig.height = 5----
source("postcheck2.R")
invisible(capture.output(postcheck2(mod3, window = nrow(df1))))

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
# call link without specifying new data so it uses original data
mu <- link(mod3, refresh = 0)

# summarize samples across cases
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)

# simulate observations
divorce.sim <- sim(mod3, n = 1e4, refresh = 0)
divorce.HPDI <- apply(divorce.sim, 2, HPDI, prob = 0.95)

## ----eval = F, echo = T, fig.align = "center"----------------------------
## p <- ggplot(data = df1, aes(x = Divorce, y = mu.mean) ) + xlab(label = "Divorce observé") + ylab(label = "Divorce prédit") +
##     geom_point(size = 5, pch = 1, col = "steelblue") +
##     geom_abline(intercept = 0, slope = 1, lty = 2) +
##     theme_bw(base_size = 25)
## 
## for (i in 1:nrow(df1) ){
##     dfplot <- data.frame(xi = df1$Divorce[i], y1 = mu.HPDI[1,i], y2 = mu.HPDI[2,i])
##     p <- p + geom_segment(inherit.aes = FALSE, aes(x = xi, xend = xi, y = y1, yend = y2), col = "steelblue", data = dfplot)
## }

## ----eval = TRUE, echo = F, fig.align = "center", fig.width = 15, fig.height = 12----
p <- ggplot(data = df1, aes(x = Divorce, y = mu.mean) ) + xlab(label = "Divorce observé") + ylab(label = "Divorce prédit") +
    geom_point(size = 5, pch = 1, col = "steelblue") +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    theme_bw(base_size = 25)

for (i in 1:nrow(df1) ){
    dfplot <- data.frame(xi = df1$Divorce[i], y1 = mu.HPDI[1,i], y2 = mu.HPDI[2,i])
    p <- p + geom_segment(inherit.aes = FALSE, aes(x = xi, xend = xi, y = y1, yend = y2), col = "steelblue", data = dfplot)
}
p

## ----eval = FALSE, echo = TRUE, fig.align = "center"---------------------
## # compute residuals
## divorce.resid <- d$Divorce - mu.mean
## 
## # get ordering by divorce rate
## o <- order(divorce.resid)
## 
## # make the plot
## dotchart(divorce.resid[o], labels = d$Loc[o], xlim = c(-6, 5) )
## abline(v = 0, col = col.alpha("black", 0.2) )
## 
## for (i in 1:nrow(d) ){
##     j <- o[i] # which State in order
##     lines(d$Divorce[j] - c(mu.HPDI[1,j], mu.HPDI[2,j]), rep(i,2) )
##     points(d$Divorce[j] - c(divorce.HPDI[1,j], divorce.HPDI[2,j]),
##             rep(i,2), pch = 3, cex = 0.6, col = "gray")
## }

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 8, fig.height = 13----
# compute residuals
divorce.resid <- df1$Divorce - mu.mean
# get ordering by divorce rate
o <- order(divorce.resid)
# make the plot
dotchart(divorce.resid[o], labels = df1$Loc[o], xlim = c(-6, 5) )
abline(v = 0, col = col.alpha("black", 0.2) )
for (i in 1:nrow(df1) ){
    j <- o[i] # which State in order
    lines(df1$Divorce[j] - c(mu.HPDI[1,j], mu.HPDI[2,j]), rep(i,2) )
    points(df1$Divorce[j] - c(divorce.HPDI[1,j], divorce.HPDI[2,j]),
            rep(i,2), pch = 3, cex = 0.6, col = "gray")
}

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
N <- 100
height <- rnorm(N, 179, 2)
leg_prop <- runif(N, 0.4, 0.5)
leg_left <- leg_prop * height + rnorm(N, 0, .5)
leg_right <- leg_prop * height + rnorm(N, 0, .5)
df2 <- data.frame(height, leg_left, leg_right)

## ----echo = F------------------------------------------------------------
library(knitr)
kable(df2[1:10,])

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod5 <- map(alist(
        height ~ dnorm(mu, sigma),
        mu <- a + bl * leg_left + br * leg_right,
        a ~ dnorm(0, 100),
        bl ~ dnorm(0, 10),
        br ~ dnorm(0, 10),
        sigma ~ dunif(0, 10) ),
        data = df2,
        start = list(a = mean(df2$height), sigma = sd(df2$height) ) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod5, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 20, fig.height = 5----
precis(mod5, prob = 0.95) %>% plot(., cex = 2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
post <- extract.samples(mod5)
plot(bl ~ br, post, pch = 16, col = "steelblue")

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
sum_blbr <- post$bl + post$br
dens(sum_blbr, col = "steelblue", lwd = 3, xlab = "sum of bl and br", cex = 2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
HPDI(sum_blbr, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod6 <- map(alist(
        height ~ dnorm(mu, sigma),
        mu <- a + bl * leg_left,
        a ~ dnorm(0, 100),
        bl ~ dnorm(0, 10),
        sigma ~ dunif(0, 10) ),
        data = df2,
        start = list(a = mean(df2$height), sigma = sd(df2$height) ) )

## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 20, fig.height = 5----
precis(mod6, prob = 0.95) %>% plot(., cex = 2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
# number of plants
N <- 100
# simulate initial heights
h0 <- rnorm(N, mean = 10, sd = 2)
# assign treatments and simulate fungus and growth
treatment <- rep(0:1, each = N / 2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment * 0.4)
h1 <- h0 + rnorm(N, mean = 5 - 3 * fungus)
# compose a clean data frame
df3 <- data.frame(h0, h1, treatment, fungus)

head(df3)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod7 <- map(alist(
        h1 ~ dnorm(mu, sigma),
        mu <- a + bh*h0 + bt*treatment + bf*fungus,
        a ~ dnorm(0, 100),
        c(bh,bt,bf) ~ dnorm(0, 10),
        sigma ~ dunif(0, 10) ),
        data = df3)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod7, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod8 <- map(alist(
        h1 ~ dnorm(mu, sigma),
        mu <- a + bh*h0 + bt*treatment,
        a ~ dnorm(0, 100),
        c(bh,bt) ~ dnorm(0, 1),
        sigma ~ dunif(0, 10) ),
        data = df3)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod8, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
data(Howell1)
df4 <- Howell1
str(df4)

## ----eval = TRUE, echo = TRUE--------------------------------------------
mod9 <- map(alist(
        height ~ dnorm(mu, sigma),
        mu <- a + bm * male,
        a ~ dnorm(178, 100),
        bm ~ dnorm(0, 10),
        sigma ~ dunif(0, 50) ),
        data = df4)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod9, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
post <- extract.samples(mod9)
mu.male <- post$a + post$bm
HPDI(mu.male, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod10 <- map(alist(
        height ~ dnorm(mu, sigma),
        mu <- af * (1 - male) + ah * male,
        af ~ dnorm(178, 100),
        ah ~ dnorm(178 ,100),
        sigma ~ dunif(0, 50) ),
        data = df4,
        start = list(af = mean(df4$height), af = mean(df4$height), sigma = sd(df4$height)))

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod10, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
post <- extract.samples(mod9)
pi <- sum(df4$male) / length(df4$male) # prop of female

beta <- post$bm # posterior samples for beta
sigma <- post$sigma # posterior samples for sigma

f1 <- pi * (beta - beta * pi)^2
rho <- f1 / (f1 + sigma^2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
BEST::plotPost(rho, showMode = TRUE, cex = 3) # krushke's plot

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
BEST::plotPost((mu.male - post$a) / post$sigma, cex = 3)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
data(milk)
df5 <- milk
str(df5)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
df5$clade.NWM <- ifelse( df5$clade=="New World Monkey", 1, 0)
df5$clade.OWM <- ifelse( df5$clade=="Old World Monkey", 1, 0)
df5$clade.S <- ifelse( df5$clade=="Strepsirrhine", 1, 0)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod11 <- map(alist(
        kcal.per.g ~ dnorm(mu, sigma),
        mu <- a + b.NWM * clade.NWM + b.OWM * clade.OWM + b.S * clade.S,
        a ~ dnorm(0.6, 10),
        b.NWM ~ dnorm(0, 1),
        b.OWM ~ dnorm(0, 1),
        b.S ~ dnorm(0, 1),
        sigma ~ dunif(0, 10) ),
        data = df5)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod11, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
# sample posterior
post <- extract.samples(mod11)

# compute averages for each category
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(data.frame(mu.ape,mu.NWM,mu.OWM,mu.S), prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
diff.NWM.OWM <- mu.NWM - mu.OWM
quantile(diff.NWM.OWM, probs = c(0.025, 0.5, 0.975) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
BEST::plotPost(diff.NWM.OWM, ROPE=c(-0.1,0.1), showMode=TRUE, showCurve=TRUE, cex = 3)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
(df5$clade_id <- coerce_index(df5$clade) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod12 <- map(alist(
        kcal.per.g ~ dnorm(mu, sigma),
        mu <- a[clade_id],
        a[clade_id] ~ dnorm(0.6, 10),
        sigma ~ dunif(0, 10) ),
        data = df5)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod12, prob = 0.95, depth = 2)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
data(tulips)
df6 <- tulips
head(df6,15)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
df6$shade.c <- df6$shade - mean(df6$shade)
df6$water.c <- df6$water - mean(df6$water)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod13.1 <- map(alist(
        blooms ~ dnorm(mu, sigma),
        mu <- a + bW*water.c + bS*shade.c,
        a ~ dnorm(130, 100),
        bW ~ dnorm(0, 100),
        bS ~ dnorm(0, 100),
        sigma ~ dunif(0, 100) ),
        data = df6,
        method = "Nelder-Mead",
        control = list(maxit = 1e4) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod13.2 <- map(alist(
        blooms ~ dnorm(mu, sigma),
        mu <- a + bW*water.c + bS*shade.c + bWS*water.c*shade.c,
        a ~ dnorm(130, 100),
        bW ~ dnorm(0, 100),
        bS ~ dnorm(0, 100),
        bWS ~ dnorm(0, 100),
        sigma ~ dunif(0, 100) ),
        data = df6,
        method = "Nelder-Mead",
        control = list(maxit = 1e4) )

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
coeftab(mod13.1, mod13.2) 

## ----eval = FALSE, echo = TRUE, fig.align = "center", fig.height = 3, fig.width = 6----
## par(mfrow = c(1, 3) ) # three-columns plots
## 
## for (w in -1:1){
##     dt <- d[d$water.c==w,]
##     plot(blooms ~ shade.c, data = dt, col = "steelblue",
##         main = paste("water.c =", w), xaxp = c(-1,1,2), ylim = c(0,362),
##         xlab = "shade (centered)")
## 
##     mu <- link(mod13.2,
##             data = data.frame(water.c = w, shade.c = -1:1),
##             refresh = 0)
##     mu.mean <- apply(mu, 2, mean)
##     mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)
## 
##     lines(-1:1, mu.mean)
##     lines(-1:1, mu.HPDI[1,], lty = 2)
##     lines(-1:1, mu.HPDI[2,], lty = 2)
## }

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.height = 6, fig.width = 12----
par(mfrow = c(1, 3) ) # three-columns plots
for (w in -1:1){
    dt <- df6[df6$water.c==w,]
    plot(blooms ~ shade.c, data = dt, col = "steelblue",
        main = paste("water.c =", w), xaxp = c(-1,1,2), ylim = c(0,362),
        xlab = "shade (centered)")
    mu <- link(mod13.1, data = data.frame(water.c = w, shade.c = -1:1), refresh = 0)
    mu.mean <- apply(mu, 2, mean)
    mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)
    lines(-1:1, mu.mean)
    lines(-1:1, mu.HPDI[1,], lty = 2)
    lines(-1:1, mu.HPDI[2,], lty = 2)
}

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.height = 6, fig.width = 12----
par(mfrow = c(1, 3) ) # three-columns plots
for (w in -1:1){
    dt <- df6[df6$water.c==w,]
    plot(blooms ~ shade.c, data = dt, col = "steelblue",
        main = paste("water.c =", w), xaxp = c(-1,1,2), ylim = c(0,362),
        xlab = "shade (centered)")
    mu <- link(mod13.2, data = data.frame(water.c = w, shade.c = -1:1), refresh = 0)
    mu.mean <- apply(mu, 2, mean)
    mu.HPDI <- apply(mu, 2, HPDI, prob = 0.95)
    lines(-1:1, mu.mean)
    lines(-1:1, mu.HPDI[1,], lty = 2)
    lines(-1:1, mu.HPDI[2,], lty = 2)
}

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
data(mtcars)
head(mtcars)

## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 12, fig.height = 8----
mtcars$disp <- as.numeric(scale(mtcars$disp))
library(ggplot2)
ggplot(mtcars, aes(x = disp, y = mpg, group = cyl, colour = as.factor(cyl))) +
        geom_point() + 
        geom_smooth(method = "lm") + theme_minimal()

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
m_cyl <- lm(mpg ~ disp * cyl, data = mtcars)
summary(m_cyl)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
mod14 <- map(alist(
        mpg ~ dnorm(mu, sigma),
        mu <- a + bd*disp + bc*cyl + bd_bc*disp*cyl,
        a ~ dnorm(20, 100),
        c(bd,bc,bd_bc) ~ dnorm(0, 100),
        sigma ~ dunif(0, 50) ),
        data = mtcars)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
precis(mod14, prob = 0.95)

## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------
glimmer(mpg ~ cyl * disp, data = mtcars)


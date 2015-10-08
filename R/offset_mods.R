library(modEvA)
library(RcmdrMisc)

# load lake data
data(all_potam)

# create binomial data
spp <- grep('^P', names(all_potam), value = T)
data_p <- all_potam
data_p[, spp] <- replace(data_p[, spp], data_p[, spp] > 1, 1)

# potamogeton mod ex

# model using p/a data
mod1 <- glm(PN ~ depth + area + secchi + color + alk + tp, 
  family = binomial(logit), data = data_p)
stepmod1 <- stepwise(mod1, direction = 'forward/backward', criterion = 'AIC')
Dsquared(stepmod1, adjust = TRUE)

# model using count data and offset
mod2 <- glm(PN ~ depth + area + secchi + color + alk + tp, 
  family = poisson(link = 'log'), offset = log(tot), data = all_potam)
stepmod2 <- stepwise(mod2, direction = 'forward/backward', criterion = 'AIC')
Dsquared(stepmod2, adjust = TRUE)

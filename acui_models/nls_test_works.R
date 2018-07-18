Chick.1 <- ChickWeight[ChickWeight$Chick == 1, ]
SSlogis(Chick.1$Time, 368, 14, 6)  # response only


local({
  Asym <- 368; xmid <- 14; scal <- 6
  SSlogis(Chick.1$Time, Asym, xmid, scal) # response _and_ gradient
})


getInitial(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1)
## Initial values are in fact the converged one here, "Number of iter...: 0" :
fm1 <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1)
summary(fm1)
pars <- coef(fm1)

# El tiempo en dias corresponde aprox a 0.4 a?os
n <- seq(from = 0, to = 30, by = 0.1)
# Valores ajustados
fittedValues <- predict(fm1, list(Time = n, Asym = pars[1], xmid = pars[2], scal = pars[3]))

plot(Chick.1$Time, Chick.1$weight, ylim = c(0,1000), xlim = c(0,30))
lines(n,fittedValues)


#### Exapmle 2
x = c(60, 80, 100, 140, 160, 180)
y = c(24.0688, 26.3774, 25.1653, 15.7559, 12.4160, 15.5849)

df = data.frame(x=x, y=y)

# nls(y ~ SSlogis(x, 25, 110, 100), df)
mod <- nls(y ~ SSlogis(x, Asym, xmid, scal), df)
pars <- coef(mod)


n <- seq(0,180,.1)

fitted <- predict(object = mod, list(x=n, Asym = pars[1], xmid = pars[2], scal = pars[3]))


plot(x, y, xlim = c(0,180), ylim = c(0, 50))
lines(n, fitted)






# 
# 
# ## but are slightly improved here:
# fm2 <- update(fm1, control=nls.control(tol = 1e-9, warnOnly=TRUE), trace = TRUE)
# all.equal(coef(fm1), coef(fm2)) # "Mean relative difference: 9.6e-6"
# str(fm2$convInfo) # 3 iterations



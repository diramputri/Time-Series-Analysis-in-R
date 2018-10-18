#AR(1) Process
par(mfrow=c(2,1))
plot(arima.sim(list(order=c(1,0,0), ar=.9), n=100), ylab="x",
main=(expression(AR(1)~~~phi==+.9)))
plot(arima.sim(list(order=c(1,0,0), ar=-.9), n=100), ylab="x",
main=(expression(AR(1)~~~phi==-.9))

#MA(1) Process
par(mfrow = c(2,1))
plot(arima.sim(list(order=c(0,0,1), ma=.5), n=100), ylab="x",
main=(expression(MA(1)~~~theta==+.5)))
plot(arima.sim(list(order=c(0,0,1), ma=-.5), n=100), ylab="x",
main=(expression(MA(1)~~~theta==-.5)))

#Calculate polynomial roots
z = c(1,-1.5,.75) # coefficients of the polynomial
(a = polyroot(z)[1]) # print one root: 1+0.57735i = 1 + i/sqrt(3)
arg = Arg(a)/(2*pi) # arg in cycles/pt
1/arg # = 12, the pseudo period

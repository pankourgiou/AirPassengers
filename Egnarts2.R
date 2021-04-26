

spaic = spec.ar(soi, log="no", ylim=c(0,.3))             # min AIC spec, order = 15
text(frequency(soi)*1/52, .07, substitute(omega==1/52))  # El Nino Cycle
text(frequency(soi)*1/12, .27, substitute(omega==1/12))  # Yearly Cycle
(soi.ar = ar(soi, order.max=100))                         # estimates and AICs
dev.new() 
plot(1:30, soi.ar$aic[-1], type="o")                     # plot AICs


# Better comparison of pseudo-ICs 
n = length(soi)
AIC = rep(0,100) -> AICc -> BIC
for (k in 1:100){
  fit = ar(soi, order=k, aic=FALSE)
# sigma2 = var(fit$resid, na.rm=TRUE)  # this doesn't seem to work right - use next line 
  sigma2 = fit$var.pred                # with this, all IC pick the order 15 model
  BIC[k] = log(sigma2) + (k*log(n)/n)
  AICc[k] = log(sigma2) + ((n+k)/(n-k-2))
  AIC[k] = log(sigma2) + ((n+2*k)/n) }
IC = cbind(AIC, BIC+1)

dev.new()
ts.plot(IC, type="o", xlab="p", ylab="AIC / BIC")
grid()
text(20.2, -1.48, "AIC")
text(20, -1.35, "BIC")

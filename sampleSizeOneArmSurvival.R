# Power and sample size estimate for a one-arm survival trial.

# nullMed is the null median survival,
# altMed is the alternative median survival
# accrualPeriod is the accrual time
# fuPeriod is the follow up

# assumes constant accrual and two sided test
# Approach taken from Lawless 1982, p. 108

# Conversion from probability of PFS at time t to median survival time.
medianOfExpTte <- function(time = 1, pfs){
   median <- time*log(0.5)/log(pfs)
   return(median)}

#medianOfExpTte(1, 0.55)
#medianOfExpTte(1, 0.6325)




# Try to reproduce the CRAB calculator
# Make a very customized function that gives the power provided by a given sample size assuming an effect size and pct Cll.
# Modeled after the source code here:
# http://www.swogstat.org/stat/public/one_survival.htm
oneArmIbruPowerCRAB <- function(
   sampleSize,
   effectSize,
   pctCll,
   pfsMcl = 0.52,                                             # probability of survival past pfsEstTime within the Mcl patients, under the null hypothesis
   pfsCll = 0.62,

   nullPfs = pctCll*pfsCll + (1 - pctCll)*pfsMcl,            # probability of survival past pfsEstTime for the cohort overall, under the null hypothesis
   altPfs = nullPfs %*% t(1 + effectSize),

   accrualPeriod = 0.001,
   fuPeriod = 2,
   alpha = 0.05,
   pfsEstTime = 1){              # outcome of interest is the prob of pfs at time pfsEstTime

   nullHaz = -log(nullPfs)/pfsEstTime
   altHaz = -log(altPfs)/pfsEstTime

   pi0 = 1 - exp(-nullHaz*fuPeriod)*(1 - exp(-nullHaz*accrualPeriod))/(nullHaz*accrualPeriod)
   pi1 = 1 - exp(-altHaz *fuPeriod)*(1 - exp(-altHaz *accrualPeriod))/(altHaz *accrualPeriod)

   psi0 = nullHaz^(1/3)
   psi1 = altHaz^(1/3)

   var0 = (psi0^2)/(9*pi0)
   var1 = (psi1^2)/(9*pi1)

   power <- pnorm(sqrt(var1/var0)*qnorm(alpha/2, lower.tail = TRUE) +
      abs(psi1 - psi0)*sqrt(sampleSize)/sqrt(var0))
   return(c(round(nullPfs, 3), round(altPfs, 3), round(100*power, 0)))}

oneArmIbruPowerCRAB(
   sampleSize = 65,
   effectSize = c(0.20),

   pctCll = 0.60,
   pfsMcl = 0.52,                                             # probability of survival past pfsEstTime within the Mcl patients, under the null hypothesis
   pfsCll = 0.62,

   accrualPeriod = 0.001,
   fuPeriod = 2,
   alpha = 0.05,
   pfsEstTime = 1)

# Now, this function automatically iterates over the scenarios using mapply.
powerTableMaker <- function(
   ss, 
   effectSizeVec = seq(0.20, 0.30, 0.05),

   pctCllVec = seq(0.3, 0.7, 0.1),
   pfsM = 0.52,                                             # probability of survival past pfsEstTime within the Mcl patients, under the null hypothesis
   pfsC = 0.62,

   accrualPd = 0.001,
   fuPd = 2,

   alph = 0.05,
   #ssOnly = FALSE,
   pfsEstTm = 1){

   scenarios <- expand.grid("pctCll" = pctCllVec, "effectSize" = effectSizeVec)
   power <- with(scenarios,
      mapply(FUN = oneArmIbruPowerCRAB,
         sampleSize = ss,
         effectSize = scenarios$effectSize,

         pctCll = scenarios$pctCll,
         pfsMcl = pfsM,                                             # probability of survival past pfsEstTime within the Mcl patients, under the null hypothesis
         pfsCll = pfsC,

         accrualPeriod = accrualPd,
         fuPeriod = fuPd,
         
         alpha = alph,
         pfsEstTime = pfsEstTm))

   return(data.frame(effectSize = round(scenarios$effectSize*100, 0), pctCll = 100*scenarios$pctCll, pctMcl = 100 - 100*scenarios$pctCll, power = t(power)))
   #return(power)

}

powerTableMaker(
   ss = 65,
   effectSizeVec = seq(0.20, 0.30, 0.05),

   pctCllVec = seq(0.30, 0.70, 0.10),

   accrualPd = 0.00001,
   fuPd = 2,
   alph = 0.05,
   pfsEstTm = 1)









# Make a very customized function that gives the power provided by a given sample size assuming an effect size and pct Cll.
# Adapted from Cody Hamilton, PhD:
# http://r.789695.n4.nabble.com/R-One-arm-survival-sample-estimates-td809218.html
oneArmIbruPower <- function(
   sampleSize,
   effectSize,
   pctCll,
   pfsMcl = 0.52,                                             # probability of survival past pfsEstTime within the Mcl patients, under the null hypothesis
   pfsCll = 0.62,

   nullPfs = pctCll*pfsCll + (1 - pctCll)*pfsMcl,            # probability of survival past pfsEstTime for the cohort overall, under the null hypothesis
   altPfs = nullPfs %*% t(1 + effectSize),


   accrualPeriod = 2,
   fuPeriod = 2,
   alpha = 0.05,
   pfsEstTime = 1){              # outcome of interest is the prob of pfs at time pfsEstTime


   nullMed = medianOfExpTte(time = pfsEstTime, pfs = nullPfs)
   altMed = medianOfExpTte(time = pfsEstTime, pfs = altPfs)

   # find proportion of patients that will experience event for given median survivals, accrual and follow up times (formula from Cox 1983)
   propWithEvent <- 1 - (exp(log(0.5)*fuPeriod/altMed) * (1 - exp(log(0.5)*accrualPeriod/altMed)) / (-log(0.5)*accrualPeriod/altMed))

   #sample size is numEventsNeeded/propWithEvent
   numEventsNeeded <- round(sampleSize*propWithEvent)
   stop("This program doesn't work properly")
   power <- pchisq(q = qchisq(p = alpha, df = 2*numEventsNeeded, lower.tail = FALSE)*nullMed/altMed, df = 2*numEventsNeeded, lower.tail = FALSE)
   return(power)}


# Now make one that gives the sample size for a given power.
oneArmIbruSs <- function(effectSize,
   pctCll,
   pfsMcl = 0.52,                           
   pfsCll = 0.62,

   nullPfs = pctCll*pfsCll + (1 - pctCll)*pfsMcl,
   altPfs = nullPfs %*% t(1 + effectSize),


   accrualPeriod = 2,
   fuPeriod = 2,
   power,
   alpha = 0.05,
   pfsEstTime = 1){              # outcome of interest is the prob of pfs at time pfsEstTime


   nullMed = medianOfExpTte(time = pfsEstTime, pfs = nullPfs)
   altMed = medianOfExpTte(time = pfsEstTime, pfs = altPfs)

   # initialize number of events to 0
   numEventsNeeded <- 0
   # find necessary number of events
   # Looks like this finds the biggest values of r such that this statement is true
   while( qchisq((1 - alpha/2), (2*numEventsNeeded + 1))*(nullMed/altMed) >= qchisq(1 - power, (2*numEventsNeeded + 1)) ){
      numEventsNeeded <- numEventsNeeded + 1}

   # find proportion of patients that will experience event for given median survivals, accrual and follow up times (formula from Cox 1983)

   propWithEvent <- 1 - (exp(log(0.5)*fuPeriod/altMed) * (1 - exp(log(0.5)*accrualPeriod/altMed)) / (-log(0.5)*accrualPeriod/altMed))

   #sample size is numEventsNeeded/propWithEvent
   n <- round(numEventsNeeded/propWithEvent, 0)

   return(n)}







oneArmIbruSs(effectSize = 0.25,
   pctCll = 0.60,
   pfsMcl = 0.52,
   pfsCll = 0.62,
   power = 0.8
)


oneArmIbruSs(effectSize = 0.20,
   accrualPeriod = 0.1,
   fuPeriod = 2, 
   pctCll = 0.40,
   pfsMcl = 0.52,
   pfsCll = 0.62,
   power = 0.80
)


















# Now, this function automatically iterates over the scenarios using mapply.
ssTableMaker <- function(pctCllVec,
   effectSizeVec,
   accrualPeriod = 2,
   fuPeriod = 2,
   alpha = 0.05,
   power = 0.8,
   power2 = 0.9,
   ssOnly = FALSE,
   pfsEstTime = 1){

   scenarios <- expand.grid("pctCll" = pctCllVec, "effectSize" = effectSizeVec)
   ss <- with(scenarios,
      mapply(FUN = oneArmIbruSs,
         pctCll = pctCll,
         effectSize = effectSize,
         accrualPeriod = accrualPeriod,
         fuPeriod = fuPeriod,
         alpha = alpha,
         power = power,
         pfsEstTime = pfsEstTime))

   if(!is.null(power2)){
   ss2 <- with(scenarios,
      mapply(FUN = oneArmIbruSs,
         pctCll = pctCll,
         effectSize = effectSize,
         accrualPeriod = accrualPeriod,
         fuPeriod = fuPeriod,
         alpha = alpha,
         power = power2,
         pfsEstTime = pfsEstTime))}


   if(!ssOnly & !is.null(power2)) return(data.frame(pctCll = 100*scenarios$pctCll, pctMcl = 100 - 100*scenarios$pctCll, ss1 = ss, ss2 = ss2))
   if(!ssOnly & is.null(power2)) return(data.frame(pctCll = 100*scenarios$pctCll, pctMcl = 100 - 100*scenarios$pctCll, ss = ss))
   if(ssOnly) return(ss)

}

























if(FALSE){
str(
ssTableMaker(
   pctCllVec = seq(0.3, 0.7, 0.1),
   effectSizeVec = seq(0.15, 0.30, 0.05),
   accrualPeriod = 2,
   fuPeriod = 2,
   alpha = 0.05,
   power = 0.8,
   pfsEstTime = 1)
)

oneArmIbruSs(effectSize = 0.15,
   pctCll = 0.30,
   accrualPeriod = 2,
   fuPeriod = 2,
   alpha = 0.05,
   power = 0.8,
   pfsEstTime = 1)


oneArmIbruSs(effectSize = 0.15,
   pctCll = 0.30,
   accrualPeriod = 2,
   fuPeriod = 2,
   alpha = 0.05,
   power = 0.9,
   pfsEstTime = 1)

# For a given power and effect size, get the sample size required for a vector of % cll.
# Can't vectorize well in the function because of the while().

pctCllVec <- seq(0.3, 0.7, 0.1)
#oneArmIbruSsCllLoop <- function()

sapply(pctCllVec, FUN = oneArmIbruSs,
   effectSize = 0.15,
   accrualPeriod = 2,
   fuPeriod = 2,
   alpha = 0.05,
   power = 0.8,
   pfsEstTime = 1)








lawlessSS <- function(
   nullMed = medianOfExpTte(time = tiime, pfs = nullPfs),
   altMed = medianOfExpTte(time = tiime, pfs = altPfs),

   tiime,
   nullPfs,
   altPfs,
   accrualPeriod, fuPeriod, power, alpha = 0.05){

   # initialize number of events to 0
   numEventsNeeded <- 0
   # find necessary number of events
   # Looks like this finds the biggest values of r such that this statement is true
   while( qchisq((1 - alpha/2), (2*numEventsNeeded + 1))*(nullMed/altMed) >= qchisq(1 - power, (2*numEventsNeeded + 1)) ){
      numEventsNeeded <- numEventsNeeded + 1}

   # find proportion of patients that will experience event for given median survivals, accrual and follow up times (formula from Cox 1983)

   propWithEvent <- 1 - (exp(log(0.5)*fuPeriod/altMed) * (1 - exp(log(0.5)*accrualPeriod/altMed)) / (-log(0.5)*accrualPeriod/altMed))

   #sample size is numEventsNeeded/propWithEvent
   n <- round(numEventsNeeded/propWithEvent, 0)

   return(n)}

lawlessSS(nullMed = 1.159425,
   altMed = 1.513174,
   accrualPeriod = 2,
   fuPeriod = 2,
   alpha = 0.05,
   power = 0.9)

lawlessSS(
   #nullMed = 1.159425,
   #altMed = 1.513174,
   nullPfs = 0.55,
   altPfs = 0.6325,
   tiime = 1,
   accrualPeriod = 2,
   fuPeriod = 2,
   alpha = 0.05,
   power = 0.9)

cox.pow <- function(med.0, med.a, a.time, f.time, alpha, beta){

   # initialize number of events to 0
   r <- 0
   # find necessary number of events
   # Looks like this finds the biggest values of r such that this statement is true
   while( qchisq((1 - alpha/2), (2*r + 1))*(med.0/med.a) >= qchisq(beta, (2*r + 1)) ){
      r <- r + 1}

   # find proportion of patients that will experience event for given median survivals, accrual and follow up times (formula from Cox 1983)

   p <- 1 - (exp(log(0.5)*f.time/med.a) * (1 - exp(log(0.5)*a.time/med.a)) / (-log(0.5)*a.time/med.a))

   #sample size is r/p
   n <- round(r/p, 0)

   return(n)}





## Example
#compute necessary sample size for alternative median 9.3 vs. null median 6.3 with 20 months accrual and 4 months follow up

# 80% power
# 0.05 prob TIE

cox.pow(med.0 = 6.3,
   med.a = 9.3,
   a.time = 20,
   f.time = 4,
   alpha = .05,
   beta = 0.2)


cox.pow(med.0 = 1.159425,
   med.a = 1.513174,
   a.time = 2,
   f.time = 2,
   alpha = .05,
   beta = 0.2)




cox.pow(med.0 = 1.159425,
   med.a = 1.513174,
   a.time = 2,
   f.time = 2,
   alpha = .05,
   beta = 0.1)
}



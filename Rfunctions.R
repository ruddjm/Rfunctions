
library(Hmisc)

###################################################
## General purpose functions
###################################################
# TK's function
tab <- function(...){ table(..., useNA='a') }

grepDfNames <- function(string, dat, ...){grep(string, names(dat), value = TRUE, ...)}
######################################
# Format a confidence interval to put in a report. Make a scalar character.
formatCI <- function(x, digs = 2){
   x <- round(x, digs)
   return(paste(x[1], " (CI: ", x[2], ", ", x[3], ")", sep = ""))}

# same thing except with C formatting
formatCIformat <- function(x, digs = 2, ...){
   x <- formatC(x, format = "f", digits = digs, ...)
   return(paste(x[1], " (CI: ", x[2], ", ", x[3], ")", sep = ""))}
##################
freqPctMarginal <- function(x, y, pval.digs = 3, test = FALSE, ...){
   OK <- complete.cases(x, y)

   tabl <- matrix(
      sprintf("%s (%0.f%%)", table(x, y), 100*prop.table(table(x, y), ...)),
      ncol = nlevels(y),
      dimnames = list(levels(x), levels(y)))
   if(test) cbind(tabl, c(ifelse(!((nlevels(factor(x[OK])) < 2L) || (nlevels(factor(y[OK])) < 2L)),
      format.pval(chisq.test(x, y)$p.value, eps = 0.001, digits = pval.digs), NA), rep(NA, nlevels(x) - 1))) else tabl}

freqPctMarginalOneDim <- function(x, ...){
   OK <- na.omit(x)
   tabl <- matrix(
      sprintf("%s (%0.f%%)", table(x), 100*prop.table(table(x), ...)),
      ncol = nlevels(x),
      dimnames = list(NULL, levels(x)))
   tabl}

# The test argument should be a logical vector
freqPctScalar <- function(test, digs = 1, ...){
   test <- na.omit(test)
   paste(sum(test), " (", round(100*mean(test), digs), "%)", sep = "")
  }

freqPctMarginalDatFrame <- function(x, y, test = FALSE, ...){
   OK <- complete.cases(x, y)

   tabl <- data.frame(matrix(
      sprintf("%s (%0.f%%)", table(x, y), 100*prop.table(table(x, y), ...)),
      ncol = 2,
      dimnames = list(levels(x), levels(y))),
      stringsAsFactors = FALSE)
   if(test) tabl$pval <- c(ifelse(!((nlevels(factor(x[OK])) < 2L) || (nlevels(factor(y[OK])) < 2L)), chisq.test(x, y)$p.value, NA), rep(NA, nlevels(x) - 1))
   tabl}

# Example
if(FALSE){
Cole <- factor(c("Yes", "Yes", "No", "No"))
Beck <- factor(c("No", "Yes", "No", "No"))
colebeck <- data.frame(Cole, Beck)
with(colebeck, freqPctMarginal(y = Cole, x = Beck))
with(colebeck, freqPctMarginal(y = Cole, x = Beck, margin = 1))
with(colebeck, freqPctMarginalDatFrame(y = Cole, x = Beck))

## Cole wrote this.
myprop <- function(formula, data) {
  trm <- as.character(attr(terms(formula), 'variables'))[-1]
  x <- table(data[,trm[1]], data[,trm[2]])
  y <- prop.table(x)
 # list(
    matrix(sprintf("%s (%0.f%%)", x, y*100), nrow=nrow(x), dimnames=dimnames(x)) #,
  #  format.pval(chisq.test(trm[1], trm[2])$p.value, eps = 0.001, digits = pval.digs)
 # )
}
myprop(Cole ~ Beck, colebeck)}
######################################
## Get concise information from test of difference in proportions.
prop.testExtract <- function(proptestObj, pctOrProp = "percent", digits = 1){
   # Make a named vector of useful elements from the obj. Make vector or list? if vector must be all same type (in this case character)
   # c(estimated prop 1 estimated prop 2, estimated difference, formatted ci (character), pvalue)

   # Percentify and round the first 4 elements at the same time. Then add formatted ci. Then select elements in the order desired
   # estimated difference
   out <- c(round(
      ifelse(pctOrProp == "percent", 100, ifelse(pctOrProp == "proportion", 1, stop("double whammy /n")))*
         c(proptestObj$estimate, "Difference" = mean(proptestObj$conf.int), proptestObj$conf.int), digits),
         "p-value" = format.pval(proptestObj$p.value, digits = 3, eps = 0.001))
   names(out)[4:5] <- c("L", "U")
   out["CI"] <- paste("[", paste(out[c("L", "U")], collapse = ", "), "]", sep = "")
   return(out[c("prop 1", "prop 2", "Difference", "CI", "p-value")])}
######################################
## Get concise information from test of difference in means.
t.testExtract <- function(ttestObj, digits = 1){
   # Make a named vector of useful elements from the obj. Make vector or list? if vector must be all same type (in this case character)
   # c(estimated mean 1 estimated mean 2, estimated difference, formatted ci (character), pvalue)

   # Round the first 4 elements at the same time. Then add formatted ci. Then select elements in the order desired
   # estimated difference
   out <- c(round(
         c(ttestObj$estimate, "Difference" = mean(ttestObj$conf.int), ttestObj$conf.int), digits),
         "p-value" = format.pval(ttestObj$p.value, digits = 3, eps = 0.001))
   grps = names(out)[1:2] = sub('mean in group ', '', names(out)[1:2])
   names(out)[4:5] <- c("L", "U")
   out["CI"] <- paste("[", paste(out[c("L", "U")], collapse = ", "), "]", sep = "")
   return(out[c(grps, "Difference", "CI", "p-value")])}
######################################
# This removes unused factors from a data frame.
updateFactors <- function(x) {
  for(i in which(sapply(x, is.factor))) {
    x[,i] <- x[,i][,drop=TRUE]
  }
  x}
####################
boxplots <- function(groupvar, yvar, data, box = TRUE, strip = TRUE, ...){
   if(box == TRUE){
      boxplot(data[[yvar]] ~ data[[groupvar]],
         #ylim = c(0, max(data[[yvar]], na.rm = TRUE)),
         ylab = label(data[[yvar]]),
         xlab = label(data[[groupvar]]),
         las = 1,
         outpch = NA, ...)            # Outpch supresses plotting "outliers."
   if(strip == TRUE){
      stripchart(data[[yvar]] ~ data[[groupvar]], vertical = TRUE, method = "jitter", pch = 19, add = TRUE)}} else
   stripchart(data[[yvar]] ~ data[[groupvar]], vertical = TRUE, method = "jitter", pch = 19,
      ylab = label(data[[yvar]]),
      xlab = label(data[[groupvar]]), ...)}
#################################
#Should work on vectors if pasteit = FALSE, but does not work for vectors if vectors = TRUE.
normalCI <- function(est, se, alpha = 0.05, digits = 2, exponentiate = FALSE, roundit = TRUE, pasteit = FALSE, ciOnly = FALSE){
   # Wald
   value <- cbind(est, est - qnorm(1-alpha/2)*se, est + qnorm(1-alpha/2)*se)
   if(exponentiate == TRUE){value <- exp(value)}
   if(ciOnly == TRUE){value <- value[ , -1]}
   if(roundit == TRUE){value <- round(value, digits = digits)}
   if(pasteit == TRUE){
      if(ciOnly == TRUE){value <- paste("[", paste(value, collapse = ", "), "]", sep = "")} else {
      value <- paste(value[ , 1], ", ", "[", paste(value[ , c(2, 3)], collapse = ", "), "]", sep = "")}}
   #dimnames(value) <- list(names(est), c("est", "lower", "upper"))
   return(value)}
######################################
spearmanCI <- function(data, x, y, digits = 2, R = 800, level = 0.95){
   # This computes percentile confidence limits for Spearman correlation using bootstrap sampling
   # x and y need to be characters, the names of the variables
   # the boot() function requires the boot package
   require(boot, quietly = TRUE)
   alpha02 <- (1 - level)/2
   jimmy <- boot(data = data,
      sim = "ordinary",
      stype = "i",
      R = R,
      statistic = function(dat, i){cor(dat[i, x], dat[i, y], use = "pairwise.complete.obs", method = "spearman")})
   return(list(est = jimmy$t0,
      ci = quantile(jimmy$t, c(alpha02, 1 - alpha02)),
      pval = format.pval(cor.test(data[[x]], data[[y]],
         use = "pairwise.complete.obs", method = "spearman")$p.value, eps = 0.0001, digits = digits)))}
##################
inlineQ <- function(data, var, digs = 0){
   paste("(Q1, Q3: ", paste(round(quantile(data[[var]], probs = c(0.25, 0.75), na.rm = TRUE), 
      digits = digs), collapse = ", "), ")", sep = "")}
########################################
# Remove variables that are all missing. 
removeAllMissing <- function(x, fracmiss = 1) {
   # fracmiss the maximum permissable proportion of NAs for a variable to be kept. Default is to keep all variables no matter how many NAs are present.
   N <- nrow(x)
   index <- logical(ncol(x))
   for(i in seq_len(ncol(x))) {
      pctmiss <- mean(is.na(x[[i]]))
      if(pctmiss > fracmiss){
        index[i] <- FALSE } else {index[i] <- TRUE}}
  return(x[index])
}
###################################################
## End of general purpose functions
###################################################

###################################################
## Regression model output functions
###################################################
# Doesn't allow splines
simpmodel <- function(outcomevarname, covars, dat, ...){
   inputformula <- as.formula(paste(outcomevarname, paste(covars, collapse=" + "), sep=" ~ "))
   mod <- lm(inputformula, data = dat, ...)
   return(mod)
}
##################
# for lm models
# Formats the basic regression results: estimates, CIs, pvals. I guess this wouldn't make much sense when the model has splines or interaction terms.
simpmodeloutput <- function(mod, rownms, lablep = "", caption = "auto", dat = dat){
   outcomevarname <- all.vars(mod$terms)[1]
   # Model-based parameter estimates.
   # Format results into a table
   parameters <- data.frame(summary(mod)$coefficients)
   names(parameters) <- c("Estimate","Std Error","t.value","p-value")
   parameters$t.value <- NULL
   row.names(parameters) <- rownms

   ## Get CI for Pparameters
   parameters$LL <- parameters[ , "Estimate"] - 1.96*parameters[ , "Std Error"]
   parameters$UL <- parameters[ , "Estimate"] + 1.96*parameters[ , "Std Error"]

   parameters[ , c("Estimate", "Std Error", "LL", "UL")] = round(parameters[ ,
      c("Estimate", "Std Error", "LL", "UL")], 3)

     # Format p-values
   parameters[ , "p-value"] <- format.pval(parameters[, "p-value"], digits = 3, eps = 0.001)

   if(caption == "auto"){
      cptn <- gsub("_", "\\\\_", gsub("%", "\\\\%", paste("Parameter estimates for linear regression model, ", outcomevarname, ":", label(dat[ , outcomevarname]),  sep = "")))}
   else cptn <- caption

   latex(parameters[ , c("Estimate","Std Error","LL", "UL", "p-value")],
      colheads = c("Estimate", "Std Error", "LL", "UL", "$p$-value"), title = "",
      caption = cptn,
      label = lablep, where = "!h",
      #align = paste('l', paste(rep('r', ncol(parameters)-1), collapse=''), sep = ""),
      file = "")}
################################
#lrm. "Generic" refers to the fact that none of the values are set to hold to constant or at which to evaluate.
latexmodeloutputGeneric <- function(mod, i,
   modlabels,   # This is used for a caption for the figures, so you will see which model each of the plots is from.
   modname              # This is used to name the graphics files. This needs to be supplied and unique for each model or they will all have the same name and all the plots for the different models in the mod obj list will be the same.
   ){
   #This automatically takes the labels from the data and uses them in the output of latex.anova.rms and plot.anova
   newnamevec <- dimnames(anova(mod[[i]]))[[1]]
   namevec <- newnamevec[!grepl("^ ", newnamevec)]
   namevec <- namevec[! namevec %in% c("TOTAL NONLINEAR", "ERROR", "TOTAL NONLINEAR + INTERACTION", "TOTAL", "TOTAL INTERACTION")]
   var2 <- strsplit(sub("  \\(Factor\\+Higher Order Factors\\)$", "", namevec), " * ", fixed = TRUE)
   varlabels <- sapply(var2, FUN = function(vars, Design) {
      paste(Design$label[match(vars, Design$name)], collapse=" * ")}, Design = mod[[i]]$Design)
   for(j in seq_along(namevec)) {
      newnamevec[which(newnamevec == namevec[j])] <- varlabels[j]}

   cat("\\subsection{", modlabels[i], " model}\n", sep = "")
   # See help(lrm) and help(prModFit)
   print(mod[[i]], latex = TRUE, digits = 2, lines.page = .Machine$integer.max,
      #labels = c(damico.Derived = "Damico"),
      long = FALSE,      #set to FALSE to suppress printing of formula and certain other model output
      title = paste("Estimates from " , tolower(modlabels[i]), " model", sep = ""))

   latex(anova(mod[[i]],
      ss = TRUE),                  # fix
      newnames = newnamevec,
      dec.F = 1,
      dec.ms = 0,
      dec.ss = 0,
      dec.P = 3,
      booktabs = TRUE,
      vnames = "labels",
      #title = paste("Estimates from " , tolower(modlabels[i]), " model", sep = ""),
      file = "",
      lines.page = .Machine$integer.max)

   bill <- summary(mod[[i]], vnames = "labels", antilog = TRUE)
      # Example: comorbidsum = c(1, 4), ageCentered = c(1, -1))
   flojito <- attr(bill, "adjust")
   bill[is.na(bill[ , "Diff."]), c("Low", "High")] <- NA
   bill <- bill[ , c("Low", "High", "Diff.", "Effect", "Lower 0.95", "Upper 0.95")] <- round(bill[ , c("Low", "High", "Diff.", "Effect", "Lower 0.95", "Upper 0.95")], 2)
   # This is only appropriate in some types of models
   colnames(bill) <- c("From", "To", "Delta", "Odds ratio", "Lower 0.95", "Upper 0.95")
   rownamesbill <- rownames(bill)
   bill <- bill[(1:(nrow(bill)/2))*2, ]
   rownames(bill) <- rownamesbill[(1:(nrow(bill)))*2-1]

   latex(bill, title = "",
      caption = paste(modlabels[i], " effect estimates", sep = ""),
      insert.bottom = paste("Adjusted to: ", flojito, sep = ""),
      longtable = TRUE, file = "", lines.page =.Machine$integer.max)

   pdf(paste("figures/", modname[i], "anovaplot.pdf", sep = ""))
       plot(anova(mod[[i]]), newnames = varlabels)
   dev.off()
   cat("\\clearpage \\clearpage \\begin{figure}\\begin{center}\\caption{", modlabels[i], " Model}\\includegraphics{figures/",
      modname[i], "anovaplot.pdf}\\end{center}\\end{figure}\n", sep = "")
   pdf(paste("figures/", modname[i], "oddsratios.pdf", sep = ""))
      plot(summary(mod[[i]], #est.all = FALSE,
         #comorbidsum = c(1, 4), ageCentered = c(1, -1),
         vnames = "labels"),
         log = TRUE, q = 0.95, col = "#55555588", col.points = "black")
   dev.off()
   cat("\\clearpage \\begin{figure}\\begin{center}\\caption{", modlabels[i], " model}\\includegraphics{figures/", modname[i], "oddsratios.pdf}\\end{center}\\end{figure}\n", sep="")}

 ## Example of usage
##junkness <- lapply(X = seq_along(models), FUN = latexmodeloutput, mod = models, modlabels = modlabels, modname = "origMods")
#####################
latexmodeloutputOls <- function(mod, i, modlabels){
   #This automatically takes the labels from the data and uses them in the output of latex.anova.rms and plot.anova
   newnamevec <- dimnames(anova(mod[[i]]))[[1]]
   namevec <- newnamevec[!grepl("^ ", newnamevec)]
   namevec <- namevec[! namevec %in% c("TOTAL NONLINEAR", "TOTAL NONLINEAR + INTERACTION", "TOTAL", "TOTAL INTERACTION", "ERROR")]
   var2 <- strsplit(sub("  \\(Factor\\+Higher Order Factors\\)$", "", namevec), " * ", fixed = TRUE)
   varlabels <- sapply(var2, FUN = function(vars, Design) {
      paste(Design$label[match(vars, Design$name)], collapse=" * ")}, Design = mod[[i]]$Design)
   for(j in seq_along(namevec)) {newnamevec[which(newnamevec == namevec[j])] <- varlabels[j]}
   cat("\\subsubsection{", modlabels[i], " model}\n", sep = "")
   print(mod[[i]], latex = TRUE, digits = 2, lines.page = .Machine$integer.max,
      #labels = c(damico.Derived = "Damico"),
      long = FALSE,
      title = "")
   latex(anova(mod[[i]],
      ss = TRUE),                  # fix
      newnames = newnamevec,
      dec.F = 1,
      dec.P = 3,
      dec.ms = 0,
      dec.ss = 0,
      booktabs = TRUE,
      vnames = "labels",
      file = "",
      lines.page = .Machine$integer.max)
   bill <- summary(mod[[i]], vnames = "labels") #,
      #age.Derived = c(60, 70), cesd.Scored.F00 = c(4, 30) # Example of how to set defaults.
      #)
   flojito <- attr(bill, "adjust")
      # comorbidsum = c(1, 4), ageCentered = c(1, -1)) example of how to use
   bill[is.na(bill[ , "Diff."]), c("Low", "High")] <- NA
   bill <- bill[ , c("Low", "High", "Diff.", "Effect", "Lower 0.95", "Upper 0.95")] <- round(bill[ , c("Low", "High", "Diff.", "Effect", "Lower 0.95", "Upper 0.95")], 1)
   colnames(bill) <- c("From", "To", "Delta", "Effect", "Lower 0.95", "Upper 0.95")
   latex(bill, title = "",
      caption = paste(modlabels[i], " effect estimates", sep = ""),
      insert.bottom = paste("Adjusted to: ", flojito, sep = ""),
      longtable = TRUE, file = "", lines.page =.Machine$integer.max)
   cat("\\clearpage \n")}

# Define a function to display results of a cox proportional hazards model in latex automatically with CI's for parameters.
# Works with coxph?
coxoutput <- function(mod, rownms, lablep = "", caption = "auto", dat, output = FALSE, table = TRUE, ...){
   outcomevarname <- all.vars(mod$terms)[3]
   temp <- data.frame(summary(mod)$conf.int)[ , -2]
   rownames(temp) <- rownms
   temp$p.value <- format.pval(summary(mod)$coefficients[ , "Pr(>|z|)"], digits = 3, eps = 0.001)   #Format p-vals

   #if(caption == "auto"){ #If a caption is not specified, generate an automatic one
   #   cptn <- gsub("_", "\\\\_", gsub("%", "\\\\%", paste("Parameter estimates for a Cox model for ", label(dat[ , outcomevarname]),  sep = "")))}
   #else cptn <- caption

   if(table == TRUE){
   latex(temp,
      colheads = c("Hazard ratio", "95\\% lower", "95\\% upper", "$p$-value"), title = "",     # "$e^{\\hat{\\beta}}$"
      dec = 3,
     col.just = rep("r", ncol(temp)),
      caption = caption,
      label = lablep,
      where = "!h",
      booktabs = TRUE,
      file = "", ...)}

   if(output == TRUE){return(temp)}
}

coxhrci <- function(coxoutputobj, digs, row = 1){
   with(round(coxoutputobj[row , -4], digs), paste("(HR = ", exp.coef., ", CI: ", lower..95, ", ", upper..95, ")", sep = ""))}

###
# This function will output model results. for what model? lme?
modeloutput <- function(mod, rownms, lablep = "", caption = "auto", dat = plfirstyear, roundit = 3){
   outcomevarname <- all.vars(mod$terms)[1]
   # Model-based parameter estimates.
   # Format results into a table

   fixedeffects <- data.frame(summary(mod)$tTable)

   names(fixedeffects) <- c("Estimate","Std Error","LL","UL","p-value")
   row.names(fixedeffects) <- rownms

   ## Get CI for Pparameters
   fixedeffects$LL <- fixedeffects[ , "Estimate"] - 1.96*fixedeffects[ , "Std Error"]
   fixedeffects$UL <- fixedeffects[ , "Estimate"] + 1.96*fixedeffects[ , "Std Error"]

   fixedeffects[ , c("Estimate", "Std Error", "LL", "UL")] = round(fixedeffects[ ,
      c("Estimate", "Std Error", "LL", "UL")], roundit)

     # Format p-values
   fixedeffects[ , "p-value"] <- format.pval(summary(mod)$tTable[, "p-value"], digits = 3, eps = 0.001)

   if(caption == "auto"){
      cptn <- gsub("_", "\\\\_", gsub("%", "\\\\%", paste("Parameter estimates for linear mixed model, ", outcomevarname, ":", label(dat[ , outcomevarname]),  sep = "")))}
   else cptn <- caption

   latex(fixedeffects,
      colheads = c("Estimate", "Std Error", "LL", "UL", "$p$-value"), title = "",
      landscape = FALSE,
      caption = cptn,
      label = lablep, where = "!h",
      #align = paste('l', paste(rep('r', ncol(fixedeffects)-1), collapse=''), sep = ""),
      file = "")
}
#########
glmoutput <- function(mod, rownms, lablep = "", caption = "auto", dat = dat, output = FALSE, table = TRUE){
   outcomevarname <- all.vars(mod$terms)[1]
   parameters <- data.frame(cbind(exp(mod$coefficients),
      exp(mod$coefficients - 1.96*summary(mod)$coefficients[ , "Std. Error"]), exp(mod$coefficients + 1.96*summary(mod)$coefficients[ , "Std. Error"]),
      summary(mod)$coefficients[ , "Pr(>|z|)"]))
   names(parameters) <- c("oddsratio","lower","upper","p-value")
   row.names(parameters) <- rownms
   parameters[ , "p-value"] <- format.pval(parameters[, "p-value"], digits = 4, eps = 0.0001)
   if(caption == "auto"){
      cptn <- gsub("_", "\\\\_", gsub("%", "\\\\%", paste("Parameter estimates for logistic regression model, ", outcomevarname, ":", label(dat[ , outcomevarname]),  sep = "")))}
   else cptn <- caption
   if(table == TRUE){
      latex(parameters, dec = 2, colheads = c("$e^{\\hat{\\beta}}$", "Lower", "Upper", "$p$-value"), label = lablep, where = "!h",
         title = "", caption = cptn, file = "")
   }
   if(output == TRUE){return(parameters)}
}
#############
# This works with lrm. May also work with glm. This is just formatting the basic output table with coefficients.
model.est.logistic <- function(mod, latex = TRUE, digits = 2, caption = NULL, rownms = NULL, labelp = NULL, output = FALSE, ...){
   est <- coef(mod)
   ses <- sqrt(diag(vcov(mod)))
   p   <- format.pval(2*pnorm(abs(est/ses), lower.tail = FALSE), eps = 0.0001, digits = 3)
   #est <- format(round((est),3),nsmall=2,scientific=FALSE)
   #Std.Error <- format(round(ses,3),nsmall=3,scientific=FALSE)
   table <- data.frame(est,p)
   if(!is.null(rownms)){
      row.names(table) <- rownms}
   table[ , c("OR", "Lower", "Uper")] <- normalCI(est = est, se = ses, exponentiate = TRUE,
      roundit = FALSE)
   names(table) <- c("beta_hat", "Pr(>|t|)", "exp(beta)", "OR lower", "OR upper")
   table <- table[ , c("beta_hat", "exp(beta)", "OR lower", "OR upper", "Pr(>|t|)")]
   if(output){return(table)}
   if(latex == TRUE){
   latex(table,
      colheads = c("$\\hat{\\beta}$", "$e^{\\hat{\\beta}}$ (Odds ratio)", "95\\% lower", "95\\% upper", "$p$-value"), title = "",
      dec = digits,
      caption = caption,
      col.just = rep("r", ncol(table)),
      label = labelp,
      where = "!h",
      booktabs = TRUE,
      file = "", ...)}}
#############
# for output of a model fit with multinom(). This is adapted from Jon's code.
multinomOutput <- function(fit, digits = 2, latex = TRUE, return = FALSE, caption = NULL, rownms = NULL, labelp = NULL, pdig = 3, ...){
   s <- summary(fit)

   # create an object to store the output for each of the k-1 comparisons
   output <- vector(mode = "list", length = length(fit$lev) - 1)

   for(i in 2:length(fit$lev)){               # the lev object are the levels of the outcome. The first one is the reference
      names(output)[i - 1] <- paste(fit$lev[i], " vs. ", fit$lev[1], sep = "")

      betaHat <- s$coefficients[(i-1),]                 # subtract one since our loop starts at 2.
      se <- s$standard.errors[(i-1),]
      zStat <- betaHat / se
      pval <- format.pval(2 * pnorm(abs(zStat), lower.tail = FALSE), digits = pdig, eps = 0.001)

      RRR <- exp(betaHat)
      RRR.lo <- exp(betaHat - qnorm(0.975)*se)
      RRR.up <- exp(betaHat + qnorm(0.975)*se)

      output[[i - 1]] <- data.frame(RRR = RRR, RRR.lo = RRR.lo, RRR.up = RRR.up, pval = pval)}

   if(return){return(output)}

   tmp <- do.call(rbind, output)
   if(is.null(rownms)) rownms <- rownames(output[[1]])

   if(latex == TRUE){
      latex(tmp,
      colheads = c("Relative risk ratio", "95\\% lower", "95\\% upper", "$p$-value"),
      title = "",
      dec = digits,
      caption = caption,
      label = labelp,
      where = "!h",
      booktabs = TRUE,
      col.just = rep("r", ncol(tmp)),
      #n.rgroup = ,
      rowname = rownms,
      rgroup = names(output),
      file = "", ...)}}
##########################
geeglmoutput <- function(mod, rownms = NULL, lablep = "", caption = "auto", dat = dat, output = FALSE, table = TRUE){
   outcomevarname <- all.vars(mod$terms)[1]
   parameters <- data.frame(cbind(exp(mod$coefficients),
      exp(mod$coefficients - 1.96*summary(mod)$coefficients[ , "Std.err"]), exp(mod$coefficients + 1.96*summary(mod)$coefficients[ , "Std.err"]),
      summary(mod)$coefficients[ , "Pr(>|W|)"]))
   names(parameters) <- c("oddsratio","lower","upper","p-value")
   if(is.null(rownms)){
      rownms <- rownames(summary(mod)$coefficients)}
   row.names(parameters) <- rownms
   parameters[ , "p-value"] <- format.pval(parameters[, "p-value"], digits = 4, eps = 0.0001)
   if(caption == "auto"){
      cptn <- gsub("_", "\\\\_", gsub("%", "\\\\%", paste("Parameter estimates for regression model, ", outcomevarname, ":", label(dat[ , outcomevarname]),  sep = "")))}
   else cptn <- caption
   if(table == TRUE){
      latex(parameters, dec = 2, colheads = c("$e^{\\hat{\\beta}}$", "Lower", "Upper", "$p$-value"), label = lablep, where = "!h",
         title = "", caption = cptn, file = "")
   }
   if(output == TRUE){return(parameters)}
}
####################
###################################################
## End of regression model output functions
###################################################

##################################################################################
##################################################################################
## Global scoring functions
##################################################################################
##################################################################################
reverseCode <- function(items, intercept, object){
   # items should be the names of the items in object that need to be reverse coded. If object is a matrix, can use colnames().
   for(i in items){
       object[ , i] <- -object[ , i] + intercept}
   return(object)}

reverseCodeWeird <- function(items, intercept, object){
   # items should be the names of the items in object that need to be reverse coded. If object is a matrix, can use colnames().
   for(i in items){
       object[ , i] <- -object[ , i] + intercept[i]}
   return(object)}

scoreItWeird <- function(itemNames, dataFrame, 
   reverseItemNames, 
   minNonmissingProp = 1, 
   scaleTo100 = FALSE,       # If true, this scales the instrument so that it goes from 0 to 100.
   itemMin = 0){
# Get relevant variables
# Make numeric
# Reverse code: happy and life
# Test missingness threshold?
# Sum
   instrMat <- subset(dataFrame, select = itemNames)

   # Make sure all the items are factors and have the same number of levels.
   nLevels <- sapply(instrMat, nlevels)

   # Make a numeric matrix
   numInstrMat <- sapply(instrMat, 
      FUN = function(q){as.numeric(q) - ifelse(itemMin == 0, 1, ifelse(itemMin == 1, 0, NA))})

   # Reverse code items # Needs update for vector nLevels
   numInstrMat <- reverseCodeWeird(items = reverseItemNames, 
      object = numInstrMat, 
      intercept = itemMin + nLevels - 1)

   # If there are different numbers of items on each question, then then each question
   #    needs to be multiplied by a different factor
   # If scaling is turned on, calculate the factor.
   numInstrMatS <- numInstrMat*100/(length(itemNames)*rep(nLevels - 1, each = nrow(numInstrMat)))

   # Test missingness and sum
   scores <- apply(numInstrMatS, 
      FUN = function(x, minNonmissingProp){
         propNonmissing <- mean(!is.na(x))
         return(if(propNonmissing >= minNonmissingProp){sum(x, na.rm = TRUE)/propNonmissing} else NA)},
      minNonmissingProp = minNonmissingProp, 
      MARGIN = 1)
   return(scores)}

scoreIt <- function(itemNames, dataFrame, 
   reverseItemNames, 
   minNonmissingProp = 1, 
   scaleTo100 = FALSE,       # If true, this scales the instrument so that it goes from 0 to 100.
   itemMin = 0){
# Get relevant variables
# Make numeric
# Reverse code: happy and life
# Test missingness threshold?
# Sum
   instrMat <- subset(dataFrame, select = itemNames)

   # Make sure all the items are factors and have the same number of levels.
   nLevels <- sapply(instrMat, nlevels)

   ## Need to warn for this. It doesn't work currently unless all items have the same number of choices.
   if(length(unique(nLevels)) != 1){stop("The items have differing numbers of choices (factor levels)! \n")}

   # Make a numeric matrix
   numInstrMat <- sapply(instrMat, 
      FUN = function(q){as.numeric(q) - ifelse(itemMin == 0, 1, ifelse(itemMin == 1, 0, NA))})

   # Reverse code items
   numInstrMat <- reverseCode(items = reverseItemNames, 
      object = numInstrMat, 
      intercept = itemMin*2 + nLevels[1] - 1)

   # If scaling is turned on, calculate the factor.
   # If there are different numbers of items on each question, then the factr needs to be calculated for each question.
   factr <- ifelse(!scaleTo100, 1, 100/(length(itemNames)*(nLevels - 1)))

   # Test missingness and sum
   scores <- apply(numInstrMat, 
      FUN = function(x, minNonmissingProp){
         propNonmissing <- mean(!is.na(x))
         return(if(propNonmissing >= minNonmissingProp){sum(x, na.rm = TRUE)/propNonmissing} else NA)},
      minNonmissingProp = minNonmissingProp, 
      MARGIN = 1)
   return(factr*scores)}

# This is for if the values for the answer choices are weird
scoreItFlex <- function(itemNames, dataFrame, choiceValues,
   reverseItemNames, 
   minNonmissingProp = 1, 
   scaleTo100 = FALSE){       # If true, this scales the instrument so that it goes from 0 to 100.
# Get relevant variables
# Make numeric
# Reverse code: happy and life
# Test missingness threshold?
# Sum
   instrMat <- subset(dataFrame, select = itemNames)

   # Make sure all the items are factors and have the same number of levels.
   nLevels <- sapply(instrMat, nlevels)

   ## Need to warn for this. It doesn't work currently unless all items have the same number of choices.
   if(length(unique(nLevels)) != 1){stop("The items have differing numbers of choices (factor levels)! \n")}

   # Make sure number of choices is the same length as the point values supplied
   if(nLevels[1] != length(choiceValues)){stop("Length of choiceValues is different from number of answer choices! \n")}

   # Make a numeric matrix
   numInstrMat <- sapply(instrMat, 
      FUN = function(q){choiceValues[as.numeric(q)]})

   # Reverse code items
   numInstrMat <- reverseCode(items = reverseItemNames, 
      object = numInstrMat, 
      intercept = min(choiceValues)*2 + nLevels[1] - 1)

   # If scaling is turned on, calculate the factor.
   # If there are different numbers of items on each question, then the factr needs to be calculated for each question.
   factr <- ifelse(!scaleTo100, 1, 100/(length(itemNames)*(nLevels - 1)))

   # Test missingness and sum
   scores <- apply(numInstrMat, 
      FUN = function(x, minNonmissingProp){
         propNonmissing <- mean(!is.na(x))
         return(if(propNonmissing >= minNonmissingProp){sum(x, na.rm = TRUE)/propNonmissing} else NA)},
      minNonmissingProp = minNonmissingProp, 
      MARGIN = 1)
   return(factr*scores)}
##################################################################################
##################################################################################
## End of global scoring functions
##################################################################################
##################################################################################
#########!!!!!!!!SYNC WITH OTHER FILE!!!!!!
maketable <- function(x, y, pval.digs = 3, test = FALSE, ...){
   OK <- complete.cases(x, y)
 
   tabl <- matrix(
      sprintf("%s (%0.f%%)", table(x, y), 100*prop.table(table(x, y), ...)),  
      ncol = 2,
      dimnames = list(levels(x), levels(y)))
   if(test) cbind(tabl, c(ifelse(!((nlevels(factor(x[OK])) < 2L) || (nlevels(factor(y[OK])) < 2L)), format.pval(chisq.test(x, y)$p.value, eps = 0.001, digits = pval.digs), NA), rep(NA, nlevels(x) - 1))) else tabl
}

maketable.df <- function(x, y, test = FALSE, ...){
   OK <- complete.cases(x, y)
 
   tabl <- data.frame(matrix(
      sprintf("%s (%0.f%%)", table(x, y), 100*prop.table(table(x, y), ...)),  
      ncol = 2,
      dimnames = list(levels(x), levels(y))), 
      stringsAsFactors = FALSE)
   if(test) tabl$pval <- c(ifelse(!((nlevels(factor(x[OK])) < 2L) || (nlevels(factor(y[OK])) < 2L)), chisq.test(x, y)$p.value, NA), rep(NA, nlevels(x) - 1))
   tabl}

      #apply(cbind(c(table(x, y)), 
      #c(paste("(", round(100*prop.table(table(x, y), ...), 0), "%)", sep = ""))), 
      #FUN = paste, MARGIN = 1, collapse = " "),

# Example
#Cole <- factor(c("Yes", "Yes", "No", "No"))
# Beck <- factor(c("No", "Yes", "No", "No"))
# colebeck <- data.frame(Cole, Beck)
#with(colebeck, maketable(y = Cole, x = Beck))
#with(colebeck, maketable(y = Cole, x = Beck, margin = 1))

# Cole wrote this. 
# myprop <- function(formula, data) {
#   trm <- as.character(attr(terms(formula), 'variables'))[-1]
#   x <- table(data[,trm[1]], data[,trm[2]])
#   y <- prop.table(x)
#  # list(
#     matrix(sprintf("%s (%0.f%%)", x, y*100), nrow=nrow(x), dimnames=dimnames(x)) #,
#   #  format.pval(chisq.test(trm[1], trm[2])$p.value, eps = 0.001, digits = pval.digs)
#  # )
# }
# myprop(Cole ~ Beck, colebeck)

#Should work on vectors if pasteit = FALSE, but does not work for vectors if vectors = TRUE.
loglogCI <- function(est, se, alpha = 0.05, digits = 2, exponentiate = FALSE, roundit = TRUE, pasteit = FALSE, ciOnly = FALSE){
     # See page 63 of Competing Risks by Pintilie
   # Wald
   A <- -qnorm(1-alpha/2)*se/(est*log(est))
   value <- cbind(est, est^(exp(A)), est^exp(-A))
   if(exponentiate == TRUE){value <- exp(value)}
   if(ciOnly == TRUE){value <- value[ , -1]}
   if(roundit == TRUE){value <- round(value, digits = digits)}
   if(pasteit == TRUE){
      if(ciOnly == TRUE){value <- paste("[", paste(value, collapse = ", "), "]", sep = "")} else {
      value <- paste(value[ , 1], ", ", "[", paste(value[ , c(2, 3)], collapse = ", "), "]", sep = "")}}
   #dimnames(value) <- list(names(est), c("est", "lower", "upper"))
   return(value)}
#########
#########
#########
#############
#############
# for output of a model fit with multinom(). This is adapted from Jon's code.
multinomOutput <- function(fit, digits = 2, latex = TRUE, return = FALSE, caption = NULL, rownms = NULL, labelp = NULL, pdig = 3, ...){
   s <- summary(fit)

   # create an object to store the output for each of the k-1 comparisons
   output <- vector(mode = "list", length = length(fit$lev) - 1)

   for(i in 2:length(fit$lev)){               # the lev object are the levels of the outcome. The first one is the reference
      names(output)[i - 1] <- paste(fit$lev[i], " vs. ", fit$lev[1], sep = "")

      betaHat <- s$coefficients[(i-1),]                 # subtract one since our loop starts at 2.
      se <- s$standard.errors[(i-1),]
      zStat <- betaHat / se
      pval <- format.pval(2 * pnorm(abs(zStat), lower.tail = FALSE), digits = pdig, eps = 0.001)

      RRR <- exp(betaHat)
      RRR.lo <- exp(betaHat - qnorm(0.975)*se)
      RRR.up <- exp(betaHat + qnorm(0.975)*se)

      output[[i - 1]] <- data.frame(RRR = RRR, RRR.lo = RRR.lo, RRR.up = RRR.up, pval = pval)}

   if(return){return(output)} 

   tmp <- do.call(rbind, output)
   if(is.null(rownms)) rownms <- rownames(output[[1]])

   if(latex == TRUE){
      latex(tmp, 
      colheads = c("Relative risk ratio", "95\\% lower", "95\\% upper", "$p$-value"),
      title = "",
      dec = digits,
      caption = caption,
      label = labelp, 
      where = "!h", 
      booktabs = TRUE,
      col.just = rep("r", ncol(tmp)),
      #n.rgroup = ,
      rowname = rownms,
      rgroup = names(output),
      file = "", ...)}}
##########################
geeglmoutput <- function(mod, rownms = NULL, lablep = "", caption = "auto", dat = dat, output = FALSE, table = TRUE){
   outcomevarname <- all.vars(mod$terms)[1]
   parameters <- data.frame(cbind(exp(mod$coefficients), 
      exp(mod$coefficients - 1.96*summary(mod)$coefficients[ , "Std.err"]), exp(mod$coefficients + 1.96*summary(mod)$coefficients[ , "Std.err"]), 
      summary(mod)$coefficients[ , "Pr(>|W|)"]))
   names(parameters) <- c("oddsratio","lower","upper","p-value")
   if(is.null(rownms)){
      rownms <- rownames(summary(mod)$coefficients)}
   row.names(parameters) <- rownms  
   parameters[ , "p-value"] <- format.pval(parameters[, "p-value"], digits = 4, eps = 0.0001)
   if(caption == "auto"){
      cptn <- gsub("_", "\\\\_", gsub("%", "\\\\%", paste("Parameter estimates for regression model, ", outcomevarname, ":", label(dat[ , outcomevarname]),  sep = "")))}
   else cptn <- caption
   if(table == TRUE){
      latex(parameters, dec = 2, colheads = c("$e^{\\hat{\\beta}}$", "Lower", "Upper", "$p$-value"), label = lablep, where = "!h",
         title = "", caption = cptn, file = "")
   }
   if(output == TRUE){return(parameters)}  
}

###
# This function will output model results. for what model? lme?
modeloutput <- function(mod, rownms, lablep = "", caption = "auto", dat = plfirstyear, roundit = 3){
   outcomevarname <- all.vars(mod$terms)[1]
   # Model-based parameter estimates.
   # Format results into a table

   fixedeffects <- data.frame(summary(mod)$tTable)
   
   names(fixedeffects) <- c("Estimate","Std Error","LL","UL","p-value")
   row.names(fixedeffects) <- rownms    

	## Get CI for Pparameters
   fixedeffects$LL <- fixedeffects[ , "Estimate"] - 1.96*fixedeffects[ , "Std Error"]
   fixedeffects$UL <- fixedeffects[ , "Estimate"] + 1.96*fixedeffects[ , "Std Error"]
 
   fixedeffects[ , c("Estimate", "Std Error", "LL", "UL")] = round(fixedeffects[ , 
      c("Estimate", "Std Error", "LL", "UL")], roundit)
 
     # Format p-values
   fixedeffects[ , "p-value"] <- format.pval(summary(mod)$tTable[, "p-value"], digits = 3, eps = 0.001)

   if(caption == "auto"){
      cptn <- gsub("_", "\\\\_", gsub("%", "\\\\%", paste("Parameter estimates for linear mixed model, ", outcomevarname, ":", label(dat[ , outcomevarname]),  sep = "")))}
   else cptn <- caption

   latex(fixedeffects, 
      colheads = c("Estimate", "Std Error", "LL", "UL", "$p$-value"), title = "",
      landscape = FALSE, 
      caption = cptn,
      label = lablep, where = "!h", 
      #align = paste('l', paste(rep('r', ncol(fixedeffects)-1), collapse=''), sep = ""),  
      file = "") 
}
###################################################
## Survival analysis functions (other than model output, which is above)
###################################################

#Should work on vectors if pasteit = FALSE, but does not work for vectors if vectors = TRUE.
loglogCI <- function(est, se, alpha = 0.05, digits = 2, exponentiate = FALSE, roundit = TRUE, pasteit = FALSE, ciOnly = FALSE){
     # See page 63 of Competing Risks by Pintilie
   # Wald
   A <- -qnorm(1-alpha/2)*se/(est*log(est))
   value <- cbind(est, est^(exp(A)), est^exp(-A))
   if(exponentiate == TRUE){value <- exp(value)}
   if(ciOnly == TRUE){value <- value[ , -1]}
   if(roundit == TRUE){value <- round(value, digits = digits)}
   if(pasteit == TRUE){
      if(ciOnly == TRUE){value <- paste("[", paste(value, collapse = ", "), "]", sep = "")} else {
      value <- paste(value[ , 1], ", ", "[", paste(value[ , c(2, 3)], collapse = ", "), "]", sep = "")}}
   #dimnames(value) <- list(names(est), c("est", "lower", "upper"))
   return(value)}


###################################################
## Misc functions
###################################################
sourceHttps <- function(url){
   library(RCurl)
   eval(expr = parse(text = getURL(url)), envir = parent.frame())}
########################
exclude <- function(constraints, data, idvarname){
   currentn <- rep(NA, length(constraints))
   tempids <- data[[idvarname]]

   for(iter in 1:length(constraints)){
      tempids <- intersect(tempids, data[[idvarname]][constraints[[iter]]])
      currentn[iter] <- length(tempids)
   }
   output <- data.frame(names(constraints), currentn)
   output$excluded <- c(0, -1 * diff(currentn))
   output$cumexcluded <- cumsum(output$excluded)
   names(output) = c("Criteria", "N", "Number excluded", "Cumulative number excluded")

   analysisdata <- data[data[[idvarname]] %in% tempids, ]
   rownames(analysisdata) <- 1:nrow(analysisdata)
   excludedData <- data[!(data[[idvarname]] %in% tempids), ]

   # Variables that are all missing
   #varsAllMissing <-
   return(list(table = output, analysisdata = analysisdata, excludedData = excludedData))}
#######################################
# This function is to control false discovery rate with the function of V is 1.
fdr <- function(Q = 0.1, pvals, testnames){
   V <- length(pvals)
   tmp <- data.frame(testnames = testnames, pvals = pvals, compare = Q*rank(pvals)/V)
   tmp$intermediate <- pmin(tmp$pvals, tmp$compare)
   tmp$comparison <- ifelse(tmp$pvals == tmp$intermediate, TRUE, FALSE)
   tmp$intermediate <- NULL
   label(tmp$testnames) <- "Test"
   label(tmp$pvals) <- "Unadjusted p-value"
   label(tmp$compare) <- paste(Q, "*i/m", sep = "")
   label(tmp$comparison) <- paste("Significant at the Q = ", Q, " level?", sep = "")
   return(tmp[order(tmp$pvals), ])}
###################################################
## End of misc functions
###################################################
###################################################
##  Simulation study functions
###################################################
mcmcCI <- function(obj, digits = 3, mean = TRUE, truth = NULL, vars = NULL){
   fred <- summary(obj)$quantiles[ , c("2.5%", "97.5%")]
   if(mean){fred <- cbind(summary(obj)$statistics[ , "Mean"], fred)
      colnames(fred)[1] <- "mean"}
   if(!is.null(vars)){fred <- fred[vars, ]}
   if(!is.null(truth)){fred <- cbind(truth, fred)}
   return(fred)}

plot.mcmcCI <- function(obj, truth = NULL, ...){
   fred <- mcmcCI(obj, ...)
   par(mar = c(3, 5, 1, 1) + 0.1)
   plot(fred[ , "mean"], nrow(fred):1, xlim = c(min(fred), max(fred)),
      yaxt = "n", pch = 19, xlab = "", ylab = "")
   arrows(x0 = fred[ , "2.5%"], x1 = fred[ , "97.5%"], y0 = nrow(fred):1, angle = 90,
      length = 0.08, code = 3)
   axis(side = 2, at = nrow(fred):1, labels = rownames(fred), las = 1)
   if(!is.null(truth)){points(truth, nrow(fred):1, pch = 8)}
   par(mar = c(5, 4, 4, 2) + 0.1)}

plot.normalCI <- function(obj, truth = NULL, ...){
   par(mar = c(3, 5, 1, 1) + 0.1)
   plot(obj[ , "est"], nrow(obj):1, xlim = c(min(obj), max(obj)),
      yaxt = "n", pch = 19, xlab = "", ylab = "")
   arrows(x0 = obj[ , 2], x1 = obj[ , 3], y0 = nrow(obj):1, angle = 90,
      length = 0.08, code = 3)
   axis(side = 2, at = nrow(obj):1, labels = rownames(obj), las = 1)
   if(!is.null(truth)){points(truth, nrow(obj):1, pch = 8)}
   par(mar = c(5, 4, 4, 2) + 0.1)}
###################################################
## End of simulation study functions
###################################################

###################################################
##  functions
###################################################



###################################################
## End of  functions
###################################################


###################################################
##  functions
###################################################



###################################################
## End of  functions
###################################################

###################################################
##  functions
###################################################



###################################################
## End of  functions
###################################################

###################################################
##  functions
###################################################



###################################################
## End of  functions
###################################################







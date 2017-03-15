# This is a function to do summaryM with the percentages calculated for the rows 
# rather than by columns, so the row percentages add up to 100. 

# 
# latex.summaryM.backwards(
#    summaryM(x1 + x2 + x3 ~ y, 
#       data = data,
#       test = FALSE,
#       overall = FALSE),
#    caption = "Impressed?",
#    long = FALSE,
#    what = "%", 
#    exclude1 = TRUE, 
#    landscape = FALSE,
#    longtable = TRUE,
#    booktabs = TRUE,
#    prn = TRUE,
#    pctdig = 0, 
#    round = 0,
#    lines.page = 200,
#    file = "")



formatCatsBackwards <- function(tab, nam, tr, type, group.freq, what = c("%", "proportion"), 
    npct, pctdig, exclude1, long, prtest, latex = FALSE, testUsed = character(0), 
    npct.size = "scriptsize", pdig = 3, eps = 0.001, footnoteTest = TRUE, 
    dotchart = FALSE) 
{
    what <- match.arg(what)
    gnames <- names(group.freq)
    nr <- nrow(tab)
    if (ncol(tab) < length(group.freq)) {
        tabfull <- matrix(NA, nrow = nr, ncol = length(group.freq), 
            dimnames = list(dimnames(tab)[[1]], gnames))
        tabfull[, dimnames(tab)[[2]]] <- tab
        tab <- tabfull
    }
    denom <- if (type == 1) 
        apply(tab, 1, sum)                          ################ changed margin from 1 to 2
    else group.freq
    pct <- if (ncol(tab) > 1) 
        sweep(tab, 1, denom, FUN = "/")               ################ changed margin from 1 to 2
    else tab/denom
    pct <- pct * (if (what == "%") 
        100
    else 1)
    cpct <- paste(format(round(pct, pctdig)), if (latex && what == 
        "%") 
        "\\%"
    else if (what == "%") 
        "%", sep = "")
    denom.rep <- matrix(rep(format(denom), nr), nrow = nr, byrow = TRUE)
    if (npct != "none") 
        cpct <- paste(cpct, if (latex) 
            switch(npct, numerator = paste("{\\", npct.size, 
                " (", format(tab), ")}", sep = ""), denominator = paste("{\\", 
                npct.size, " of", denom.rep, "}"), both = paste("{\\", 
                npct.size, " $\\frac{", format(tab), "}{", denom.rep, 
                "}$}", sep = ""))
        else switch(npct, numerator = paste("(", format(tab), 
            ")", sep = ""), denominator = paste("of", denom.rep), 
            both = paste(format(tab), "/", denom.rep, sep = "")))
    if (latex) 
        cpct <- sedit(cpct, " ", "~")
    dim(cpct) <- dim(pct)
    dimnames(cpct) <- dimnames(pct)
    cpct[is.na(pct)] <- ""
    lev <- dimnames(pct)[[1]]
    exc <- exclude1 && (nr == 2) && (type == 1)
    rl <- casefold(dimnames(pct)[[1]])
    binary <- type == 1 && exc && (all(rl %in% c("0", "1")) | 
        all(rl %in% c("false", "true")) | all(rl %in% c("absent", 
        "present")))
    if (binary) 
        long <- FALSE
    jstart <- if (exc) 
        2
    else 1
    nw <- if (lg <- length(group.freq)) 
        lg
    else 1
    lab <- if (binary) 
        nam
    else if (long) 
        c(nam, paste("   ", lev[jstart:nr]))
    else c(paste(nam, ":", lev[jstart]), if (nr > jstart) paste("   ", 
        lev[(jstart + 1):nr]))
    cs <- matrix("", nrow = long + (if (exc) 
        nr - 1
    else nr), ncol = nw + (length(tr) > 0), dimnames = list(lab, 
        c(gnames, if (length(tr)) "" else NULL)))
    if (nw == 1) 
        cs[(long + 1):nrow(cs), 1] <- cpct[jstart:nr, ]
    else cs[(long + 1):nrow(cs), 1:nw] <- cpct[jstart:nrow(cpct), 
        gnames]
    if (latex && dotchart && ncol(pct) <= 3) {
        locs <- c(3, -3, 5, -5, 7, -7, 9, -9)
        points <- c("\\circle*{4}", "\\circle{4}", "\\drawline(0,2)(-1.414213562,-1)(1.414213562,-1)(0,2)")
        point.loc <- sapply(jstart:nrow(pct), function(i) {
            paste(ifelse(is.na(pct[i, ]), "", paste("\\put(", 
                pct[i, ], ",0){", points[1:ncol(pct)], "}", sep = "")), 
                collapse = "")
        })
        error.loc <- character(nrow(tab) - exc)
        k <- 0
        for (i in jstart:ncol(tab)) {
            if (i > jstart) {
                p1prime <- (tab[, i] + 1)/(denom[i] + 2)
                d1 <- p1prime * (1 - p1prime)/denom[i]
                for (j in jstart:(i - 1)) {
                  k <- k + 1
                  p2prime <- (tab[, j] + 1)/(denom[j] + 2)
                  error <- 196 * sqrt(d1 + p2prime * (1 - p2prime)/denom[j])
                  bar <- ifelse(is.na(error), "", paste("\\put(", 
                    (pct[, i] + pct[, j])/2 - error, ",", locs[k], 
                    "){\\line(1,0){", error * 2, "}}", sep = ""))
                  error.loc <- paste(error.loc, bar, sep = "")
                }
            }
        }
        scale <- character(nrow(tab) - exc)
        scale[1] <- "\\multiput(0,2)(25,0){5}{\\color[gray]{0.5}\\line(0,-1){4}}\\put(-5,0){\\makebox(0,0){\\tiny 0}}\\put(108,0){\\makebox(0,0){\\tiny 1}}"
        cl <- paste("\\setlength\\unitlength{1in/100}\\begin{picture}(100,10)(0,-5)", 
            scale, "\\put(0,0){\\color[gray]{0.5}\\line(1,0){100}}", 
            point.loc, error.loc, "\\end{picture}", sep = "")
        cs[(long + 1):nrow(cs), ncol(cs)] <- cl
    }
    if (length(tr)) {
        ct <- formatTestStats(tr, type == 3, if (type == 1) 
            1
        else 1:nr, prtest, latex = latex, testUsed = testUsed, 
            pdig = pdig, eps = eps, footnoteTest = footnoteTest)
        if (length(ct) == 1) 
            cs[1, ncol(cs)] <- ct
        else cs[(long + 1):nrow(cs), ncol(cs)] <- ct
    }
    cs
}


## overall must be set to FALSE 
latex.summaryM.backwards <- function (object, title = first.word(deparse(substitute(object))), 
    file = paste(title, "tex", sep = "."), append = FALSE, digits, 
    prn = any(n != N), what = c("proportion", "%"), pctdig = if (what == 
        "%") 0 else 2, npct = c("numerator", "both", "denominator", 
        "none"), npct.size = "scriptsize", Nsize = "scriptsize", 
    exclude1 = TRUE, vnames = c("labels", "names"), prUnits = TRUE, 
    middle.bold = FALSE, outer.size = "scriptsize", caption, 
    rowlabel = "", insert.bottom = TRUE, dcolumn = FALSE, formatArgs = NULL, 
    round = NULL, prtest = c("P", "stat", "df", "name"), prmsd = FALSE, 
    msdsize = NULL, long = FALSE, pdig = 3, eps = 0.001, auxCol = NULL, 
    table.env = TRUE, ...) 
{
    if (!append) 
        cat("", file = file)
    append <- TRUE
    what <- match.arg(what)
    npct <- match.arg(npct)
    vnames <- match.arg(vnames)
    if (is.logical(prtest) && !prtest) 
        prtest <- "none"
    strats <- names(object$results)
    for (strat in strats) {
        x <- object$results[[strat]]
        stats <- x$stats
        nv <- length(stats)
        cstats <- lab <- character(0)
        nn <- integer(0)
        type <- x$type
        n <- x$n
        N <- x$N
        nams <- names(stats)
        labels <- x$labels
        Units <- x$units
        nw <- if (lg <- length(x$group.freq)) 
            lg
        else 1
        gnames <- names(x$group.freq)
        test <- x$testresults
        if (!length(test)) 
            prtest <- "none"
        gt1.test <- if (all(prtest == "none")) 
            FALSE
        else length(unique(sapply(test, function(a) a$testname))) > 
            1
        if (!missing(digits)) {
            oldopt <- options(digits = digits)
            on.exit(options(oldopt))
        }
        if (missing(caption)) 
            caption <- paste("Descriptive Statistics", if (length(x$group.label)) 
                paste(" by", x$group.label)
            else paste("  $(N=", x$N, ")$", sep = ""), sep = "")
        bld <- if (middle.bold) 
            "\\bf "
        else ""
        cstats <- NULL
        testUsed <- auxc <- character(0)
        for (i in 1:nv) {
            if (length(auxCol)) 
                auxc <- c(auxc, auxCol[[1]][i])
            nn <- c(nn, n[i])
            nam <- if (vnames == "names") 
                nams[i]
            else labels[i]
            if (prUnits && nchar(Units[i]) > 0) 
                nam <- paste(nam, "~\\hfill\\tiny{", translate(Units[i], 
                  "*", " "), "}", sep = "")
            tr <- if (length(test) && all(prtest != "none")) 
                test[[nams[i]]]
            else NULL
            if (length(test) && all(prtest != "none")) 
                testUsed <- unique(c(testUsed, tr$testname))
            if (type[i] == 1 || type[i] == 3) {
                cs <- formatCatsBackwards(stats[[i]], nam, tr, type[i], 
                  if (length(x$group.freq)) 
                    x$group.freq
                  else x$n[i], what, npct, pctdig, exclude1, 
                  long, prtest, latex = TRUE, testUsed = testUsed, 
                  npct.size = npct.size, pdig = pdig, eps = eps, 
                  footnoteTest = gt1.test)
                nn <- c(nn, rep(NA, nrow(cs) - 1))
            }
            else cs <- formatCons(stats[[i]], nam, tr, x$group.freq, 
                prmsd, prtest = prtest, formatArgs = formatArgs, 
                round = round, testUsed = testUsed, 
                middle.bold = middle.bold, outer.size = outer.size, 
                msdsize = msdsize, pdig = pdig, eps = eps, footnoteTest = gt1.test)
            cstats <- rbind(cstats, cs)
            if (length(auxc) && nrow(cstats) > 1) 
                auxc <- c(auxc, rep(NA, nrow(cs) - 1))
        }
        lab <- dimnames(cstats)[[1]]
        gl <- names(x$group.freq)
        if (!length(gl)) 
            gl <- " "
        lab <- sedit(lab, c(" ", "&"), c("~", "\\&"))
        lab <- latexTranslate(lab, greek = TRUE)
        gl <- latexTranslate(gl, greek = TRUE)
        extracolheads <- if (any(gl != " ")) 
            c(if (prn) "", paste("$N=", x$group.freq, "$", sep = ""))
        else NULL
        if (length(test) && !all(prtest == "none")) {
            gl <- c(gl, if (length(prtest) == 1 && prtest != 
                "stat") if (prtest == "P") "P-value" else prtest else "Test Statistic")
            if (length(extracolheads)) 
                extracolheads <- c(extracolheads, "")
        }
        dimnames(cstats) <- list(NULL, gl)
        cstats <- data.frame(cstats, check.names = FALSE, stringsAsFactors = FALSE)
        col.just <- rep("c", length(gl))
        if (dcolumn && all(prtest != "none") && gl[length(gl)] %in% 
            c("P-value", "Test Statistic")) 
            col.just[length(col.just)] <- "."
        if (prn) {
            cstats <- data.frame(N = nn, cstats, check.names = FALSE, 
                stringsAsFactors = FALSE)
            col.just <- c("r", col.just)
        }
        legend <- character()
        if (any(type == 2)) 
            legend <- paste("{\\", outer.size, " $a$\\ }{", bld, 
                "$b$\\ }{\\", outer.size, " $c$\\ } represent the lower quartile $a$, the median $b$, and the upper quartile $c$\\ for continuous variables.", 
                if (prmsd) 
                  "~~$x\\pm s$ represents $\\bar{X}\\pm 1$ SD."
                else "", sep = "")
        if (prn) 
            legend <- c(legend, "$N$\\ is the number of non--missing values.")
        if (any(type == 1) && npct == "numerator") 
            legend <- c(legend, "Numbers after percents are frequencies.")
        if (length(testUsed)) 
            legend <- c(legend, if (length(testUsed) == 1) "\\noindent Test used:" else "\\indent Tests used:", 
                if (length(testUsed) == 1) paste(testUsed, "test") else paste(paste("\\textsuperscript{\\normalfont ", 
                  1:length(testUsed), "}", testUsed, " test", 
                  sep = ""), collapse = "; "))
        if (length(auxc)) {
            if (length(auxc) != nrow(cstats)) 
                stop(paste("length of auxCol (", length(auxCol[[1]]), 
                  ") is not equal to number or variables in table (", 
                  nv, ").", sep = ""))
            auxcc <- format(auxc)
            auxcc[is.na(auxc)] <- ""
            cstats <- cbind(auxcc, cstats)
            nax <- names(auxCol)
            heads <- get2rowHeads(nax)
            names(cstats)[1] <- heads[[1]]
            if (length(col.just)) 
                col.just <- c("r", col.just)
            if (length(extracolheads)) 
                extracolheads <- c(heads[2], extracolheads)
        }
        if (length(legend) && !table.env) 
            legend[1] <- paste("\n", legend[1], sep = "")
        laststrat <- strat == strats[length(strats)]
        noib <- is.logical(insert.bottom) && !insert.bottom
        w <- latex(cstats, title = title, file = file, append = TRUE, 
            caption = if (table.env) 
                paste(caption, if (laststrat) 
                  paste(legend, collapse = " "), sep = ". "), 
            rowlabel = rowlabel, table.env = table.env, col.just = col.just, 
            numeric.dollar = FALSE, insert.bottom = if (!noib && 
                laststrat && !table.env) 
                legend, rowname = lab, dcolumn = dcolumn, extracolheads = extracolheads, 
            extracolsize = Nsize, insert.top = if (strat != ".ALL.") 
                strat, ...)
        attr(w, "legend") <- legend
    }
    w
}

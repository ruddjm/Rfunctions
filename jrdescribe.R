###############################################################################
## jrdescribe.R This defines three functions to customize describe().
###############################################################################

library(moments)
library(Hmisc)

jrdescribe.vector <- function (x, descript, exclude.missing = TRUE, digits = 4, listunique = 0,
    listnchar = 12, weights = NULL, normwt = FALSE, minlength = NULL,
    ...)
{
    oldopt <- options(digits = digits)
    on.exit(options(oldopt))
    if (length(weights) == 0) {
        weights <- rep(1, length(x))
    }
    special.codes <- attr(x, "special.miss")$codes
    labx <- attr(x, "label")
    if (missing(descript)) {
        descript <- as.character(sys.call())[2]
    }
    if (length(labx) && labx != descript) {
        descript <- paste(descript, ":", labx)
    }
    un <- attr(x, "units")
    if (length(un) && un == "") {
        un <- NULL
    }
    fmt <- attr(x, "format")
    if (length(fmt) && (is.function(fmt) || fmt == "")) {
        fmt <- NULL
    }
    if (length(fmt) > 1) {
        fmt <- paste(as.character(fmt[[1]]), as.character(fmt[[2]]))
    }
    present <- if (all(is.na(x)))
        rep(FALSE, length(x))
    else if (is.character(x))
        (if (.R.)
            x != "" & x != " " & !is.na(x)
        else x != "" & x != " ")
    else !is.na(x)
    present <- present & !is.na(weights)
    if (length(weights) != length(x))
        stop("length of weights must equal length of x")
    if (normwt) {
        weights <- sum(present) * weights/sum(weights[present])
        n <- sum(present)
    }
    else {
        n <- sum(weights[present])
    }
    if (exclude.missing && n == 0) {
        return(structure(NULL, class = "describe"))
    }
    missing <- sum(weights[!present], na.rm = TRUE)
    atx <- attributes(x)
    atx$names <- atx$dimnames <- atx$dim <- atx$special.miss <- NULL
    atx$class <- atx$class[atx$class != "special.miss"]
    isdot <- testDateTime(x, "either")
    isdat <- testDateTime(x, "both")
    x <- x[present, drop = FALSE]
    x.unique <- sort(unique(x))
    weights <- weights[present]
    n.unique <- length(x.unique)
    attributes(x) <- attributes(x.unique) <- atx
    isnum <- (is.numeric(x) || isdat) && !is.category(x)
    timeUsed <- isdat && testDateTime(x.unique, "timeVaries")
    z <- list(descript = descript, units = un, format = fmt)
    
####################################################
    counts <- c(n, missing)

##################################################
    lab <- c("n", "missing")
    if (length(special.codes)) {
        tabsc <- table(special.codes)
        counts <- c(counts, tabsc)
        lab <- c(lab, names(tabsc))
    }
    if (length(atx$imputed)) {
        counts <- c(counts, length(atx$imputed))
        lab <- c(lab, "imputed")
    }
    if (length(pd <- atx$partial.date)) {
        if ((nn <- length(pd$month)) > 0) {
            counts <- c(counts, nn)
            lab <- c(lab, "missing month")
        }
        if ((nn <- length(pd$day)) > 0) {
            counts <- c(counts, nn)
            lab <- c(lab, "missing day")
        }
        if ((nn <- length(pd$both)) > 0) {
            counts <- c(counts, nn)
            lab <- c(lab, "missing month,day")
        }
    }
    if (length(atx$substi.source)) {
        tabss <- table(atx$substi.source)
        counts <- c(counts, tabss)
        lab <- c(lab, names(tabss))
    }
    counts <- c(counts, n.unique)
    lab <- c(lab, "unique")
    x.binary <- n.unique == 2 && isnum && x.unique[1] == 0 &&
        x.unique[2] == 1
    if (x.binary) {
        counts <- c(counts, sum(weights[x == 1]))
        lab <- c(lab, "Sum")
    }
    if (isnum) {
        xnum <- if (.SV4.)
            as.numeric(x)
        else oldUnclass(x)
        if (isdot) {
            dd <- sum(weights * xnum)/sum(weights)
            fval <- formatDateTime(dd, atx, !timeUsed)
            counts <- c(counts, fval)
        }
        else {
############################
            counts <- c(counts, format(sum(weights * x)/sum(weights)), format(sd(x))
                )
        }
        lab <- c(lab, "Mean", "Standard Deviation")
    }
    else if (n.unique == 1) {
        counts <- c(counts, x.unique)
        lab <- c(lab, "value")
    }
    # Take out precentiles.
    if (n.unique >= 6 & isnum) {
        q <- if (any(weights != 1)) {
            wtd.quantile(xnum, weights, normwt = FALSE, na.rm = FALSE,
                probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
        }
        else {
            quantile(xnum, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9,
                0.95), na.rm = FALSE)
        }
        fval <- if (isdot)
            formatDateTime(q, atx, !timeUsed)
        else format(q, ...)
        counts <- c(counts, fval)
        lab <- c(lab, ".05", ".10", ".25", ".50", ".75", ".90",
            ".95")
    }
    names(counts) <- lab
    z$counts <- counts
    counts <- NULL
    tableIgnoreCaseWhiteSpace <- function(x) {
        x <- gsub("\r", " ", x)
        x <- gsub("^[[:space:]]+", "", gsub("[[:space:]]+$",
            "", x))
        x <- gsub("[[:space:]]+", " ", x)
        y <- tolower(x)
        f <- table(y)
        names(f) <- x[match(names(f), y)]
        f
    }
    if (inherits(x, "mChoice"))
        z$mChoice <- summary(x, minlength = minlength)
    else {
        if (n.unique <= listunique && !isnum && !is.category(x) &&
            max(nchar(x)) > listnchar)
            counts <- tableIgnoreCaseWhiteSpace(x)
        else {
   # Extend histogram to more variables.  (?)
            if (n.unique >= 6 & !is.category(x) & !is.character(x)) {
                if (isnum) {
                  r <- range(xnum)
                  xg <- pmin(1 + floor((100 * (xnum - r[1]))/(r[2] -
                    r[1])), 100)
                  z$intervalFreq <- list(range = as.single(r),
                    count = as.integer(tabulate(xg)))
                }
                loandhi <- x.unique[c(1:5, (n.unique - 4):n.unique)]
                fval <- if (isdot && (class(loandhi) %nin% "timeDate")) {
                  formatDateTime(oldUnclass(loandhi), at = atx,
                    roundDay = !timeUsed)
                }
                else format(format(loandhi), ...)
                counts <- fval
                names(counts) <- c("L1", "L2", "L3", "L4", "L5",
                  "H5", "H4", "H3", "H2", "H1")
            }
   # Take out frequency table except for categorical vars
            if (is.character(x) | is.category(x) | n.unique <= 10) {
                tab <- wtd.table(if (isnum && isdat)
                  format(x)
                else x, weights, normwt = FALSE, na.rm = FALSE,
                  type = "table")
                pct <- round(100 * tab/sum(tab))
                counts <- t(as.matrix(tab))
                counts <- rbind(counts, pct)
                dimnames(counts)[[1]] <- c("Frequency", "%")
            }
        }
    }
    z$values <- counts
    structure(z, class = "describe")
}

################################################################
jrdescribe.matrix <- function (x, descript, exclude.missing = TRUE, digits = 4, ...)
{
    if (missing(descript))
        descript <- as.character(sys.call())[2]
    nam <- dimnames(x)[[2]]
    if (length(nam) == 0)
        stop("matrix does not have column names")
    Z <- vector("list", length(nam))
    names(Z) <- nam
    d <- dim(x)
    missing.vars <- NULL
    for (i in 1:ncol(x)) {
        z <- jrdescribe.vector(x[, i], nam[i], exclude.missing = exclude.missing,
            digits = digits, ...)
        Z[[i]] <- z
        if (exclude.missing && length(z) == 0)
            missing.vars <- c(missing.vars, nam[i])
    }
    attr(Z, "descript") <- descript
    attr(Z, "dimensions") <- d
    attr(Z, "missing.vars") <- missing.vars
    structure(Z, class = "describe")
}

###############################################################
jrdescribe.data.frame <- function (x, descript, exclude.missing = TRUE, digits = 4, ...)
{
    if (missing(descript))
        descript <- as.character(sys.call())[2]
    nam <- names(x)
    Z <- list()
    nams <- character(0)
    i <- 0
    missing.vars <- NULL
    for (xx in x) {
        mat <- is.matrix(xx)
        i <- i + 1
        z <- if (mat)
            jrdescribe.matrix(xx, nam[i], exclude.missing = exclude.missing,
                digits = digits, ...)
        else jrdescribe.vector(xx, nam[i], exclude.missing = exclude.missing,
            digits = digits, ...)
        all.missing <- length(z) == 0
        if (exclude.missing && all.missing)
            missing.vars <- c(missing.vars, nam[i])
        else {
            Z <- c(Z, if (mat) z else list(z))
            nams <- c(nams, if (mat) names(z) else nam[i])
        }
    }
    names(Z) <- nams
    attr(Z, "descript") <- descript
    attr(Z, "dimensions") <- dim(x)
    attr(Z, "missing.vars") <- missing.vars
    structure(Z, class = "describe")
}

#save(list = c("jrdescribe.vector", "jrdescribe.matrix", "jrdescribe.data.frame"), file = "jrdescribe.rdata")

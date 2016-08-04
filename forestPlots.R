 
## Function to make forest plots after data are ready


# Args:
# dats: a matrix with 3 columns. Column 1 is the estimate, and 2 and 3 are the lower and upper limits of the CI.
# rownms: will be used as labels for each etimate and CI.
# refLine: the x coordinate for a vertical reference line. If not give, none will be plotted.

forestplot <- function(dats, rownms, estimateLabel, refLine){
   if(nrow(dats) != length(rownms)) stop("What the heck are you trying to do?\n")
   ys <- rev(seq_len(nrow(dats)))

   currentPar <- par(no.readonly = TRUE) # the whole list of settable par's.
   par(mar = c(4, 17, 2, 0) + 0.1)
   plot(x = dats[ , 1],
      y = ys,
      xlim = c(min(dats), max(dats)) + c(-1, 1)*0.2,
      ylab = "",
      bty = "n",
      ylim = c(0.5, max(ys) + 0.5),
      las = 1,
      yaxt = "n",
      xlab = estimateLabel)

   axis(side = 2,
      at = ys, # + 0.5,
      tcl = 0,
      las = 1,
      hadj = 0.5,
      font.axis = 2,
      line = 7,
      lty = "blank",
      labels = rownms)

   arrows(x0 = dats[ , 2],
      x1 = dats[, 3],
      angle = 90,
      lty = 3,
      length = 0.07,           # length of arrow heads
      code = 3,               # heads at both ends of arrows
      y0 = ys)
   if(!is.null(refLine)){
      abline(v = refLine, lty = 4, col = 328)}
   par(currentPar)}

## Example
runExample <- FALSE
if(runExample){
   dats <- matrix(c(c(2.6, 3.8, -4), c(2.55, 0.4, -6), c(2.71, 6, 2)),
      ncol = 3,
      byrow = FALSE,
      dimnames = list(NULL, c("p", "l", "u")))
   rownms = c("penny", "loves", "kenny")
   print(dats)
   print(rownms)
   forestplot(dats = dats, rownms = rownms, estimateLabel = "Hebrides", refLine = 0)}
rm(runExample)







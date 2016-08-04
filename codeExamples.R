

## Tile plot from ggplot
# Show counts of two categorical vars
library(ggplot2)
tabl <- with(SRA, data.frame(prop.table(table(employ.F00, income.Derived), margin = 2)))

ggplot(tabl, aes(x = employ.F00, y = income.Derived)) +
   geom_tile(aes(fill = Freq)) +
   scale_fill_gradient(low="white", high="blue")+theme_bw()




## Display counts of two categorical variables
# Note that margin = 1 in the table.
geniusPlot <- function(xvar, yvar, dat){




   tabl <- data.frame(prop.table(table(dat[[xvar]], dat[[yvar]]), margin = 1))
   ggplot(tabl, aes(x = tabl[ , 1], y = tabl[ , 2]),
      environment = environment()) +
   geom_tile(aes(fill = tabl[, "Freq"])) +
   scale_fill_gradient(low="white", high="blue") +
   ylab(label(dat[[xvar]])) +
   xlab(label(dat[[yvar]]))


}

library(Hmisc)
xx <- factor(Cs(a, b, a, a, a, b, c, b, a, c, c, c, a, a, b), ordered = T)
yy <- factor(Cs(d, d, d, e, e, d, e, d, e, e, e, d, e, d, e), ordered = T)


geniusPlot(xvar = "xx", yvar = "yy", dat = data.frame(xx, yy))



x11()
ggplot(data.frame(xx, yy), aes(xx, yy)) + geom_jitter()

ggplot(diamonds, aes(cut, color)) + geom_jitter()








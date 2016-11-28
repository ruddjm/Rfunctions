under.unix <- TRUE

latex.anova.rms <-
function(object,
           title=if(under.unix)
           paste('anova',attr(object,'obj.name'),sep='.')
           else
           paste("ano",substring(first.word(attr(object,"obj.name")),
                                 1,5),sep=""), 
           psmall=TRUE,
           dec.chisq=2, dec.F=2, dec.ss=NA,
           dec.ms=NA, dec.P=4, table.env=TRUE, caption=NULL, 
           newnames, ...)
{
  if(missing(newnames))
    rowl <- latexTranslate(dimnames(object)[[1]])
  else
    rowl <- newnames

  ## Translate interaction symbol (*) to times symbol
  rowl <- sedit(rowl, "*", "$\\times$", wild.literal=TRUE)
  
  ## Put TOTAL rows in boldface
  rowl <- ifelse(substring(rowl,1,5) %in% c("TOTAL","ERROR"),
                 paste("{\\bf",rowl,"}"),rowl)

  rowl <- ifelse(substring(rowl,1,1)==" ",
                 paste("~~{\\it ",substring(rowl,2),"}",sep=""),
                 rowl) # preserve leading blank

  P <- object[,3]
  
  dstats <- as.data.frame(object)
  attr(dstats, 'row.names') <- rowl
  
  if(psmall)
    {
      psml <- !is.na(dstats$P) & dstats$P < 0.00005
      if(any(psml))
        dstats$P <- ifelse(is.na(dstats$P),'',
                           ifelse(psml, "$<0.0001$",
                                  paste("~",format(round(dstats$P,dec.P)),sep="")))
    }

  digits <- c('Chi-Square'=dec.chisq, F=dec.F, 'd.f.'=0,
              'Partial SS'=dec.ss, MS=dec.ms, P=dec.P)

  sn <- dimnames(object)[[2]]
  dig <- digits[sn]
  sn[sn=='Chi-Square'] <- '\\chi^2'
  names(dstats) <- paste('$',sn,'$',sep='')

  resp <- latexTranslate(as.character(attr(object,"formula")[2]))
  ## Make LaTeX preserve spaces in heading
  if(!length(caption))
    caption <- paste(if(any(sn=='F'))"Analysis of Variance"
    else "Wald Statistics", "for {\\tt", resp, "}")

  latex(dstats, cdec=dig, title=title,
        caption = if(table.env) caption else NULL,
        rowlabel="", col.just=rep('r',length(sn)), table.env=table.env, ...)
}


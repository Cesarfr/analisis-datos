regModel <- function(y,x, models=c("lineal", "cuadratico", "cubico", "potencial", "exponencial", "logaritmico", "inverso", "inversoCuad", "inversoCub", "sigmoidal"),
                     subset=NULL, decimals=4){
  if (length(models)==0){
    stop("Debe seleccionar un modelo!")
  }
  names = c()
  r=NULL
  p=NULL
  if ("lineal" %in% models){
    names <- c(names,"Lineal")
    m <- lm(y~x,subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  if ("cuadratico" %in% models){
    names <- c(names,"Cuadrático")
    m <- lm(y~x+I(x^2),subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))	
  }
  if ("cubico" %in% models){
    names <- c(names,"Cúbico")
    m <- lm(y~x+I(x^2)+I(x^3),subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  if ("potencial" %in% models){
    names <- c(names,"Potencial")
    m <- lm(log(y)~log(x),subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  if ("exponencial" %in% models){
    names <- c(names,"Exponencial")
    m <- lm(log(y)~x,subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  if ("logaritmico" %in% models){
    names <- c(names,"Logarítmico")
    m <- lm(y~log(x),subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  if ("inverso" %in% models){
    names <- c(names,"Inverso")
    m <- lm(y~I(1/x),subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  if ("inversoCuad" %in% models){
    names <- c(names,"Inverso Cuadrado")
    m <- lm(y~I(1/x^2),subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  if ("inversoCub" %in% models){
    names <- c(names,"Inverso Cúbico")
    m <- lm(y~I(1/x^3),subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  if ("sigmoidal" %in% models){
    names <- c(names,"Sigmoidal")
    m <- lm(log(y)~I(1/x),subset=subset)
    r <- c(r,summary(m)$r.squared)
    p <- c(p,pf(q=c(summary(m)$fstatistic["value"]),df1=summary(m)$fstatistic["numdf"],df2=summary(m)$fstatistic["dendf"],lower.tail=FALSE))
  }
  t <- data.frame(names,round(r,decimals),format.pval(p))
  t <- t[order(-r),]
  colnames(t) <- c("Modelo","R²","P-valor")
  rownames(t) <- rep(NULL,nrow(t))
  return(t)
}

compModels <- function(y, x, md){
  regModel(y, x, models = c(md))
}

calcularRegresion <- function(y, x, mds){
  mdl <- NULL
  switch(mds,
         "lineal" = {mdl <- lm(y~x)},
         "cuadratico" = {mdl <- lm(y~x+I(x^2))},
         "cubico" = {mdl <- lm(y~x+I(x^2)+I(x^3))},
         "potencial" = {mdl <- lm(log(y)~log(x))},
         "exponencial" = {mdl <- lm(log(y)~x)},
         "logaritmico" = {mdl <- lm(y~log(x))},
         "inverso" = {mdl <- lm(y~I(1/x))},
         "inversoCuad" = {mdl <- lm(y~I(1/x^2))},
         "inversoCub" = {mdl <- lm(y~I(1/x^3))},
         "sigmoidal" = {mdl <- lm(log(y)~I(1/x))}
  )
  return(mdl)
}

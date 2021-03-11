
fastbw1 <- function (fit, rule = c("aic", "p"), type = c("residual", "individual", 
                                              "total"), sls = 0.05, aics = 0, eps = 1e-09, k.aic = 2, 
          force = NULL) 
{
  rule <- match.arg(rule)
  type <- match.arg(type)
  ns <- num.intercepts(fit)
  if (length(force)) 
    force <- force + ns
  L <- if (ns == 0) 
    NULL
  else 1:ns
  pt <- length(fit$coef)
  p <- pt - ns
  atr <- fit$Design
  assume <- atr$assume.code
  if (!length(assume)) 
    stop("fit does not have design information")
  assign <- fit$assign
  nama <- names(assign)[1]
  asso <- 1 * (nama == "(Intercept)" | nama == "Intercept")
  f <- sum(assume != 8)
  strt <- integer(f)
  len <- strt
  j <- 0
  for (i in 1:length(assume)) {
    if (assume[i] != 8) {
      j <- j + 1
      aj <- assign[[j + asso]]
      strt[j] <- min(aj)
      len[j] <- length(aj)
    }
  }
  name <- atr$name[assume != 8]
  ed <- as.integer(strt + len - 1)
  if (type == "total") 
    type <- "residual"
  if (length(force) && type != "individual") 
    warning("force probably does not work unless type=\"individual\"")
  factors.in <- 1:f
  parms.in <- 1:pt
  factors.del <- integer(f)
  chisq.del <- double(f)
  df.del <- integer(f)
  resid.del <- double(f)
  df.resid <- integer(f)
  beta <- fit$coef
  
  Cov <- vcov(fit, regcoef.only = TRUE, intercepts = "all")
  cov <- Cov
  Coef <- matrix(NA, nrow = f, ncol = pt, dimnames = list(NULL, 
                                                          names(beta)))
  d <- 0
  dor2 <- inherits(fit, "ols") && (length(fit$y) || (length(fit$fitted.values) && 
                                                       length(fit$residuals)))
  if (dor2) {
    Y <- if (length(fit$y)) 
      fit$y
    else fit$fitted.values + fit$residuals
    r2 <- double(f)
    sst <- sum((Y - mean(Y))^2)
    sigma2 <- fit$stats["Sigma"]^2
    xpy <- matrix(solvet(Cov, beta, tol = eps) * sigma2, 
                  ncol = 1)
    ypy <- sum(Y^2)
  }
  for (i in 1:f) {
    fi <- length(factors.in)
    ln <- len[factors.in]
    st <- as.integer(ns + c(1, 1 + cumsum(ln[-fi]))[1:fi])
    en <- as.integer(st + ln - 1)
    if (any(en > nrow(cov))) 
      stop("program logic error")
    crit.min <- 1e+10
    chisq.crit.min <- 1e+10
    jmin <- 0
    dfmin <- 0
    k <- 0
    factors.in.loop <- factors.in
    for (j in factors.in.loop) {
      k <- k + 1
      q <- st[k]:en[k]
      chisq <- if (any(q %in% force)) 
        Inf
      else beta[q] %*% solvet(cov[q, q], beta[q], tol = eps)
      df <- length(q)
      crit <- switch(rule, aic = chisq - k.aic * df, p = pchisq(chisq, 
                                                                df))
      if (crit < crit.min) {
        jmin <- j
        crit.min <- crit
        chisq.crit.min <- chisq
        df.min <- df
      }
    }
    factors.in <- factors.in[factors.in != jmin]
    parms.in <- parms.in[parms.in < strt[jmin] | parms.in > 
                           ed[jmin]]
    if (length(parms.in) == 0) 
      q <- 1:pt
    else q <- (1:pt)[-parms.in]
    resid <- fit$coef[q] %*% solvet(Cov[q, q], fit$coef[q], 
                                    tol = eps)
    resid.df <- length(q)
    del <- switch(type, residual = switch(rule, aic = resid - 
                                            k.aic * resid.df <= aics, p = 1 - pchisq(resid, 
                                                                                     resid.df) > sls), 
                        individual = switch(rule, aic = crit.min <= aics, p = 1 - crit.min > sls))
    if (del) {
      d <- d + 1
      factors.del[d] <- jmin
      chisq.del[d] <- chisq.crit.min
      df.del[d] <- df.min
      resid.del[d] <- resid
      df.resid[d] <- resid.df
      if (length(parms.in)) {
        cov.rm.inv <- solvet(Cov[-parms.in, -parms.in], 
                             tol = eps)
        cov.cross <- Cov[parms.in, -parms.in, drop = FALSE]
        w <- cov.cross %*% cov.rm.inv
        beta <- fit$coef[parms.in] - w %*% fit$coef[-parms.in]
        cov <- Cov[parms.in, parms.in] - w %*% t(cov.cross)
        #cof <- rep(0, pt)
        #cof[parms.in] <- beta
       # Coef[d, ] <- cof
        #if (dor2) {
         # sse <- ypy - t(xpy[parms.in, , drop = FALSE]) %*% 
          #  beta
        #  r2[d] <- 1 - sse/sst
        #}
      }
      else {
        beta <- NULL
        cov <- NULL
        #if (dor2) 
         # r2[d] <- 0
      }
    }
    else break
  }
  if (d > 0) {
    u <- 1:d
    fd <- factors.del[u]
   # if (dor2) {
    #  r2 <- r2[u]
     # Coef <- Coef[u, , drop = FALSE]
    #}

    if (length(fd) == f) 
      fk <- NULL
    else fk <- (1:f)[-fd]
  }
  else {
    fd <- NULL
    res <- NULL
    fk <- 1:f
  }
  nf <- name[fk]
  pd <- NULL
  if (d > 0) 
    for (i in 1:d) pd <- c(pd, (strt[fd[i]]:ed[fd[i]]))
  if (length(fd) == f) 
    fk <- NULL
  else if (d == 0) 
    fk <- 1:f
  else fk <- (1:f)[-fd]
  if (length(pd) == p) 
    pk <- L
  else if (d == 0) 
    pk <- 1:pt
  else pk <- (1:pt)[-pd]
  

  if (length(pd) != p) {
    beta <- as.vector(beta)
    names(beta) <- names(fit$coef)[pk]
    dimnames(cov) <- list(names(beta), names(beta))
  }

  r <- list(names.kept = nf,
            coefficients = beta, var = cov)
  class(r) <- "fastbw1"
  r
}





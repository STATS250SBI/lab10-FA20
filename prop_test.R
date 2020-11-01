prop_test <-
  function(
    x,
    n,
    p = NULL,
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95,
    correct = FALSE
  ) {
    alternative <- match.arg(alternative)
    k <- length(x)
    if (k > 2) 
      stop("This function can only handle 1 or 2 groups. Check your input for x.")
    pt <- suppressWarnings(prop.test(x, n, p, alternative, conf.level, correct))
    sd <- sqrt(pt$estimate * (1 - pt$estimate) / n)
    
    pt$statistic <-
      c("Z" = sqrt(unname(pt$statistic)) *
          ifelse(k == 1, sign(pt$estimate - pt$null.value),
                 sign((pt$estimate[1] - pt$estimate[2]) -
                        ifelse(is.null(pt$null.value), 0, pt$null.value)
                 )))
    pt$parameter <- NULL
    
    YATES <- if (correct && (k <= 2)) 
      0.5
    else 0
    
    if (k == 1) {
      z <- qnorm(if (alternative == "two.sided") 
        (1 + conf.level)/2
        else conf.level)
      YATES <- min(YATES, abs(x - n * p))
      p.c <- pt$estimate + YATES/n
      p.u <- if (p.c >= 1) 
        1
      else (p.c  + z * sqrt(p.c * (1 - p.c)/n))
      p.c <- pt$estimate - YATES/n
      p.l <- if (p.c <= 0) 
        0
      else (p.c - z * sqrt(p.c * (1 - p.c)/n))
      CINT <- switch(alternative, 
                     two.sided = c(max(p.l, 0), min(p.u, 1)),
                     greater = c(max(p.l, 0), 1),
                     less = c(0, min(p.u, 1)))
      attr(CINT, "conf.level") <- conf.level
      pt$conf.int <- CINT
    }
    
    return(pt)
  }
plotNorm <- function(mean = 0, sd = 1, shadeValues = NULL,
                     direction = c("less", "greater", "outside", "inside"),
                     col.shade = "cornflowerblue",
                     ...) {
  
  checks <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(shadeValues, null.ok = TRUE, max.len = 2, add = checks)
  checkmate::assert_number(mean, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::assert_number(sd, na.ok = FALSE, finite = TRUE, add = checks)
  checkmate::reportAssertions(checks)
  
  direction <- match.arg(direction)
  
  dots <- list(...)
  if (any(names(dots) == "xlim")) {
    xlim <- dots$xlim
    dots$xlim <- NULL
  } else xlim <- c(mean - 3*sd, mean + 3 * sd)
  
  if (any(names(dots) == "xlab")) {
    xlab <- dots$xlab
    dots$xlab <- NULL
  } else xlab <- ""
  
  if (any(names(dots) == "main")) {
    main <- dots$main
    dots$main <- NULL
  } else main <- paste0("Normal(", mean, ", ", sd, ") Distribution")
  
  xRange <- seq(mean - 3 * sd, mean + 3 * sd, length = 300)
  height <- dnorm(xRange, mean = mean, sd = sd)
  
  do.call(plot, c(list(height ~ xRange, type = "l", axes = F, ylab = "",
                     xlab = xlab, main = main, xlim = xlim, frame.plot = F), 
                     dots))
  axis(1, at = seq(mean - 3 * sd, mean + 3 * sd, sd),
       labels = prettyNum(seq(mean - 3 * sd, mean + 3 * sd, sd)),
       pos = 0)
  
  if (length(shadeValues) == 2) {
    shadeValues <- sort(shadeValues)
    if (!any(c("outside", "inside") == direction))
      stop(paste("When you provide two shadeValues for shading the plot,",
                 "you must also specify direction as 'inside' or 'outside'.",
                 "Fix this and try again."))
    else if (direction == "inside") {
      xShade <- seq(shadeValues[1], shadeValues[2], length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]), 
              c(0, shadeHeight, 0), col = col.shade)
    }
    else if (direction == "outside") {
      xShade <- c(seq(min(xRange), shadeValues[1], length = 100),
                  seq(shadeValues[2], max(xRange), length = 100))
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade[1:100], shadeValues[1]), 
              c(0, shadeHeight[1:100], 0), col = col.shade)
      polygon(c(shadeValues[2], xShade[101:200], xShade[200]), 
              c(0, shadeHeight[101:200], 0), col = col.shade)
    }
    
    text(x = shadeValues[1], y = 0, xpd = TRUE, 
         labels = prettyNum(formatC(shadeValues[1], digits = 3)), 
         pos = 1, font = 2, offset = 0.6, col = col.shade)
    text(x = shadeValues[2], y = 0, xpd = TRUE, 
         labels = prettyNum(formatC(shadeValues[2], digits = 3)), 
         pos = 1, font = 2, offset = 0.6, col = col.shade)
    
  } else if (length(shadeValues == 1)) {
    if (!any(c("less", "greater") == direction))
      stop(paste("When you provide one shadeValue for shading the plot,",
                 "you must also specify direction as 'less' or 'greater'.",
                 "Fix this and try again."))
    if (direction == "less") {
      xShade <- seq(mean - 3 * sd, shadeValues, length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    } else if (direction == "greater") {
      xShade <- seq(shadeValues, mean + 3 * sd, length = 100)
      shadeHeight <- dnorm(xShade, mean = mean, sd = sd)
      polygon(c(xShade[1], xShade, xShade[100]), c(0, shadeHeight, 0),
              col = col.shade)
    }
    text(x = shadeValues, y = 0, xpd = TRUE,
         labels = prettyNum(formatC(shadeValues, digits = 3)), pos = 1,
         font = 2, offset = 0.6, col = col.shade)
  }
  
}
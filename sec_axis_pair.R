# automate the generation of transformation pair for sec_axis

sec_axis_pair <- function(
    lim1=NULL, lim2=NULL, slope=NULL, intercept=NULL, swap_axes=FALSE
){
  
  if (is.null(slope) && is.null(intercept)){
    # generate slope and intercept from the pair of limits    
    m2 <- (lim2[2] - lim2[1]) / (lim1[2] - lim1[1])
    c2 = (lim2[1] * lim1[2] - lim1[1] * lim2[2]) / (lim1[2] - lim1[1])
  } else {
    if (is.null(slope)) { 
      # interpret slope when only intercept is given
      m2 <- 1.0 
      c2 <- intercept
    } else if (is.null(intercept)) { 
      # interpret intercept when only slope is given
      m2 <- slope
      c2 <- 0.0
    } else {
      # otherwise use both input slope and intercept
      m2 <- slope
      c2 <- intercept
    }
  }
  
  if (swap_axes) {
    # reinterpret slope and intercept as x = m1 * y + c1
    m1 <- m2
    c1 <- c2
    # compute the inverse y = m2 * x + c2
    m2 <- 1/m1
    c2 <- -c1/m1
  } else { 
    # assume slope and intercept as y = m2 * x + c2
    # compute the inverse x = m1 * y + c1
    m1 <- 1/m2
    c1 <- -c2/m2
  }
  
  # create transformation formula and inverse transform function
  func <- \(y) { m1 * y + c1 }
  form <- ~ . * m2 + c2
  
  # pack the result in a list, and returns
  list(func = func, formula = form)

}
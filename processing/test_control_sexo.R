RICLPM.ext2 <- '
  # Create between components (random intercepts)
    RI_x =~ 1*dom_index1 + 1*dom_index2 + 1*dom_index3 + 1*dom_index4
    RI_y =~ 1*conf_sin1 + 1*conf_sin2 + 1*conf_sin3 + 1*conf_sin4
  
    # Create the components within
    DOM_INDEX1 =~ 1*dom_index1
    DOM_INDEX2 =~ 1*dom_index2
    DOM_INDEX3 =~ 1*dom_index3
    DOM_INDEX4 =~ 1*dom_index4

    
    CS1 =~ 1*conf_sin1
    CS2 =~ 1*conf_sin2
    CS3 =~ 1*conf_sin3
    CS4 =~ 1*conf_sin4
  
  # Estimate lagged effects between within-person centered variables
  DOM_INDEX2 + CS2 ~ DOM_INDEX1 + CS1
  DOM_INDEX3 + CS3 ~ DOM_INDEX2 + CS2
  DOM_INDEX4 + CS4 ~ DOM_INDEX3 + CS3

  # Estimate covariance beteen ithin-person centered variables at first ave
  DOM_INDEX1 ~~ CS1 # Covariance
  
  # Estimate covariances beteen residuals of ithin-person centered variables
  # (i.e., innovations)
  DOM_INDEX2 ~~ CS2
  DOM_INDEX3 ~~ CS3
  DOM_INDEX4 ~~ CS4
  
  # Estimate variance and covariance of random intercepts
  RI_x ~~ RI_x
  RI_y ~~ RI_y
  RI_x ~~ RI_y
  
  # Estimate (residual) variance of within-person centered variables
    DOM_INDEX1 ~~ DOM_INDEX1 # Variances
    CS1 ~~ CS1 
    DOM_INDEX2 ~~ DOM_INDEX2 # Residual variances
    CS2 ~~ CS2 
    DOM_INDEX3 ~~ DOM_INDEX3 
    CS3 ~~ CS3 
    DOM_INDEX4 ~~ DOM_INDEX4 
    CS4 ~~ CS4 
'
RICLPM.ext2.fit <- lavaan(RICLPM.ext2, 
                          data = elsoc, 
                          missing = 'ML', 
                          group = "sexo1", 
                          meanstructure = T, 
                          int.ov.free = T
)
summary(RICLPM.ext2.fit)

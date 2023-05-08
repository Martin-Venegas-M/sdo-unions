text_basic <- function(varx, vary){

  ## Estimate variance between
  bwcomp <- glue::glue(
    '
    # Create the between components
    RI_x =~ 1*{vary}1 + 1*{vary}2 + 1*{vary}3 + 1*{vary}4
    RI_y =~ 1*{varx}1 + 1*{varx}2 + 1*{varx}3 + 1*{varx}4

    # Create the components within
    dep1 =~ 1*{vary}1
    dep2 =~ 1*{vary}2
    dep3 =~ 1*{vary}3
    dep4 =~ 1*{vary}4


    indep1 =~ 1*{varx}1
    indep2 =~ 1*{varx}2
    indep3 =~ 1*{varx}3
    indep4 =~ 1*{varx}4


    # Construct the measurement error variances to zero
    {vary}1 ~~ 0*{vary}1
    {vary}2 ~~ 0*{vary}2
    {vary}3 ~~ 0*{vary}3
    {vary}4 ~~ 0*{vary}4
    {varx}1 ~~ 0*{varx}1
    {varx}2 ~~ 0*{varx}2
    {varx}3 ~~ 0*{varx}3
    {varx}4 ~~ 0*{varx}4
          ')
    
    ## Estimate variance within
    varcov <- '
    # Estimate the covariance between the components within t=1
    dep1 ~~ indep1

    # Estimate the covariances between the residuals of the within component.
    dep2 ~~ indep2
    dep3 ~~ indep3
    dep4 ~~ indep4

    # Estimate the residual variances of the within component
    dep1 ~~ dep1 # Variances
    indep1 ~~ indep1
    dep2 ~~ dep2 # Residual variances
    indep2 ~~ indep2
    dep3 ~~ dep3
    indep3 ~~ indep3
    dep4 ~~ dep4
    indep4 ~~ indep4

    # Estimate the variance and covariance between RIs.
    RI_x ~~ RI_x
    RI_y ~~ RI_y
    RI_x ~~ RI_y

    # Set the correlation between the RI and components within t=1 to zero
    RI_x ~~ 0*dep1
    RI_x ~~ 0*indep1
    RI_y ~~ 0*dep1
    RI_y ~~ 0*indep1
          '
  
  ## Estimating regressions
  
  ### Autoregressive: Free
  a1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1
    indep3 ~ indep2
    indep4 ~ indep3
'
  
  ### Autoregressive: constrained
  a2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3
'
  
  ### Forward: free
  b1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ dep1 + indep1
    indep3 ~ dep2 + indep2
    indep4 ~ dep3 + indep3
'
  
  ### Forward: constrained
  b2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1
    indep3 ~ c*dep2 + d*indep2
    indep4 ~ c*dep3 + d*indep3
'
  
  ### Backward: free
  c1 <- '
    dep2 ~ dep1 + indep1
    dep3 ~ dep2 + indep2
    dep4 ~ dep3 + indep3
    indep2 ~ indep1
    indep3 ~ indep2
    indep4 ~ indep3
'
  
  ### Backward: constrained
  c2 <- '
    dep2 ~ a*dep1 + b*indep1
    dep3 ~ a*dep2 + b*indep2
    dep4 ~ a*dep3 + b*indep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3
'
  
  ### Bidrectional: free
  d1 <- '
    dep2 ~ dep1 + indep1
    dep3 ~ dep2 + indep2
    dep4 ~ dep3 + indep3
    indep2 ~ dep1 + indep1
    indep3 ~ dep2 + indep2
    indep4 ~ dep3 + indep3
'
  
  ### Bidrectional: Constrained
  d2 <- '
    dep2 ~ a*dep1 + b*indep1
    dep3 ~ a*dep2 + b*indep2
    dep4 ~ a*dep3 + b*indep3
    indep2 ~ c*dep1 + d*indep1
    indep3 ~ c*dep2 + d*indep2
    indep4 ~ c*dep3 + d*indep3
'
  
  return(list(bwcomp = bwcomp,
              varcov = varcov,
              a1 = a1,
              a2 = a2,
              b1 = b1,
              b2 = b2,
              c1 = c1,
              c2 = c2,
              d1 = d1,
              d2 = d2))
}

text_con_uno <- function(varx, vary, con_uno){
  
  ## Estimate variance between
  bwcomp <- glue::glue(
    '
    # Create the between components
    RI_x =~ 1*{vary}1 + 1*{vary}2 + 1*{vary}3 + 1*{vary}4
    RI_y =~ 1*{varx}1 + 1*{varx}2 + 1*{varx}3 + 1*{varx}4
    RI_z =~ 1*{con_uno}1 + 1*{con_uno}2 + 1*{con_uno}3 + 1*{con_uno}4

    # Create the components within
    dep1 =~ 1*{vary}1
    dep2 =~ 1*{vary}2
    dep3 =~ 1*{vary}3
    dep4 =~ 1*{vary}4


    indep1 =~ 1*{varx}1
    indep2 =~ 1*{varx}2
    indep3 =~ 1*{varx}3
    indep4 =~ 1*{varx}4
    
    con1 =~ 1*{con_uno}1
    con2 =~ 1*{con_uno}2
    con3 =~ 1*{con_uno}3
    con4 =~ 1*{con_uno}4


    # Construct the measurement error variances to zero
    {vary}1 ~~ 0*{vary}1
    {vary}2 ~~ 0*{vary}2
    {vary}3 ~~ 0*{vary}3
    {vary}4 ~~ 0*{vary}4
    
    {varx}1 ~~ 0*{varx}1
    {varx}2 ~~ 0*{varx}2
    {varx}3 ~~ 0*{varx}3
    {varx}4 ~~ 0*{varx}4
    
    # {con_uno}1 ~~ 0*{con_uno}1
    # {con_uno}2 ~~ 0*{con_uno}2
    # {con_uno}3 ~~ 0*{con_uno}3
    # {con_uno}4 ~~ 0*{con_uno}4
    
          ')
    
    ## Estimate variance within
    varcov <- '
    # Estimate the covariance between the components within t=1
    dep1 ~~ indep1
    dep1 ~~ con1

    # Estimate the covariances between the residuals of the within component.
    dep2 ~~ indep2
    dep2 ~~ con2
    
    dep3 ~~ indep3
    dep3 ~~ con3
    
    dep4 ~~ indep4
    dep4 ~~ con4

    # Estimate the residual variances of the within component
    dep1 ~~ dep1 # Variances
    dep2 ~~ dep2 # Residual variances
    dep3 ~~ dep3
    dep4 ~~ dep4

    indep1 ~~ indep1
    indep2 ~~ indep2
    indep3 ~~ indep3
    indep4 ~~ indep4
    
    con1 ~~ con1 # Variances
    con2 ~~ con2
    con3 ~~ con3
    con4 ~~ con4

    # Estimate the variance and covariance between RIs.
    RI_x ~~ RI_x
    RI_y ~~ RI_y
    RI_x ~~ RI_y
    
    RI_z ~~ RI_z
    RI_z ~~ RI_y
    RI_z ~~ RI_x

    # Set the correlation between the RI and components within t=1 to zero
    RI_x ~~ 0*dep1
    RI_x ~~ 0*indep1
    RI_x ~~ 0*con1
    
    RI_y ~~ 0*dep1
    RI_y ~~ 0*indep1
    RI_y ~~ 0*con1
    
    RI_z ~~ 0*dep1
    RI_z ~~ 0*indep1
    RI_y ~~ 0*con1
          '
  
  ## Estimating regressions
  
  ### Autoregressive: Free
  a1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1
    indep3 ~ indep2
    indep4 ~ indep3
  
    con2 ~ con1
    con3 ~ con2
    con4 ~ con3
'
  
  ### Autoregressive: constrained
  a2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3
  
    con2 ~ con1
    con3 ~ con2
    con4 ~ con3
'
  
  ### Forward: free
  b1 <- '
    dep2 ~ dep1 + indep1 + con1
    dep3 ~ dep2 + indep2 + con2
    dep4 ~ dep3 + indep3 + con3
    indep2 ~ dep1
    indep3 ~ dep2
    indep4 ~ dep3
  
    con2 ~ con1
    con3 ~ con2
    con4 ~ con3
'
  
  ### Forward: constrained
  b2 <- '
    dep2 ~ a*dep1 + b*indep1 + con1dep*con1
    dep3 ~ a*dep2 + b*indep2 + con1dep*con2
    dep4 ~ a*dep3 + b*indep3 + con1dep*con3
    indep2 ~ c*dep1 
    indep3 ~ c*dep2 
    indep4 ~ c*dep3 
  
    con2 ~ con1
    con3 ~ con2
    con4 ~ con3
'
  
  ### Backward: free
  c1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1 + c*dep1 + d*indep1 + con1indep*con1
    indep3 ~ indep2 + c*dep2 + d*indep2 + con1indep*con2
    indep4 ~ indep3 + c*dep3 + d*indep3 + con1indep*con3
  
    con2 ~ con1
    con3 ~ con2
    con4 ~ con3
'
  
  ### Backward: constrained
  c2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1 + con1indep*con1
    indep3 ~ c*dep2 + d*indep2 + con1indep*con2
    indep4 ~ c*dep3 + d*indep3 + con1indep*con3
  
    con2 ~ con1
    con3 ~ con2
    con4 ~ con3
'
  
  ### Bidrectional: free
  d1 <- '
    dep2 ~ dep1 + indep1 + con1
    dep3 ~ dep2 + indep2 + con2
    dep4 ~ dep3 + indep3 + con3
    indep2 ~ dep1 + indep1 + con1
    indep3 ~ dep2 + indep2 + con2
    indep4 ~ dep3 + indep3 + con3
  
    con2 ~ con1
    con3 ~ con2
    con4 ~ con3
'
  
  ### Bidrectional: Constrained
  d2 <- '
    dep2 ~ a*dep1 + b*indep1 + con1dep*con1
    dep3 ~ a*dep2 + b*indep2 + con1dep*con2
    dep4 ~ a*dep3 + b*indep3 + con1dep*con3
    indep2 ~ c*dep1 + d*indep1 + con1indep*con1
    indep3 ~ c*dep2 + d*indep2 + con1indep*con2
    indep4 ~ c*dep3 + d*indep3 + con1indep*con3
  
    con2 ~ con1
    con3 ~ con2
    con4 ~ con3
'
  
  return(list(bwcomp = bwcomp,
              varcov = varcov,
              a1 = a1,
              a2 = a2,
              b1 = b1,
              b2 = b2,
              c1 = c1,
              c2 = c2,
              d1 = d1,
              d2 = d2))
}


create.text.object <- function(varx, vary, con_uno = NULL, con_dos = NULL, con_tres = NULL, controls = 0, demo = FALSE){
  
  
  if(controls == 0 & demo == FALSE){
    return(text_basic(varx, vary))
  } else if(controls == 1 & demo == FALSE){
    return(text_con_uno(varx, vary, con_uno))
  }
  
}

create.text.object("conf", "sdo", "ideologia", controls = 1, demo = FALSE) # Probar


# 7.3 Compare GOF ---------------

### Create function

gof.comp  = function(data, pairs,
                     measures = c("CFI","TLI","RMSEA","SRMR",
                                  "AIC","BIC","aBIC","par","LL")){
  comp <- list()
  for (i in 1:length(pairs)){
    gof <- data
    nest <- pairs[[i]][1]
    full <- pairs[[i]][2]
    delta <- NULL
    for (k in measures){
      delta[paste0(k,"_D")] <- gof[m==nest, get(k)] - gof[m==full, get(k)]
    }
    par_LLcorf_nest <- gof[m==nest,par]*gof[m==nest,LLcorrectf]
    par_LLcorf_full <- gof[m==full,par]*gof[m==full,LLcorrectf]
    delta["CD"] <- (par_LLcorf_nest-par_LLcorf_full)/delta["par_D"]
    delta["TRd"] <- (-2*delta["LL_D"])/delta["CD"]
    delta["TRd_df"] <- gof[m==full, "par"] - gof[m==nest, "par"]
    delta["TRd_pvalue"] <- pchisq(as.numeric(delta["TRd"]),
                                  as.numeric(delta["TRd_df"]), lower.tail = F)
    comp[[paste0(nest," vs. ",full,sep="")]] <- delta
  }
  comp <- data.table(comp=names(comp),dplyr::bind_rows(comp))
  return(comp)
}
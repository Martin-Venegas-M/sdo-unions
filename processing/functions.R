create.text.object <-
  function(varx,
           vary,
           con_uno = NULL,
           con_dos = NULL,
           con_tres = NULL,
           controls = 0,
           demo = FALSE) {
    text_basic <- function(varx, vary) {
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
          '
      )
      
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
      
      ### forward: free
      b1 <- '
    dep2 ~ dep1 + indep1
    dep3 ~ dep2 + indep2
    dep4 ~ dep3 + indep3
    indep2 ~ indep1
    indep3 ~ indep2
    indep4 ~ indep3
'
      
      ### forward: constrained
      b2 <- '
    dep2 ~ a*dep1 + b*indep1
    dep3 ~ a*dep2 + b*indep2
    dep4 ~ a*dep3 + b*indep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3
'
      
      ### Backward: free
      c1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ dep1 + indep1
    indep3 ~ dep2 + indep2
    indep4 ~ dep3 + indep3
'
      
      ### Backward: constrained
      c2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1
    indep3 ~ c*dep2 + d*indep2
    indep4 ~ c*dep3 + d*indep3
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
      
      return(
        list(
          bwcomp = bwcomp,
          varcov = varcov,
          a1 = a1,
          a2 = a2,
          b1 = b1,
          b2 = b2,
          c1 = c1,
          c2 = c2,
          d1 = d1,
          d2 = d2
        )
      )
    }
    
    text_con_uno <- function(varx, vary, con_uno) {
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

    conz1 =~ 1*{con_uno}1
    conz2 =~ 1*{con_uno}2
    conz3 =~ 1*{con_uno}3
    conz4 =~ 1*{con_uno}4


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

          '
      )
      
      ## Estimate variance within
      varcov <- '
    # Estimate the covariance between the components within t=1
    dep1 ~~ indep1
    dep1 ~~ conz1

    # Estimate the covariances between the residuals of the within component.
    dep2 ~~ indep2
    dep2 ~~ conz2

    dep3 ~~ indep3
    dep3 ~~ conz3

    dep4 ~~ indep4
    dep4 ~~ conz4

    # Estimate the residual variances of the within component
    dep1 ~~ dep1 # Variances
    dep2 ~~ dep2 # Residual variances
    dep3 ~~ dep3
    dep4 ~~ dep4

    indep1 ~~ indep1
    indep2 ~~ indep2
    indep3 ~~ indep3
    indep4 ~~ indep4

    conz1 ~~ conz1 # Variances
    conz2 ~~ conz2
    conz3 ~~ conz3
    conz4 ~~ conz4

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
    RI_x ~~ 0*conz1

    RI_y ~~ 0*dep1
    RI_y ~~ 0*indep1
    RI_y ~~ 0*conz1

    RI_z ~~ 0*dep1
    RI_z ~~ 0*indep1
    RI_z ~~ 0*conz1
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

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Autoregressive: constrained
      a2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Forward: free
      b1 <- '
    dep2 ~ dep1 + indep1 + conz1
    dep3 ~ dep2 + indep2 + conz2
    dep4 ~ dep3 + indep3 + conz3
    indep2 ~ dep1
    indep3 ~ dep2
    indep4 ~ dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Forward: constrained
      b2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3
    indep2 ~ c*dep1
    indep3 ~ c*dep2
    indep4 ~ c*dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Backward: free
      c1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1 + c*dep1 + d*indep1 + conz1
    indep3 ~ indep2 + c*dep2 + d*indep2 + conz2
    indep4 ~ indep3 + c*dep3 + d*indep3 + conz3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Backward: constrained
      c2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Bidrectional: free
      d1 <- '
    dep2 ~ dep1 + indep1 + conz1
    dep3 ~ dep2 + indep2 + conz2
    dep4 ~ dep3 + indep3 + conz3
    indep2 ~ dep1 + indep1 + conz1
    indep3 ~ dep2 + indep2 + conz2
    indep4 ~ dep3 + indep3 + conz3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Bidrectional: Constrained
      d2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      return(
        list(
          bwcomp = bwcomp,
          varcov = varcov,
          a1 = a1,
          a2 = a2,
          b1 = b1,
          b2 = b2,
          c1 = c1,
          c2 = c2,
          d1 = d1,
          d2 = d2
        )
      )
    }
    
    text_con_dos <- function(varx, vary, con_uno, con_dos) {
      ## Estimate variance between
      bwcomp <- glue::glue(
        '
    # Create the between components
    RI_x =~ 1*{vary}1 + 1*{vary}2 + 1*{vary}3 + 1*{vary}4
    RI_y =~ 1*{varx}1 + 1*{varx}2 + 1*{varx}3 + 1*{varx}4
    RI_z =~ 1*{con_uno}1 + 1*{con_uno}2 + 1*{con_uno}3 + 1*{con_uno}4
    RI_w =~ 1*{con_dos}1 + 1*{con_dos}2 + 1*{con_dos}3 + 1*{con_dos}4

    # Create the components within
    dep1 =~ 1*{vary}1
    dep2 =~ 1*{vary}2
    dep3 =~ 1*{vary}3
    dep4 =~ 1*{vary}4


    indep1 =~ 1*{varx}1
    indep2 =~ 1*{varx}2
    indep3 =~ 1*{varx}3
    indep4 =~ 1*{varx}4

    conz1 =~ 1*{con_uno}1
    conz2 =~ 1*{con_uno}2
    conz3 =~ 1*{con_uno}3
    conz4 =~ 1*{con_uno}4

    conw1 =~ 1*{con_dos}1
    conw2 =~ 1*{con_dos}2
    conw3 =~ 1*{con_dos}3
    conw4 =~ 1*{con_dos}4


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

    # {con_dos}1 ~~ 0*{con_dos}1
    # {con_dos}2 ~~ 0*{con_dos}2
    # {con_dos}3 ~~ 0*{con_dos}3
    # {con_dos}4 ~~ 0*{con_dos}4

          '
      )
      
      ## Estimate variance within
      varcov <- '
    # Estimate the covariance between the components within t=1
    dep1 ~~ indep1
    dep1 ~~ conz1
    dep1 ~~ conw1

    # Estimate the covariances between the residuals of the within component.
    dep2 ~~ indep2
    dep2 ~~ conz2
    dep2 ~~ conw2

    dep3 ~~ indep3
    dep3 ~~ conz3
    dep3 ~~ conw3

    dep4 ~~ indep4
    dep4 ~~ conz4
    dep4 ~~ conw4

    # Estimate the residual variances of the within component
    dep1 ~~ dep1 # Variances
    dep2 ~~ dep2 # Residual variances
    dep3 ~~ dep3
    dep4 ~~ dep4

    indep1 ~~ indep1
    indep2 ~~ indep2
    indep3 ~~ indep3
    indep4 ~~ indep4

    conz1 ~~ conz1 # Variances
    conz2 ~~ conz2
    conz3 ~~ conz3
    conz4 ~~ conz4

    conw1 ~~ conw1 # Variances
    conw2 ~~ conw2
    conw3 ~~ conw3
    conw4 ~~ conw4

    # Estimate the variance and covariance between RIs.
    RI_x ~~ RI_x
    RI_y ~~ RI_y
    RI_x ~~ RI_y

    RI_z ~~ RI_z
    RI_z ~~ RI_y
    RI_z ~~ RI_x

    RI_w ~~ RI_w
    RI_w ~~ RI_y
    RI_w ~~ RI_x

    # Set the correlation between the RI and components within t=1 to zero
    RI_x ~~ 0*dep1
    RI_x ~~ 0*indep1
    RI_x ~~ 0*conz1
    RI_x ~~ 0*conw1

    RI_y ~~ 0*dep1
    RI_y ~~ 0*indep1
    RI_y ~~ 0*conz1
    RI_y ~~ 0*conw1

    RI_z ~~ 0*dep1
    RI_z ~~ 0*indep1
    RI_z ~~ 0*conz1
    RI_z ~~ 0*conw1

    RI_w ~~ 0*dep1
    RI_w ~~ 0*indep1
    RI_w ~~ 0*conz1
    RI_w ~~ 0*conw1
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

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Autoregressive: constrained
      a2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3


'
      
      ### Forward: free
      b1 <- '
    dep2 ~ dep1 + indep1 + conz1  + conw1
    dep3 ~ dep2 + indep2 + conz2  + conw2
    dep4 ~ dep3 + indep3 + conz3  + conw3
    indep2 ~ dep1
    indep3 ~ dep2
    indep4 ~ dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Forward: constrained
      b2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + conwdep*conw1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + conwdep*conw2
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + conwdep*conw3
    indep2 ~ c*dep1
    indep3 ~ c*dep2
    indep4 ~ c*dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Backward: free
      c1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1 + c*dep1 + d*indep1 + conz1 + conw1
    indep3 ~ indep2 + c*dep2 + d*indep2 + conz2 + conw2
    indep4 ~ indep3 + c*dep3 + d*indep3 + conz3 + conw3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Backward: constrained
      c2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1  + conwindep*conw1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2  + conwindep*conw2
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3  + conwindep*conw3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Bidrectional: free
      d1 <- '
    dep2 ~ dep1 + indep1 + conz1 + conw1
    dep3 ~ dep2 + indep2 + conz2 + conw2
    dep4 ~ dep3 + indep3 + conz3 + conw3
    indep2 ~ dep1 + indep1 + conz1 + conw1
    indep3 ~ dep2 + indep2 + conz2 + conw2
    indep4 ~ dep3 + indep3 + conz3 + conw3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Bidrectional: Constrained
      d2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + conwdep*conw1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + conwdep*conw2
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + conwdep*conw3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1 + conwindep*conw1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2 + conwindep*conw2
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3 + conwindep*conw3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      return(
        list(
          bwcomp = bwcomp,
          varcov = varcov,
          a1 = a1,
          a2 = a2,
          b1 = b1,
          b2 = b2,
          c1 = c1,
          c2 = c2,
          d1 = d1,
          d2 = d2
        )
      )
    }
    
    text_con_tres <- function(varx, vary, con_uno, con_dos, con_tres) {
      ## Estimate variance between
      bwcomp <- glue::glue(
        '
    # Create the between components
    RI_x =~ 1*{vary}1 + 1*{vary}2 + 1*{vary}3 + 1*{vary}4
    RI_y =~ 1*{varx}1 + 1*{varx}2 + 1*{varx}3 + 1*{varx}4
    RI_z =~ 1*{con_uno}1 + 1*{con_uno}2 + 1*{con_uno}3 + 1*{con_uno}4
    RI_w =~ 1*{con_dos}1 + 1*{con_dos}2 + 1*{con_dos}3 + 1*{con_dos}4
    RI_v =~ 1*{con_tres}1 + 1*{con_tres}2 + 1*{con_tres}3 + 1*{con_tres}4

    # Create the components within
    dep1 =~ 1*{vary}1
    dep2 =~ 1*{vary}2
    dep3 =~ 1*{vary}3
    dep4 =~ 1*{vary}4


    indep1 =~ 1*{varx}1
    indep2 =~ 1*{varx}2
    indep3 =~ 1*{varx}3
    indep4 =~ 1*{varx}4

    conz1 =~ 1*{con_uno}1
    conz2 =~ 1*{con_uno}2
    conz3 =~ 1*{con_uno}3
    conz4 =~ 1*{con_uno}4

    conw1 =~ 1*{con_dos}1
    conw2 =~ 1*{con_dos}2
    conw3 =~ 1*{con_dos}3
    conw4 =~ 1*{con_dos}4


    conv1 =~ 1*{con_tres}1
    conv2 =~ 1*{con_tres}2
    conv3 =~ 1*{con_tres}3
    conv4 =~ 1*{con_tres}4


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

    # {con_dos}1 ~~ 0*{con_dos}1
    # {con_dos}2 ~~ 0*{con_dos}2
    # {con_dos}3 ~~ 0*{con_dos}3
    # {con_dos}4 ~~ 0*{con_dos}4

    # {con_tres}1 ~~ 0*{con_tres}1
    # {con_tres}2 ~~ 0*{con_tres}2
    # {con_tres}3 ~~ 0*{con_tres}3
    # {con_tres}4 ~~ 0*{con_tres}4

          '
      )
      
      ## Estimate variance within
      varcov <- '
    # Estimate the covariance between the components within t=1
    dep1 ~~ indep1
    dep1 ~~ conz1
    dep1 ~~ conw1
    dep1 ~~ conv1

    # Estimate the covariances between the residuals of the within component.
    dep2 ~~ indep2
    dep2 ~~ conz2
    dep2 ~~ conw2
    dep2 ~~ conv2

    dep3 ~~ indep3
    dep3 ~~ conz3
    dep3 ~~ conw3
    dep3 ~~ conv3

    dep4 ~~ indep4
    dep4 ~~ conz4
    dep4 ~~ conw4
    dep4 ~~ conv4

    # Estimate the residual variances of the within component
    dep1 ~~ dep1 # Variances
    dep2 ~~ dep2 # Residual variances
    dep3 ~~ dep3
    dep4 ~~ dep4

    indep1 ~~ indep1
    indep2 ~~ indep2
    indep3 ~~ indep3
    indep4 ~~ indep4

    conz1 ~~ conz1 # Variances
    conz2 ~~ conz2
    conz3 ~~ conz3
    conz4 ~~ conz4

    conw1 ~~ conw1 # Variances
    conw2 ~~ conw2
    conw3 ~~ conw3
    conw4 ~~ conw4

    conv1 ~~ conv1 # Variances
    conv2 ~~ conv2
    conv3 ~~ conv3
    conv4 ~~ conv4

    # Estimate the variance and covariance between RIs.
    RI_x ~~ RI_x
    RI_y ~~ RI_y
    RI_x ~~ RI_y

    RI_z ~~ RI_z
    RI_z ~~ RI_y
    RI_z ~~ RI_x

    RI_w ~~ RI_w
    RI_w ~~ RI_y
    RI_w ~~ RI_x

    RI_v ~~ RI_v
    RI_v ~~ RI_y
    RI_v ~~ RI_x

    # Set the correlation between the RI and components within t=1 to zero
    RI_x ~~ 0*dep1
    RI_x ~~ 0*indep1
    RI_x ~~ 0*conz1
    RI_x ~~ 0*conw1
    RI_x ~~ 0*conv1

    RI_y ~~ 0*dep1
    RI_y ~~ 0*indep1
    RI_y ~~ 0*conz1
    RI_y ~~ 0*conw1
    RI_y ~~ 0*conv1

    RI_z ~~ 0*dep1
    RI_z ~~ 0*indep1
    RI_z ~~ 0*conz1
    RI_z ~~ 0*conw1
    RI_z ~~ 0*conv1

    RI_w ~~ 0*dep1
    RI_w ~~ 0*indep1
    RI_w ~~ 0*conz1
    RI_w ~~ 0*conw1
    RI_w ~~ 0*conv1

    RI_v ~~ 0*dep1
    RI_v ~~ 0*indep1
    RI_v ~~ 0*conz1
    RI_v ~~ 0*conw1
    RI_v ~~ 0*conv1
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

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
      
      ### Autoregressive: constrained
      a2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3

'
      
      ### Forward: free
      b1 <- '
    dep2 ~ dep1 + indep1 + conz1  + conw1  + conv1
    dep3 ~ dep2 + indep2 + conz2  + conw2  + conv2
    dep4 ~ dep3 + indep3 + conz3  + conw3  + conv3
    indep2 ~ dep1
    indep3 ~ dep2
    indep4 ~ dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
      
      ### Forward: constrained
      b2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + conwdep*conw1 + conwdep*conv1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + conwdep*conw2 + conwdep*conv2
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + conwdep*conw3 + conwdep*conv3
    indep2 ~ c*dep1
    indep3 ~ c*dep2
    indep4 ~ c*dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
      
      ### Backward: free
      c1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1 + c*dep1 + d*indep1 + conz1 + conw1 + conv1
    indep3 ~ indep2 + c*dep2 + d*indep2 + conz2 + conw2 + conv2
    indep4 ~ indep3 + c*dep3 + d*indep3 + conz3 + conw3 + conv3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
      
      ### Backward: constrained
      c2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1  + conwindep*conw1  + conwindep*conv1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2  + conwindep*conw2  + conwindep*conv2
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3  + conwindep*conw3  + conwindep*conv3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
      
      ### Bidrectional: free
      d1 <- '
    dep2 ~ dep1 + indep1 + conz1 + conw1 + conv1
    dep3 ~ dep2 + indep2 + conz2 + conw2 + conv2
    dep4 ~ dep3 + indep3 + conz3 + conw3 + conv3
    indep2 ~ dep1 + indep1 + conz1 + conw1 + conv1
    indep3 ~ dep2 + indep2 + conz2 + conw2 + conv2
    indep4 ~ dep3 + indep3 + conz3 + conw3 + conv3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
      
      ### Bidrectional: Constrained
      d2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + conwdep*conw1 + conwdep*conv1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + conwdep*conw2 + conwdep*conv2
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + conwdep*conw3 + conwdep*conv3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1 + conwindep*conw1 + conwindep*conv1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2 + conwindep*conw2 + conwindep*conv2
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3 + conwindep*conw3 + conwindep*conv3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
      
      return(
        list(
          bwcomp = bwcomp,
          varcov = varcov,
          a1 = a1,
          a2 = a2,
          b1 = b1,
          b2 = b2,
          c1 = c1,
          c2 = c2,
          d1 = d1,
          d2 = d2
        )
      )
    }
    
    text_con_uno_demo <- function(varx, vary, con_uno) {
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

    conz1 =~ 1*{con_uno}1
    conz2 =~ 1*{con_uno}2
    conz3 =~ 1*{con_uno}3
    conz4 =~ 1*{con_uno}4


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

          '
      )
      
      ## Estimate variance within
      varcov <- '
    # Estimate the covariance between the components within t=1
    dep1 ~~ indep1
    dep1 ~~ conz1

    # Estimate the covariances between the residuals of the within component.
    dep2 ~~ indep2
    dep2 ~~ conz2

    dep3 ~~ indep3
    dep3 ~~ conz3

    dep4 ~~ indep4
    dep4 ~~ conz4

    # Estimate the residual variances of the within component
    dep1 ~~ dep1 # Variances
    dep2 ~~ dep2 # Residual variances
    dep3 ~~ dep3
    dep4 ~~ dep4

    indep1 ~~ indep1
    indep2 ~~ indep2
    indep3 ~~ indep3
    indep4 ~~ indep4

    conz1 ~~ conz1 # Variances
    conz2 ~~ conz2
    conz3 ~~ conz3
    conz4 ~~ conz4

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
    RI_x ~~ 0*conz1

    RI_y ~~ 0*dep1
    RI_y ~~ 0*indep1
    RI_y ~~ 0*conz1

    RI_z ~~ 0*dep1
    RI_z ~~ 0*indep1
    RI_y ~~ 0*conz1
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

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Autoregressive: constrained
      a2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Forward: free
      b1 <- '
    dep2 ~ dep1 + indep1 + conz1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ dep2 + indep2 + conz2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ dep3 + indep3 + conz3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ dep1
    indep3 ~ dep2
    indep4 ~ dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Forward: constrained
      b2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ c*dep1
    indep3 ~ c*dep2
    indep4 ~ c*dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Backward: free
      c1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1 + c*dep1 + d*indep1 + conz1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ indep2 + c*dep2 + d*indep2 + conz2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ indep3 + c*dep3 + d*indep3 + conz3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Backward: constrained
      c2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Bidrectional: free
      d1 <- '
    dep2 ~ dep1 + indep1 + conz1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ dep2 + indep2 + conz2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ dep3 + indep3 + conz3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ dep1 + indep1 + conz1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ dep2 + indep2 + conz2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ dep3 + indep3 + conz3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      ### Bidrectional: Constrained
      d2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3
'
      
      return(
        list(
          bwcomp = bwcomp,
          varcov = varcov,
          a1 = a1,
          a2 = a2,
          b1 = b1,
          b2 = b2,
          c1 = c1,
          c2 = c2,
          d1 = d1,
          d2 = d2
        )
      )
    }
    
    text_con_dos_demo <- function(varx, vary, con_uno, con_dos) {
      ## Estimate variance between
      bwcomp <- glue::glue(
        '
    # Create the between components
    RI_x =~ 1*{vary}1 + 1*{vary}2 + 1*{vary}3 + 1*{vary}4
    RI_y =~ 1*{varx}1 + 1*{varx}2 + 1*{varx}3 + 1*{varx}4
    RI_z =~ 1*{con_uno}1 + 1*{con_uno}2 + 1*{con_uno}3 + 1*{con_uno}4
    RI_w =~ 1*{con_dos}1 + 1*{con_dos}2 + 1*{con_dos}3 + 1*{con_dos}4

    # Create the components within
    dep1 =~ 1*{vary}1
    dep2 =~ 1*{vary}2
    dep3 =~ 1*{vary}3
    dep4 =~ 1*{vary}4


    indep1 =~ 1*{varx}1
    indep2 =~ 1*{varx}2
    indep3 =~ 1*{varx}3
    indep4 =~ 1*{varx}4

    conz1 =~ 1*{con_uno}1
    conz2 =~ 1*{con_uno}2
    conz3 =~ 1*{con_uno}3
    conz4 =~ 1*{con_uno}4

    conw1 =~ 1*{con_dos}1
    conw2 =~ 1*{con_dos}2
    conw3 =~ 1*{con_dos}3
    conw4 =~ 1*{con_dos}4


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

    # {con_dos}1 ~~ 0*{con_dos}1
    # {con_dos}2 ~~ 0*{con_dos}2
    # {con_dos}3 ~~ 0*{con_dos}3
    # {con_dos}4 ~~ 0*{con_dos}4

          '
      )
      
      ## Estimate variance within
      varcov <- '
    # Estimate the covariance between the components within t=1
    dep1 ~~ indep1
    dep1 ~~ conz1
    dep1 ~~ conw1

    # Estimate the covariances between the residuals of the within component.
    dep2 ~~ indep2
    dep2 ~~ conz2
    dep2 ~~ conw2

    dep3 ~~ indep3
    dep3 ~~ conz3
    dep3 ~~ conw3

    dep4 ~~ indep4
    dep4 ~~ conz4
    dep4 ~~ conw4

    # Estimate the residual variances of the within component
    dep1 ~~ dep1 # Variances
    dep2 ~~ dep2 # Residual variances
    dep3 ~~ dep3
    dep4 ~~ dep4

    indep1 ~~ indep1
    indep2 ~~ indep2
    indep3 ~~ indep3
    indep4 ~~ indep4

    conz1 ~~ conz1 # Variances
    conz2 ~~ conz2
    conz3 ~~ conz3
    conz4 ~~ conz4

    conw1 ~~ conw1 # Variances
    conw2 ~~ conw2
    conw3 ~~ conw3
    conw4 ~~ conw4

    # Estimate the variance and covariance between RIs.
    RI_x ~~ RI_x
    RI_y ~~ RI_y
    RI_x ~~ RI_y

    RI_z ~~ RI_z
    RI_z ~~ RI_y
    RI_z ~~ RI_x

    RI_w ~~ RI_w
    RI_w ~~ RI_y
    RI_w ~~ RI_x

    # Set the correlation between the RI and components within t=1 to zero
    RI_x ~~ 0*dep1
    RI_x ~~ 0*indep1
    RI_x ~~ 0*conz1
    RI_x ~~ 0*conw1

    RI_y ~~ 0*dep1
    RI_y ~~ 0*indep1
    RI_y ~~ 0*conz1
    RI_y ~~ 0*conw1

    RI_z ~~ 0*dep1
    RI_z ~~ 0*indep1
    RI_z ~~ 0*conz1
    RI_z ~~ 0*conw1

    RI_w ~~ 0*dep1
    RI_w ~~ 0*indep1
    RI_w ~~ 0*conz1
    RI_w ~~ 0*conw1
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

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Autoregressive: constrained
      a2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3


'
      
      ### Forward: free
      b1 <- '
    dep2 ~ dep1 + indep1 + conz1  + conw1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ dep2 + indep2 + conz2  + conw2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ dep3 + indep3 + conz3  + conw3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ dep1
    indep3 ~ dep2
    indep4 ~ dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Forward: constrained
      b2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + conwdep*conw1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + conwdep*conw2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + conwdep*conw3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ c*dep1
    indep3 ~ c*dep2
    indep4 ~ c*dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Backward: free
      c1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1 + c*dep1 + d*indep1 + conz1 + conw1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ indep2 + c*dep2 + d*indep2 + conz2 + conw2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ indep3 + c*dep3 + d*indep3 + conz3 + conw3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Backward: constrained
      c2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1  + conwindep*conw1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2  + conwindep*conw2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3  + conwindep*conw3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Bidrectional: free
      d1 <- '
    dep2 ~ dep1 + indep1 + conz1 + conw1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ dep2 + indep2 + conz2 + conw2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ dep3 + indep3 + conz3 + conw3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ dep1 + indep1 + conz1 + conw1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ dep2 + indep2 + conz2 + conw2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ dep3 + indep3 + conz3 + conw3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      ### Bidrectional: Constrained
      d2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + conwdep*conw1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + conwdep*conw2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + conwdep*conw3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1 + conwindep*conw1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2 + conwindep*conw2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3 + conwindep*conw3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3
'
      
      return(
        list(
          bwcomp = bwcomp,
          varcov = varcov,
          a1 = a1,
          a2 = a2,
          b1 = b1,
          b2 = b2,
          c1 = c1,
          c2 = c2,
          d1 = d1,
          d2 = d2
        )
      )
    }
    
    text_con_tres_demo <-
      function(varx, vary, con_uno, con_dos, con_tres) {
        ## Estimate variance between
        bwcomp <- glue::glue(
          '
    # Create the between components
    RI_x =~ 1*{vary}1 + 1*{vary}2 + 1*{vary}3 + 1*{vary}4
    RI_y =~ 1*{varx}1 + 1*{varx}2 + 1*{varx}3 + 1*{varx}4
    RI_z =~ 1*{con_uno}1 + 1*{con_uno}2 + 1*{con_uno}3 + 1*{con_uno}4
    RI_w =~ 1*{con_dos}1 + 1*{con_dos}2 + 1*{con_dos}3 + 1*{con_dos}4
    RI_v =~ 1*{con_tres}1 + 1*{con_tres}2 + 1*{con_tres}3 + 1*{con_tres}4

    # Create the components within
    dep1 =~ 1*{vary}1
    dep2 =~ 1*{vary}2
    dep3 =~ 1*{vary}3
    dep4 =~ 1*{vary}4


    indep1 =~ 1*{varx}1
    indep2 =~ 1*{varx}2
    indep3 =~ 1*{varx}3
    indep4 =~ 1*{varx}4

    conz1 =~ 1*{con_uno}1
    conz2 =~ 1*{con_uno}2
    conz3 =~ 1*{con_uno}3
    conz4 =~ 1*{con_uno}4

    conw1 =~ 1*{con_dos}1
    conw2 =~ 1*{con_dos}2
    conw3 =~ 1*{con_dos}3
    conw4 =~ 1*{con_dos}4


    conv1 =~ 1*{con_tres}1
    conv2 =~ 1*{con_tres}2
    conv3 =~ 1*{con_tres}3
    conv4 =~ 1*{con_tres}4


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

    # {con_dos}1 ~~ 0*{con_dos}1
    # {con_dos}2 ~~ 0*{con_dos}2
    # {con_dos}3 ~~ 0*{con_dos}3
    # {con_dos}4 ~~ 0*{con_dos}4

    # {con_tres}1 ~~ 0*{con_tres}1
    # {con_tres}2 ~~ 0*{con_tres}2
    # {con_tres}3 ~~ 0*{con_tres}3
    # {con_tres}4 ~~ 0*{con_tres}4

          '
        )
        
        ## Estimate variance within
        varcov <- '
    # Estimate the covariance between the components within t=1
    dep1 ~~ indep1
    dep1 ~~ conz1
    dep1 ~~ conw1
    dep1 ~~ conv1

    # Estimate the covariances between the residuals of the within component.
    dep2 ~~ indep2
    dep2 ~~ conz2
    dep2 ~~ conw2
    dep2 ~~ conv2

    dep3 ~~ indep3
    dep3 ~~ conz3
    dep3 ~~ conw3
    dep3 ~~ conv3

    dep4 ~~ indep4
    dep4 ~~ conz4
    dep4 ~~ conw4
    dep4 ~~ conv4

    # Estimate the residual variances of the within component
    dep1 ~~ dep1 # Variances
    dep2 ~~ dep2 # Residual variances
    dep3 ~~ dep3
    dep4 ~~ dep4

    indep1 ~~ indep1
    indep2 ~~ indep2
    indep3 ~~ indep3
    indep4 ~~ indep4

    conz1 ~~ conz1 # Variances
    conz2 ~~ conz2
    conz3 ~~ conz3
    conz4 ~~ conz4

    conw1 ~~ conw1 # Variances
    conw2 ~~ conw2
    conw3 ~~ conw3
    conw4 ~~ conw4

    conv1 ~~ conv1 # Variances
    conv2 ~~ conv2
    conv3 ~~ conv3
    conv4 ~~ conv4

    # Estimate the variance and covariance between RIs.
    RI_x ~~ RI_x
    RI_y ~~ RI_y
    RI_x ~~ RI_y

    RI_z ~~ RI_z
    RI_z ~~ RI_y
    RI_z ~~ RI_x

    RI_w ~~ RI_w
    RI_w ~~ RI_y
    RI_w ~~ RI_x

    RI_v ~~ RI_v
    RI_v ~~ RI_y
    RI_v ~~ RI_x

    # Set the correlation between the RI and components within t=1 to zero
    RI_x ~~ 0*dep1
    RI_x ~~ 0*indep1
    RI_x ~~ 0*conz1
    RI_x ~~ 0*conw1
    RI_x ~~ 0*conv1

    RI_y ~~ 0*dep1
    RI_y ~~ 0*indep1
    RI_y ~~ 0*conz1
    RI_y ~~ 0*conw1
    RI_y ~~ 0*conv1

    RI_z ~~ 0*dep1
    RI_z ~~ 0*indep1
    RI_z ~~ 0*conz1
    RI_z ~~ 0*conw1
    RI_z ~~ 0*conv1

    RI_w ~~ 0*dep1
    RI_w ~~ 0*indep1
    RI_w ~~ 0*conz1
    RI_w ~~ 0*conw1
    RI_w ~~ 0*conv1

    RI_v ~~ 0*dep1
    RI_v ~~ 0*indep1
    RI_v ~~ 0*conz1
    RI_v ~~ 0*conw1
    RI_v ~~ 0*conv1
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

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
        
        ### Autoregressive: constrained
        a2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ d*indep1
    indep3 ~ d*indep2
    indep4 ~ d*indep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3

'
        
        ### Forward: free
        b1 <- '
    dep2 ~ dep1 + indep1 + conz1  + conw1  + conv1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ dep2 + indep2 + conz2  + conw2  + conv2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ dep3 + indep3 + conz3  + conw3  + conv3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ dep1
    indep3 ~ dep2
    indep4 ~ dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
        
        ### Forward: constrained
        b2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + conwdep*conw1 + conwdep*conv1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + conwdep*conw2 + conwdep*conv2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + conwdep*conw3 + conwdep*conv3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ c*dep1
    indep3 ~ c*dep2
    indep4 ~ c*dep3

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
        
        ### Backward: free
        c1 <- '
    dep2 ~ dep1
    dep3 ~ dep2
    dep4 ~ dep3
    indep2 ~ indep1 + c*dep1 + d*indep1 + conz1 + conw1 + conv1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ indep2 + c*dep2 + d*indep2 + conz2 + conw2 + conv2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ indep3 + c*dep3 + d*indep3 + conz3 + conw3 + conv3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
        
        ### Backward: constrained
        c2 <- '
    dep2 ~ a*dep1
    dep3 ~ a*dep2
    dep4 ~ a*dep3
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1  + conwindep*conw1  + conwindep*conv1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2  + conwindep*conw2  + conwindep*conv2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3  + conwindep*conw3  + conwindep*conv3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
        
        ### Bidrectional: free
        d1 <- '
    dep2 ~ dep1 + indep1 + conz1 + conw1 + conv1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ dep2 + indep2 + conz2 + conw2 + conv2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ dep3 + indep3 + conz3 + conw3 + conv3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ dep1 + indep1 + conz1 + conw1 + conv1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ dep2 + indep2 + conz2 + conw2 + conv2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ dep3 + indep3 + conz3 + conw3 + conv3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
        
        ### Bidrectional: Constrained
        d2 <- '
    dep2 ~ a*dep1 + b*indep1 + conzdep*conz1 + conwdep*conw1 + conwdep*conv1 + edaddep*edad1 + sexodep*sexo1
    dep3 ~ a*dep2 + b*indep2 + conzdep*conz2 + conwdep*conw2 + conwdep*conv2 + edaddep*edad1 + sexodep*sexo1
    dep4 ~ a*dep3 + b*indep3 + conzdep*conz3 + conwdep*conw3 + conwdep*conv3 + edaddep*edad1 + sexodep*sexo1
    indep2 ~ c*dep1 + d*indep1 + conzindep*conz1 + conwindep*conw1 + conwindep*conv1 + edadindep*edad1 + sexoindep*sexo1
    indep3 ~ c*dep2 + d*indep2 + conzindep*conz2 + conwindep*conw2 + conwindep*conv2 + edadindep*edad1 + sexoindep*sexo1
    indep4 ~ c*dep3 + d*indep3 + conzindep*conz3 + conwindep*conw3 + conwindep*conv3 + edadindep*edad1 + sexoindep*sexo1

    conz2 ~ conz1
    conz3 ~ conz2
    conz4 ~ conz3

    conw2 ~ conw1
    conw3 ~ conw2
    conw4 ~ conw3

    conv2 ~ conv1
    conv3 ~ conv2
    conv4 ~ conv3
'
        
        return(
          list(
            bwcomp = bwcomp,
            varcov = varcov,
            a1 = a1,
            a2 = a2,
            b1 = b1,
            b2 = b2,
            c1 = c1,
            c2 = c2,
            d1 = d1,
            d2 = d2
          )
        )
      }
    
    if (controls == 0 & demo == FALSE) {
      return(text_basic(varx, vary))
    } else if (controls == 1 & demo == FALSE) {
      return(text_con_uno(varx, vary, con_uno))
    } else if (controls == 2 & demo == FALSE) {
      return(text_con_dos(varx, vary, con_uno, con_dos))
    } else if (controls == 3 & demo == FALSE) {
      return(text_con_tres(varx, vary, con_uno, con_dos, con_tres))
    } else if (controls == 1 & demo == TRUE) {
      return(text_con_uno_demo(varx, vary, con_uno))
    } else if (controls == 2 & demo == TRUE) {
      return(text_con_dos_demo(varx, vary, con_uno, con_dos))
    } else if (controls == 3 & demo == TRUE) {
      return(text_con_tres_demo(varx, vary, con_uno, con_dos, con_tres))
    }
    
  }

create.text.object("conf",
                   "sdo",
                   "ideologia",
                   "xiwawa",
                   "sod",
                   controls = 3,
                   demo = TRUE) # Probar


# 7.3 Compare GOF ---------------

### Create function

gof.comp  = function(data,
                     pairs,
                     measures = c("CFI", "TLI", "RMSEA", "SRMR",
                                  "AIC", "BIC", "aBIC", "par", "LL")) {
  comp <- list()
  for (i in 1:length(pairs)) {
    gof <- data
    nest <- pairs[[i]][1]
    full <- pairs[[i]][2]
    delta <- NULL
    for (k in measures) {
      delta[paste0(k, "_D")] <-
        gof[m == nest, get(k)] - gof[m == full, get(k)]
    }
    par_LLcorf_nest <- gof[m == nest, par] * gof[m == nest, LLcorrectf]
    par_LLcorf_full <- gof[m == full, par] * gof[m == full, LLcorrectf]
    delta["CD"] <- (par_LLcorf_nest - par_LLcorf_full) / delta["par_D"]
    delta["TRd"] <- (-2 * delta["LL_D"]) / delta["CD"]
    delta["TRd_df"] <- gof[m == full, "par"] - gof[m == nest, "par"]
    delta["TRd_pvalue"] <- pchisq(as.numeric(delta["TRd"]),
                                  as.numeric(delta["TRd_df"]), lower.tail = F)
    comp[[paste0(nest, " vs. ", full, sep = "")]] <- delta
  }
  comp <- data.table(comp = names(comp), dplyr::bind_rows(comp))
  return(comp)
}
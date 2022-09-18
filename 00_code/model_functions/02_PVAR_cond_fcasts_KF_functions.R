# Functions copied from Banbura, Giannone & Lenza (2015), Journal of Forecasting
# Adapted and tested by Lukas Vashold
SKF <- function(Yt,Z,R,Tt,Q,A_0,P_0,c1,c2) {
  # Functions copied from Banbura, Giannone & Lenza (2015), Journal of Forecasting
  # Adapted and tested by Lukas Vashold
  #______________________________________________________________________
  # Kalman filter for stationary systems with time-varying system matrices
  # and missing data.
  #
  # The model is        y_t   = Z %*% a_t + eps_t       
  #                     a_t+1 = T %*% a_t + u_t       
  #
  #______________________________________________________________________
  # INPUT  
  #        Y         Data                                 (n.obs x n)  
  # OUTPUT 
  #        S.Am       Predicted state vector  A_t|t-1      (n.obs x m)  
  #        S.AmU      Filtered  state vector  A_t|t        (n.obs+1 x m)  
  #        S.Pm       Predicted covariance of A_t|t-1      (n.obs x m x m)  
  #        S.PmU      Filtered  covariance of A_t|t        (n.obs+1 x m x m)  
  #        S.loglik   Value of likelihood function
  
  # Output structure & dimensions
  
  n <- dim(Z)[1]
  m <- dim(Z)[2]
  n.obs <- dim(Yt)[2]
  
  Am <- AmU <- matrix(NA, m, n.obs)
  Pm <- PmU <- array(NA, dim = c(m,m,n.obs))
  ZF <- V <- list()
  # 
  # S.ZF = cell(n.obs);
  # S.V = cell(n.obs);
  #______________________________________________________________________
  Au = A_0  # A_0|0;
  Pu = P_0  # P_0|0
  
  for(i in 1:n.obs) {
    A   = Tt%*%Au+c2
    P   = Tt%*%Pu%*%t(Tt) + Q
    P   =  0.5 * (P+t(P))
    
    # handling the missing data
    m_data <-  MissData(Yt[,i],Z,R,c1)
    y_t <- m_data$y_t
    Z_t <- m_data$Z_t
    R_t <- m_data$R_t
    c1_t <- m_data$c1_t
    
    
    if (length(y_t)==0) {
      Au = A
      Pu = P
      ZFt = matrix(0, m, 1)
      Vt = 0
    } else {
      PZ = P%*%t(Z_t)
      Ft  = (Z_t%*%PZ + R_t)
      ZFt = t(Z_t) %*% solve(Ft)
      PZF = P%*%ZFt
      
      Vt   = y_t - Z_t%*%A- c1_t
      Au  = A  + PZF %*% Vt
      Pu  = P  - PZF %*% t(PZ)
      Pu   =  0.5 * (Pu+t(Pu))
    }
    V[[i]] = Vt
    ZF[[i]] = ZFt
    Am[,i]   = A
    Pm[,,i] = P
  
    AmU[,i]    = Au
    PmU[,,i]  = Pu
  }
  return(list("Am" = Am, "AmU" = AmU, "Pm" = Pm, "PmU" = PmU, "ZF" = ZF, "V" = V))
}



FIS <- function(Yt,Z,R,Tt,S) {
  # Functions copied from Banbura, Giannone & Lenza (2015), Journal of Forecasting
  # Adapted and tested by Lukas Vashold
  #______________________________________________________________________
  # Fixed interval smoother (see Durbin and Koopman, 2001, p. 64-71)
  # FIS returns the smoothed state vector AmT and its covar matrix PmT             
  # Use this in conjnuction with function SKF
  #______________________________________________________________________
  # INPUT  
  #        Yt         Data                                 (n.obs x n)  
  #        S Estimates from Kalman filter SKF                                                          
  #          S.Am   : Estimates     a_t|t-1                  (n.obs x m) 
  #          S.Pm   : P_t|t-1 = Cov(a_t|t-1)             (n.obs x m x m)
  # OUTPUT 
  #        S Smoothed estimates added to above
  #          S.AmT  : Estimates     a_t|T                    (n.obs x m) 
  #        where m is the dim of state vector and t = 1 ...T is time
  
  m <- dim(S$Am)[1]
  n.obs <- dim(S$Am)[2]
  AmT <- matrix(0, m, n.obs)
  PmT <- array(0, dim = c(m,m,n.obs))
  AmT[,n.obs] <- S$AmU[,n.obs]
  r <- matrix(0, M, 1)

  for(i in rev(seq_len(n.obs))) {
    m_data <-  MissData(Yt[,i],Z,R,matrix(0, 1, m))
    y_t <- m_data$y_t
    Z_t <- m_data$Z_t
    if(length(Z_t) == 0) Z_t <- matrix(0,1,m)
    r = S$ZF[[i]]%*%S$V[[i]]+t((Tt%*%(diag(m)-S$Pm[,,i]%*%S$ZF[[i]]%*%Z_t)))%*%r
    AmT[,i] = S$Am[,i] + S$Pm[,,i] %*% r
  }
  return("AmT" = AmT)
}



MissData <- function(y,Ct,R,c1) {
  ix <- !is.na(y)
  y <- y[ix]
  c1 <- c1[ix]
  Ct <- Ct[ix,]
  R <- R[ix,ix]
  return(list("y_t" = y, "c1_t" = c1, "Z_t" = Ct, "R_t" = R))
}

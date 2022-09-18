#####
# Conditional forecasts using Kalman filter
source("./00_code/model_functions/02_PVAR_cond_fcasts_KF_functions.R")


# compute unconditional forecasts first as input for conditional ones
pred_store <- array(NA, c(thindraws, M, fhorz, N))
base_store <- array(NA, c(thindraws, M, fhorz, N))
SIGMA_store <- array(NA, c(thindraws, M, M, N))
for (irep in 1:thindraws) {
  for(cc in 1:N) {
    Y.c  <- as.matrix(Y[[cc]])
    X.c  <- as.matrix(X[[cc]][, 1:K])
    
    S.c  <- Sv_store[[cc]][irep, , ]
    Htt <- S.c[bigT[cc], ]
    
    
    z1 <- X.c[(nrow(X.c)),]
    
    J_draw <- J_store[irep,,,cc]
    J_drawinv <- try(solve(J_draw),silent=TRUE)
    if(is(J_drawinv,"try-error")) next
    Sigmat <- J_drawinv %*% diag(exp(Htt)) %*% t(J_drawinv)
    SIGMA_store[irep,,,cc] <- Sigmat
    
    Mean00 <- X.c[nrow(X.c),]
    Sigma00 <- matrix(0,K,K)
    Sigma00[1:M, 1:M] <- Sigmat
    
    compMat <- gen_compMat(A_store[irep,,,cc], M, plag)
    
    if(cons) {
      add_cons <- A_store[irep,,,cc]["cons", ]
    }
    if(trend) {
      add_trend <- A_store[irep,,,cc]["trend", ]
    }
    
    Mm <- compMat$Cm
    Jm <- compMat$Jm
    Cm <- diag(M)
    
    if (max(abs(Re(eigen(Mm)$values)))>1) next
    for(ih in 1:fhorz) {
      z1 <- Mm %*% z1
      if(cons) {
        z1 <- z1 + add_cons
      }
      if(trend) {
        z1 <- z1 + add_trend * (bigT[cc] + ih)
      }
      
      cholSig <- try(t(chol(Sigma00[1:M,1:M])),silent=TRUE)
      if(is(cholSig,"try-error")){next} else {
        yf <- z1[1:M] + cholSig%*%rnorm(M,0,1)
      }
      
      pred_store[irep,,ih,cc] <- yf
      base_store[irep,,ih,cc] <- z1[1:M]
      
      # update first and second moments
      Mean00 <- Mm%*%Mean00
      Sigma00 <- Mm %*% Sigma00 %*% t(Mm) + Jm%*%Sigmat%*%t(Jm)
    }
  }
}

## get dimensions
thindraws <- dim(A_store)[1]
M <- dim(A_store)[3]
N <- dim(A_store)[4]
k <- dim(A_store)[2]
det_terms <- sum(dimnames(A_store)[[2]] %in% c("cons", "trend", "trendsq", "trendlog", "trendbreak"))
p <- (k-det_terms) / M
var_names <- dimnames(A_store)[[3]]
horizon <- dim(cond_data[[1]])[1]

## create conditioning path of variables
paths <- list()
for(cc in seq_len(N)) {
  pos <- which(var_names %in% colnames(cond_data[[cc]]))
  constr_mat <- matrix(NA_real_, horizon, M)
  constr_mat[seq_len(nrow(cond_data[[cc]])), pos] <- as.matrix(cond_data[[cc]])
  colnames(constr_mat) <- var_names
  paths[[cc]] <- constr_mat
}


## create container
fcast_cond <- array(NA, dim = c(thindraws, horizon, M, N))

## start loop for conditional forecasts
for(cc in 1:N) {
  # country-specific setup
  yinput <- paths[[cc]]
  X.c <- X[[cc]]
  Y.c <- Y[[cc]]
  initx <- Y.c[nrow(Y.c), ]
  if(p > 1) {
    initx <- c(initx, X.c[nrow(X.c), 1:(M*(p-1))])
  } 
  initV <- diag(length(initx)) * 1e-7
  initx <- t(initx)
  initx_zeros <- rep(0, length(initx))
  RR <- diag(M) * 1e-12
  
  for(irep in 1:thindraws) {
    # posterior draws of autoregressive coefficients and vcov-matrix
    compMat <- gen_compMat(A_store[irep,,,cc], M, p)
    CC <- compMat$Jm
    AA <- compMat$Cm
    if (max(abs(Re(eigen(AA)$values)))>1) next
    
    QQ <- matrix(0, M*p, M*p)
    QQ[1:M, 1:M] <- SIGMA_store[irep,,,cc]
    cholSig <- try(t(chol(QQ[1:M,1:M])),silent=TRUE)
    if(is(cholSig,"try-error")) next
    yplus <- base_store[irep,,,cc]
    ystar <- t(yinput) - yplus
    
    ## Kalman stuff here
    S <- SKF(Yt = ystar, Z = CC, R = RR, Tt = AA, Q = QQ, A_0 = initx_zeros, 
             P_0 = initV, c1 = matrix(0, M, 1), c2 = initx_zeros)
    ahatstar <- FIS(Y = ystar, Z = CC, R = RR, Tt = AA, S = S)
    atilda <- ahatstar[1:M,] + yplus
    ## store
    fcast_cond[irep,,,cc] <- t(atilda)
  }
}

if(rep_end > 0) {
  horizon <- fhorz <- horizon - rep_end
  fcast_cond <- fcast_cond[,1:horizon,,]
  pred_store <- pred_store[,,1:horizon,]
}


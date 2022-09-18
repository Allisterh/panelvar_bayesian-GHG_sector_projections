#############################################################
###  Panel Vector Autoregression (PVAR) with NG setup    ####
#############################################################

#---------------------------load stuff---------------------------------------------#
library(GIGrvg)
library(stochvol)
library(Rcpp)
library(MASS)
Rcpp::sourceCpp("./00_code/model_functions/00_auxiliary_functions.cpp")
source("./00_code/model_functions/00_auxiliary_functions.R")

#----------------------------kill NAs----------------------------------------------#
na_present <- unlist(lapply(Yraw,function(l) !any(is.na(l))))
Yraw <- Yraw[na_present]

#----------------------------------------------------------------------------------#
cN = names(Yraw)
N  = length(cN)
varNames = colnames(Yraw[[1]])
if(is.null(varNames)) varNames = paste0("var.",seq(1,ncol(Yraw[[1]])))
timeraw  = lapply(Yraw, rownames)

# dimensions and parameter counts
Traw = unlist(lapply(Yraw, nrow))
M    = length(varNames)
K    = M*plag
k    = K + ifelse(cons,1,0) + ifelse(trend,1,0)
MN   = M*N
v    = (M*(M-1))/2

# create data lists
Xraw = lapply(Yraw, function(y) mlag(y, plag, cons))
for(i in 1:N) rownames(Xraw[[i]]) <- rownames(Yraw[[i]])
nameslags = colnames(Xraw[[1]])
if(trend) nameslags <- c(nameslags, "trend")
X    = lapply(Xraw,function(x)x[(plag+1):nrow(x),,drop=FALSE])
Y    = lapply(Yraw,function(y)y[(plag+1):nrow(y),,drop=FALSE])
if(trend) X <- lapply(X, function(cc) cbind(cc, trend = seq(1,nrow(cc))))

bigT = unlist(lapply(Y,nrow))

#--------------------------Hyperparameters------------------------------------------#

# prior mean for own first lag
prmean = 1

# inverse-gamma setup for variances
c0 = 0.01   # hyperparamter shape inverse gamma homoscedastic vola
d0 = 0.01   # hyperparameter rate inverse gamma homoscedastic vola

# Normal-Gamma prior
d_lambda = 0.01 # hyperparameter global
e_lambda = 0.01 # hyperparmater global

tau_start <- phi_start <- 0.5


#--------------------------OLS estimates--------------------------------------------#
XtXinv <- lapply(1:N, function(cc){
  temp <- try(solve(crossprod(X[[cc]])),silent=TRUE)
  if(is(temp,"try-error")) temp <- ginv(crossprod(X[[cc]]))
  return(temp)
})
A_OLS = array(NA_real_, c(k, M, N), dimnames=list(nameslags, varNames, cN))
V_OLS = array(NA_real_, c(k, M, N), dimnames=list(nameslags, varNames, cN))
J_OLS = array(NA_real_, c(M, M, N), dimnames=list(varNames, varNames, cN))
Z_OLS = array(NA_real_, c(M, M, N), dimnames=list(varNames, varNames, cN))
for(cc in 1:N) J_OLS[,,cc] <- diag(M)
for(cc in 1:N) Z_OLS[,,cc] <- diag(M)
E_OLS = list()
S_OLS = list()
for(cc in 1:N){
  Y.c  = as.matrix(Y[[cc]])
  X.c  = as.matrix(X[[cc]])
  X1 = X.c
  E.c = matrix(NA,bigT[cc],M)
  S.c = matrix(NA,bigT[cc],M)
  for(mm in 1:M){
    if(mm>1) X1 = cbind(Y.c[,1:(mm-1)],X.c)
    
    XtXinv1 = try(chol2inv(chol(crossprod(X1))),silent=TRUE)
    
    if(is(XtXinv1,"try-error")) XtXinv1 = try(solve(crossprod(X1)),silent=TRUE)
    if(is(XtXinv1,"try-error")) XtXinv1 = ginv(crossprod(X1))
    temp = XtXinv1%*%t(X1)%*%Y.c[,mm]
    A_OLS[,mm,cc] = temp[((mm-1)+1):nrow(temp),]
    if(mm>1) J_OLS[mm,1:(mm-1),cc] = -temp[mm,]
    E.c[,mm] = Y.c[,mm] - X1%*%temp
    S.c[,mm] = crossprod(E.c[,mm])/(bigT[mm]-ncol(X1))
    temp = diag(XtXinv1*S.c[1,mm])
    V_OLS[,mm,cc] = temp[((mm-1)+1):length(temp)]
    if(mm>1) Z_OLS[mm,1:(mm-1),cc] = temp[mm]
  }
  E_OLS[[cc]] = E.c
  S_OLS[[cc]] = S.c
}

#--------------------------Initial Values--------------------------------------------#
A_draw = A_OLS
J_draw = J_OLS
Sv_draw = S_OLS
Em_str = E_OLS


#--------------------------Priors----------------------------------------------------#

# 1st stage prior
alpha_draw <- array(0, c(k, M), dimnames=list(nameslags, varNames))
gamma_draw <- array(0, c(M, M), dimnames=list(varNames, varNames)) 
diag(gamma_draw) <- 1


# NG A_draw / J_draw
lambda2_draw <- array(0.01, c(plag+1,N), 
                      dimnames=list(c(paste0("lag.",seq(1,plag)),"cov"),cN))
tau_draw     <- array(tau_start, c(plag+1,N), 
                      dimnames=list(c(paste0("lag.",seq(1,plag)),"cov"),cN))

theta_draw <- array(10, c(k, M, N), dimnames=list(nameslags, varNames, cN))
if(k > K) {
  theta_draw[(K+1):k, , ] <- 1e4 
}
zeta_draw <- array(10, c(M, M, N), dimnames=list(varNames, varNames, cN))


# 2nd stage prior
a_prior <- matrix(0,k,M)
diag(a_prior) <- prmean
g_prior <- array(0, c(M,M)) 
diag(g_prior) <- 1

# Normal-Gamma components
delta2_draw <- matrix(0.01, plag+1, 1, dimnames=list(c(paste0("lag.",seq(1,plag)),"cov")))
phi_draw <- matrix(phi_start, plag+1, 1, dimnames=list(c(paste0("lag.",seq(1,plag)),"cov")))

kappa_draw <- matrix(10,k,M) 
if(k > K) {
  kappa_draw[(K+1):k, ] <- 1e4 
}
iota_draw <- matrix(10,M,M)

# Main diagonal of triangularized variance matrix
Sv_draw <- lapply(bigT, function(tt) matrix(-3,tt,M))


#--------------------------Sampler Settings------------------------------------------#
ntot <- nburn + nsave
maxTries <- 100
thin <- 5
# thinning
count      = 0
thindraws  = nsave/thin
thin.draws = seq(nburn+1,ntot,by=thin)


#--------------------------Storage---------------------------------------------------#

# autoregressive coefficients and deterministics
A_store       = array(NA_real_,c(thindraws,k,M,N),
                      dimnames=list(NULL,nameslags,varNames,cN))
alpha_store   = array(NA_real_,c(thindraws,k,M),
                      dimnames=list(NULL,nameslags,varNames))
J_store       = array(NA_real_,c(thindraws,M,M,N),
                      dimnames=list(NULL,varNames,varNames,cN))
gamma_store   = array(NA_real_,c(thindraws,M,M),
                      dimnames=list(NULL,varNames,varNames))
ind_store     = matrix(NA_real_, thindraws, N)

# variances
Sv_store      = lapply(1:N, function(cc) array(NA_real_,c(thindraws,bigT[cc],M),
                                               dimnames=list(NULL,NULL,varNames)))

# NG 1st stage
theta_store   = array(NA_real_,c(thindraws,k,M,N),
                      dimnames=list(NULL,nameslags,varNames,cN))
zeta_store    = array(NA_real_,c(thindraws,M,M,N),
                      dimnames=list(NULL,varNames,varNames,cN))
lambda2_store = array(NA_real_,c(thindraws,plag+1,N),
                      dimnames=list(NULL,NULL,cN))

# NG 2nd stage
kappa_store   = array(NA_real_,c(thindraws,k,M),
                      dimnames=list(NULL,nameslags,varNames))
iota_store    = array(NA_real_,c(thindraws,M,M),
                      dimnames=list(NULL,varNames,varNames))
delta2_store  = array(NA_real_,c(thindraws,plag+1),
                      dimnames=list(NULL,c(paste0("lag.",seq(1,plag)),"cov")))

#--------------------------MCMC Loop--------------------------------------------------#
for(irep in 1:ntot){
  #----------------------------------------------------------------------------
  # Step 1: Sample country-specific coefficients
  ind_vec <- rep(0, N)
  for(cc in 1:N){
    Y.c  = as.matrix(Y[[cc]])
    X.c  = as.matrix(X[[cc]])
    S.c  = Sv_draw[[cc]]
    E.c  = Em_str[[cc]]
    ind <- 0
    counter <- 1
    if(irep > nburn) max_tries <- maxTries else max_tries <- ceiling(maxTries / 5)
    while(ind == 0 & counter < max_tries) {
      for(mm in 1:M){
        Y.i = Y.c[,mm]*exp(-0.5*S.c[,mm])
        if(mm>1) X1 = cbind(Y.c[,1:(mm-1)],X.c) else X1 = X.c
        X.i = as.matrix(X1*exp(-0.5*S.c[,mm]))
        
        Aprior = alpha_draw[,mm,drop=FALSE] 
        Vprior = as.matrix(theta_draw[,mm,cc])
        if(mm>1){
          for(ll in (mm-1):1){
            Aprior = rbind(matrix(gamma_draw[mm,ll],1),Aprior)
            Vprior = rbind(matrix(zeta_draw[mm,ll,cc],1),Vprior)
          }
        }
        Vpriorinv = diag(1/c(Vprior))
        
        V_post = try(chol2inv(chol(crossprod(X.i)+Vpriorinv)),silent=TRUE)
        if(is(V_post,"try-error")) V_post = try(solve(crossprod(X.i)+Vpriorinv),silent=TRUE)
        if(is(V_post,"try-error")) V_post = ginv(crossprod(X.i)+Vpriorinv)
        A_post = V_post%*%(crossprod(X.i,Y.i)+Vpriorinv%*%Aprior)
        
        A_draw.i = try(A_post+t(chol(V_post))%*%rnorm(ncol(X.i)),silent=TRUE)
        if(is(A_draw.i,"try-error")) A_draw.i = matrix(MASS::mvrnorm(1,A_post,V_post),ncol(X.i),1)
        A_draw[,mm,cc] = A_draw.i[((mm-1)+1):nrow(A_draw.i),]
        if(mm>1) J_draw[mm,1:(mm-1),cc] = -A_draw.i[1:(mm-1),]
        E.c[,mm] = Y.c[,mm]-X1%*%A_draw.i
      }
      
      J_drawinv <- try(solve(J_draw[,,cc]),silent=TRUE)
      if(is(J_drawinv,"try-error")){
        counter <- counter + 1
        next
      }
      
      A_draw[,,cc] <- A_draw[,,cc] %*% t(J_drawinv)
      Em_str[[cc]] = E.c
      
      compMat <- gen_compMat(A_draw[,,cc], M, plag)$Cm
      if(max(abs(Re(eigen(compMat)$values)))<1) {
        ind <- 1
        ind_vec[cc] <- 1
      }
      counter <- counter + 1
    }
  }
  
  #----------------------------------------------------------------------------
  # Step 2: Sample shrinkage prior - 1st stage
  
  for(cc in 1:N){
    #################################
    # covariances
    V.cov = zeta_draw[,,cc]
    V.cov = V.cov[lower.tri(V.cov)]
    # Global shrinkage parameter
    lambda2_draw["cov",cc] = rgamma(n     = 1,
                                    shape = d_lambda + tau_draw["cov",cc]*v,
                                    rate  = e_lambda + 0.5*tau_draw["cov",cc]*sum(V.cov))
    # Local shrinkage parameter
    for(mm in 2:M){
      for(ii in 1:(mm-1)){
        temp = do_rgig1(lambda = tau_draw["cov",cc]-0.5, 
                        chi    = (J_draw[mm,ii,cc] - gamma_draw[mm,ii])^2,
                        psi    = tau_draw["cov",cc]*lambda2_draw["cov",cc])
        # offsetting
        zeta_draw[mm,ii,cc] = ifelse(temp<1e-8,1e-8,ifelse(temp>1e+8,1e+8,temp))
      }
    }

    ###################################
    # autoregressive coefficients
    slct.i = grep(paste0("(?=.*lag)"), rownames(A_draw), perl=TRUE)
    # Global Shrinkage Parameter
    lambda2_draw[1,cc] = rgamma(n = 1,
                                shape = d_lambda + tau_draw[1,cc]*M^2*plag,
                                rate  = e_lambda + 0.5*tau_draw[1,cc]*sum(theta_draw[slct.i,,cc]))
    # Local Shrinkage Parameter
    for(ss in slct.i){
      for(mm in 1:M){
        temp = do_rgig1(lambda = tau_draw[1,cc] - 0.5,
                        chi    = (A_draw[ss,mm,cc] - alpha_draw[ss,mm])^2,
                        psi    = tau_draw[1,cc]*lambda2_draw[1,cc])
        # offsetting
        theta_draw[ss,mm,cc] = ifelse(temp<1e-8,1e-8,ifelse(temp>1e+8,1e+8,temp))
      }
    }
  }
  
  #----------------------------------------------------------------------------
  # Step 3: Sample common means
  ind <- 0
  counter <- 1
  
  while(ind == 0 & counter < max_tries) {
    for(mm in 1:M){
      A.i = A_draw[,mm,]
      V.i = theta_draw[,mm,]
      
      aprior = a_prior[,mm,drop=FALSE] 
      vprior = kappa_draw[,mm,drop=FALSE] 
      if(mm>1){
        for(ll in (mm-1):1){
          A.i = rbind(matrix(J_draw[mm,ll,],1,N), A.i)
          V.i = rbind(matrix(zeta_draw[mm,ll,],1,N), V.i)
          
          aprior = rbind(matrix(g_prior[mm,ll],1,1), aprior) 
          vprior = rbind(matrix(iota_draw[mm,ll],1,1), vprior)
        }
      }
      vpriorinv = diag(1/c(vprior))
      
      v_post = try(chol2inv(chol(diag(apply(1/V.i,1,sum)) + vpriorinv)),silent=TRUE)
      if(is(v_post,"try-error")) v_post = try(solve(diag(apply(V.i,1,sum)) + vpriorinv),silent=TRUE)
      if(is(v_post,"try-error")) v_post = ginv(diag(apply(V.i,1,sum)) + vpriorinv)
      a_post = v_post %*% (apply(matrixcalc::hadamard.prod(1/V.i,A.i),1,sum) + vpriorinv%*%aprior)
      
      alpha_draw.i = try(a_post + t(chol(v_post))%*%rnorm(nrow(A.i)),silent=TRUE)
      if(is(alpha_draw.i,"try-error")) alpha_draw.i = matrix(MASS::mvrnorm(1,a_post,v_post),nrow(A.i),1)
      alpha_draw[,mm] = alpha_draw.i[((mm-1)+1):nrow(alpha_draw.i),]
      if(mm>1) gamma_draw[mm,1:(mm-1)] = alpha_draw.i[1:(mm-1),]
    }
    
    gamma_inv <- try(solve(gamma_draw),silent=TRUE)
    if(is(gamma_inv,"try-error")){
      counter <- counter + 1
      next
    }
    alpha_draw <- alpha_draw %*% t(gamma_inv)
    
    compMat <- gen_compMat(alpha_draw, M, plag)$Cm
    if(max(abs(Re(eigen(compMat)$values)))<1) ind <- 1
    counter <- counter + 1
  }
  

  #----------------------------------------------------------------------------
  # # Step 4: Sample shrinkage prior - 2nd stage
  
  ###################################
  # covariances
  v.cov = iota_draw[lower.tri(iota_draw)]
  # Global Shrinkage Parameter
  delta2_draw["cov",1] = rgamma(n = 1,
                                shape = d_lambda + phi_draw["cov",1]*v,
                                rate  = e_lambda + 0.5*phi_draw["cov",1]*sum(v.cov))
  # Local Shrinkage Parameter
  for(ii in 2:M){
    for(mm in 1:(ii-1)){
      temp = do_rgig1(lambda = phi_draw["cov",1] - 0.5,
                      chi    = (gamma_draw[ii,mm] - g_prior[ii,mm])^2, 
                      psi    = phi_draw["cov",1]*delta2_draw["cov",1])
      # offsetting
      iota_draw[ii,mm] = ifelse(temp<1e-8,1e-8,ifelse(temp>1e+8,1e+8,temp))
    }
  }
  ###################################
  # autoregressive coefficients
  slct.i = grep(paste0("(?=.*lag)"), rownames(alpha_draw), perl=TRUE)
  # Global Shrinkage Parameter
  delta2_draw[1,1] = rgamma(n = 1,
                            shape = d_lambda + phi_draw[1,1]*M^2*plag,
                            rate  = e_lambda + 0.5*phi_draw[1,1]*sum(kappa_draw[slct.i,]))
  # Local Shrinkage Parameter
  for(ss in slct.i){
    for(mm in 1:M){
      temp = do_rgig1(lambda = phi_draw[1,1] - 0.5,
                      chi    = (alpha_draw[ss,mm] - a_prior[ss,mm])^2, 
                      psi    = phi_draw[1,1]*delta2_draw[1,1])
      # offsetting
      kappa_draw[ss,mm] = ifelse(temp<1e-8,1e-8,ifelse(temp>1e+8,1e+8,temp))
    }
  }
  
  
  #----------------------------------------------------------------------------
  # Step 5: Sample variances
  for (cc in 1:N){
      for(mm in 1:M){
        S_1 <- c0 + 0.5 * bigT
        S_2 <- d0 + 0.5 * crossprod(Em_str[[cc]][,mm])
        sig_eta <- 1/rgamma(1,S_1,S_2)
        Sv_draw[[cc]][,mm] <- log(sig_eta)
    }
  }

  #----------------------------------------------------------------------------
  # Step 6: Store everything needed
  if(irep %in% thin.draws){
    count <- count+1
    A_store[count,,,] <- A_draw 
    alpha_store[count,,] <- alpha_draw
    J_store[count,,,] <- J_draw 
    gamma_store[count,,] <- gamma_draw
    for(cc in 1:N) Sv_store[[cc]][count,,] <- Sv_draw[[cc]]
    theta_store[count,,,] <- theta_draw
    zeta_store[count,,,] <- zeta_draw
    lambda2_store[count,,] <- lambda2_draw
    kappa_store[count,,] <- kappa_draw
    iota_store[count,,] <- iota_draw
    delta2_store[count,] <- delta2_draw
    ind_store[count, ] <- ind_vec
  }
  
  if(irep %% 50 == 0) {
    cat(paste0("Current draw: ", irep, "/", ntot, ".\n"))
  }
}







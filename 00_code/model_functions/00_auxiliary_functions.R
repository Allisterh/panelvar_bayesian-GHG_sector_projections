#####
# Function for generating companion matrix

gen_compMat <- function(A, M, p){
  Jm          <- matrix(0, M*p, M)
  Jm[1:M,1:M] <- diag(M)
  
  Cm  <- matrix(0, M*p, M*p)
  if(p==1) Cm <- t(A[1:(M*p),]) else {
    for(j in 1:(p-1)){
      Cm[(j*M+1):(M*(j+1)),(M*(j-1)+1):(j*M)] <- diag(M)
    }
  }
  bbtemp <- A[1:(M*p),]
  splace <- 0
  for(ii in 1:p){
    for(iii in 1:M) {
      Cm[iii,((ii-1)*M+1):(ii*M)] <- t(bbtemp[(splace+1):(splace+M),iii])
    }
    splace <- splace+M
  }
  return(list(Cm=Cm,
              Jm=Jm))
}

#####
# Function for transforming matrix to list, including some input checks

matrix_to_list <- function(datamat, time.frame = NULL, check.length = TRUE){
  
  if(!all(grepl("\\.",colnames(datamat)))){
    stop("Please separate country- and variable names with a point.")
  }
  cN <- unique(unlist(lapply(strsplit(colnames(datamat),".",fixed=TRUE),function(l) l[1])))
  N  <- length(cN)
  if(!all(nchar(cN)>1)){
    stop("Please provide entity names with minimal two characters.")
  }
  datalist <- list()
  
  # check for NAs other than leading/lagging ones.
  for(cc in 1:N){
    temp <- datamat[,grepl(cN[cc],substr(colnames(datamat), 1, nchar(cN))),drop=FALSE]
    temp[abs(temp) == Inf] <- NA
    if(check.length) {
      temp <- dplyr::filter_all(temp, 
                                dplyr::all_vars(pmin(cumsum(!is.na(.)), 
                                                     rev(cumsum(!is.na(rev(.))))) != 0))
      if(nrow(temp) < 10) {
        print(paste0("Data of ", cN[cc], " contains only ", nrow(temp), 
                     "observations. Will be skipped."))
        next
        if(any(is.na(temp))) {
          stop(paste0("Data of '", cN[cc], 
                      "' contains non-leading/lagging NAs. Please check the data."))
        }
      }
    }
    datalist[[cN[cc]]] <- as.matrix(temp)
    colnames(datalist[[cN[cc]]]) <- unlist(lapply(
      strsplit(colnames(datalist[[cN[cc]]]), "." , fixed=TRUE), 
      function (l) l[2]))
    rownames(datalist[[cN[cc]]]) <- time.frame[
      (length(time.frame) - nrow(datalist[[cN[cc]]]) + 1):length(time.frame)]
  }
  return(datalist)
}


#####
# Function for finding maximums in emission intensity
# Adopted from Raftery et al (2017) [https://doi.org/10.1038/nclimate3352]
# Code to be found here: https://github.com/PPgp/CO2projections

find_maxes <- function(x, list.shorten = NULL, variable = c("level", "intensity"), 
                       excl.countries=NULL) {
  # find_maxes finds when each country peaks in GHG intensity.
  # If the peak for a country is within the last 13 years of the data,
  # we list that country under rejects.late and don't report a peak
  max.vals <- list()
  max.vals[["iso3c"]] <- c()
  max.vals[["year"]] <- c()
  rejects.late <- list()
  rejects.late[["iso3c"]] <- c()
  rejects.late[["year"]] <- c()
  
  if(variable == "level") {
    var.slct <- "CO2"
  } else {
    var.slct <- "CO2GDPPPP"
  }
  
  form_var <- as.formula(paste0(var.slct, "~year"))
  
  isolist <- unique(x$iso3c)
  
  for (iso in isolist) {
    data.tmp <- x[x$iso3c == iso,]
    data.tmp <- data.tmp[!is.na(data.tmp[,var.slct]),]
    if(nrow(data.tmp) < 10) next
    if(iso %in% list.shorten) {
      pos <- which(data.tmp$year == 1995)
      if(length(pos) > 0) {
        data.tmp <- data.tmp[pos:nrow(data.tmp), ]
      }
    }
    
    model.smoothed <- loess(form_var, data=data.tmp, span=0.25)
    predicted <- predict(model.smoothed)
    max.ind <- which(predicted == max(predicted))
    stopifnot(length(max.ind) == 1)
    
    # Record unsmoothed observations at this point, if we're not near
    # the start or end of the period
    year.last <- max(x$year) # Allow for out-of-sample validation
    year.last.cutoff <- min(2005, year.last - 13)
    if (data.tmp$year[max.ind] <= year.last.cutoff & !iso %in% excl.countries) {
      max.vals[["iso3c"]] <- c(max.vals[["iso3c"]], iso)
      max.vals[["year"]] <- c(max.vals[["year"]], data.tmp$year[max.ind])
    } else {
      rejects.late[["iso3c"]] <- c(rejects.late[["iso3c"]], iso)
      rejects.late[["year"]] <- c(rejects.late[["year"]], data.tmp$year[1])
    }
  }
  
  print(paste0("Rejects because peak was after ", min(2005, year.last - 13)))
  print(rejects.late$iso3c)
  print(length(rejects.late$iso3c))
  
  list(max.vals, rejects.late)
}


#####
# Function for lagging data matrix

mlag <- function(X,lag,cons=FALSE){
  p <- lag
  X <- as.matrix(X)
  Traw <- nrow(X)
  N <- ncol(X)
  Xlag <- matrix(0,Traw,p*N)
  for (ii in 1:p){
    Xlag[(p+1):Traw,(N*(ii-1)+1):(N*ii)] <- X[(p+1-ii):(Traw-ii),(1:N)]
  }
  colnames(Xlag) <- paste0(colnames(X),".lag",rep(seq(p),each=N))
  if(cons){
    Xlag <- cbind(Xlag,1)
    colnames(Xlag)[ncol(Xlag)] <- "cons"
  }
  return(Xlag)
}


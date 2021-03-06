# Author: Jean-Philippe Fortin, fortin946@gmail.com
# This is a modification of the ComBat function code from the sva package that can be found at
# https://bioconductor.org/packages/release/bioc/html/sva.html 
# The original and present code is under the Artistic License 2.0.
# If using this code, make sure you agree and accept this license. 
# Code optimization improved by Richard Beare 

# source("utils.R")
# set.seed(10)
# nrow <- 200
# ncol <- 10
# data <- matrix(rnorm(nrow*ncol), nrow, ncol)
# data[,c(1,3,5,7,9)] <- data[,c(1,3,5,7,9)]+3
# data[,6:10] <- (data[,6:10]+5)*1.5
# batch = c(1,1,1,1,1,2,2,2,2,2)
# pheno <- rep(0, ncol(data))
# pheno[c(1,3,5,7,9)] <- 1
# mod=model.matrix(~pheno)
# dat=data
# eb=TRUE
# verbose=TRUE
# parametric=TRUE


combat <- function(dat, batch, mod=NULL, eb=TRUE, verbose=TRUE, parametric=TRUE){
  dat <- as.matrix(dat)
  .checkConstantRows(dat)
  .checkNARows(dat)

  if (eb){
      if (verbose) cat("[combat] Performing ComBat with empirical Bayes\n")
  } else {
      if (verbose) cat("[combat] Performing ComBat without empirical Bayes (L/S model)\n")
  }
  # make batch a factor and make a set of indicators for batch
  batch <- as.factor(batch)
  batchmod <- model.matrix(~-1+batch)  
  if (verbose) cat("[combat] Found",nlevels(batch),'batches\n')
  
  # A few other characteristics on the batches
  n.batch <- nlevels(batch)
  batches <- lapply(levels(batch), function(x)which(batch==x))
  n.batches <- sapply(batches, length)
  n.array <- sum(n.batches)
  #combine batch variable and covariates
  design <- cbind(batchmod,mod)
  # check for intercept in covariates, and drop if present
  check <- apply(design, 2, function(x) all(x == 1))
  design <- as.matrix(design[,!check])
  
  # Number of covariates or covariate levels
  if (verbose) cat("[combat] Adjusting for",ncol(design)-ncol(batchmod),'covariate(s) or covariate level(s)\n')
  
  # Check if the design is confounded
  if(qr(design)$rank<ncol(design)){
    if(ncol(design)==(n.batch+1)){
      stop("[combat] The covariate is confounded with batch. Remove the covariate and rerun ComBat.")
    }
    if(ncol(design)>(n.batch+1)){
      if((qr(design[,-c(1:n.batch)])$rank<ncol(design[,-c(1:n.batch)]))){
        stop('The covariates are confounded. Please remove one or more of the covariates so the design is not confounded.')
      } else {
        stop("At least one covariate is confounded with batch. Please remove confounded covariates and rerun ComBat.")
      }
    }
  }
    

  ## Check for missing values
  hasNAs <- any(is.na(dat))
  if (hasNAs & verbose){
    cat(paste0("[combat] Found ", sum(is.na(dat)), " missing data values. \n"))
  }
  
  ##Standardize Data across features
  if (verbose) cat('[combat] Standardizing Data across features\n')
  if (!hasNAs){
    B.hat <- solve(crossprod(design))
    B.hat <- tcrossprod(B.hat, design)
    B.hat <- tcrossprod(B.hat, dat)
  } else {
    B.hat <- apply(dat, 1, .betaNA, design)
  }
  

  #Standarization Model
  grand.mean <- crossprod(n.batches/n.array, B.hat[1:n.batch,])
  stand.mean <- crossprod(grand.mean, t(rep(1,n.array)))
  if (!hasNAs){
    factors <- (n.array/(n.array-1))
    var.pooled <- rowVars(dat-t(design %*% B.hat), na.rm=TRUE)/factors
  } else {
    ns <- rowSums(!is.na(dat))
    factors <- (ns/(ns-1))
    var.pooled <- rowVars(dat-t(design %*% B.hat), na.rm=TRUE)/factors
  }
  
  
  if(!is.null(design)){
    tmp <- design;tmp[,c(1:n.batch)] <- 0
    stand.mean <- stand.mean+t(tmp%*%B.hat)
  }	
  s.data <- (dat-stand.mean)/(tcrossprod(sqrt(var.pooled), rep(1,n.array)))
  
  ##Get regression batch effect parameters
  if (eb){
      if (verbose) cat("[combat] Fitting L/S model and finding priors\n")
  } else {
      if (verbose) cat("[combat] Fitting L/S model\n")
  }
  batch.design <- design[,1:n.batch]
  if (!hasNAs){
      gamma.hat <- tcrossprod(solve(crossprod(batch.design, batch.design)), batch.design)
      gamma.hat <- tcrossprod(gamma.hat, s.data)
  } else{
      gamma.hat <- apply(s.data, 1, .betaNA, batch.design) 
  }
  
  delta.hat <- NULL
  for (i in batches){
    delta.hat <- rbind(delta.hat,rowVars(s.data, cols=i, na.rm=TRUE))
  }

  # Empirical Bayes correction:
  gamma.star <- delta.star <- NULL
  gamma.bar <- t2 <- a.prior <- b.prior <- NULL
  if (eb){
      ##Find Priors
      #gamma.bar <- apply(gamma.hat, 1, mean)
      #t2 <- apply(gamma.hat, 1, var)
      gamma.bar <- rowMeans(gamma.hat)
      t2 <- rowVars(gamma.hat)
      a.prior <- apriorMat(delta.hat)
      b.prior <- bpriorMat(delta.hat)
      
      ##Find EB batch adjustments
      if (parametric){
        if (verbose) cat("[combat] Finding parametric adjustments\n")
        for (i in 1:n.batch){
            temp <- it.sol(s.data[,batches[[i]]],gamma.hat[i,],delta.hat[i,],gamma.bar[i],t2[i],a.prior[i],b.prior[i])
            gamma.star <- rbind(gamma.star,temp[1,])
            delta.star <- rbind(delta.star,temp[2,])
        }
      } else {
        if (verbose) cat("[combat] Finding non-parametric adjustments\n")
        for (i in 1:n.batch){
            temp <- int.eprior(as.matrix(s.data[, batches[[i]]]),gamma.hat[i,], delta.hat[i,])
            gamma.star <- rbind(gamma.star,temp[1,])
            delta.star <- rbind(delta.star,temp[2,])
        }
      }
      
  } 

  ### Normalize the Data ###
  if (verbose) cat("[combat] Adjusting the Data\n")
  bayesdata <- s.data
  j <- 1
  for (i in batches){
    if (eb){
        bayesdata[,i] <- (bayesdata[,i]-t(batch.design[i,]%*%gamma.star))/tcrossprod(sqrt(delta.star[j,]), rep(1,n.batches[j]))
    } else {
        bayesdata[,i] <- (bayesdata[,i]-t(batch.design[i,]%*%gamma.hat))/tcrossprod(sqrt(delta.hat[j,]), rep(1,n.batches[j]))
    }
    j <- j+1
  }
  
  bayesdata <- (bayesdata*(tcrossprod(sqrt(var.pooled), rep(1,n.array))))+stand.mean
  return(list(dat.combat=bayesdata, 
    gamma.hat=gamma.hat, delta.hat=delta.hat, 
    gamma.star=gamma.star, delta.star=delta.star, 
    gamma.bar=gamma.bar, t2=t2, a.prior=a.prior, b.prior=b.prior, batch=batch, mod=mod, 
    stand.mean=stand.mean, stand.sd=sqrt(var.pooled))
  )
}
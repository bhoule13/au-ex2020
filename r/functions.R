
# for the a0 in the first year of life 
Calculate_a0 <- function(m0,sex) {
  #Andreev-Kingkade formulas for computing a0 given m0
  # HMD full protocol Table 1 pg37
  #Males
  if(sex=="m"){
    if (m0<0.02300) {a0<-0.14929-(1.99545*m0)}
    if ((0.0230<= m0)&(m0<0.08307)) {a0<-0.02832+(3.26021*m0)}
    if (0.08307<= m0) {a0<-0.29915}
  }
  if (sex=="f"){
    #Females
    if (m0<0.01724) {a0<-0.14903-(2.05527*m0)}
    if ((0.01724 <= m0)&(m0< 0.06891)) {a0<-0.04667+(3.88089*m0)}
    if (0.06891<= m0) {a0<-0.31411}
  }
  return(a0) }


lifetable.mx<-function(mx,sex){
  
  N<-length(mx)
  AgeI<-rep(1,N)
  a0<-Calculate_a0(mx[1],sex)
  ax<-c(a0,rep(0.5,(N-1)))
  if(mx[N]>0){ax[N]<-1/mx[N]}
  qx<-mx/(1+(1-ax)*mx)
  qx[N]<-1             
  
  px<-1-qx
  
  lx<-1
  
  for(y in 1:(N-1)){          
    lx[y+1]<-lx[y]*px[y]
  }
  
  dx<-lx*qx
  dx[N]<-lx[N]
  
  Lx<-lx+(ax-AgeI)*dx
  Lx[N]<-lx[N]*ax[N]                 
  
  Tx<-c()
  for(y in 1:N){
    Tx[y]<-sum(Lx[y:N])
  }
  
  ex<-Tx/lx
  Age<-0:(N-1) 
  AgeI<-rep(1,N)
  ALL<-cbind(Age,AgeI,ax,mx,qx,lx,dx,Lx,Tx,ex)
  return(ALL)
}


Kannisto<-function(mx){
  names(mx) <- 0:100
  mx1<-kannisto(mx, est.ages = seq(85, 100), proj.ages = seq(85, 110))
  return(mx1)}


RxiMatrix<-function(PP,Cum){
  #PP<-B
  NumC<-dim(PP)[2]
  NumR<-dim(PP)[1]
  
  G<-PP
  FD<-colSums(PP)
  
  if (Cum==1){
    G<-t(apply(G,1,cumsum))
    FD<-cumsum(FD)
  } 
  
  FRx3<-t(matrix(rep(FD,(NumR)),NumC))
  FRx<-G/FRx3
  FRx[is.infinite(FRx)]<-0
  return(FRx)
}


AgeDecomp<-function(LT1,LT2){
  N<-dim(LT1)[1]
  
  lx1<-LT1[,6]
  lx2<-LT2[,6]
  Lx1<-LT1[,8]
  Lx2<-LT2[,8] 
  Tx1<-LT1[,9]
  Tx2<-LT2[,9] 

  AgeD<-lx1[-N]*((Lx2[-N]/lx2[-N])-(Lx1[-N]/lx1[-N]))+Tx2[-1]*((lx1[-N]/lx2[-N])-(lx1[-1]/lx2[-1]))
  AgeD<-c(AgeD,lx1[N]*((Tx2[N]/lx2[N])-(Tx1[N]/lx1[N])))
  return(AgeD)
  }


CauseDecomp<-function(LT1,LT2,ICD1,ICD2,SEX){
  a<-AgeDecomp(LT1,LT2)
  AgeGD<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  
  a<-LT1[,7]
  dx1<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  a<-LT1[,8]
  Lx1<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  mx1<-dx1/Lx1
  
  a<-LT2[,7]
  dx2<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  a<-LT2[,8]
  Lx2<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  mx2<-dx2/Lx2
  
  ICD1f<-ICD1[ICD1$Males==SEX,-1]
  R1<-ICD1f[-c(1,13),-c(1,2)]/matrix(rep(rowSums(ICD1f[-c(1,13),-c(1,2)]),6),11)
  ICD2f<-ICD2[ICD2$Males==SEX,-1]
  R2<-ICD2f[-c(1,13),-c(1,2)]/matrix(rep(rowSums(ICD2f[-c(1,13),-c(1,2)]),6),11)
  
  CauseD<-AgeGD*(R2*mx2-R1*mx1)/(mx2-mx1)
  return(colSums(CauseD))}
CauseDecomp2<-function(LT1,LT2,ICD1,ICD2){
  a<-AgeDecomp(LT1,LT2)
  AgeGD<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  
  a<-LT1[,7]
  dx1<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  a<-LT1[,8]
  Lx1<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  mx1<-dx1/Lx1
  
  a<-LT2[,7]
  dx2<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  a<-LT2[,8]
  Lx2<-c(a[1],sum(a[2:15]),sum(a[16:25]),sum(a[26:35]),sum(a[36:45]),sum(a[46:55]),sum(a[56:65]),sum(a[66:75]),sum(a[76:85]),sum(a[86:95]),sum(a[96:111]))
  mx2<-dx2/Lx2
  
  
  R1<-ICD1
  R2<-ICD2
  
  CauseD<-AgeGD*(R2*mx2-R1*mx1)/(mx2-mx1)
  return(colSums(CauseD))}

# functions to ungroup 
pclm <- function(y, C, X, lambda = 1, deg = 2, show = F){
  
  # Fit a PCLM (estimate b in ) E(y) = C %*% exp(X %*% b)
  
  # y = the vector of observed counts of length i
  
  # C = the composition matrix of dimension IxJ
  
  # X = the identity matrix of dimension JxJ; or B-spline basis
  
  # lambda = smoothing parameter
  
  # deg = order of differences of the components of b
  
  # show = indicates whether iteration details should be shown
  
  # Fit the penalized composite link model
  
  # Some preparations
  
  nx <- dim(X)[2]
  
  D <- diff(diag(nx), diff=deg)
  
  la2 <- sqrt(lambda)
  
  it <- 0
  
  bstart <- log(sum(y) / nx);
  
  b <- rep(bstart, nx);
  
  # Perform the iterations
  
  for (it in 1:50) {
    
    b0 <- b
    
    eta <- X %*% b
    
    gam <- exp(eta)
    
    mu <- C %*% gam
    
    w <- c(1 / mu, rep(la2, nx - deg))
    
    Gam <- gam %*% rep(1, nx)
    
    Q <- C %*% (Gam * X)
    
    z <- c(y - mu + Q %*% b, rep(0, nx - deg))
    
    Fit <- lsfit(rbind(Q, D), z, wt = w, intercept = F)
    
    b <- Fit$coef
    
    db <- max(abs(b - b0))
    
    if (show) cat(it, " ", db, "\n")
    
    if (db < 1e-6) break
    
  }
  
  cat(it, " ", db, "\n")
  
  # Regression diagnostic
  
  R <- t(Q) %*% diag(c(1 / mu)) %*% Q
  
  H <- solve(R + lambda * t(D) %*% D) %*% R
  
  fit <- list()
  
  fit$trace <- sum(diag(H))
  
  ok <- y > 0 & mu > 0
  
  fit$dev <- 2 * sum(y[ok] * log(y[ok] / mu[ok]))
  
  fit$gamma <- gam
  
  fit$aic<- fit$dev + 2 * fit$trace
  
  fit$mu <- mu
  
  fit
  
}
UnGroup<-function(y){
  #### now ungrouping deaths age 100+ 
  ##  y<-D2020m
  
  x<-c(0:110)
  
  # Make C matrix and (trivial) basis B
  n<-length(y)
  m<-length(x)
  C1<-diag(1,100,100)
  C <- matrix(0, n, m)
  
  C[1:100, 1:100]<-C1
  C[101, 101:111] <- 1
  
  B <- diag(m)
  lambda <- 10^7
  mod <- pclm(y, C, B,lambda = lambda, deg = 2)
  
  DX<-as.numeric(as.character(c(y[1:100],y[101]*(mod$gamma[101:111]/sum(mod$gamma[101:111])))))
  return(DX)}

# CI for e0  
LTci <- function(Dx,mx,sex) {
  # Dx<-UnGroup(D2020m)[1:111]
  # mx<-R2020m
  # sex<-"m"
  
  m <- length(mx)
  Ntil <- round(Dx/mx)
  Y <- suppressWarnings(matrix(rbinom(m * 1000,
                                      Ntil,
                                      mx),
                               m, 1000))
  MX <- Y/Ntil
  fun.ex <- function(mx) {
    return(lifetable.mx(mx,sex)[1,10])
  }
 
  exsim.ex <- apply(MX, 2, fun.ex)
 
  ## confidence interval
  CI.ex <- quantile(exsim.ex,
                    probs = c((1-0.95)/2,0.5,
                              1 - (1-0.95)/2))
 # output
  out <- data.frame(
    #meanex=mean(exsim),
    CIex=CI.ex
     # exsim=exsim
  )
  return(out)
}

# CI for differences in e0
LTci2 <- function(Dx,mx,Dx2,mx2,sex) {
  # Dx<-UnGroup(D2020m)[1:111]
  # mx<-R2020m
  # Dx2<-UnGroup(D2019m)[1:111]
  # mx2<-R2019m
  # sex<-"m"
  
  m <- length(mx)
  Ntil <- round(Dx/mx)
  Y <- suppressWarnings(matrix(rbinom(m * 1000,
                                      Ntil,
                                      mx),
                               m, 1000))
  MX <- Y/Ntil
  
  Ntil2 <- round(Dx2/mx2)
  Y2 <- suppressWarnings(matrix(rbinom(m * 1000,
                                      Ntil2,
                                      mx2),
                               m, 1000))
  MX2 <- Y2/Ntil2
  
  fun.ex <- function(mx) {
    return(lifetable.mx(mx,sex)[1,10])
  }
  
  exsim.ex <- apply(MX, 2, fun.ex)
  exsim.ex2 <- apply(MX2, 2, fun.ex)
  
  ## confidence interval
  CI.ex <- quantile((exsim.ex-exsim.ex2),
                    probs = c((1-0.95)/2,0.5,
                              1 - (1-0.95)/2))
  # output
  out <- data.frame(
    #meanex=mean(exsim),
    CIex=CI.ex
    # exsim=exsim
  )
  return(out)
}

# CI for e0  
LTcie60 <- function(Dx,mx,sex) {
  # Dx<-UnGroup(D2020m)[1:111]
  # mx<-R2020m
  # sex<-"m"
  
  m <- length(mx)
  Ntil <- round(Dx/mx)
  Y <- suppressWarnings(matrix(rbinom(m * 1000,
                                      Ntil,
                                      mx),
                               m, 1000))
  MX <- Y/Ntil
  fun.ex <- function(mx) {
    return(lifetable.mx(mx,sex)[61,10])
  }
  
  exsim.ex <- apply(MX, 2, fun.ex)
  
  ## confidence interval
  CI.ex <- quantile(exsim.ex,
                    probs = c((1-0.95)/2,0.5,
                              1 - (1-0.95)/2))
  # output
  out <- data.frame(
    #meanex=mean(exsim),
    CIex=CI.ex
    # exsim=exsim
  )
  return(out)
}

# CI for differences in e60
LTci2b <- function(Dx,mx,Dx2,mx2,sex) {
  # Dx<-UnGroup(D2020m)[1:111]
  # mx<-R2020m
  # Dx2<-UnGroup(D2019m)[1:111]
  # mx2<-R2019m
  # sex<-"m"
  
  m <- length(mx)
  Ntil <- round(Dx/mx)
  Y <- suppressWarnings(matrix(rbinom(m * 1000,
                                      Ntil,
                                      mx),
                               m, 1000))
  MX <- Y/Ntil
  
  Ntil2 <- round(Dx2/mx2)
  Y2 <- suppressWarnings(matrix(rbinom(m * 1000,
                                       Ntil2,
                                       mx2),
                                m, 1000))
  MX2 <- Y2/Ntil2
  
  fun.ex <- function(mx) {
    return(lifetable.mx(mx,sex)[61,10])
  }
  
  exsim.ex <- apply(MX, 2, fun.ex)
  exsim.ex2 <- apply(MX2, 2, fun.ex)
  
  ## confidence interval
  CI.ex <- quantile((exsim.ex-exsim.ex2),
                    probs = c((1-0.95)/2,0.5,
                              1 - (1-0.95)/2))
  # output
  out <- data.frame(
    #meanex=mean(exsim),
    CIex=CI.ex
    # exsim=exsim
  )
  return(out)
}

# CI for Age-decomp in e0
LTci2c <- function(Dx,mx,Dx2,mx2,sex) {
  # Dx<-UnGroup(D2019m)[1:111]
  # mx<-R2019m
  # Dx2<-UnGroup(D2020m)[1:111]
  # mx2<-R2020m
  # sex<-"m"
  
  m <- length(mx)
  Ntil <- round(Dx/mx)
  Y <- suppressWarnings(matrix(rbinom(m * 1000,
                                      Ntil,
                                      mx),
                               m, 1000))
  MX <- Y/Ntil
  
  Ntil2 <- round(Dx2/mx2)
  Y2 <- suppressWarnings(matrix(rbinom(m * 1000,
                                       Ntil2,
                                       mx2),
                                m, 1000))
  MX2 <- Y2/Ntil2
  
  fun.AD <- function(mxA,mxB) {
    A<-AgeDecomp(lifetable.mx(mxA,sex),lifetable.mx(mxB,sex))
    return(c(sum(A[1:60]),sum(A[61:80]),sum(A[81:111])))
  }
  
  MXA<-lapply(seq_len(ncol(MX)), function(x) MX[,x])
  
  MXB<-lapply(seq_len(ncol(MX2)), function(x) MX2[,x])
  
  exsim.AD <-mapply(fun.AD, mxA=MXA, mxB=MXB)
  
  Mat <- matrix(unlist(exsim.AD), 3)
  
  
  ## confidence interval
 CIlyl1<-c()
  for (i in 1:3){
    CI.lyli3 <- quantile(Mat[i,],
                         probs = c((1-0.95)/2,.5,
                                   1 - (1-0.95)/2),na.rm = TRUE)
    CIlyl1<-rbind(CIlyl1,CI.lyli3)} 
  # output
  out <- data.frame(
    #meanex=mean(exsim),
    CIex=CIlyl1
    # exsim=exsim
  )
  return(out)
}

# CI for Cause-decomp in e0
LTci2d <- function(Dx,mx,B,Dx2,mx2,B2,sex) {
  # Dx<-UnGroup(DD2019m)[1:111]
  # mx<-R2019m
  # Dx2<-UnGroup(D2020m)[1:111]
  # mx2<-R2020m
  # sex<-"m"
  # B<-
  # B2<-
 
  SEX<-c()
  SEX[sex=="m"]<-"Males"
  SEX[sex=="f"]<-"Females"
  
  b<-B[B$Males==SEX,-1]
  mxi<-b[-c(1,13),-c(1,2)]/matrix(rep(rowSums(b[-c(1,13),-c(1,2)]),6),11)
  b2<-B2[B2$Males==SEX,-1]
  mxi2<-b2[-c(1,13),-c(1,2)]/matrix(rep(rowSums(b2[-c(1,13),-c(1,2)]),6),11)
  
  mxi[is.na(mxi)]<-0
  mxi2[is.na(mxi2)]<-0
  Nmxi<-dim(mxi)[2]  # causes of death
  Ncol<-dim(mxi)[1]
  
  m <- length(mx)
  Ntil <- round(Dx/mx)
  Y <- suppressWarnings(matrix(rbinom(m * 10000,
                                      Ntil,
                                      mx),
                               m, 1000))
  MX <- Y/Ntil
  
  Ntil2 <- round(Dx2/mx2)
  Y2 <- suppressWarnings(matrix(rbinom(m * 10000,
                                       Ntil2,
                                       mx2),
                                m, 1000))
  MX2 <- Y2/Ntil2
  
  #rmultinom
  BM<-list() 
  BMa<-BM
  BM2<-BM
  BMa2<-BM
  n=1000
  
  for (t in 1:Ncol){
    xx<-rmultinom(n, size =10000, prob = mxi[t,])/10000
    BM[[t]]<-lapply(seq_len(ncol(xx)), function(i) xx[,i])
    xx2<-rmultinom(n, size =10000, prob = mxi2[t,])/10000
    BM2[[t]]<-lapply(seq_len(ncol(xx2)), function(i) xx2[,i])
  }
  BMa<- Map(rbind,BM[[1]])
  BMa2<- Map(rbind,BM2[[1]])
  for (tt in 2:Ncol){
    BMa<- Map(rbind,BMa,BM[[tt]])   
    BMa2<- Map(rbind,BMa2,BM2[[tt]])
  }
  
  fun.CD <- function(mxA,mxB,BB1,BB2) {
    return(CauseDecomp2(lifetable.mx(mxA,sex),lifetable.mx(mxB,sex),BB1,BB2))
    }
  
  MXA<-lapply(seq_len(ncol(MX)), function(x) MX[,x])
  
  MXB<-lapply(seq_len(ncol(MX2)), function(x) MX2[,x])
  
  exsim.CD <-mapply(fun.CD, mxA=MXA, mxB=MXB,BB1=BMa,BB2=BMa2)
  
  Mat <- matrix(unlist(exsim.CD), 6)
  
  
  ## confidence interval
  CIlyl1<-c()
  for (i in 1:6){
    CI.lyli3 <- quantile(Mat[i,],
                         probs = c((1-0.95)/2,.5,
                                   1 - (1-0.95)/2),na.rm = TRUE)
    CIlyl1<-rbind(CIlyl1,CI.lyli3)} 
  # output
  out <- data.frame(
    #meanex=mean(exsim),
    CIex=CIlyl1
    # exsim=exsim
  )
  return(out)
}

# CI for Cause-decomp in e0
LTci2e <- function(Dx,mx,B,Dx2,mx2,B2,sex) {
  # Dx<-UnGroup(DD2019m)[1:111]
  # mx<-R2019m
  # Dx2<-UnGroup(D2020m)[1:111]
  # mx2<-R2020m
  # sex<-"m"
  # B<-IICD19m
  # B2<-CoDABS(CoD20m)
  
  SEX<-c()
  SEX[sex=="m"]<-"Males"
  SEX[sex=="f"]<-"Females"
  
  mxi<-B
  mxi2<-B2
  
  mxi[is.na(mxi)]<-0
  mxi2[is.na(mxi2)]<-0
  Nmxi<-dim(mxi)[2]  # causes of death
  Ncol<-dim(mxi)[1]
  
  m <- length(mx)
  Ntil <- round(Dx/mx)
  Y <- suppressWarnings(matrix(rbinom(m * 10000,
                                      Ntil,
                                      mx),
                               m, 1000))
  MX <- Y/Ntil
  
  Ntil2 <- round(Dx2/mx2)
  Y2 <- suppressWarnings(matrix(rbinom(m * 10000,
                                       Ntil2,
                                       mx2),
                                m, 1000))
  MX2 <- Y2/Ntil2
  
  #rmultinom
  BM<-list() 
  BMa<-BM
  BM2<-BM
  BMa2<-BM
  n=1000
  
  for (t in 1:Ncol){
    xx<-rmultinom(n, size =10000, prob = mxi[t,])/10000
    BM[[t]]<-lapply(seq_len(ncol(xx)), function(i) xx[,i])
    xx2<-rmultinom(n, size =10000, prob = mxi2[t,])/10000
    BM2[[t]]<-lapply(seq_len(ncol(xx2)), function(i) xx2[,i])
  }
  BMa<- Map(rbind,BM[[1]])
  BMa2<- Map(rbind,BM2[[1]])
  for (tt in 2:Ncol){
    BMa<- Map(rbind,BMa,BM[[tt]])   
    BMa2<- Map(rbind,BMa2,BM2[[tt]])
  }
  
  fun.CD <- function(mxA,mxB,BB1,BB2) {
    return(CauseDecomp2(lifetable.mx(mxA,sex),lifetable.mx(mxB,sex),BB1,BB2))
  }
  
  MXA<-lapply(seq_len(ncol(MX)), function(x) MX[,x])
  
  MXB<-lapply(seq_len(ncol(MX2)), function(x) MX2[,x])
  
  exsim.CD <-mapply(fun.CD, mxA=MXA, mxB=MXB,BB1=BMa,BB2=BMa2)
  
  Mat <- matrix(unlist(exsim.CD), 6)
  
  
  ## confidence interval
  CIlyl1<-c()
  for (i in 1:6){
    CI.lyli3 <- quantile(Mat[i,],
                         probs = c((1-0.95)/2,.5,
                                   1 - (1-0.95)/2),na.rm = TRUE)
    CIlyl1<-rbind(CIlyl1,CI.lyli3)} 
  # output
  out <- data.frame(
    #meanex=mean(exsim),
    CIex=CIlyl1
    # exsim=exsim
  )
  return(out)
}
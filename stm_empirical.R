
library(tidyr)

win_data <- function(data, alpha) {
  n <- length(data)
  r <- floor(n*alpha) + 1
  s <- n-floor(n*alpha)
  
  data.ord <- sort(data)
  data_win    <- c(rep(data.ord[r], r-1), data.ord[r:s], rep(data.ord[s], r-1))
  data_win
}

J_fun      <- function(u, alpha, gamma) {
  w <- 0
  if (u < alpha) {w <- 0}
  else if (u <= gamma) {w <- (u-alpha)/(gamma-alpha)}
  else if (u <= 1 - gamma) {w <- 1}
  else if (u <= 1 - alpha) {w <- (1-alpha-u)/(gamma-alpha)}
  else {w <- 0}
  w
}

ST_mean    <- function(data, alpha, gamma) {
  n <- length(data)
  weights <- sapply(seq(1:n)/(n+1), J_fun, alpha, gamma)
  m <- sum(weights!=0)
  w.norm <- weights*m/sum(weights)
  stmean <- sum(sort(data)*w.norm)/m
  stmean
}

stmeanvar.asym  <- function(data, alpha, gamma) {
  
  data_sort <- sort(data)
  n <- length(data)
  r  <- floor(alpha*n)
  m  <- floor(gamma*n)
  
  weights <- sapply(seq(1:n)/(n+1), J_fun, alpha, gamma)
  const <- n/sum(weights)
  
  if (alpha >= gamma) NA
  else {
    
    c <- 1/(m-r)*((m + m*r/n - r - m^2/n)*data_sort[m+1] - (1 + r/n)*sum(data_sort[(r+1):m]) + 
                      2/n*sum(data_sort[(r+1):m]*((r+1):m)) + (2-r/n)*sum(data_sort[(n-m+1):(n-r)]) + 
                      (r*m - m^2)/n*data_sort[n-m] - 2/n*sum(data_sort[(n-m+1):(n-r)]*((n-m+1):(n-r)))) +
      1/n*(sum(data_sort[(m+1):(n-m)]) + m*data_sort[n-m] + (m-n)*data_sort[m+1])
    
    infl <- c()
    
    for (i in 1:length(data)){
      
      x <- data_sort[i]
      
      if (data_sort[i] <= data_sort[r]){
        
        infl[i] <- -c
        
      } else if ((data_sort[i] > data_sort[r]) & (data_sort[i] <= data_sort[m])){
        
        infl[i] <- 1/(m-r)*((i-r)*x - sum(data_sort[(r+1):i])) - c
        
      } else if ((data_sort[i] > data_sort[m]) & (data_sort[i] <= data_sort[n-m])) {
        
        infl[i] <- 1/(m-r)*((m-r)*data_sort[m+1] - sum(data_sort[(r+1):m])) + x - data_sort[m+1] -c
        
      } else if ((data_sort[i] > data_sort[n-m]) & (data_sort[i] <= data_sort[n-r])) {
        
        infl[i] <- 1/(m-r)*((m-r)*data_sort[m+1] - sum(data_sort[(r+1):m])) + data_sort[n-m] - data_sort[m+1] +
          1/(m-r)*((n-r)*x + (r-m)*data_sort[n-m] - i*x + sum(data_sort[(n-m+1):i])) - c
        
      } else if(data_sort[i] > data_sort[n-r]){
        
        infl[i] <- 1/(m-r)*((m-r)*data_sort[m+1] - sum(data_sort[(r+1):m])) + data_sort[n-m] - data_sort[m+1] +
          1/(m-r)*((r-m)*data_sort[n-m] + sum(data_sort[(n-m+1):(n-r)])) - c
      }
    }
    
    sum(infl^2)*const^2/n^2
  }
}

emp.lik.stmean <- function(mu, data, alpha, gamma) {
  
  data  <- sort(data)
  
  n <- length(data)
  r <- floor(n*alpha)
  
  weights <- sapply(seq(1:n)/(n+1), J_fun, alpha, gamma)
  weights <- weights/sum(weights)
  
  m   <- n-2*r
  Wni <- (data-mu)[(r+1):(n-r)]
  
  
  lam.fun <- function(lambda)
  {
    sum(weights[(r+1):(n-r)]*Wni/(1+lambda*Wni))
  }
  
  lam.fun <- Vectorize(lam.fun)
  
  
  gal1 <- 1/(-max(Wni)) + 0.0001
  gal2 <- 1/(-min(Wni)) - 0.0001
  
  
  if (gal1 >= gal2) {
    stat <- 100
  } else {
    
    root <- uniroot(function(lam) lam.fun(lam), c(gal1, gal2))$root
    
    sigma1sq <- sum((weights*(data - ST_mean(data, alpha, gamma))^2))
    sigma2sq <- stmeanvar.asym(data, alpha, gamma)*n
    a <- sigma1sq/sigma2sq/(1-2*alpha)
    
    stat <- 2*a*sum(weights[(r+1):(n-r)]*m*log(1+root*Wni))
  }
  stat
}

emp.conf.intervals.stmean <- function (step = 0.01, initStep = 0, level, data, alpha, gamma) {
  
  mu <- ST_mean(data, alpha, gamma)
  value <- 0
  step1 <- step
  Lbeta <- mu - initStep
  while (value < level) {
    Lbeta <- Lbeta - step1
    value <- emp.lik.stmean(Lbeta, data, alpha, gamma)
  }
  Lbeta0 <- Lbeta
  Lbeta1 <- Lbeta + step1
  
  tempfun <- function(beta1) {
    return(level - emp.lik.stmean(beta1, data, alpha, gamma))
  }
  
  if (round(abs(Lbeta0-mu),2)<=0.01) {
    Lbeta <- mu
  } else {
    temp1 <- uniroot(tempfun, lower = Lbeta0, upper = Lbeta1)
    Lbeta <- temp1$root
  }
  
  value <- 0
  Ubeta <- mu + initStep
  while (value < level) {
    Ubeta <- Ubeta + step
    value <- emp.lik.stmean(Ubeta, data, alpha, gamma)
  }
  Ubeta0 <- Ubeta
  Ubeta1 <- Ubeta - step
  
  if (round(abs(Ubeta0-mu),2)<=0.01) {
    Ubeta <- mu
  } else {
    temp2 <- uniroot(tempfun, lower = Ubeta1, upper = Ubeta0)
    Ubeta <- temp2$root
  }
  c(Lbeta, Ubeta)
}

empirical.stm <- function(data, alpha, gamma, conf.level = 0.95) {
  
  level <- qchisq(conf.level, df=1)
  estimate   <- ST_mean(data = data, alpha = alpha, gamma = gamma)
  conf.int   <- emp.conf.intervals.stmean(data = data, alpha = alpha, gamma = gamma, level = level)
  
  list(estimate = estimate, conf.int = conf.int)
}



win_izlase<-function(izlase,alpha){
  n<-length(izlase)
  r<-floor(n*alpha)+1
  s<-n-floor(n*alpha)
  
  izlase.ord<-sort(izlase)
  izl_win<-c(rep(izlase.ord[r],r-1),izlase.ord[r:s],rep(izlase.ord[s],r-1))
  izl_win
}

trim.var <- function(izlase, alpha){
  n <- length(izlase)
  win.izlase <- win_izlase(izlase, alpha)
  trim.sigma <- 1/(n-1)/n/(1-2*alpha)^2 * sum((win.izlase - mean(win.izlase))^2)
  trim.sigma
}

emp.lik.tmean<-function(mu,dati,alpha,var.tmean){

  izlase<-sort(dati)
  n<-length(dati)
  r<-floor(n*alpha)
  Wni<-(izlase-mu)[(r+1):(n-r)]
  m<-n-2*r

  lam.fun<-function(lambda){
    m*sum(Wni/(1+lambda*Wni))}

  lam.fun<-Vectorize(lam.fun)

  gal1<-1/(-max(Wni))+0.0001
  gal2<-1/(-min(Wni))-0.0001

  if(gal1>=gal2){
    stat<-100
  }else{
    sigma1sq<-1/m*sum((izlase[(r+1):(n-r)]-mean(dati,alpha))^2)
    #sigma2sq<-trimvar(izlase,alpha)*n
    sigma2sq<-var.tmean*n
    a<-sigma1sq/sigma2sq/(1-2*alpha)

    sakne<-uniroot(function(lam)lam.fun(lam),c(gal1,gal2))$root
    stat<-2*a*sum(log(1+sakne*Wni))
  }
  stat
}


emp.conf.intervals.tmean<-function(step=0.01,initStep=0,level= 3.84,dati,alpha){
  mu <- mean(dati, alpha)
  var.tmean <- length(dati)*trim.var(dati, alpha)
value<-0
step1<-step
Lbeta<-mu-initStep
while(value<level){
Lbeta<-Lbeta-step1
value<-emp.lik.tmean(Lbeta,dati,alpha,var.tmean)
}
Lbeta0<-Lbeta
Lbeta1<-Lbeta+step1

tempfun<-function(beta1){
return(level-emp.lik.tmean(beta1,dati,alpha,var.tmean))}

if(round(abs(Lbeta0-mu),2)<=0.01){
Lbeta<-mu
}else{
temp1<-uniroot(tempfun,lower=Lbeta0,upper=Lbeta1)
Lbeta<-temp1$root
}

value<-0
Ubeta<-mu+initStep
while(value<level){
Ubeta<-Ubeta+step
value<-emp.lik.tmean(Ubeta,dati,alpha,var.tmean)
}
Ubeta0<-Ubeta
Ubeta1<-Ubeta-step

if(round(abs(Ubeta0-mu),2)<=0.01){
Ubeta<-mu
}else{
temp2<-uniroot(tempfun,lower=Ubeta1,upper=Ubeta0)
Ubeta<-temp2$root
}

int_length<-Ubeta-Lbeta

indic<-prod(c(Ubeta,Lbeta)-0)
c(indic,int_length)
}

empirical.tm <- function(data, alpha,  conf.level = 0.95) {
  
  level <- qchisq(conf.level, df=1)
  estimate   <- mean(data, alpha)
  conf.int   <- emp.conf.intervals.tmean(dati = data, alpha = alpha, level = level)
  
  list(estimate = estimate, conf.int = conf.int)
}


################################################################################

x <- c(-10, rnorm(20), 10, 20, 50)

empirical.tm(x, 0.1, 0.2)
empirical.stm(x, 0.1, 0.2, conf.level = 0.95)

# estimate    smoothly trimmed mean 
# conf.int    a confidence interval for the smoothly trimmed mean 


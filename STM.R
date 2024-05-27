source('stm_empirical.R')
library(tidyverse)
library(stringi)

coverage <- function(mu, lb, ub){
  if(is.na(lb)|is.na(ub)){
    NA
  }else if(mu > lb & mu < ub){
    1
  }else{
    0
  }
}

# coverage(0, 0, 1)
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


norm.apprt.stmean <- function(dati, alpha, gamma){
  stmean <- ST_mean(dati, alpha, gamma)
  var.stmean <- stmeanvar.asym(dati, alpha, gamma)
lb<-stmean-qnorm(1-(1-0.95)/2)*sqrt(var.stmean)
ub<-stmean+qnorm(1-(1-0.95)/2)*sqrt(var.stmean)

return(list('conf.int' = c(lb, ub)))
}

norm.apprt.tmean <- function(dati, alpha){
  tmean <- mean(dati, alpha)
  var.tmean <- trim.var(dati, alpha)
  lb<-tmean-qnorm(1-(1-0.95)/2)*sqrt(var.tmean)
  ub<-tmean+qnorm(1-(1-0.95)/2)*sqrt(var.tmean)
  
  return(list('conf.int' = c(lb, ub)))
}






# datu ģenerēšanas funkcijas ----

F.cn <- function(x){
  0.8 * pnorm(x)+
    0.2 * pnorm(x, 0, 25)
}
F.cn2 <- function(x){
  0.1 * pnorm(x + 10) + 
    0.8 * pnorm(x) + 
    0.1 * pnorm(x - 10)
}


gen.N <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cn(x) - y}, c(-1000,1000))$root)
}
gen.N2 <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cn2(x) - y}, c(-1000,1000))$root)
}


ff.N <- function(N, alpha, gamma){
dati <- replicate(N, gen.N())

ci.elst <- empirical.stm(dati, alpha = alpha, gamma = gamma)$conf.int
ci.st <- norm.apprt.stmean(dati, alpha = alpha, gamma = gamma)$conf.int

return(list(
  'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                coverage(0, ci.elst[1],ci.elst[2])),
  'ci.len' = c(ci.st[2] - ci.st[1],
               ci.elst[2] - ci.elst[1])))
}
ff.N2 <- function(N, alpha, gamma){
  dati <- replicate(N, gen.N2())
  ci.elst <- empirical.stm(dati, alpha = alpha, gamma = gamma)$conf.int
  ci.st <- norm.apprt.stmean(dati, alpha = alpha, gamma = gamma)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}
ff.NU <- function(N, alpha, gamma){
  dati <- replicate(N, gen.NU())
  ci.elst <- empirical.stm(dati, alpha = alpha, gamma = gamma)$conf.int
  ci.st <- norm.apprt.stmean(dati, alpha = alpha, gamma = gamma)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}
ff.NU2 <- function(N, alpha, gamma){
  dati <- replicate(N, gen.NU2())
  ci.elst <- empirical.stm(dati, alpha = alpha, gamma = gamma)$conf.int
  ci.st <- norm.apprt.stmean(dati, alpha = alpha, gamma = gamma)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}
ff.NU3 <- function(N, alpha, gamma){
  dati <- replicate(N, gen.NU3())
  ci.elst <- empirical.stm(dati, alpha = alpha, gamma = gamma)$conf.int
  ci.st <- norm.apprt.stmean(dati, alpha = alpha, gamma = gamma)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}

NN <- c(30, 50, 100, 200, 500)
alpha <- c(0.05, 0.1, 0.15, 0.2)
gamma <- c(0.1 , 0.2, 0.3, 0.4)
# t(replicate(10, ff.N(100, 0.1, 0.11)$ci.len))

# datu sim ----
# coverage(0, NA, NA)

set.seed(1)
b4 <- Sys.time()

# N1 ----

N1.fNN <- as.data.frame(matrix(nrow = 1000, ncol = 12*2))
N2.fNN <- as.data.frame(matrix(nrow = 1000, ncol = 12*2))
N3.fNN <- as.data.frame(matrix(nrow = 1000, ncol = 12*2))
N4.fNN <- as.data.frame(matrix(nrow = 1000, ncol = 12*2))

N1.fNN2 <- as.data.frame(matrix(nrow = 1000, ncol = 12*2))
N2.fNN2 <- as.data.frame(matrix(nrow = 1000, ncol = 12*2))
N3.fNN2 <- as.data.frame(matrix(nrow = 1000, ncol = 12*2))
N4.fNN2 <- as.data.frame(matrix(nrow = 1000, ncol = 12*2))

# N1.fNN[1,] <- rep(1,12)
b4 <- Sys.time()
for(i in 1:1000){
  N1.fNN[i,] <- c(ff.N(NN[1], alpha[1], gamma[1])$coverage, 
                  ff.N(NN[1], alpha[1], gamma[2])$coverage, 
                  ff.N(NN[1], alpha[1], gamma[3])$coverage, 
                  ff.N(NN[1], alpha[1], gamma[4])$coverage,
                  ff.N(NN[1], alpha[2], gamma[2])$coverage,
                  ff.N(NN[1], alpha[2], gamma[3])$coverage,
                  ff.N(NN[1], alpha[2], gamma[4])$coverage,
                  ff.N(NN[1], alpha[3], gamma[2])$coverage,
                  ff.N(NN[1], alpha[3], gamma[3])$coverage,
                  ff.N(NN[1], alpha[3], gamma[4])$coverage,
                  ff.N(NN[1], alpha[4], gamma[3])$coverage,
                  ff.N(NN[1], alpha[4], gamma[4])$coverage)
  
  N2.fNN[i,] <- c(ff.N(NN[2], alpha[1], gamma[1])$coverage,
                  ff.N(NN[2], alpha[1], gamma[2])$coverage,
                  ff.N(NN[2], alpha[1], gamma[3])$coverage,
                  ff.N(NN[2], alpha[1], gamma[4])$coverage,
                  ff.N(NN[2], alpha[2], gamma[2])$coverage,
                  ff.N(NN[2], alpha[2], gamma[3])$coverage,
                  ff.N(NN[2], alpha[2], gamma[4])$coverage,
                  ff.N(NN[2], alpha[3], gamma[2])$coverage,
                  ff.N(NN[2], alpha[3], gamma[3])$coverage,
                  ff.N(NN[2], alpha[3], gamma[4])$coverage,
                  ff.N(NN[2], alpha[4], gamma[3])$coverage,
                  ff.N(NN[2], alpha[4], gamma[4])$coverage)

  N3.fNN[i,] <- c(ff.N(NN[3], alpha[1], gamma[1])$coverage,
                  ff.N(NN[3], alpha[1], gamma[2])$coverage,
                  ff.N(NN[3], alpha[1], gamma[3])$coverage,
                  ff.N(NN[3], alpha[1], gamma[4])$coverage,
                  ff.N(NN[3], alpha[2], gamma[2])$coverage,
                  ff.N(NN[3], alpha[2], gamma[3])$coverage,
                  ff.N(NN[3], alpha[2], gamma[4])$coverage,
                  ff.N(NN[3], alpha[3], gamma[2])$coverage,
                  ff.N(NN[3], alpha[3], gamma[3])$coverage,
                  ff.N(NN[3], alpha[3], gamma[4])$coverage,
                  ff.N(NN[3], alpha[4], gamma[3])$coverage,
                  ff.N(NN[3], alpha[4], gamma[4])$coverage)

  N4.fNN[i,] <- c(ff.N(NN[4], alpha[1], gamma[1])$coverage,
                  ff.N(NN[4], alpha[1], gamma[2])$coverage,
                  ff.N(NN[4], alpha[1], gamma[3])$coverage,
                  ff.N(NN[4], alpha[1], gamma[4])$coverage,
                  ff.N(NN[4], alpha[2], gamma[2])$coverage,
                  ff.N(NN[4], alpha[2], gamma[3])$coverage,
                  ff.N(NN[4], alpha[2], gamma[4])$coverage,
                  ff.N(NN[4], alpha[3], gamma[2])$coverage,
                  ff.N(NN[4], alpha[3], gamma[3])$coverage,
                  ff.N(NN[4], alpha[3], gamma[4])$coverage,
                  ff.N(NN[4], alpha[4], gamma[3])$coverage,
                  ff.N(NN[4], alpha[4], gamma[4])$coverage)
  
  ###################### 
  
  N1.fNN2[i,] <- c(ff.N2(NN[1], alpha[1], gamma[1])$coverage, 
                  ff.N2(NN[1], alpha[1], gamma[2])$coverage, 
                  ff.N2(NN[1], alpha[1], gamma[3])$coverage, 
                  ff.N2(NN[1], alpha[1], gamma[4])$coverage,
                  ff.N2(NN[1], alpha[2], gamma[2])$coverage,
                  ff.N2(NN[1], alpha[2], gamma[3])$coverage,
                  ff.N2(NN[1], alpha[2], gamma[4])$coverage,
                  ff.N2(NN[1], alpha[3], gamma[2])$coverage,
                  ff.N2(NN[1], alpha[3], gamma[3])$coverage,
                  ff.N2(NN[1], alpha[3], gamma[4])$coverage,
                  ff.N2(NN[1], alpha[4], gamma[3])$coverage,
                  ff.N2(NN[1], alpha[4], gamma[4])$coverage)
  
  N2.fNN2[i,] <- c(ff.N2(NN[2], alpha[1], gamma[1])$coverage,
                  ff.N2(NN[2], alpha[1], gamma[2])$coverage,
                  ff.N2(NN[2], alpha[1], gamma[3])$coverage,
                  ff.N2(NN[2], alpha[1], gamma[4])$coverage,
                  ff.N2(NN[2], alpha[2], gamma[2])$coverage,
                  ff.N2(NN[2], alpha[2], gamma[3])$coverage,
                  ff.N2(NN[2], alpha[2], gamma[4])$coverage,
                  ff.N2(NN[2], alpha[3], gamma[2])$coverage,
                  ff.N2(NN[2], alpha[3], gamma[3])$coverage,
                  ff.N2(NN[2], alpha[3], gamma[4])$coverage,
                  ff.N2(NN[2], alpha[4], gamma[3])$coverage,
                  ff.N2(NN[2], alpha[4], gamma[4])$coverage)
  
  N3.fNN2[i,] <- c(ff.N2(NN[3], alpha[1], gamma[1])$coverage,
                  ff.N2(NN[3], alpha[1], gamma[2])$coverage,
                  ff.N2(NN[3], alpha[1], gamma[3])$coverage,
                  ff.N2(NN[3], alpha[1], gamma[4])$coverage,
                  ff.N2(NN[3], alpha[2], gamma[2])$coverage,
                  ff.N2(NN[3], alpha[2], gamma[3])$coverage,
                  ff.N2(NN[3], alpha[2], gamma[4])$coverage,
                  ff.N2(NN[3], alpha[3], gamma[2])$coverage,
                  ff.N2(NN[3], alpha[3], gamma[3])$coverage,
                  ff.N2(NN[3], alpha[3], gamma[4])$coverage,
                  ff.N2(NN[3], alpha[4], gamma[3])$coverage,
                  ff.N2(NN[3], alpha[4], gamma[4])$coverage)
  
  N4.fNN2[i,] <- c(ff.N2(NN[4], alpha[1], gamma[1])$coverage,
                  ff.N2(NN[4], alpha[1], gamma[2])$coverage,
                  ff.N2(NN[4], alpha[1], gamma[3])$coverage,
                  ff.N2(NN[4], alpha[1], gamma[4])$coverage,
                  ff.N2(NN[4], alpha[2], gamma[2])$coverage,
                  ff.N2(NN[4], alpha[2], gamma[3])$coverage,
                  ff.N2(NN[4], alpha[2], gamma[4])$coverage,
                  ff.N2(NN[4], alpha[3], gamma[2])$coverage,
                  ff.N2(NN[4], alpha[3], gamma[3])$coverage,
                  ff.N2(NN[4], alpha[3], gamma[4])$coverage,
                  ff.N2(NN[4], alpha[4], gamma[3])$coverage,
                  ff.N2(NN[4], alpha[4], gamma[4])$coverage)
  
 
  print(i)
}

now <- Sys.time()
difftime(now, b4)

ff.N.tm <- function(N, alpha){
  dati <- replicate(N, gen.N())
  
  ci.elst <- empirical.tm(dati, alpha = alpha)$conf.int
  ci.st <- norm.apprt.tmean(dati, alpha = alpha)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}
ff.N2.tm <- function(N, alpha){
  dati <- replicate(N, gen.N2())
  ci.elst <- empirical.tm(dati, alpha = alpha)$conf.int
  ci.st <- norm.apprt.tmean(dati, alpha = alpha)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}
ff.NU.tm <- function(N, alpha){
  dati <- replicate(N, gen.NU())
  ci.elst <- empirical.tm(dati, alpha = alpha)$conf.int
  ci.st <- norm.apprt.tmean(dati, alpha = alpha)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}
ff.NU2.tm <- function(N, alpha){
  dati <- replicate(N, gen.NU2())
  ci.elst <- empirical.tm(dati, alpha = alpha)$conf.int
  ci.st <- norm.apprt.tmean(dati, alpha = alpha)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}
ff.NU3.tm <- function(N, alpha){
  dati <- replicate(N, gen.NU3())
  ci.elst <- empirical.tm(dati, alpha = alpha)$conf.int
  ci.st <- norm.apprt.tmean(dati, alpha = alpha)$conf.int
  
  return(list(
    'coverage'= c(coverage(0, ci.st[1],ci.st[2]),
                  coverage(0, ci.elst[1],ci.elst[2])),
    'ci.len' = c(ci.st[2] - ci.st[1],
                 ci.elst[2] - ci.elst[1])))
}

# TM sim ----

N1.fNN.tm <- as.data.frame(matrix(nrow = 1000, ncol = 4*2))
N2.fNN.tm <- as.data.frame(matrix(nrow = 1000, ncol = 4*2))
N3.fNN.tm <- as.data.frame(matrix(nrow = 1000, ncol = 4*2))
N4.fNN.tm <- as.data.frame(matrix(nrow = 1000, ncol = 4*2))

N1.fNN2.tm <- as.data.frame(matrix(nrow = 1000, ncol = 4*2))
N2.fNN2.tm <- as.data.frame(matrix(nrow = 1000, ncol = 4*2))
N3.fNN2.tm <- as.data.frame(matrix(nrow = 1000, ncol = 4*2))
N4.fNN2.tm <- as.data.frame(matrix(nrow = 1000, ncol = 4*2))


# N1.fNN[1,] <- rep(1,12)
b4 <- Sys.time()
for(i in 1:1000){
  N1.fNN.tm[i,] <- c(ff.N.tm(NN[1], alpha[1])$coverage,
                  ff.N.tm(NN[1], alpha[2])$coverage,
                  ff.N.tm(NN[1], alpha[3])$coverage,
                  ff.N.tm(NN[1], alpha[4])$coverage)

  N2.fNN.tm[i,] <- c(ff.N.tm(NN[2], alpha[1])$coverage,
                     ff.N.tm(NN[2], alpha[2])$coverage,
                     ff.N.tm(NN[2], alpha[3])$coverage,
                     ff.N.tm(NN[2], alpha[4])$coverage)

  N3.fNN.tm[i,] <- c(ff.N.tm(NN[3], alpha[1])$coverage,
                     ff.N.tm(NN[3], alpha[2])$coverage,
                     ff.N.tm(NN[3], alpha[3])$coverage,
                     ff.N.tm(NN[3], alpha[4])$coverage)

  N4.fNN.tm[i,] <- c(ff.N.tm(NN[4], alpha[1])$coverage,
                     ff.N.tm(NN[4], alpha[2])$coverage,
                     ff.N.tm(NN[4], alpha[3])$coverage,
                     ff.N.tm(NN[4], alpha[4])$coverage)


  ###########################

  N1.fNN2.tm[i,] <- c(ff.N2.tm(NN[1], alpha[1])$coverage,
                     ff.N2.tm(NN[1], alpha[2])$coverage,
                     ff.N2.tm(NN[1], alpha[3])$coverage,
                     ff.N2.tm(NN[1], alpha[4])$coverage)

  N2.fNN2.tm[i,] <- c(ff.N2.tm(NN[2], alpha[1])$coverage,
                     ff.N2.tm(NN[2], alpha[2])$coverage,
                     ff.N2.tm(NN[2], alpha[3])$coverage,
                     ff.N2.tm(NN[2], alpha[4])$coverage)

  N3.fNN2.tm[i,] <- c(ff.N2.tm(NN[3], alpha[1])$coverage,
                     ff.N2.tm(NN[3], alpha[2])$coverage,
                     ff.N2.tm(NN[3], alpha[3])$coverage,
                     ff.N2.tm(NN[3], alpha[4])$coverage)

  N4.fNN2.tm[i,] <- c(ff.N2.tm(NN[4], alpha[1])$coverage,
                     ff.N2.tm(NN[4], alpha[2])$coverage,
                     ff.N2.tm(NN[4], alpha[3])$coverage,
                     ff.N2.tm(NN[4], alpha[4])$coverage)


  
  print(i)
}

now <- Sys.time()
difftime(now, b4)

N1.fNN

tr.data.stm <- function(dati){
  # dati <- N1.fNN
names(dati)<-c('EL_a1_g1','NORM_a1_g1',
               'EL_a1_g2','NORM_a1_g2',
               'EL_a1_g3','NORM_a1_g3',
               'EL_a1_g4','NORM_a1_g4',
               'EL_a2_g2','NORM_a2_g2',
               'EL_a2_g3','NORM_a2_g3',
               'EL_a2_g4','NORM_a2_g4',
               'EL_a3_g2','NORM_a3_g2',
               'EL_a3_g3','NORM_a3_g3',
               'EL_a3_g4','NORM_a3_g4',
               'EL_a4_g3','NORM_a4_g3',
               'EL_a4_g4','NORM_a4_g4')

dati <- dati %>% 
  pivot_longer(cols = 1:24) %>% 
  group_by(name) %>% 
  summarise(cov_g1 = mean(value)) 

rez2 <- dati %>% 
  mutate(alpha = ifelse(grepl('a1', name), 0.05,
                        ifelse(grepl('a2', name), 0.1,
                               ifelse(grepl('a3', name), 0.15,
                                      ifelse(grepl('a4', name), 0.2, NA)))),
         gamma = as.character(ifelse(grepl('g1', name), 0.1,
                                     ifelse(grepl('g2', name), 0.2,
                                            ifelse(grepl('g3', name), 0.3,
                                                   ifelse(grepl('g4', name), 0.4, NA))))),
         norm_fl = as.character(ifelse(grepl('NORM', name), 1, 0)))

return(rez2)
}


tr.data.tm <- function(dati){
  # dati <- N1.fNN
  names(dati)<-c('EL_a1','NORM_a1',
                 'EL_a2','NORM_a2',
                 'EL_a3','NORM_a3',
                 'EL_a4','NORM_a4')
  
  dati <- dati %>% 
    pivot_longer(cols = 1:8) %>% 
    group_by(name) %>% 
    summarise(cov_g1 = mean(value)) 
  
  rez2 <- dati %>% 
    mutate(alpha = ifelse(grepl('a1', name), 0.05,
                          ifelse(grepl('a2', name), 0.1,
                                 ifelse(grepl('a3', name), 0.15,
                                        ifelse(grepl('a4', name), 0.2, NA)))),
           method = as.character(ifelse(grepl('NORM', name), 'normal', 'emp')),
           gamma = 'tm')
  
  return(rez2)
}

rezz <- tr.data.stm(N1.fNN)
rezz.tm <- tr.data.tm(N1.fNN.tm)
gamma_colors <- c("0.1" = "darkmagenta", "0.2" = "darkblue", "0.3" = "forestgreen", "0.4" = "gold4", "tm" = "deeppink1")
g.N1.fN <- rezz %>% 
  rbind(rezz.tm) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(size = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  scale_color_manual(values = gamma_colors)
  

rezz <- tr.data.stm(N2.fNN)
rezz.tm <- tr.data.tm(N2.fNN.tm)
g.N2.fN <- rezz %>% 
  rbind(rezz.tm) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(size = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('')+
  scale_color_manual(values = gamma_colors)

rezz <- tr.data.stm(N3.fNN)
rezz.tm <- tr.data.tm(N3.fNN.tm)
g.N3.fN <- rezz %>% 
  rbind(rezz.tm) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(size = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  scale_color_manual(values = gamma_colors)

rezz <- tr.data.stm(N4.fNN)
rezz.tm <- tr.data.tm(N4.fNN.tm)
g.N4.fN <- rezz %>% 
  rbind(rezz.tm) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(size = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('')+
  scale_color_manual(values = gamma_colors)

graph.fN <- ggpubr::ggarrange(g.N1.fN, g.N2.fN, g.N3.fN, g.N4.fN, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                  labels = c('N = 50',
                             'N = 100',
                             'N = 200',
                             'N = 500'),
                  label.x = 0.7,
                  font.label = list(size = 10, color = "black", family = NULL))

####################################

rezz <- tr.data.stm(N1.fNN2)
rezz.tm <- tr.data.tm(N1.fNN2.tm)
gamma_colors <- c("0.1" = "darkmagenta", "0.2" = "darkblue", "0.3" = "forestgreen", "0.4" = "gold4", "tm" = "deeppink1")
g.N1.fN <- rezz %>% 
  rbind(rezz.tm) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(size = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  scale_color_manual(values = gamma_colors)


rezz <- tr.data.stm(N2.fNN2)
rezz.tm <- tr.data.tm(N2.fNN2.tm)
g.N2.fN <- rezz %>% 
  rbind(rezz.tm) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(size = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('')+
  scale_color_manual(values = gamma_colors)

rezz <- tr.data.stm(N3.fNN2)
rezz.tm <- tr.data.tm(N3.fNN2.tm)
g.N3.fN <- rezz %>% 
  rbind(rezz.tm) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(size = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  scale_color_manual(values = gamma_colors)

rezz <- tr.data.stm(N4.fNN2)
rezz.tm <- tr.data.tm(N4.fNN2.tm)
g.N4.fN <- rezz %>% 
  rbind(rezz.tm) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(size = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('')+
  scale_color_manual(values = gamma_colors)

graph.fN2 <- ggpubr::ggarrange(g.N1.fN, g.N2.fN, g.N3.fN, g.N4.fN, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                              labels = c('N = 50',
                                         'N = 100',
                                         'N = 200',
                                         'N = 500'),
                              label.x = 0.7,
                              font.label = list(size = 10, color = "black", family = NULL))





graph.fN
ggsave('fn.png', width = 10)
graph.fN2
ggsave('fn2.png', width = 10)

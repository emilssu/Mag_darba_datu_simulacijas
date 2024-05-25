source('funkcijas.R')
library(rstatix)
library(tidyverse)

F.cn3 <- function(x){
  0.2 * pnorm(x + 10) + 
    0.8 * pnorm(x) 
}

gen.N3 <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cn3(x) - y}, c(-1000,1000))$root)
}


alpha <- c(0.05, 0.1, 0.15, 0.2)
gamma <- c(0.1, 0.2, 0.3, 0.4)
NN <- c(30, 50, 100, 200, 500)
alph_gam <- as.data.frame(cbind(c(rep(alpha[1],4),rep(alpha[2],3), rep(alpha[3],3), rep(alpha[4],2)),
                                c(gamma, gamma[-1], gamma[-1], gamma[-(1:2)])))

names(alph_gam) <- c('alpha', 'gamma')  

iter <- 10000
p.aov.norm <- matrix(ncol = 2, nrow = length(NN))
p.aov.lnorm <- matrix(ncol = 2, nrow = length(NN))
p.aov.chi <- matrix(ncol = 2, nrow = length(NN))
p.aov.N <- matrix(ncol = 2, nrow = length(NN))
p.aov.N2 <- matrix(ncol = 2, nrow = length(NN))
p.aov.NU <- matrix(ncol = 2, nrow = length(NN))
p.aov.N3 <- matrix(ncol = 2, nrow = length(NN))

p.y.norm.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.lnorm.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.chi.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.N.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.N2.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.NU.tm <- matrix(ncol = 4, nrow = length(NN))
p.y.N3.tm <- matrix(ncol = 4, nrow = length(NN))

p.el.norm.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.lnorm.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.chi.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.N.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.N2.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.NU.tm <- matrix(ncol = 4, nrow = length(NN))
p.el.N3.tm <- matrix(ncol = 4, nrow = length(NN))

p.el.norm.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.lnorm.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.chi.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.N.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.N2.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.NU.stm <- matrix(ncol = 12, nrow = length(NN))
p.el.N3.stm <- matrix(ncol = 12, nrow = length(NN))
# n <- 1

set.seed(1)
for(n in 1:length(NN)){
  temp.aov.norm <- matrix(ncol = 2, nrow = iter)
  temp.aov.lnorm <- matrix(ncol = 2, nrow = iter)
  temp.aov.chi <- matrix(ncol = 2, nrow = iter)
  temp.aov.N <- matrix(ncol = 2, nrow = iter)
  temp.aov.N2 <- matrix(ncol = 2, nrow = iter)
  temp.aov.NU <- matrix(ncol = 2, nrow = iter)
  temp.aov.N3 <- matrix(ncol = 2, nrow = iter)
  
  temp.y.norm.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.lnorm.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.chi.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.N.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.N2.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.NU.tm <- matrix(ncol = 4, nrow = iter)
  temp.y.N3.tm <- matrix(ncol = 4, nrow = iter)
  
  temp.el.norm.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.lnorm.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.chi.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.N.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.N2.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.NU.tm <- matrix(ncol = 4, nrow = iter)
  temp.el.N3.tm <- matrix(ncol = 4, nrow = iter)
  
  temp.el.norm.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.lnorm.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.chi.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.N.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.N2.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.NU.stm <- matrix(ncol = 12, nrow = iter)
  temp.el.N3.stm <- matrix(ncol = 12, nrow = iter)
  
  for(i in 1:iter){
    ww.norm <- list(rnorm(NN[n]), rnorm(NN[n]), rnorm(NN[n]))
    ww.lnorm <- list(rlnorm(NN[n], sdlog = 5), rlnorm(NN[n], sdlog = 5), rlnorm(NN[n], sdlog = 5))
    ww.chi <- list(rchisq(NN[n], df = 3), rchisq(NN[n], df = 3), rchisq(NN[n], df = 3))
    ww.N <- list(replicate(NN[n], gen.N()), replicate(NN[n], gen.N()), replicate(NN[n], gen.N()))
    ww.N2 <- list(replicate(NN[n], gen.N2()), replicate(NN[n], gen.N2()), replicate(NN[n], gen.N2()))
    ww.NU <- list(replicate(NN[n], gen.NU()), replicate(NN[n], gen.NU()), replicate(NN[n], gen.NU()))
    ww.N3 <- list(replicate(NN[n], gen.N3()), replicate(NN[n], gen.N3()), replicate(NN[n], gen.N3()))
    
    dati.norm <- data.frame(data = c(ww.norm[[1]], ww.norm[[2]], ww.norm[[3]]),
                            gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.lnorm <- data.frame(data = c(ww.lnorm[[1]], ww.lnorm[[2]], ww.lnorm[[3]]),
                            gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.chi <- data.frame(data = c(ww.chi[[1]], ww.chi[[2]], ww.chi[[3]]),
                            gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.N <- data.frame(data = c(ww.N[[1]], ww.N[[2]], ww.N[[3]]),
                            gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.N2 <- data.frame(data = c(ww.N2[[1]], ww.N2[[2]], ww.N2[[3]]),
                         gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.NU <- data.frame(data = c(ww.NU[[1]], ww.NU[[2]], ww.NU[[3]]),
                         gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati.N3 <- data.frame(data = c(ww.N3[[1]], ww.N3[[2]], ww.N3[[3]]), 
                          gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    
    temp.aov.norm[i,1] <- summary(aov(data ~ gr, data = dati.norm))[[1]]$`Pr(>F)`[1]
    temp.aov.lnorm[i,1] <- summary(aov(data ~ gr, data = dati.lnorm))[[1]]$`Pr(>F)`[1]
    temp.aov.chi[i,1] <- summary(aov(data ~ gr, data = dati.chi))[[1]]$`Pr(>F)`[1]
    temp.aov.N[i,1] <- summary(aov(data ~ gr, data = dati.N))[[1]]$`Pr(>F)`[1]
    temp.aov.N2[i,1] <- summary(aov(data ~ gr, data = dati.N2))[[1]]$`Pr(>F)`[1]
    temp.aov.NU[i,1] <- summary(aov(data ~ gr, data = dati.NU))[[1]]$`Pr(>F)`[1]
    temp.aov.N3[i,1] <- summary(aov(data ~ gr, data = dati.N3))[[1]]$`Pr(>F)`[1]
    
    temp.aov.norm[i,2] <- welch_anova_test(dati.norm, data ~ gr)$p
    temp.aov.lnorm[i,2] <- welch_anova_test(dati.lnorm, data ~ gr)$p
    temp.aov.chi[i,2] <- welch_anova_test(dati.chi, data ~ gr)$p
    temp.aov.N[i,2] <- welch_anova_test(dati.N, data ~ gr)$p
    temp.aov.N2[i,2] <- welch_anova_test(dati.N2, data ~ gr)$p
    temp.aov.NU[i,2] <- welch_anova_test(dati.NU, data ~ gr)$p
    temp.aov.N3[i,2] <- welch_anova_test(dati.N3, data ~ gr)$p
    
    temp.alph.N.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.N2.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.NU.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.norm.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.lnorm.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.N3.el <- matrix(ncol = 12, nrow = 1)
    
    for(a in 1:length(alpha)){
      # a <- 1
      temp.y.norm.tm[i,a] <- t1way(data ~ gr, data = dati.norm, nboot = 1, tr = alpha[a])$p.value
      temp.y.lnorm.tm[i,a] <- t1way(data ~ gr, data = dati.lnorm, nboot = 1, tr = alpha[a])$p.value
      temp.y.chi.tm[i,a] <- t1way(data ~ gr, data = dati.chi, nboot = 1, tr = alpha[a])$p.value
      temp.y.N.tm[i,a] <- t1way(data ~ gr, data = dati.N, nboot = 1, tr = alpha[a])$p.value
      temp.y.N2.tm[i,a] <- t1way(data ~ gr, data = dati.N2, nboot = 1, tr = alpha[a])$p.value
      temp.y.NU.tm[i,a] <- t1way(data ~ gr, data = dati.NU, nboot = 1, tr = alpha[a])$p.value
      temp.y.N3.tm[i,a] <- t1way(data ~ gr, data = dati.N3, nboot = 1, tr = alpha[a])$p.value
      
      temp.el.norm.tm[i,a] <- EL.anova.tm(ww.norm, alpha[a], alpha[a])$p.value
      temp.el.lnorm.tm[i,a] <- EL.anova.tm(ww.lnorm, alpha[a], alpha[a])$p.value
      temp.el.chi.tm[i,a] <- EL.anova.tm(ww.chi, alpha[a], alpha[a])$p.value
      temp.el.N.tm[i,a] <- EL.anova.tm(ww.N, alpha[a], alpha[a])$p.value
      temp.el.N2.tm[i,a] <- EL.anova.tm(ww.N2, alpha[a], alpha[a])$p.value
      temp.el.NU.tm[i,a] <- EL.anova.tm(ww.NU, alpha[a], alpha[a])$p.value
      temp.el.N3.tm[i,a] <- EL.anova.tm(ww.N3, alpha[a], alpha[a])$p.value
      
      
      gam <- alph_gam[alph_gam$alpha == alpha[a],]$gamma
      
      if(a == 1){
        indx <- 1
      }else if(a == 2){
        indx <- 5
      }else if(a == 3){
        indx <- 8
      }else{
        indx <- 11
      }
      
      temp.gam.N.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N2.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.NU.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.norm.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.lnorm.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N3.el <- matrix(ncol = length(gam), nrow = 1)
      
      for(g in 1:length(gam)){
        temp.gam.norm.el[1,g] <- EL.anova.stm(ww.norm, alpha[a], gam[g])$p.value
        temp.gam.lnorm.el[1,g] <- EL.anova.stm(ww.lnorm, alpha[a], gam[g])$p.value
        temp.gam.chi.el[1,g] <- EL.anova.stm(ww.chi, alpha[a], gam[g])$p.value
        temp.gam.N.el[1,g] <- EL.anova.stm(ww.N, alpha[a], gam[g])$p.value
        temp.gam.N2.el[1,g] <- EL.anova.stm(ww.N2, alpha[a], gam[g])$p.value
        temp.gam.NU.el[1,g] <- EL.anova.stm(ww.NU, alpha[a], gam[g])$p.value
        temp.gam.N3.el[1,g] <- EL.anova.stm(ww.N3, alpha[a], gam[g])$p.value
        
      }
      
      temp.alph.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.el
      temp.alph.N2.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N2.el
      temp.alph.NU.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.NU.el
      temp.alph.norm.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.el
      temp.alph.lnorm.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.lnorm.el
      temp.alph.chi.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.el
      temp.alph.N3.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N3.el
    
      # print(temp.alph.N.el)
      # print(indx:(length(gam)+indx - 1))
    }
    
    temp.el.N.stm[i,] <- temp.alph.N.el
    temp.el.N2.stm[i,] <- temp.alph.N2.el
    temp.el.NU.stm[i,] <- temp.alph.NU.el
    temp.el.norm.stm[i,] <- temp.alph.norm.el
    temp.el.lnorm.stm[i,] <- temp.alph.lnorm.el
    temp.el.chi.stm[i,] <- temp.alph.chi.el
    temp.el.N3.stm[i,] <- temp.alph.N3.el
    
    print(paste('i: ', i, '; ', 'n: ', n, sep = ''))
  }
  p.aov.norm[n,1] <- mean(temp.aov.norm[,1] < 0.05)
  p.aov.lnorm[n,1] <- mean(temp.aov.lnorm[,1] < 0.05)
  p.aov.chi[n,1] <- mean(temp.aov.chi[,1] < 0.05)
  p.aov.N[n,1] <- mean(temp.aov.N[,1] < 0.05)
  p.aov.N2[n,1] <- mean(temp.aov.N2[,1] < 0.05)
  p.aov.NU[n,1] <- mean(temp.aov.NU[,1] < 0.05)
  p.aov.N3[n,1] <- mean(temp.aov.N3[,1] < 0.05)
  
  p.aov.norm[n,2] <- mean(temp.aov.norm[,2] < 0.05)
  p.aov.lnorm[n,2] <- mean(temp.aov.lnorm[,2] < 0.05)
  p.aov.chi[n,2] <- mean(temp.aov.chi[,2] < 0.05)
  p.aov.N[n,2] <- mean(temp.aov.N[,2] < 0.05)
  p.aov.N2[n,2] <- mean(temp.aov.N2[,2] < 0.05)
  p.aov.NU[n,2] <- mean(temp.aov.NU[,2] < 0.05)
  p.aov.N3[n,2] <- mean(temp.aov.N3[,2] < 0.05)
  
  p.el.norm.tm[n,] <- colMeans(temp.el.norm.tm < 0.05, na.rm = TRUE)
  p.el.lnorm.tm[n,] <- colMeans(temp.el.lnorm.tm< 0.05, na.rm = TRUE)
  p.el.chi.tm[n,] <- colMeans(temp.el.chi.tm< 0.05, na.rm = TRUE)
  p.el.N.tm[n,] <- colMeans(temp.el.N.tm< 0.05, na.rm = TRUE)
  p.el.N2.tm[n,] <- colMeans(temp.el.N2.tm< 0.05, na.rm = TRUE)
  p.el.NU.tm[n,] <- colMeans(temp.el.NU.tm< 0.05, na.rm = TRUE)
  p.el.N3.tm[n,] <- colMeans(temp.el.N3.tm< 0.05, na.rm = TRUE)
  
  p.y.norm.tm[n,] <- colMeans(temp.y.norm.tm < 0.05, na.rm = TRUE)
  p.y.lnorm.tm[n,] <- colMeans(temp.y.lnorm.tm< 0.05, na.rm = TRUE)
  p.y.chi.tm[n,] <- colMeans(temp.y.chi.tm< 0.05, na.rm = TRUE)
  p.y.N.tm[n,] <- colMeans(temp.y.N.tm< 0.05, na.rm = TRUE)
  p.y.N2.tm[n,] <- colMeans(temp.y.N2.tm< 0.05, na.rm = TRUE)
  p.y.NU.tm[n,] <- colMeans(temp.y.NU.tm< 0.05, na.rm = TRUE)
  p.y.N3.tm[n,] <- colMeans(temp.y.N3.tm< 0.05, na.rm = TRUE)
  
  p.el.norm.stm[n,] <- colMeans(temp.el.norm.stm < 0.05, na.rm = TRUE)
  p.el.lnorm.stm[n,] <- colMeans(temp.el.lnorm.stm< 0.05, na.rm = TRUE)
  p.el.chi.stm[n,] <- colMeans(temp.el.chi.stm< 0.05, na.rm = TRUE)
  p.el.N.stm[n,] <- colMeans(temp.el.N.stm< 0.05, na.rm = TRUE)
  p.el.N2.stm[n,] <- colMeans(temp.el.N2.stm< 0.05, na.rm = TRUE)
  p.el.NU.stm[n,] <- colMeans(temp.el.NU.stm< 0.05, na.rm = TRUE)
  p.el.N3.stm[n,] <- colMeans(temp.el.N3.stm< 0.05, na.rm = TRUE)
  
}



# F ANOVA ----

rezz.boot.a.N3 <- rbind(as.data.frame(t(p.aov.N3[,1])),
                          as.data.frame(t(p.aov.N3[,1])),
                          as.data.frame(t(p.aov.N3[,1])),
                          as.data.frame(t(p.aov.N3[,1])))
names(rezz.boot.a.N3) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.a.N3<- rezz.boot.a.N3 %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')


rezz.boot.a.norm <- rbind(as.data.frame(t(p.aov.norm[,1])),
                          as.data.frame(t(p.aov.norm[,1])),
                          as.data.frame(t(p.aov.norm[,1])),
                          as.data.frame(t(p.aov.norm[,1])))
names(rezz.boot.a.norm) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.a.norm <- rezz.boot.a.norm %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot.a.lnorm <- rbind(as.data.frame(t(p.aov.lnorm[,1])),
                           as.data.frame(t(p.aov.lnorm[,1])),
                           as.data.frame(t(p.aov.lnorm[,1])),
                           as.data.frame(t(p.aov.lnorm[,1])))
names(rezz.boot.a.lnorm) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.a.lnorm <- rezz.boot.a.lnorm %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot.a.chi <- rbind(as.data.frame(t(p.aov.chi[,1])),
                         as.data.frame(t(p.aov.chi[,1])),
                         as.data.frame(t(p.aov.chi[,1])),
                         as.data.frame(t(p.aov.chi[,1])))
names(rezz.boot.a.chi) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.a.chi <- rezz.boot.a.chi %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot.a.N <- rbind(as.data.frame(t(p.aov.N[,1])),
                       as.data.frame(t(p.aov.N[,1])),
                       as.data.frame(t(p.aov.N[,1])),
                       as.data.frame(t(p.aov.N[,1])))
names(rezz.boot.a.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.a.N <- rezz.boot.a.N %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot.a.N2 <- rbind(as.data.frame(t(p.aov.N2[,1])),
                        as.data.frame(t(p.aov.N2[,1])),
                        as.data.frame(t(p.aov.N2[,1])),
                        as.data.frame(t(p.aov.N2[,1])))
names(rezz.boot.a.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.a.N2 <- rezz.boot.a.N2 %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot.a.NU <- rbind(as.data.frame(t(p.aov.NU[,1])),
                        as.data.frame(t(p.aov.NU[,1])),
                        as.data.frame(t(p.aov.NU[,1])),
                        as.data.frame(t(p.aov.NU[,1])))
names(rezz.boot.a.NU) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.a.NU <- rezz.boot.a.NU %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')



# WELCH ANOVA ----
rezz.boot.w.N3 <- rbind(as.data.frame(t(p.aov.N3[,2])),
                       as.data.frame(t(p.aov.N3[,2])),
                       as.data.frame(t(p.aov.N3[,2])),
                       as.data.frame(t(p.aov.N3[,2])))
names(rezz.boot.w.N3) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.w.N3 <- rezz.boot.w.N3 %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'Welch',
                method = 'F')

rezz.boot.w.norm <- rbind(as.data.frame(t(p.aov.norm[,2])),
                          as.data.frame(t(p.aov.norm[,2])),
                          as.data.frame(t(p.aov.norm[,2])),
                          as.data.frame(t(p.aov.norm[,2])))
names(rezz.boot.w.norm) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.w.norm <- rezz.boot.w.norm %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'Welch',
                method = 'F')

rezz.boot.w.lnorm <- rbind(as.data.frame(t(p.aov.lnorm[,2])),
                          as.data.frame(t(p.aov.lnorm[,2])),
                          as.data.frame(t(p.aov.lnorm[,2])),
                          as.data.frame(t(p.aov.lnorm[,2])))
names(rezz.boot.w.lnorm) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.w.lnorm <- rezz.boot.w.lnorm %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'Welch',
                method = 'F')

rezz.boot.w.chi <- rbind(as.data.frame(t(p.aov.chi[,2])),
                          as.data.frame(t(p.aov.chi[,2])),
                          as.data.frame(t(p.aov.chi[,2])),
                          as.data.frame(t(p.aov.chi[,2])))
names(rezz.boot.w.chi) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.w.chi <- rezz.boot.w.chi %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'Welch',
                method = 'F')

rezz.boot.w.N <- rbind(as.data.frame(t(p.aov.N[,2])),
                          as.data.frame(t(p.aov.N[,2])),
                          as.data.frame(t(p.aov.N[,2])),
                          as.data.frame(t(p.aov.N[,2])))
names(rezz.boot.w.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.w.N <- rezz.boot.w.N %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'Welch',
                method = 'F')

rezz.boot.w.N2 <- rbind(as.data.frame(t(p.aov.N2[,2])),
                          as.data.frame(t(p.aov.N2[,2])),
                          as.data.frame(t(p.aov.N2[,2])),
                          as.data.frame(t(p.aov.N2[,2])))
names(rezz.boot.w.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.w.N2 <- rezz.boot.w.N2 %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'Welch',
                method = 'F')

rezz.boot.w.NU <- rbind(as.data.frame(t(p.aov.NU[,2])),
                          as.data.frame(t(p.aov.NU[,2])),
                          as.data.frame(t(p.aov.NU[,2])),
                          as.data.frame(t(p.aov.NU[,2])))
names(rezz.boot.w.NU) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot.w.NU <- rezz.boot.w.NU %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'Welch',
                method = 'F')


# EL TM ANOVA ---- 
rezz.boot.tm.N3.el <- as.data.frame(t(p.el.N3.tm))
names(rezz.boot.tm.N3.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.N3.el <- rezz.boot.tm.N3.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot.tm.norm.el <- as.data.frame(t(p.el.norm.tm))
names(rezz.boot.tm.norm.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.norm.el <- rezz.boot.tm.norm.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot.tm.lnorm.el <- as.data.frame(t(p.el.lnorm.tm))
names(rezz.boot.tm.lnorm.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.lnorm.el <- rezz.boot.tm.lnorm.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot.tm.chi.el <- as.data.frame(t(p.el.chi.tm))
names(rezz.boot.tm.chi.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.chi.el <- rezz.boot.tm.chi.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot.tm.N.el <- as.data.frame(t(p.el.N.tm))
names(rezz.boot.tm.N.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.N.el <- rezz.boot.tm.N.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot.tm.N2.el <- as.data.frame(t(p.el.N2.tm))
names(rezz.boot.tm.N2.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.N2.el <- rezz.boot.tm.N2.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot.tm.NU.el <- as.data.frame(t(p.el.NU.tm))
names(rezz.boot.tm.NU.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.NU.el <- rezz.boot.tm.NU.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')




# YUEN ANOVA ---- 
rezz.boot.tm.N3.y <- as.data.frame(t(p.y.N3.tm))
names(rezz.boot.tm.N3.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.N3.y <- rezz.boot.tm.N3.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')

rezz.boot.tm.norm.y <- as.data.frame(t(p.y.norm.tm))
names(rezz.boot.tm.norm.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.norm.y <- rezz.boot.tm.norm.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 'F')

rezz.boot.tm.lnorm.y <- as.data.frame(t(p.y.lnorm.tm))
names(rezz.boot.tm.lnorm.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.lnorm.y <- rezz.boot.tm.lnorm.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')

rezz.boot.tm.chi.y <- as.data.frame(t(p.y.chi.tm))
names(rezz.boot.tm.chi.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.chi.y <- rezz.boot.tm.chi.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')

rezz.boot.tm.N.y <- as.data.frame(t(p.y.N.tm))
names(rezz.boot.tm.N.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.N.y <- rezz.boot.tm.N.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')

rezz.boot.tm.N2.y <- as.data.frame(t(p.y.N2.tm))
names(rezz.boot.tm.N2.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.N2.y <- rezz.boot.tm.N2.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')

rezz.boot.tm.NU.y <- as.data.frame(t(p.y.NU.tm))
names(rezz.boot.tm.NU.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.tm.NU.y <- rezz.boot.tm.NU.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')



# EL STM ANOVA ----
rezz.boot.stm.N3 <- as.data.frame(t(p.el.N3.stm))
names(rezz.boot.stm.N3) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.stm.N3 <- rezz.boot.stm.N3 %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')

rezz.boot.stm.norm <- as.data.frame(t(p.el.norm.stm))
names(rezz.boot.stm.norm) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.stm.norm <- rezz.boot.stm.norm %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 'el')

rezz.boot.stm.chi <- as.data.frame(t(p.el.chi.stm))
names(rezz.boot.stm.chi) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.stm.chi <- rezz.boot.stm.chi %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')

rezz.boot.stm.lnorm <- as.data.frame(t(p.el.lnorm.stm))
names(rezz.boot.stm.lnorm) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.stm.lnorm <- rezz.boot.stm.lnorm %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')

rezz.boot.stm.N <- as.data.frame(t(p.el.N.stm))
names(rezz.boot.stm.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.stm.N <- rezz.boot.stm.N %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')

rezz.boot.stm.N2 <- as.data.frame(t(p.el.N2.stm))
names(rezz.boot.stm.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.stm.N2 <- rezz.boot.stm.N2 %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')


rezz.boot.stm.NU <- as.data.frame(t(p.el.NU.stm))
names(rezz.boot.stm.NU) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot.stm.NU <- rezz.boot.stm.NU %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')

gamma_colors <- c("0.1" = "darkmagenta", "0.2" = "darkblue", 
                  "0.3" = "forestgreen", "0.4" = "gold4", 
                  "tm" = "deeppink1", 'aov' = 'darkred', 'Welch' = 'slateblue')
g.N1.fnorm <- rezz.boot.stm.norm %>%
  rbind(rezz.boot.tm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.w.norm, 
        rezz.boot.a.norm) %>%
  select(cov_g1 = N1, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)

g.N2.fnorm <- rezz.boot.stm.norm %>%
  rbind(rezz.boot.tm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.w.norm, 
        rezz.boot.a.norm) %>%
  select(cov_g1 = N2, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.046,0.067))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N3.fnorm <- rezz.boot.stm.norm %>%
  rbind(rezz.boot.tm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.w.norm, 
        rezz.boot.a.norm) %>%
  select(cov_g1 = N3, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.046,0.067))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N4.fnorm <- rezz.boot.stm.norm %>%
  rbind(rezz.boot.tm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.w.norm, 
        rezz.boot.a.norm) %>%
  select(cov_g1 = N4, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.046,0.067))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N5.fnorm <- rezz.boot.stm.norm %>%
  rbind(rezz.boot.tm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.w.norm, 
        rezz.boot.a.norm) %>%
  select(cov_g1 = N5, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.046,0.067))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')

g.norm <- ggpubr::ggarrange(g.N2.fnorm, g.N3.fnorm, g.N4.fnorm, g.N5.fnorm,
                            ncol = 2, nrow = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))


g.N2.flnorm <- rezz.boot.stm.lnorm %>%
  rbind(rezz.boot.tm.lnorm.y,
        rezz.boot.tm.lnorm.el,
        rezz.boot.w.lnorm, 
        rezz.boot.a.lnorm) %>%
  select(cov_g1 = N2, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.05))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N3.flnorm <- rezz.boot.stm.lnorm %>%
  rbind(rezz.boot.tm.lnorm.y,
        rezz.boot.tm.lnorm.el,
        rezz.boot.w.lnorm, 
        rezz.boot.a.lnorm) %>%
  select(cov_g1 = N3, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.05))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N4.flnorm <- rezz.boot.stm.lnorm %>%
  rbind(rezz.boot.tm.lnorm.y,
        rezz.boot.tm.lnorm.el,
        rezz.boot.w.lnorm, 
        rezz.boot.a.lnorm) %>%
  select(cov_g1 = N4, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.05))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N5.flnorm <- rezz.boot.stm.lnorm %>%
  rbind(rezz.boot.tm.lnorm.y,
        rezz.boot.tm.lnorm.el,
        rezz.boot.w.lnorm, 
        rezz.boot.a.lnorm) %>%
  select(cov_g1 = N5, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.05))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')

g.lnorm <- ggpubr::ggarrange(g.N2.flnorm, g.N3.flnorm, g.N4.flnorm, g.N5.flnorm,
                            ncol = 2, nrow = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))



g.N2.fchi <- rezz.boot.stm.chi %>%
  rbind(rezz.boot.tm.chi.y,
        rezz.boot.tm.chi.el,
        rezz.boot.w.chi, 
        rezz.boot.a.chi) %>%
  select(cov_g1 = N2, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.044,0.07))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N3.fchi <- rezz.boot.stm.chi %>%
  rbind(rezz.boot.tm.chi.y,
        rezz.boot.tm.chi.el,
        rezz.boot.w.chi, 
        rezz.boot.a.chi) %>%
  select(cov_g1 = N3, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.044,0.07))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N4.fchi <- rezz.boot.stm.chi %>%
  rbind(rezz.boot.tm.chi.y,
        rezz.boot.tm.chi.el,
        rezz.boot.w.chi, 
        rezz.boot.a.chi) %>%
  select(cov_g1 = N4, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.044,0.07))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N5.fchi <- rezz.boot.stm.chi %>%
  rbind(rezz.boot.tm.chi.y,
        rezz.boot.tm.chi.el,
        rezz.boot.w.chi, 
        rezz.boot.a.chi) %>%
  select(cov_g1 = N5, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.044,0.07))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')

g.chi <- ggpubr::ggarrange(g.N2.fchi, g.N3.fchi, g.N4.fchi, g.N5.fchi,
                            ncol = 2, nrow = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))


g.N2.fN <- rezz.boot.stm.N %>%
  rbind(rezz.boot.tm.N.y,
        rezz.boot.tm.N.el,
        rezz.boot.w.N, 
        rezz.boot.a.N) %>%
  select(cov_g1 = N2, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.065))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N3.fN <- rezz.boot.stm.N %>%
  rbind(rezz.boot.tm.N.y,
        rezz.boot.tm.N.el,
        rezz.boot.w.N, 
        rezz.boot.a.N) %>%
  select(cov_g1 = N3, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.065))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N4.fN <- rezz.boot.stm.N %>%
  rbind(rezz.boot.tm.N.y,
        rezz.boot.tm.N.el,
        rezz.boot.w.N, 
        rezz.boot.a.N) %>%
  select(cov_g1 = N4, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.065))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N5.fN <- rezz.boot.stm.N %>%
  rbind(rezz.boot.tm.N.y,
        rezz.boot.tm.N.el,
        rezz.boot.w.N, 
        rezz.boot.a.N) %>%
  select(cov_g1 = N5, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.065))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')

g.N <- ggpubr::ggarrange(g.N2.fN, g.N3.fN, g.N4.fN, g.N5.fN,
                            ncol = 2, nrow = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))


g.N2.fN2 <- rezz.boot.stm.N2 %>%
  rbind(rezz.boot.tm.N2.y,
        rezz.boot.tm.N2.el,
        rezz.boot.w.N2, 
        rezz.boot.a.N2) %>%
  select(cov_g1 = N2, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.067))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N3.fN2 <- rezz.boot.stm.N2 %>%
  rbind(rezz.boot.tm.N2.y,
        rezz.boot.tm.N2.el,
        rezz.boot.w.N2, 
        rezz.boot.a.N2) %>%
  select(cov_g1 = N3, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.067))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N4.fN2 <- rezz.boot.stm.N2 %>%
  rbind(rezz.boot.tm.N2.y,
        rezz.boot.tm.N2.el,
        rezz.boot.w.N2, 
        rezz.boot.a.N2) %>%
  select(cov_g1 = N4, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.067))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N5.fN2 <- rezz.boot.stm.N2 %>%
  rbind(rezz.boot.tm.N2.y,
        rezz.boot.tm.N2.el,
        rezz.boot.w.N2, 
        rezz.boot.a.N2) %>%
  select(cov_g1 = N5, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0,0.067))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')

g.N2 <- ggpubr::ggarrange(g.N2.fN2, g.N3.fN2, g.N4.fN2, g.N5.fN2,
                            ncol = 2, nrow = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))


g.N2.fNU <- rezz.boot.stm.NU %>%
  rbind(rezz.boot.tm.NU.y,
        rezz.boot.tm.NU.el,
        rezz.boot.w.NU, 
        rezz.boot.a.NU) %>%
  select(cov_g1 = N2, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.02,0.07))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N3.fNU <- rezz.boot.stm.NU %>%
  rbind(rezz.boot.tm.NU.y,
        rezz.boot.tm.NU.el,
        rezz.boot.w.NU, 
        rezz.boot.a.NU) %>%
  select(cov_g1 = N3, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.02,0.07))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N4.fNU <- rezz.boot.stm.NU %>%
  rbind(rezz.boot.tm.NU.y,
        rezz.boot.tm.NU.el,
        rezz.boot.w.NU, 
        rezz.boot.a.NU) %>%
  select(cov_g1 = N4, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.02,0.07))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N5.fNU <- rezz.boot.stm.NU %>%
  rbind(rezz.boot.tm.NU.y,
        rezz.boot.tm.NU.el,
        rezz.boot.w.NU, 
        rezz.boot.a.NU) %>%
  select(cov_g1 = N5, alpha, gamma, method ) %>%
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method, group = gamma))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), size = 0.65)+
  geom_hline(aes(yintercept = 0.05), linetype = 2, size = 1) +
  lims(y = c(0.02,0.07))+
  ylab('empirical level')+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')

g.NU <- ggpubr::ggarrange(g.N2.fNU, g.N3.fNU, g.N4.fNU, g.N5.fNU,
                            ncol = 2, nrow = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))


g.norm
ggsave('anova_norm.png', width = 10, height = 7, bg = 'white')

g.lnorm
ggsave('anova_lnorm.png', width = 10, height = 7, bg = 'white')

g.chi
ggsave('anova_chi.png', width = 10, height = 7, bg = 'white')

g.N
ggsave('anova_N.png', width = 10, height = 7, bg = 'white')

g.N2
ggsave('anova_N2.png', width = 10, height = 7, bg = 'white')

g.NU
ggsave('anova_NU.png', width = 10, height = 7, bg = 'white')
NN

rez_norm <- rezz.boot.stm.norm %>%
  mutate(method = 'EL stm') %>% 
  rbind(rezz.boot.tm.norm.y %>% 
        mutate(method = 'Yuen',
               gamma = NA) ,
        rezz.boot.tm.norm.el %>% 
          mutate(method = 'EL tm',
                 gamma = NA),
        rezz.boot.w.norm %>% 
          mutate(method = 'Welch',
                 gamma = NA), 
        rezz.boot.a.norm %>% 
          mutate(method = 'F-test',
                 gamma = NA)) %>% 
  mutate(n = 50,id = n()) %>% 
  select(n, alpha, gamma,method,N2 ) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N2) %>% 
  rbind(
    rezz.boot.stm.norm %>%
          mutate(method = 'EL stm') %>% 
          rbind(rezz.boot.tm.norm.y %>% 
                  mutate(method = 'Yuen',
                         gamma = NA) ,
                rezz.boot.tm.norm.el %>% 
                  mutate(method = 'EL tm',
                         gamma = NA),
                rezz.boot.w.norm %>% 
                  mutate(method = 'Welch',
                         gamma = NA
                         ), 
                rezz.boot.a.norm %>% 
                  mutate(method = 'F-test',
                         gamma = NA
                         )) %>% 
          mutate(n = 100) %>% 
          select(n, alpha, gamma,method,N3 ) %>% 
          pivot_wider(names_from = c(alpha,method), values_from = N3) ,
    rezz.boot.stm.norm %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.norm.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.norm.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.norm %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.norm %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 200) %>% 
      select(n, alpha, gamma,method,N4 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N4) ,
    rezz.boot.stm.norm %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.norm.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.norm.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.norm %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.norm %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 500) %>% 
      select(n, alpha, gamma,method,N5 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N5) )


rez_chi <- rezz.boot.stm.chi %>%
  mutate(method = 'EL stm') %>% 
  rbind(rezz.boot.tm.chi.y %>% 
          mutate(method = 'Yuen',
                 gamma = NA) ,
        rezz.boot.tm.chi.el %>% 
          mutate(method = 'EL tm',
                 gamma = NA),
        rezz.boot.w.chi %>% 
          mutate(method = 'Welch',
                 gamma = NA
                 ), 
        rezz.boot.a.chi %>% 
          mutate(method = 'F-test',
                 gamma = NA
                 )) %>% 
  mutate(n = 50) %>% 
  select(n, alpha, gamma,method,N2 ) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N2) %>% 
  rbind(
    rezz.boot.stm.chi %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.chi.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.chi.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.chi %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.chi %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 100) %>% 
      select(n, alpha, gamma,method,N3 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N3) ,
    rezz.boot.stm.chi %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.chi.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.chi.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.chi %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.chi %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 200) %>% 
      select(n, alpha, gamma,method,N4 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N4) ,
    rezz.boot.stm.chi %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.chi.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.chi.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.chi %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.chi %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 500) %>% 
      select(n, alpha, gamma,method,N5 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N5) )


rez_N <- rezz.boot.stm.N %>%
  mutate(method = 'EL stm') %>% 
  rbind(rezz.boot.tm.N.y %>% 
          mutate(method = 'Yuen',
                 gamma = NA) ,
        rezz.boot.tm.N.el %>% 
          mutate(method = 'EL tm',
                 gamma = NA),
        rezz.boot.w.N %>% 
          mutate(method = 'Welch',
                 gamma = NA
                 ), 
        rezz.boot.a.N %>% 
          mutate(method = 'F-test',
                 gamma = NA
                 )) %>% 
  mutate(n = 50) %>% 
  select(n, alpha, gamma,method,N2 ) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N2) %>% 
  rbind(
    rezz.boot.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 100) %>% 
      select(n, alpha, gamma,method,N3 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N3) ,
    rezz.boot.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 200) %>% 
      select(n, alpha, gamma,method,N4 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N4) ,
    rezz.boot.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 500) %>% 
      select(n, alpha, gamma,method,N5 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N5) )


rez_N2 <- rezz.boot.stm.N2 %>%
  mutate(method = 'EL stm') %>% 
  rbind(rezz.boot.tm.N2.y %>% 
          mutate(method = 'Yuen',
                 gamma = NA) ,
        rezz.boot.tm.N2.el %>% 
          mutate(method = 'EL tm',
                 gamma = NA),
        rezz.boot.w.N2 %>% 
          mutate(method = 'Welch',
                 gamma = NA
                 ), 
        rezz.boot.a.N2 %>% 
          mutate(method = 'F-test',
                 gamma = NA
                 )) %>% 
  mutate(n = 50) %>% 
  select(n, alpha, gamma,method,N2 ) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N2) %>% 
  rbind(
    rezz.boot.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 100) %>% 
      select(n, alpha, gamma,method,N3 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N3) ,
    rezz.boot.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 200) %>% 
      select(n, alpha, gamma,method,N4 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N4) ,
    rezz.boot.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
                     ), 
            rezz.boot.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
                     )) %>% 
      mutate(n = 500) %>% 
      select(n, alpha, gamma,method,N5 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N5) )


openxlsx::write.xlsx(rez_norm, 'norm.xlsx')
openxlsx::write.xlsx(rez_chi, 'chi.xlsx')
openxlsx::write.xlsx(rez_N, 'N.xlsx')
openxlsx::write.xlsx(rez_N2, 'N2.xlsx')

alpha <- c(0.05, 0.1, 0.15, 0.2)
gamma <- c(0.1, 0.2, 0.3, 0.4)
NN <- c(30, 50, 100, 200, 500)
d1 <- c(1,2,3)
d2 <- c(1,1,6)
alph_gam <- as.data.frame(cbind(c(rep(alpha[1],4),rep(alpha[2],3), rep(alpha[3],3), rep(alpha[4],2)),
                                c(gamma, gamma[-1], gamma[-1], gamma[-(1:2)])))

names(alph_gam) <- c('alpha', 'gamma')  

iter <- 10000

p.aov1.N <- matrix(ncol = 2, nrow = length(NN))
p.aov1.N2 <- matrix(ncol = 2, nrow = length(NN))
p.aov2.N <- matrix(ncol = 2, nrow = length(NN))
p.aov2.N2 <- matrix(ncol = 2, nrow = length(NN))

p.y1.N.tm <- matrix(ncol = 4, nrow = length(NN))
p.y1.N2.tm <- matrix(ncol = 4, nrow = length(NN))
p.y2.N.tm <- matrix(ncol = 4, nrow = length(NN))
p.y2.N2.tm <- matrix(ncol = 4, nrow = length(NN))

p.el1.N.tm <- matrix(ncol = 4, nrow = length(NN))
p.el1.N2.tm <- matrix(ncol = 4, nrow = length(NN))
p.el2.N.tm <- matrix(ncol = 4, nrow = length(NN))
p.el2.N2.tm <- matrix(ncol = 4, nrow = length(NN))

p.el1.N.stm <- matrix(ncol = 12, nrow = length(NN))
p.el1.N2.stm <- matrix(ncol = 12, nrow = length(NN))
p.el2.N.stm <- matrix(ncol = 12, nrow = length(NN))
p.el2.N2.stm <- matrix(ncol = 12, nrow = length(NN))
# n <- 1
NN <- c(30, 50, 100, 200, 500)
set.seed(1)
for(n in 1:length(NN)){
  
  temp.aov1.N <- matrix(ncol = 2, nrow = iter)
  temp.aov1.N2 <- matrix(ncol = 2, nrow = iter)
  temp.aov2.N <- matrix(ncol = 2, nrow = iter)
  temp.aov2.N2 <- matrix(ncol = 2, nrow = iter)
  
  temp.y1.N.tm <- matrix(ncol = 4, nrow = iter)
  temp.y1.N2.tm <- matrix(ncol = 4, nrow = iter)
  temp.y2.N.tm <- matrix(ncol = 4, nrow = iter)
  temp.y2.N2.tm <- matrix(ncol = 4, nrow = iter)
  

  temp.el1.N.tm <- matrix(ncol = 4, nrow = iter)
  temp.el1.N2.tm <- matrix(ncol = 4, nrow = iter)
  temp.el2.N.tm <- matrix(ncol = 4, nrow = iter)
  temp.el2.N2.tm <- matrix(ncol = 4, nrow = iter)
  
  temp.el1.N.stm <- matrix(ncol = 12, nrow = iter)
  temp.el1.N2.stm <- matrix(ncol = 12, nrow = iter)
  temp.el2.N.stm <- matrix(ncol = 12, nrow = iter)
  temp.el2.N2.stm <- matrix(ncol = 12, nrow = iter)
  
  for(i in 1:iter){
    ww1.N <- list(d1[1]*replicate(NN[n], gen.N()), 
                 d1[2]*replicate(NN[n], gen.N()), 
                 d1[3]*replicate(NN[n], gen.N()))
    
    ww2.N <- list(d2[1]*replicate(NN[n], gen.N()), 
                  d2[2]*replicate(NN[n], gen.N()), 
                  d2[3]*replicate(NN[n], gen.N()))
    
    ww1.N2 <- list(d1[1]*replicate(NN[n], gen.N2()), 
                  d1[2]*replicate(NN[n], gen.N2()), 
                  d1[3]*replicate(NN[n], gen.N2()))
    
    ww2.N2 <- list(d2[1]*replicate(NN[n], gen.N2()), 
                  d2[2]*replicate(NN[n], gen.N2()), 
                  d2[3]*replicate(NN[n], gen.N2()))
    
   
    dati1.N <- data.frame(data = c(ww1.N[[1]], ww1.N[[2]], ww1.N[[3]]), 
                         gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati2.N <- data.frame(data = c(ww2.N[[1]], ww2.N[[2]], ww2.N[[3]]), 
                          gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    
    dati1.N2 <- data.frame(data = c(ww1.N2[[1]], ww1.N2[[2]], ww1.N2[[3]]), 
                          gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    dati2.N2 <- data.frame(data = c(ww2.N2[[1]], ww2.N2[[2]], ww2.N2[[3]]), 
                          gr = c(rep(1, NN[n]), rep(2,NN[n]), rep(3,NN[n])))
    
    temp.aov1.N[i,1] <- summary(aov(data ~ gr, data = dati1.N))[[1]]$`Pr(>F)`[1]
    temp.aov2.N[i,1] <- summary(aov(data ~ gr, data = dati2.N))[[1]]$`Pr(>F)`[1]
    temp.aov1.N2[i,1] <- summary(aov(data ~ gr, data = dati1.N2))[[1]]$`Pr(>F)`[1]
    temp.aov2.N2[i,1] <- summary(aov(data ~ gr, data = dati2.N2))[[1]]$`Pr(>F)`[1]
    
    
    temp.aov1.N[i,2] <- welch_anova_test(dati1.N, data ~ gr)$p
    temp.aov2.N[i,2] <- welch_anova_test(dati2.N, data ~ gr)$p
    temp.aov1.N2[i,2] <- welch_anova_test(dati1.N2, data ~ gr)$p
    temp.aov2.N2[i,2] <- welch_anova_test(dati2.N2, data ~ gr)$p
    
    temp.alph1.N.el <- matrix(ncol = 12, nrow = 1)
    temp.alph1.N2.el <- matrix(ncol = 12, nrow = 1)
    temp.alph2.N.el <- matrix(ncol = 12, nrow = 1)
    temp.alph2.N2.el <- matrix(ncol = 12, nrow = 1)
    
    for(a in 1:length(alpha)){
      # a <- 1
      
      temp.y1.N.tm[i,a] <- t1way(data ~ gr, data = dati1.N, nboot = 1, tr = alpha[a])$p.value
      temp.y2.N.tm[i,a] <- t1way(data ~ gr, data = dati2.N, nboot = 1, tr = alpha[a])$p.value
      
      temp.y1.N2.tm[i,a] <- t1way(data ~ gr, data = dati1.N2, nboot = 1, tr = alpha[a])$p.value
      temp.y2.N2.tm[i,a] <- t1way(data ~ gr, data = dati2.N2, nboot = 1, tr = alpha[a])$p.value
      
      temp.el1.N.tm[i,a] <- EL.anova.tm(ww1.N, alpha = alpha[a], beta = alpha[a] )$p.value
      temp.el2.N.tm[i,a] <- EL.anova.tm(ww2.N, alpha = alpha[a], beta = alpha[a] )$p.value
      
      temp.el1.N2.tm[i,a] <- EL.anova.tm(ww1.N2, alpha = alpha[a], beta = alpha[a] )$p.value
      temp.el2.N2.tm[i,a] <- EL.anova.tm(ww2.N2, alpha = alpha[a], beta = alpha[a] )$p.value
      
      
      gam <- alph_gam[alph_gam$alpha == alpha[a],]$gamma
      
      if(a == 1){
        indx <- 1
      }else if(a == 2){
        indx <- 5
      }else if(a == 3){
        indx <- 8
      }else{
        indx <- 11
      }
      
      temp.gam1.N.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam1.N2.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam2.N.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam2.N2.el <- matrix(ncol = length(gam), nrow = 1)
      
      for(g in 1:length(gam)){
        
        temp.gam1.N2.el[1,g] <- EL.anova.stm(ww1.N2, alpha[a], gam[g])$p.value
        temp.gam1.N.el[1,g] <- EL.anova.stm(ww1.N, alpha[a], gam[g])$p.value
        temp.gam2.N2.el[1,g] <- EL.anova.stm(ww2.N2, alpha[a], gam[g])$p.value
        temp.gam2.N.el[1,g] <- EL.anova.stm(ww2.N, alpha[a], gam[g])$p.value
        
      }
      
      temp.alph1.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam1.N.el
      temp.alph1.N2.el[1,indx:(length(gam)+indx - 1)] <- temp.gam1.N2.el
      temp.alph2.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam2.N.el
      temp.alph2.N2.el[1,indx:(length(gam)+indx - 1)] <- temp.gam2.N2.el
      
      # print(temp.alph.N.el)
      # print(indx:(length(gam)+indx - 1))
    }
    
    temp.el1.N.stm[i,] <- temp.alph1.N.el
    temp.el2.N.stm[i,] <- temp.alph2.N.el
    temp.el1.N2.stm[i,] <- temp.alph1.N2.el
    temp.el2.N2.stm[i,] <- temp.alph2.N2.el
    
    print(paste('i: ', i, '; ', 'n: ', n, sep = ''))
  }
  
  p.aov1.N[n,1] <- mean(temp.aov1.N[,1] < 0.05)
  p.aov1.N2[n,1] <- mean(temp.aov1.N2[,1] < 0.05)
  p.aov2.N[n,1] <- mean(temp.aov2.N[,1] < 0.05)
  p.aov2.N2[n,1] <- mean(temp.aov2.N2[,1] < 0.05)
  
  p.aov1.N[n,2] <- mean(temp.aov1.N[,2] < 0.05)
  p.aov1.N2[n,2] <- mean(temp.aov1.N2[,2] < 0.05)
  p.aov2.N[n,2] <- mean(temp.aov2.N[,2] < 0.05)
  p.aov2.N2[n,2] <- mean(temp.aov2.N2[,2] < 0.05)
  
  
  p.el1.N.tm[n,] <- colMeans(temp.el1.N.tm< 0.05, na.rm = TRUE)
  p.el1.N2.tm[n,] <- colMeans(temp.el1.N2.tm< 0.05, na.rm = TRUE)
  p.el2.N.tm[n,] <- colMeans(temp.el2.N.tm< 0.05, na.rm = TRUE)
  p.el2.N2.tm[n,] <- colMeans(temp.el2.N2.tm< 0.05, na.rm = TRUE)
  
  p.y1.N.tm[n,] <- colMeans(temp.y1.N.tm< 0.05, na.rm = TRUE)
  p.y1.N2.tm[n,] <- colMeans(temp.y1.N2.tm< 0.05, na.rm = TRUE)
  p.y2.N.tm[n,] <- colMeans(temp.y2.N.tm< 0.05, na.rm = TRUE)
  p.y2.N2.tm[n,] <- colMeans(temp.y2.N2.tm< 0.05, na.rm = TRUE)
  
  p.el1.N.stm[n,] <- colMeans(temp.el1.N.stm< 0.05, na.rm = TRUE)
  p.el1.N2.stm[n,] <- colMeans(temp.el1.N2.stm< 0.05, na.rm = TRUE)
  p.el2.N.stm[n,] <- colMeans(temp.el2.N.stm< 0.05, na.rm = TRUE)
  p.el2.N2.stm[n,] <- colMeans(temp.el2.N2.stm< 0.05, na.rm = TRUE)
  
}

rezz.boot1.stm.N <- as.data.frame(t(p.el1.N.stm))
names(rezz.boot1.stm.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot1.stm.N <- rezz.boot1.stm.N %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')

rezz.boot1.stm.N2 <- as.data.frame(t(p.el1.N2.stm))
names(rezz.boot1.stm.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot1.stm.N2 <- rezz.boot1.stm.N2 %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')

rezz.boot2.stm.N <- as.data.frame(t(p.el2.N.stm))
names(rezz.boot2.stm.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot2.stm.N <- rezz.boot2.stm.N %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')

rezz.boot2.stm.N2 <- as.data.frame(t(p.el2.N2.stm))
names(rezz.boot2.stm.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot2.stm.N2 <- rezz.boot2.stm.N2 %>%
  dplyr::mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
                gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
                method = 'el')



rezz.boot1.tm.N.y <- as.data.frame(t(p.y1.N.tm))
names(rezz.boot1.tm.N.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot1.tm.N.y <- rezz.boot1.tm.N.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')

rezz.boot1.tm.N2.y <- as.data.frame(t(p.y1.N2.tm))
names(rezz.boot1.tm.N2.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot1.tm.N2.y <- rezz.boot1.tm.N2.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')

rezz.boot2.tm.N.y <- as.data.frame(t(p.y2.N.tm))
names(rezz.boot2.tm.N.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot2.tm.N.y <- rezz.boot2.tm.N.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')

rezz.boot2.tm.N2.y <- as.data.frame(t(p.y2.N2.tm))
names(rezz.boot2.tm.N2.y) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot2.tm.N2.y <- rezz.boot2.tm.N2.y %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'F')


rezz.boot1.tm.N.el <- as.data.frame(t(p.el1.N.tm))
names(rezz.boot1.tm.N.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot1.tm.N.el <- rezz.boot1.tm.N.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot1.tm.N2.el <- as.data.frame(t(p.el1.N2.tm))
names(rezz.boot1.tm.N2.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot1.tm.N2.el <- rezz.boot1.tm.N2.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot2.tm.N.el <- as.data.frame(t(p.el2.N.tm))
names(rezz.boot2.tm.N.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot2.tm.N.el <- rezz.boot2.tm.N.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot2.tm.N2.el <- as.data.frame(t(p.el2.N2.tm))
names(rezz.boot2.tm.N2.el) <- c('N1', 'N2', 'N3', 'N4', 'N5')
rezz.boot2.tm.N2.el <- rezz.boot2.tm.N2.el %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'tm',
                method = 'el')

rezz.boot1.a.N <- rbind(as.data.frame(t(p.aov1.N[,1])),
                       as.data.frame(t(p.aov1.N[,1])),
                       as.data.frame(t(p.aov1.N[,1])),
                       as.data.frame(t(p.aov1.N[,1])))
names(rezz.boot1.a.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot1.a.N <- rezz.boot1.a.N %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot1.a.N2 <- rbind(as.data.frame(t(p.aov1.N2[,1])),
                        as.data.frame(t(p.aov1.N2[,1])),
                        as.data.frame(t(p.aov1.N2[,1])),
                        as.data.frame(t(p.aov1.N2[,1])))
names(rezz.boot1.a.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot1.a.N2 <- rezz.boot1.a.N2 %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')



rezz.boot2.a.N <- rbind(as.data.frame(t(p.aov2.N[,1])),
                        as.data.frame(t(p.aov2.N[,1])),
                        as.data.frame(t(p.aov2.N[,1])),
                        as.data.frame(t(p.aov2.N[,1])))
names(rezz.boot2.a.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot2.a.N <- rezz.boot2.a.N %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot2.a.N2 <- rbind(as.data.frame(t(p.aov2.N2[,1])),
                         as.data.frame(t(p.aov2.N2[,1])),
                         as.data.frame(t(p.aov2.N2[,1])),
                         as.data.frame(t(p.aov2.N2[,1])))
names(rezz.boot2.a.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot2.a.N2 <- rezz.boot2.a.N2 %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')




rezz.boot1.w.N <- rbind(as.data.frame(t(p.aov1.N[,2])),
                        as.data.frame(t(p.aov1.N[,2])),
                        as.data.frame(t(p.aov1.N[,2])),
                        as.data.frame(t(p.aov1.N[,2])))
names(rezz.boot1.w.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot1.w.N <- rezz.boot1.w.N %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot1.w.N2 <- rbind(as.data.frame(t(p.aov1.N2[,2])),
                         as.data.frame(t(p.aov1.N2[,2])),
                         as.data.frame(t(p.aov1.N2[,2])),
                         as.data.frame(t(p.aov1.N2[,2])))
names(rezz.boot1.w.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot1.w.N2 <- rezz.boot1.w.N2 %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')



rezz.boot2.w.N <- rbind(as.data.frame(t(p.aov2.N[,2])),
                        as.data.frame(t(p.aov2.N[,2])),
                        as.data.frame(t(p.aov2.N[,2])),
                        as.data.frame(t(p.aov2.N[,2])))
names(rezz.boot2.w.N) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot2.w.N <- rezz.boot2.w.N %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')

rezz.boot2.w.N2 <- rbind(as.data.frame(t(p.aov2.N2[,2])),
                         as.data.frame(t(p.aov2.N2[,2])),
                         as.data.frame(t(p.aov2.N2[,2])),
                         as.data.frame(t(p.aov2.N2[,2])))
names(rezz.boot2.w.N2) <- c('N1', 'N2', 'N3', 'N4', 'N5')

rezz.boot2.w.N2 <- rezz.boot2.w.N2 %>% 
  dplyr::mutate(alpha = as.character(alpha),
                gamma = 'aov',
                method = 'F')



rez1_N <- rezz.boot1.stm.N %>%
  mutate(method = 'EL stm') %>% 
  rbind(rezz.boot1.tm.N.y %>% 
          mutate(method = 'Yuen',
                 gamma = NA) ,
        rezz.boot1.tm.N.el %>% 
          mutate(method = 'EL tm',
                 gamma = NA),
        rezz.boot1.w.N %>% 
          mutate(method = 'Welch',
                 gamma = NA
          ), 
        rezz.boot1.a.N %>% 
          mutate(method = 'F-test',
                 gamma = NA
          )) %>% 
  mutate(n = 50) %>% 
  select(n, alpha, gamma,method,N1 ) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N1) %>% 
  rbind(
    rezz.boot1.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot1.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot1.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot1.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot1.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 100) %>% 
      select(n, alpha, gamma,method,N2 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N2) ,
    rezz.boot1.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot1.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot1.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot1.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot1.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 200) %>% 
      select(n, alpha, gamma,method,N3 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N3) ,
    rezz.boot1.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot1.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot1.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot1.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot1.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 500) %>% 
      select(n, alpha, gamma,method,N4 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N4) )


rez1_N2 <- rezz.boot1.stm.N2 %>%
  mutate(method = 'EL stm') %>% 
  rbind(rezz.boot1.tm.N2.y %>% 
          mutate(method = 'Yuen',
                 gamma = NA) ,
        rezz.boot1.tm.N2.el %>% 
          mutate(method = 'EL tm',
                 gamma = NA),
        rezz.boot1.w.N2 %>% 
          mutate(method = 'Welch',
                 gamma = NA
          ), 
        rezz.boot1.a.N2 %>% 
          mutate(method = 'F-test',
                 gamma = NA
          )) %>% 
  mutate(n = 50) %>% 
  select(n, alpha, gamma,method,N1 ) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N1) %>% 
  rbind(
    rezz.boot1.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot1.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot1.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot1.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot1.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 100) %>% 
      select(n, alpha, gamma,method,N2 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N2) ,
    rezz.boot1.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot1.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot1.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot1.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot1.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 200) %>% 
      select(n, alpha, gamma,method,N3 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N3) ,
    rezz.boot1.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot1.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot1.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot1.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot1.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 500) %>% 
      select(n, alpha, gamma,method,N4 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N4) )



rez2_N <- rezz.boot2.stm.N %>%
  mutate(method = 'EL stm') %>% 
  rbind(rezz.boot2.tm.N.y %>% 
          mutate(method = 'Yuen',
                 gamma = NA) ,
        rezz.boot2.tm.N.el %>% 
          mutate(method = 'EL tm',
                 gamma = NA),
        rezz.boot2.w.N %>% 
          mutate(method = 'Welch',
                 gamma = NA
          ), 
        rezz.boot2.a.N %>% 
          mutate(method = 'F-test',
                 gamma = NA
          )) %>% 
  mutate(n = 50) %>% 
  select(n, alpha, gamma,method,N1 ) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N1) %>% 
  rbind(
    rezz.boot2.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot2.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot2.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot2.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot2.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 100) %>% 
      select(n, alpha, gamma,method,N2 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N2) ,
    rezz.boot2.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot2.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot2.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot2.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot2.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 200) %>% 
      select(n, alpha, gamma,method,N3 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N3) ,
    rezz.boot2.stm.N %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot2.tm.N.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot2.tm.N.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot2.w.N %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot2.a.N %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 500) %>% 
      select(n, alpha, gamma,method,N4 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N4) )


rez2_N2 <- rezz.boot2.stm.N2 %>%
  mutate(method = 'EL stm') %>% 
  rbind(rezz.boot2.tm.N2.y %>% 
          mutate(method = 'Yuen',
                 gamma = NA) ,
        rezz.boot2.tm.N2.el %>% 
          mutate(method = 'EL tm',
                 gamma = NA),
        rezz.boot2.w.N2 %>% 
          mutate(method = 'Welch',
                 gamma = NA
          ), 
        rezz.boot2.a.N2 %>% 
          mutate(method = 'F-test',
                 gamma = NA
          )) %>% 
  mutate(n = 50) %>% 
  select(n, alpha, gamma,method,N1 ) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N1) %>% 
  rbind(
    rezz.boot2.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot2.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot2.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot2.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot2.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 100) %>% 
      select(n, alpha, gamma,method,N2 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N2) ,
    rezz.boot2.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot2.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot2.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot2.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot2.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 200) %>% 
      select(n, alpha, gamma,method,N3 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N3) ,
    rezz.boot2.stm.N2 %>%
      mutate(method = 'EL stm') %>% 
      rbind(rezz.boot2.tm.N2.y %>% 
              mutate(method = 'Yuen',
                     gamma = NA) ,
            rezz.boot2.tm.N2.el %>% 
              mutate(method = 'EL tm',
                     gamma = NA),
            rezz.boot2.w.N2 %>% 
              mutate(method = 'Welch',
                     gamma = NA
              ), 
            rezz.boot2.a.N2 %>% 
              mutate(method = 'F-test',
                     gamma = NA
              )) %>% 
      mutate(n = 500) %>% 
      select(n, alpha, gamma,method,N4 ) %>% 
      pivot_wider(names_from = c(alpha,method), values_from = N4) )




openxlsx::write.xlsx(rez1_N, "anova1_N.xlsx")
openxlsx::write.xlsx(rez2_N, "anova2_N.xlsx")
openxlsx::write.xlsx(rez1_N2, "anova1_N2.xlsx")
openxlsx::write.xlsx(rez2_N2, "anova2_N2.xlsx")

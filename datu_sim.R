source('funkcijas.R')
library(WRS2)
library(tidyverse)

# alpha <- 0.05
# gamma <- 0.1


### DATA SIMULATIONS---- 
NN <- c(50,100,200,500)
alpha <- c(0.05,0.1,0.15,0.2)
gamma <- c(0.1,0.2,0.3,0.4)

alph_gam <- as.data.frame(cbind(c(rep(alpha[1],4),rep(alpha[2],3), rep(alpha[3],3), rep(alpha[4],2)),
                                c(gamma, gamma[-1], gamma[-1], gamma[-(1:2)])))

names(alph_gam) <- c('alpha', 'gamma')


stm.norm.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.t.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.chi.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N2.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.NU.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))


iter <- 1000
set.seed(1)

b4 <- Sys.time()

for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE n
  
  
  stm.temp.N.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.N2.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.NU.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.norm.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.t.el <- matrix(ncol = 12, nrow = iter)
  stm.temp.chi.el <- matrix(ncol = 12, nrow = iter)
  
  
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    ### GENERATING DATA
    x.N <- replicate(NN[n], gen.N())
    y.N <- replicate(NN[n], gen.N())
    
    x.N2 <- replicate(NN[n], gen.N2())
    y.N2 <- replicate(NN[n], gen.N2())
    
    x.NU <- replicate(NN[n], gen.NU())
    y.NU <- replicate(NN[n], gen.NU())
    
    x.norm <- rnorm(NN[n])
    y.norm <- rnorm(NN[n])
    
    x.t <- rt(NN[n], df = 2)
    y.t <- rt(NN[n], df = 2)
    
    x.chi <- rchisq(NN[n], df = 3)
    y.chi <- rchisq(NN[n], df = 3)
    ### END
    
    ### TRANSOFRMING DATA FOR yuen.tm() FUNNCTION
    
    temp.alph.N.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.N2.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.NU.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.norm.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.t.el <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.el <- matrix(ncol = 12, nrow = 1)
    ### END
    
    for(a in 1:length(alpha)){ ### LOOP FOR ALPHAS

      
      
      ### FIND VALID GAMMAS FOR GIVEN ALPHA
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
      ### END
      

      temp.gam.N.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N2.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.NU.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.norm.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.t.el <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.el <- matrix(ncol = length(gam), nrow = 1)
      
      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS
        
        
        ci.N.el <- EL.stm(x.N,y.N, alpha = alpha[a], gamma = gam[g])$conf.int
        ci.N2.el <- EL.stm(x.N2,y.N2, alpha = alpha[a], gamma = gam[g])$conf.int
        ci.NU.el <- EL.stm(x.NU,y.NU, alpha = alpha[a], gamma = gam[g])$conf.int
        ci.norm.el <- EL.stm(x.norm,y.norm, alpha = alpha[a], gamma = gam[g])$conf.int
        ci.t.el <- EL.stm(x.t,y.t, alpha[a], alpha = alpha[a], gamma = gam[g])$conf.int
        ci.chi.el <- EL.stm(x.chi,y.chi, alpha = alpha[a], gamma = gam[g])$conf.int
        
        
        temp.gam.N.el[1,g] <- coverage(0, ci.N.el[1], ci.N.el[2])
        temp.gam.N2.el[1,g] <- coverage(0, ci.N2.el[1], ci.N2.el[2])
        temp.gam.NU.el[1,g] <- coverage(0, ci.NU.el[1], ci.NU.el[2])
        temp.gam.norm.el[1,g] <- coverage(0, ci.norm.el[1], ci.norm.el[2])
        temp.gam.t.el[1,g] <- coverage(0, ci.t.el[1], ci.t.el[2])
        temp.gam.chi.el[1,g] <- coverage(0, ci.chi.el[1], ci.chi.el[2])
        
      } ### LOOP FOR GAMMAS ENDS
      
      
      temp.alph.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.el
      temp.alph.N2.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N2.el
      temp.alph.NU.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.NU.el
      temp.alph.norm.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.el
      temp.alph.t.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.el
      temp.alph.chi.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.el
      
    } ### LOOP FOR ALPHAS ENDS
    
    
    stm.temp.N.el[i,] <- temp.alph.N.el
    stm.temp.N2.el[i,] <- temp.alph.N2.el
    stm.temp.NU.el[i,] <- temp.alph.NU.el
    stm.temp.norm.el[i,] <- temp.alph.norm.el
    stm.temp.t.el[i,] <- temp.alph.t.el
    stm.temp.chi.el[i,] <- temp.alph.chi.el
    
    print(paste('i: ', i, '; n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  
  stm.N.el[n,] <- colMeans(stm.temp.N.el, na.rm = TRUE)
  stm.N2.el[n,] <- colMeans(stm.temp.N2.el, na.rm = TRUE)
  stm.NU.el[n,] <- colMeans(stm.temp.NU.el, na.rm = TRUE)
  stm.norm.el[n,] <- colMeans(stm.temp.norm.el, na.rm = TRUE)
  stm.t.el[n,] <- colMeans(stm.temp.t.el, na.rm = TRUE)
  stm.chi.el[n,] <- colMeans(stm.temp.chi.el, na.rm = TRUE)
  
} ### LOOP FOR SAMPLE SIZE n ENDS
now <- Sys.time() 

difftime(now, b4)



NN <- c(50,100,200,500)
alpha <- c(0.05,0.1,0.15,0.2)
gamma <- c(0.1,0.2,0.3,0.4)

alph_gam <- as.data.frame(cbind(c(rep(alpha[1],4),rep(alpha[2],3), rep(alpha[3],3), rep(alpha[4],2)),
                                c(gamma, gamma[-1], gamma[-1], gamma[-(1:2)])))

names(alph_gam) <- c('alpha', 'gamma')


tm.norm.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.t.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.chi.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N2.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.NU.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))

tm.norm.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.t.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.chi.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N2.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.NU.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))

stm.norm.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.t.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.chi.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N2.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.NU.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))

iter <- 10000
set.seed(1)

b4 <- Sys.time()

for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE n
  
  tm.temp.norm.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.t.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.chi.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.N.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.N2.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.NU.el <- matrix(ncol = 4, nrow = iter)

  tm.temp.norm.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.t.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.chi.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.N.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.N2.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.NU.y <- matrix(ncol = 4, nrow = iter)

  stm.temp.N.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.N2.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.NU.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.norm.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.t.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.chi.y <- matrix(ncol = 12, nrow = iter)

  
  
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    ### GENERATING DATA
    x.N <- replicate(NN[n], gen.N())
    y.N <- replicate(NN[n], gen.N())
    
    x.N2 <- replicate(NN[n], gen.N2())
    y.N2 <- replicate(NN[n], gen.N2())
    
    x.NU <- replicate(NN[n], gen.NU())
    y.NU <- replicate(NN[n], gen.NU())
    
    x.norm <- rnorm(NN[n])
    y.norm <- rnorm(NN[n])
    
    x.t <- rt(NN[n], df = 2)
    y.t <- rt(NN[n], df = 2)
    
    x.chi <- rchisq(NN[n], df = 3)
    y.chi <- rchisq(NN[n], df = 3)
    ### END
    
    ### TRANSOFRMING DATA FOR yuen.tm() FUNNCTION
    gr <- c(rep(1, NN[n]), rep(2, NN[n]))

    data.N <- data.frame(data = c(x.N,y.N), gr = gr)
    data.N2 <- data.frame(data = c(x.N2,y.N2), gr = gr)
    data.NU <- data.frame(data = c(x.NU,y.NU), gr = gr)

    data.norm <- data.frame(data = c(x.norm,y.norm), gr = gr)
    data.t <- data.frame(data = c(x.t,y.t), gr = gr)
    data.chi <- data.frame(data = c(x.chi,y.chi), gr = gr)
    ### END

    ## FOR SMOOTHLY TRIMMED MEAN
    temp.alph.N.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.N2.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.NU.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.norm.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.t.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.y <- matrix(ncol = 12, nrow = 1)
    
    ### END
    
    for(a in 1:length(alpha)){ ### LOOP FOR ALPHAS
      
      ci.N.y <- yuen.tm(data ~ gr, data = data.N, tr = alpha[a])$conf.int
      ci.N2.y <- yuen.tm(data ~ gr, data = data.N2, tr = alpha[a])$conf.int
      ci.NU.y <- yuen.tm(data ~ gr, data = data.NU, tr = alpha[a])$conf.int
      ci.norm.y <- yuen.tm(data ~ gr, data = data.norm, tr = alpha[a])$conf.int
      ci.t.y <- yuen.tm(data ~ gr, data = data.t, tr = alpha[a])$conf.int
      ci.chi.y <- yuen.tm(data ~ gr, data = data.chi, tr = alpha[a])$conf.int

      ci.N.el <- EL.tm(x.N, y.N, alpha = alpha[a], beta = alpha[a])$conf.int
      ci.N2.el <- EL.tm(x.N2, y.N2, alpha = alpha[a], beta = alpha[a])$conf.int
      ci.NU.el <- EL.tm(x.NU, y.NU, alpha = alpha[a], beta = alpha[a])$conf.int
      ci.norm.el <- EL.tm(x.norm, y.norm, alpha = alpha[a], beta = alpha[a])$conf.int
      ci.t.el <- EL.tm(x.t, y.t, alpha = alpha[a], beta = alpha[a])$conf.int
      ci.chi.el <- EL.tm(x.chi, y.chi, alpha = alpha[a], beta = alpha[a])$conf.int

      tm.temp.N.y[i,a] <- coverage(0, ci.N.y[1], ci.N.y[2])
      tm.temp.N2.y[i,a] <- coverage(0, ci.N2.y[1], ci.N2.y[2])
      tm.temp.NU.y[i,a] <- coverage(0, ci.NU.y[1], ci.NU.y[2])
      tm.temp.norm.y[i,a] <- coverage(0, ci.norm.y[1], ci.norm.y[2])
      tm.temp.t.y[i,a] <- coverage(0, ci.t.y[1], ci.t.y[2])
      tm.temp.chi.y[i,a] <- coverage(0, ci.chi.y[1], ci.chi.y[2])

      tm.temp.N.el[i,a] <- coverage(0, ci.N.el[1], ci.N.el[2])
      tm.temp.N2.el[i,a] <- coverage(0, ci.N2.el[1], ci.N2.el[2])
      tm.temp.NU.el[i,a] <- coverage(0, ci.NU.el[1], ci.NU.el[2])
      tm.temp.norm.el[i,a] <- coverage(0, ci.norm.el[1], ci.norm.el[2])
      tm.temp.t.el[i,a] <- coverage(0, ci.t.el[1], ci.t.el[2])
      tm.temp.chi.el[i,a] <- coverage(0, ci.chi.el[1], ci.chi.el[2])
      
      
      ### FIND VALID GAMMAS FOR GIVEN ALPHA
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
      ### END
      
      temp.gam.N.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N2.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.NU.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.norm.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.t.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.y <- matrix(ncol = length(gam), nrow = 1)
    
      
      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS
        
        ci.N.y <- yuen.stm(x.N,y.N, alpha[a], gam[g])$conf.int
        ci.N2.y <- yuen.stm(x.N2,y.N2, alpha[a], gam[g])$conf.int
        ci.NU.y <- yuen.stm(x.NU,y.NU, alpha[a], gam[g])$conf.int
        ci.norm.y <- yuen.stm(x.norm,y.norm, alpha[a], gam[g])$conf.int
        ci.t.y <- yuen.stm(x.t,y.t, alpha[a], gam[g])$conf.int
        ci.chi.y <- yuen.stm(x.chi,y.chi, alpha[a], gam[g])$conf.int
        
        
        temp.gam.N.y[1,g] <- coverage(0, ci.N.y[1], ci.N.y[2])
        temp.gam.N2.y[1,g] <- coverage(0, ci.N2.y[1], ci.N2.y[2])
        temp.gam.NU.y[1,g] <- coverage(0, ci.NU.y[1], ci.NU.y[2])
        temp.gam.norm.y[1,g] <- coverage(0, ci.norm.y[1], ci.norm.y[2])
        temp.gam.t.y[1,g] <- coverage(0, ci.t.y[1], ci.t.y[2])
        temp.gam.chi.y[1,g] <- coverage(0, ci.chi.y[1], ci.chi.y[2])
        
        
      } ### LOOP FOR GAMMAS ENDS
      
      
      temp.alph.N.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.y
      temp.alph.N2.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N2.y
      temp.alph.NU.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.NU.y
      temp.alph.norm.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.y
      temp.alph.t.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.y
      temp.alph.chi.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.y
    
      
    } ### LOOP FOR ALPHAS ENDS
    
    stm.temp.N.y[i,] <- temp.alph.N.y
    stm.temp.N2.y[i,] <- temp.alph.N2.y
    stm.temp.NU.y[i,] <- temp.alph.NU.y
    stm.temp.norm.y[i,] <- temp.alph.norm.y
    stm.temp.t.y[i,] <- temp.alph.t.y
    stm.temp.chi.y[i,] <- temp.alph.chi.y
    
    
    print(paste('i: ', i, '; n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  
  tm.N.y[n,] <- colMeans(tm.temp.N.y, na.rm = TRUE)
  tm.N2.y[n,] <- colMeans(tm.temp.N2.y, na.rm = TRUE)
  tm.NU.y[n,] <- colMeans(tm.temp.NU.y, na.rm = TRUE)
  tm.norm.y[n,] <- colMeans(tm.temp.norm.y, na.rm = TRUE)
  tm.t.y[n,] <- colMeans(tm.temp.t.y, na.rm = TRUE)
  tm.chi.y[n,] <- colMeans(tm.temp.chi.y, na.rm = TRUE)

  tm.N.el[n,] <- colMeans(tm.temp.N.el, na.rm = TRUE)
  tm.N2.el[n,] <- colMeans(tm.temp.N2.el, na.rm = TRUE)
  tm.NU.el[n,] <- colMeans(tm.temp.NU.el, na.rm = TRUE)
  tm.norm.el[n,] <- colMeans(tm.temp.norm.el, na.rm = TRUE)
  tm.t.el[n,] <- colMeans(tm.temp.t.el, na.rm = TRUE)
  tm.chi.el[n,] <- colMeans(tm.temp.chi.el, na.rm = TRUE)

  stm.N.y[n,] <- colMeans(stm.temp.N.y, na.rm = TRUE)
  stm.N2.y[n,] <- colMeans(stm.temp.N2.y, na.rm = TRUE)
  stm.NU.y[n,] <- colMeans(stm.temp.NU.y, na.rm = TRUE)
  stm.norm.y[n,] <- colMeans(stm.temp.norm.y, na.rm = TRUE)
  stm.t.y[n,] <- colMeans(stm.temp.t.y, na.rm = TRUE)
  stm.chi.y[n,] <- colMeans(stm.temp.chi.y, na.rm = TRUE)
  
  
} ### LOOP FOR SAMPLE SIZE n ENDS
now <- Sys.time() 

difftime(now, b4)

rezz.boot.stm.norm.el <- as.data.frame(t(stm.norm.el))

names(rezz.boot.stm.norm.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.norm.el <- rezz.boot.stm.norm.el %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 'el')

rezz.boot.stm.t.el <- as.data.frame(t(stm.t.el))

names(rezz.boot.stm.t.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.t.el <- rezz.boot.stm.t.el %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 'el')

rezz.boot.stm.chi.el <- as.data.frame(t(stm.chi.el))

names(rezz.boot.stm.chi.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.chi.el <- rezz.boot.stm.chi.el %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 'el')

rezz.boot.stm.N.el <- as.data.frame(t(stm.N.el))

names(rezz.boot.stm.N.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.N.el <- rezz.boot.stm.N.el %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 'el')

rezz.boot.stm.N2.el <- as.data.frame(t(stm.N2.el))

names(rezz.boot.stm.N2.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.N2.el <- rezz.boot.stm.N2.el %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 'el')

rezz.boot.stm.NU.el <- as.data.frame(t(stm.NU.el))

names(rezz.boot.stm.NU.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.NU.el <- rezz.boot.stm.NU.el %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 'el')




rezz.boot.stm.norm.y <- as.data.frame(t(stm.norm.y))

names(rezz.boot.stm.norm.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.norm.y <- rezz.boot.stm.norm.y %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 't')

rezz.boot.stm.t.y <- as.data.frame(t(stm.t.y))

names(rezz.boot.stm.t.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.t.y <- rezz.boot.stm.t.y %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 't')

rezz.boot.stm.chi.y <- as.data.frame(t(stm.chi.y))

names(rezz.boot.stm.chi.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.chi.y <- rezz.boot.stm.chi.y %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 't')

rezz.boot.stm.N.y <- as.data.frame(t(stm.N.y))

names(rezz.boot.stm.N.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.N.y <- rezz.boot.stm.N.y %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 't')

rezz.boot.stm.N2.y <- as.data.frame(t(stm.N2.y))

names(rezz.boot.stm.N2.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.N2.y <- rezz.boot.stm.N2.y %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 't')

rezz.boot.stm.NU.y <- as.data.frame(t(stm.NU.y))

names(rezz.boot.stm.NU.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.NU.y <- rezz.boot.stm.NU.y %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 't')



rezz.boot.tm.norm.y <- as.data.frame(t(tm.norm.y))

names(rezz.boot.tm.norm.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.norm.y <- rezz.boot.tm.norm.y %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 't')

rezz.boot.tm.t.y <- as.data.frame(t(tm.t.y))

names(rezz.boot.tm.t.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.t.y <- rezz.boot.tm.t.y %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 't')

rezz.boot.tm.chi.y <- as.data.frame(t(tm.chi.y))

names(rezz.boot.tm.chi.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.chi.y <- rezz.boot.tm.chi.y %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 't')

rezz.boot.tm.N.y <- as.data.frame(t(tm.N.y))

names(rezz.boot.tm.N.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.N.y <- rezz.boot.tm.N.y %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 't')

rezz.boot.tm.N2.y <- as.data.frame(t(tm.N2.y))

names(rezz.boot.tm.N2.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.N2.y <- rezz.boot.tm.N2.y %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 't')

rezz.boot.tm.NU.y <- as.data.frame(t(tm.NU.y))

names(rezz.boot.tm.NU.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.NU.y <- rezz.boot.tm.NU.y %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 't')


rezz.boot.tm.norm.el <- as.data.frame(t(tm.norm.el))

names(rezz.boot.tm.norm.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.norm.el <- rezz.boot.tm.norm.el %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 'el')

rezz.boot.tm.t.el <- as.data.frame(t(tm.t.el))

names(rezz.boot.tm.t.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.t.el <- rezz.boot.tm.t.el %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 'el')

rezz.boot.tm.chi.el <- as.data.frame(t(tm.chi.el))

names(rezz.boot.tm.chi.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.chi.el <- rezz.boot.tm.chi.el %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 'el')

rezz.boot.tm.N.el <- as.data.frame(t(tm.N.el))

names(rezz.boot.tm.N.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.N.el <- rezz.boot.tm.N.el %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 'el')

rezz.boot.tm.N2.el <- as.data.frame(t(tm.N2.el))

names(rezz.boot.tm.N2.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.N2.el <- rezz.boot.tm.N2.el %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 'el')

rezz.boot.tm.NU.el <- as.data.frame(t(tm.NU.el))

names(rezz.boot.tm.NU.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.NU.el <- rezz.boot.tm.NU.el %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 'el')






gamma_colors <- c("0.1" = "darkmagenta", "0.2" = "darkblue", "0.3" = "forestgreen", "0.4" = "gold4", "tm" = "deeppink1")




g.N1.norm <- rezz.boot.stm.norm.el %>% 
  rbind(rezz.boot.stm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.tm.norm.y) %>% 
  select(cov_g1 = N1, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.94, 0.97)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N2.norm <- rezz.boot.stm.norm.el %>% 
  rbind(rezz.boot.stm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.tm.norm.y) %>% 
  select(cov_g1 = N2, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.94, 0.97)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N3.norm <- rezz.boot.stm.norm.el %>% 
  rbind(rezz.boot.stm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.tm.norm.y) %>% 
  select(cov_g1 = N3, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.94, 0.97)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N4.norm <- rezz.boot.stm.norm.el %>% 
  rbind(rezz.boot.stm.norm.y,
        rezz.boot.tm.norm.el,
        rezz.boot.tm.norm.y) %>% 
  select(cov_g1 = N4, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.94, 0.97)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')




g.norm <- ggpubr::ggarrange(g.N1.norm, g.N2.norm, g.N3.norm, g.N4.norm, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                                   label.x = 0.7,
                                   font.label = list(size = 10, color = "black", family = NULL))

##

g.N1.t <- rezz.boot.stm.t.el %>% 
  rbind(rezz.boot.stm.t.y,
        rezz.boot.tm.t.el,
        rezz.boot.tm.t.y) %>% 
  select(cov_g1 = N1, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.934, 0.97)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N2.t <- rezz.boot.stm.t.el %>% 
  rbind(rezz.boot.stm.t.y,
        rezz.boot.tm.t.el,
        rezz.boot.tm.t.y) %>% 
  select(cov_g1 = N2, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.934, 0.97)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N3.t <- rezz.boot.stm.t.el %>% 
  rbind(rezz.boot.stm.t.y,
        rezz.boot.tm.t.el,
        rezz.boot.tm.t.y) %>% 
  select(cov_g1 = N3, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.934, 0.97)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N4.t <- rezz.boot.stm.t.el %>% 
  rbind(rezz.boot.stm.t.y,
        rezz.boot.tm.t.el,
        rezz.boot.tm.t.y) %>% 
  select(cov_g1 = N4, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.934, 0.97)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')




g.t <- ggpubr::ggarrange(g.N1.t, g.N2.t, g.N3.t, g.N4.t, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))



##

g.N1.chi <- rezz.boot.stm.chi.el %>% 
  rbind(rezz.boot.stm.chi.y,
        rezz.boot.tm.chi.el,
        rezz.boot.tm.chi.y) %>% 
  select(cov_g1 = N1, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.935, 0.965)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N2.chi <- rezz.boot.stm.chi.el %>% 
  rbind(rezz.boot.stm.chi.y,
        rezz.boot.tm.chi.el,
        rezz.boot.tm.chi.y) %>% 
  select(cov_g1 = N2, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.935, 0.965)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N3.chi <- rezz.boot.stm.chi.el %>% 
  rbind(rezz.boot.stm.chi.y,
        rezz.boot.tm.chi.el,
        rezz.boot.tm.chi.y) %>% 
  select(cov_g1 = N3, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.935, 0.965)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N4.chi <- rezz.boot.stm.chi.el %>% 
  rbind(rezz.boot.stm.chi.y,
        rezz.boot.tm.chi.el,
        rezz.boot.tm.chi.y) %>% 
  select(cov_g1 = N4, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.935, 0.965)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')




g.chi <- ggpubr::ggarrange(g.N1.chi, g.N2.chi, g.N3.chi, g.N4.chi, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))


##

g.N1.N <- rezz.boot.stm.N.el %>% 
  rbind(rezz.boot.stm.N.y,
        rezz.boot.tm.N.el,
        rezz.boot.tm.N.y) %>% 
  select(cov_g1 = N1, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.91, 1)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N2.N <- rezz.boot.stm.N.el %>% 
  rbind(rezz.boot.stm.N.y,
        rezz.boot.tm.N.el,
        rezz.boot.tm.N.y) %>% 
  select(cov_g1 = N2, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.91, 1)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N3.N <- rezz.boot.stm.N.el %>% 
  rbind(rezz.boot.stm.N.y,
        rezz.boot.tm.N.el,
        rezz.boot.tm.N.y) %>% 
  select(cov_g1 = N3, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.91, 1)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N4.N <- rezz.boot.stm.N.el %>% 
  rbind(rezz.boot.stm.N.y,
        rezz.boot.tm.N.el,
        rezz.boot.tm.N.y) %>% 
  select(cov_g1 = N4, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.91, 1)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')




g.N <- ggpubr::ggarrange(g.N1.N, g.N2.N, g.N3.N, g.N4.N, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))


##

g.N1.N2 <- rezz.boot.stm.N2.el %>% 
  rbind(rezz.boot.stm.N2.y,
        rezz.boot.tm.N2.el,
        rezz.boot.tm.N2.y) %>% 
  select(cov_g1 = N1, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.89, 0.99)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N2.N2 <- rezz.boot.stm.N2.el %>% 
  rbind(rezz.boot.stm.N2.y,
        rezz.boot.tm.N2.el,
        rezz.boot.tm.N2.y) %>% 
  select(cov_g1 = N2, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.89, 0.99)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N3.N2 <- rezz.boot.stm.N2.el %>% 
  rbind(rezz.boot.stm.N2.y,
        rezz.boot.tm.N2.el,
        rezz.boot.tm.N2.y) %>% 
  select(cov_g1 = N3, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.89, 0.99)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N4.N2 <- rezz.boot.stm.N2.el %>% 
  rbind(rezz.boot.stm.N2.y,
        rezz.boot.tm.N2.el,
        rezz.boot.tm.N2.y) %>% 
  select(cov_g1 = N4, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.89, 0.99)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')




g.N2 <- ggpubr::ggarrange(g.N1.N2, g.N2.N2, g.N3.N2, g.N4.N2, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))


##

g.N1.NU <- rezz.boot.stm.NU.el %>% 
  rbind(rezz.boot.stm.NU.y,
        rezz.boot.tm.NU.el,
        rezz.boot.tm.NU.y) %>% 
  select(cov_g1 = N1, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.94, 0.965)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N2.NU <- rezz.boot.stm.NU.el %>% 
  rbind(rezz.boot.stm.NU.y,
        rezz.boot.tm.NU.el,
        rezz.boot.tm.NU.y) %>% 
  select(cov_g1 = N2, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.94, 0.965)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N3.NU <- rezz.boot.stm.NU.el %>% 
  rbind(rezz.boot.stm.NU.y,
        rezz.boot.tm.NU.el,
        rezz.boot.tm.NU.y) %>% 
  select(cov_g1 = N3, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.94, 0.965)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N4.NU <- rezz.boot.stm.NU.el %>% 
  rbind(rezz.boot.stm.NU.y,
        rezz.boot.tm.NU.el,
        rezz.boot.tm.NU.y) %>% 
  select(cov_g1 = N4, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.94, 0.965)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')




g.NU <- ggpubr::ggarrange(g.N1.NU, g.N2.NU, g.N3.NU, g.N4.NU, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                            label.x = 0.7,
                            font.label = list(size = 10, color = "black", family = NULL))



g.norm
ggsave('2samp_norm.png', width = 10, height = 7, bg = 'white')
g.t
ggsave('2samp_t.png', width = 10, height = 7, bg = 'white')
g.chi
ggsave('2samp_chi.png', width = 10, height = 7, bg = 'white')
g.N
ggsave('2samp_N.png', width = 10, height = 7, bg = 'white')
g.N2
ggsave('2samp_N2.png', width = 10, height = 7, bg = 'white')
g.NU
ggsave('2samp_NU.png', width = 10, height = 7, bg = 'white')



F.cn3 <- function(x){
  0.2 * pnorm(x + 10) + 
    0.8 * pnorm(x) 
}

gen.N3 <- function(){
  y <- runif(1)
  return(uniroot(function(x) {F.cn3(x) - y}, c(-1000,1000))$root)
}


stm.N3.el <- as.data.frame(matrix(nrow = length(NN), ncol = 12))


iter <- 1000
set.seed(1)

b4 <- Sys.time()

for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE n
  
  
  stm.temp.N.el <- matrix(ncol = 12, nrow = iter)
  
  
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    ### GENERATING DATA
    x.N <- replicate(NN[n], gen.N3())
    y.N <- replicate(NN[n], gen.N3())
    ### END
    
    ### TRANSOFRMING DATA FOR yuen.tm() FUNNCTION
    
    temp.alph.N.el <- matrix(ncol = 12, nrow = 1)
    ### END
    
    for(a in 1:length(alpha)){ ### LOOP FOR ALPHAS
      
      
      
      ### FIND VALID GAMMAS FOR GIVEN ALPHA
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
      ### END
      
      
      temp.gam.N.el <- matrix(ncol = length(gam), nrow = 1)
      
      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS
        
        
        ci.N.el <- EL.stm(x.N,y.N, alpha = alpha[a], gamma = gam[g])$conf.int
        
        
        temp.gam.N.el[1,g] <- coverage(0, ci.N.el[1], ci.N.el[2])
        
      } ### LOOP FOR GAMMAS ENDS
      
      
      temp.alph.N.el[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.el
      
    } ### LOOP FOR ALPHAS ENDS
    
    
    stm.temp.N.el[i,] <- temp.alph.N.el
    
    print(paste('i: ', i, '; n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  
  stm.N3.el[n,] <- colMeans(stm.temp.N.el, na.rm = TRUE)
  
} ### LOOP FOR SAMPLE SIZE n ENDS
now <- Sys.time() 

difftime(now, b4)








NN <- c(50,100,200,500)
alpha <- c(0.05,0.1,0.15,0.2)
gamma <- c(0.1,0.2,0.3,0.4)

alph_gam <- as.data.frame(cbind(c(rep(alpha[1],4),rep(alpha[2],3), rep(alpha[3],3), rep(alpha[4],2)),
                                c(gamma, gamma[-1], gamma[-1], gamma[-(1:2)])))

names(alph_gam) <- c('alpha', 'gamma')

tm.N3.el <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N3.y <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
stm.N3.y <- as.data.frame(matrix(nrow = length(NN), ncol = 12))

iter <- 10000
set.seed(1)

b4 <- Sys.time()

for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE n
  
  tm.temp.N.el <- matrix(ncol = 4, nrow = iter)
  tm.temp.N.y <- matrix(ncol = 4, nrow = iter)
  stm.temp.N.y <- matrix(ncol = 12, nrow = iter)
  
  
  
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    x.N3 <- replicate(NN[n], gen.N3())
    y.N3 <- replicate(NN[n], gen.N3())
    

    gr <- c(rep(1, NN[n]), rep(2, NN[n]))
    data.N <- data.frame(data = c(x.N3,y.N3), gr = gr)

    temp.alph.N.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.N.el <- matrix(ncol = 12, nrow = 1)
    ### END
    
    for(a in 1:length(alpha)){ ### LOOP FOR ALPHAS
      
      ci.N.y <- yuen.tm(data ~ gr, data = data.N, tr = alpha[a])$conf.int
      ci.N.el <- tryCatch(
        expr = {EL.tm(x.N3, y.N3, alpha = alpha[a], beta = alpha[a])$conf.int}
        ,
        error = function(e)
          c(NA,NA)
      )
       
      
      tm.temp.N.y[i,a] <- coverage(0, ci.N.y[1], ci.N.y[2])
      tm.temp.N.el[i,a] <- coverage(0, ci.N.el[1], ci.N.el[2])
      
      ### FIND VALID GAMMAS FOR GIVEN ALPHA
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
      ### END
      
      temp.gam.N.y <- matrix(ncol = length(gam), nrow = 1)
      
      
      for(g in 1:length(gam)){ 
        
        ci.N.y <- yuen.stm(x.N3,y.N3, alpha[a], gam[g])$conf.int
        temp.gam.N.y[1,g] <- coverage(0, ci.N.y[1], ci.N.y[2])
      }
      temp.alph.N.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.y
      
    } 
    
    stm.temp.N.y[i,] <- temp.alph.N.y
    
    
    print(paste('i: ', i, '; n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  
  tm.N3.y[n,] <- colMeans(tm.temp.N.y, na.rm = TRUE)
  tm.N3.el[n,] <- colMeans(tm.temp.N.el, na.rm = TRUE)
  stm.N3.y[n,] <- colMeans(stm.temp.N.y, na.rm = TRUE)
  
  
} ### LOOP FOR SAMPLE SIZE n ENDS
now <- Sys.time() 

difftime(now, b4)


rezz.boot.stm.N3.el <- as.data.frame(t(stm.N3.el))

names(rezz.boot.stm.N3.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.N3.el <- rezz.boot.stm.N3.el %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 'el')

rezz.boot.stm.N3.y <- as.data.frame(t(stm.N3.y))

names(rezz.boot.stm.N3.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.stm.N3.y <- rezz.boot.stm.N3.y %>% 
  mutate(alpha = as.character(c(0.05, 0.05, 0.05, 0.05, 0.1, 0.1, 0.1,0.15, 0.15, 0.15, 0.2, 0.2)),
         gamma = as.character(c(0.1, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.3, 0.4)),
         method = 't')

rezz.boot.tm.N3.el <- as.data.frame(t(tm.N3.el))

names(rezz.boot.tm.N3.el) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.N3.el <- rezz.boot.tm.N3.el %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 'el')

rezz.boot.tm.N3.y <- as.data.frame(t(tm.N3.y))

names(rezz.boot.tm.N3.y) <- c('N1', 'N2', 'N3', 'N4')
rezz.boot.tm.N3.y <- rezz.boot.tm.N3.y %>% 
  mutate(alpha = as.character(alpha),
         gamma = 'tm',
         method = 't')


g.N1.N3 <- rezz.boot.stm.N3.el %>% 
  rbind(rezz.boot.stm.N3.y,
        rezz.boot.tm.N3.el,
        rezz.boot.tm.N3.y) %>% 
  select(cov_g1 = N1, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.875, 1)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 50')

g.N2.N3 <- rezz.boot.stm.N3.el %>% 
  rbind(rezz.boot.stm.N3.y,
        rezz.boot.tm.N3.el,
        rezz.boot.tm.N3.y) %>% 
  select(cov_g1 = N2, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.875, 1)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 100')

g.N3.N3 <- rezz.boot.stm.N3.el %>% 
  rbind(rezz.boot.stm.N3.y,
        rezz.boot.tm.N3.el,
        rezz.boot.tm.N3.y) %>% 
  select(cov_g1 = N3, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.875, 1)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 200')

g.N4.N3 <- rezz.boot.stm.N3.el %>% 
  rbind(rezz.boot.stm.N3.y,
        rezz.boot.tm.N3.el,
        rezz.boot.tm.N3.y) %>% 
  select(cov_g1 = N4, alpha, gamma, method ) %>% 
  ggplot(aes(x = alpha, y = cov_g1, col = gamma, shape = method))+
  geom_point(size = 2.3)+
  geom_line(aes(x = alpha, y = cov_g1, col = gamma, group = interaction(gamma, method)), linewidth = 0.65)+
  geom_hline(aes(yintercept = 0.95), linetype = 2, size = 1.6) +
  ylab('coverage accuracy')+
  ylim(0.875, 1)+
  scale_color_manual(values = gamma_colors)+
  theme_minimal()+
  ggtitle('N = 500')




g.N3 <- ggpubr::ggarrange(g.N1.N3, g.N2.N3, g.N3.N3, g.N4.N3, nrow = 2, ncol = 2, common.legend = TRUE, legend = 'bottom',
                          label.x = 0.7,
                          font.label = list(size = 10, color = "black", family = NULL))

ggsave('2samp_N3.png', width = 10, height = 7, bg = 'white')
### DISTRIBUTIONS  ---- 


get.q <- function(dati){
  q1 <- quantile(dati, probs = 0.025)
  q2 <- quantile(dati, probs = 0.975)
  
  return(c(q1, q2))
}


NN <- c(50,100,200,500)
alpha <- c(0.05,0.1,0.15,0.2)
gamma <- c(0.1,0.2,0.3,0.4)

alph_gam <- as.data.frame(cbind(c(rep(alpha[1],4),rep(alpha[2],3), rep(alpha[3],3), rep(alpha[4],2)),
                                c(gamma, gamma[-1], gamma[-1], gamma[-(1:2)])))

names(alph_gam) <- c('alpha', 'gamma')



tm.norm.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.t.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.chi.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N2.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.NU.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 4))
tm.N3.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 4))

stm.norm.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.t.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.chi.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N2.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.NU.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
stm.N3.sad <- as.data.frame(matrix(nrow = length(NN), ncol = 12))

iter <- 10000
set.seed(1)

b4 <- Sys.time()

for(n in 1:length(NN)){ ### LOOP FOR SAMPLE SIZE n
  
  tm.temp.norm.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.t.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.chi.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.N.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.N2.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.NU.y <- matrix(ncol = 4, nrow = iter)
  tm.temp.N3.y <- matrix(ncol = 4, nrow = iter)
  
  stm.temp.N.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.N2.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.NU.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.norm.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.t.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.chi.y <- matrix(ncol = 12, nrow = iter)
  stm.temp.N3.y <- matrix(ncol = 12, nrow = iter)
  
  
  # n <- 1
  # a <- 1
  # g <- 1
  # i <- 1
  
  for(i in 1:iter){ ### LOOP FOR ITERATIONS
    
    ### GENERATING DATA
    x.N <- replicate(NN[n], gen.N())
    y.N <- replicate(NN[n], gen.N())

    x.N2 <- replicate(NN[n], gen.N2())
    y.N2 <- replicate(NN[n], gen.N2())

    x.NU <- replicate(NN[n], gen.NU())
    y.NU <- replicate(NN[n], gen.NU())
    
    x.N3 <- replicate(NN[n], gen.N3())
    y.N3 <- replicate(NN[n], gen.N3())
    
    x.norm <- rnorm(NN[n])
    y.norm <- rnorm(NN[n])

    x.t <- rt(NN[n], df = 2)
    y.t <- rt(NN[n], df = 2)

    x.chi <- rchisq(NN[n], df = 3)
    y.chi <- rchisq(NN[n], df = 3)
    ### END
    
    ### TRANSOFRMING DATA FOR yuen.tm() FUNNCTION
    gr <- c(rep(1, NN[n]), rep(2, NN[n]))
    
    data.N <- data.frame(data = c(x.N,y.N), gr = gr)
    data.N2 <- data.frame(data = c(x.N2,y.N2), gr = gr)
    data.NU <- data.frame(data = c(x.NU,y.NU), gr = gr)

    data.norm <- data.frame(data = c(x.norm,y.norm), gr = gr)
    data.t <- data.frame(data = c(x.t,y.t), gr = gr)
    data.chi <- data.frame(data = c(x.chi,y.chi), gr = gr)
    
    data.N3 <- data.frame(data = c(x.N3,y.N3), gr = gr)
    ### END
    
    # # FOR SMOOTHLY TRIMMED MEAN
    temp.alph.N.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.N2.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.NU.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.norm.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.t.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.chi.y <- matrix(ncol = 12, nrow = 1)
    temp.alph.N3.y <- matrix(ncol = 12, nrow = 1)
    
    ### END
    
    for(a in 1:length(alpha)){ ### LOOP FOR ALPHAS
      
      tm.temp.N.y[i,a] <- yuen.tm(data ~ gr, data = data.N, tr = alpha[a])$test
      tm.temp.N2.y[i,a] <- yuen.tm(data ~ gr, data = data.N2, tr = alpha[a])$test
      tm.temp.NU.y[i,a] <- yuen.tm(data ~ gr, data = data.NU, tr = alpha[a])$test
      tm.temp.norm.y[i,a] <- yuen.tm(data ~ gr, data = data.norm, tr = alpha[a])$test
      tm.temp.t.y[i,a] <- yuen.tm(data ~ gr, data = data.t, tr = alpha[a])$test
      tm.temp.chi.y[i,a] <- yuen.tm(data ~ gr, data = data.chi, tr = alpha[a])$test
      tm.temp.N3.y[i,a] <- yuen.tm(data ~ gr, data = data.N3, tr = alpha[a])$test
      
      ### FIND VALID GAMMAS FOR GIVEN ALPHA
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
      ### END
      
      temp.gam.N.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N2.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.NU.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.norm.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.t.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.chi.y <- matrix(ncol = length(gam), nrow = 1)
      temp.gam.N3.y <- matrix(ncol = length(gam), nrow = 1)
      
      
      for(g in 1:length(gam)){ ### LOOP FOR GAMMAS
        
        
        temp.gam.N.y[1,g] <- yuen.stm(x.N,y.N, alpha[a], gam[g])$st
        temp.gam.N2.y[1,g] <- yuen.stm(x.N2,y.N2, alpha[a], gam[g])$st
        temp.gam.NU.y[1,g] <- yuen.stm(x.NU,y.NU, alpha[a], gam[g])$st
        temp.gam.norm.y[1,g] <- yuen.stm(x.norm,y.norm, alpha[a], gam[g])$st
        temp.gam.t.y[1,g] <- yuen.stm(x.t,y.t, alpha[a], gam[g])$st
        temp.gam.chi.y[1,g] <- yuen.stm(x.chi,y.chi, alpha[a], gam[g])$st
        temp.gam.N3.y[1,g] <- yuen.stm(x.N3,y.N3, alpha[a], gam[g])$st
        
        
      } ### LOOP FOR GAMMAS ENDS
      
      
      temp.alph.N.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N.y
      temp.alph.N2.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N2.y
      temp.alph.NU.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.NU.y
      temp.alph.norm.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.y
      temp.alph.t.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.t.y
      temp.alph.chi.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.chi.y
      temp.alph.N3.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.N3.y
      
      
    } ### LOOP FOR ALPHAS ENDS
    
    stm.temp.N.y[i,] <- temp.alph.N.y
    stm.temp.N2.y[i,] <- temp.alph.N2.y
    stm.temp.NU.y[i,] <- temp.alph.NU.y
    stm.temp.norm.y[i,] <- temp.alph.norm.y
    stm.temp.t.y[i,] <- temp.alph.t.y
    stm.temp.chi.y[i,] <- temp.alph.chi.y
    stm.temp.N3.y[i,] <- temp.alph.N3.y
    
    
    print(paste('i: ', i, '; n: ', n, sep = ''))
  } ### LOOP FOR ITERATIONS ENDS
  
  
  tm.N.sad[n,] <- sapply(as.data.frame(tm.temp.N.y), function(x) get.q(x)[2])
  tm.N2.sad[n,] <- sapply(as.data.frame(tm.temp.N2.y), function(x) get.q(x)[2])
  tm.NU.sad[n,] <- sapply(as.data.frame(tm.temp.NU.y), function(x) get.q(x)[2])
  tm.norm.sad[n,] <- sapply(as.data.frame(tm.temp.norm.y), function(x) get.q(x)[2])
  tm.t.sad[n,] <- sapply(as.data.frame(tm.temp.t.y), function(x) get.q(x)[2])
  tm.chi.sad[n,] <- sapply(as.data.frame(tm.temp.chi.y), function(x) get.q(x)[2])
  
  tm.N3.sad[n,] <- sapply(as.data.frame(tm.temp.N3.y), function(x) get.q(x)[2])
  
  
  stm.N.sad[n,] <- sapply(as.data.frame(stm.temp.N.y), function(x) get.q(x)[2])
  stm.N2.sad[n,] <- sapply(as.data.frame(stm.temp.N2.y), function(x) get.q(x)[2])
  stm.NU.sad[n,] <- sapply(as.data.frame(stm.temp.NU.y), function(x) get.q(x)[2])
  stm.norm.sad[n,] <- sapply(as.data.frame(stm.temp.norm.y), function(x) get.q(x)[2])
  stm.t.sad[n,] <- sapply(as.data.frame(stm.temp.t.y), function(x) get.q(x)[2])
  stm.chi.sad[n,] <- sapply(as.data.frame(stm.temp.chi.y), function(x) get.q(x)[2])
  
  stm.N3.sad[n,] <- sapply(as.data.frame(stm.temp.N3.y), function(x) get.q(x)[2])
  
} ### LOOP FOR SAMPLE SIZE n ENDS
now <- Sys.time() 


# find df for stm and tm t -tests ----
set.seed(1)

stm.t.df <- as.data.frame(matrix(nrow = length(NN), ncol = 12))
tm.t.df <- as.data.frame(matrix(nrow = length(NN), ncol = 4))

for(n in 1:length(NN)){ 
  
  tm.temp.norm.y <- matrix(ncol = 4, nrow = iter)
  stm.temp.norm.y <- matrix(ncol = 12, nrow = iter)
for(i in 1:iter){ 
  x.norm <- rnorm(NN[n])
  y.norm <- rnorm(NN[n])
  gr <- c(rep(1, NN[n]), rep(2, NN[n]))
  data.norm <- data.frame(data = c(x.norm,y.norm), gr = gr)
  temp.alph.norm.y <- matrix(ncol = 12, nrow = 1)
  for(a in 1:length(alpha)){ 
    
    tm.temp.norm.y[i,a] <- yuen.tm(data ~ gr, data = data.norm, tr = alpha[a])$df
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
    temp.gam.norm.y <- matrix(ncol = length(gam), nrow = 1)
    
    for(g in 1:length(gam)){ 
      temp.gam.norm.y[1,g] <- yuen.stm(x.norm,y.norm, alpha[a], gam[g])$df
     } 
    temp.alph.norm.y[1,indx:(length(gam)+indx - 1)] <- temp.gam.norm.y
  }
  stm.temp.norm.y[i,] <- temp.alph.norm.y
  print(paste('i: ', i, '; n: ', n, sep = ''))
} 
  tm.t.df[n,] <- colMeans(tm.temp.norm.y)
  stm.t.df[n,] <- colMeans(stm.temp.norm.y)
}



sad.N.stm <- data.frame(t(stm.N.sad))
sad.N.tm <- data.frame(t(tm.N.sad))
names(sad.N.tm) <- c('N1', 'N2', 'N3', 'N4')
names(sad.N.stm) <- c('N1', 'N2', 'N3', 'N4')

sad.N <- sad.N.stm %>% 
  mutate(method = 'stm',
         alpha = c(alpha[1], alpha[1], alpha[1], alpha[1],
                   alpha[2], alpha[2], alpha[2],
                   alpha[3], alpha[3], alpha[3],
                   alpha[4], alpha[4]),
         gamma = c(gamma,
                   gamma[2:4],
                   gamma[2:4],
                   gamma[3:4])) %>% 
  rbind(sad.N.tm %>% 
          mutate(method = 'tm',
                 alpha = alpha,
                 gamma = NA))

sad.N2.stm <- data.frame(t(stm.N2.sad))
sad.N2.tm <- data.frame(t(tm.N2.sad))
names(sad.N2.tm) <- c('N1', 'N2', 'N3', 'N4')
names(sad.N2.stm) <- c('N1', 'N2', 'N3', 'N4')

sad.N2 <- sad.N2.stm %>% 
  mutate(method = 'stm',
         alpha = c(alpha[1], alpha[1], alpha[1], alpha[1],
                   alpha[2], alpha[2], alpha[2],
                   alpha[3], alpha[3], alpha[3],
                   alpha[4], alpha[4]),
         gamma = c(gamma,
                   gamma[2:4],
                   gamma[2:4],
                   gamma[3:4])) %>% 
  rbind(sad.N2.tm %>% 
          mutate(method = 'tm',
                 alpha = alpha,
                 gamma = NA))

sad.NU.stm <- data.frame(t(stm.NU.sad))
sad.NU.tm <- data.frame(t(tm.NU.sad))
names(sad.NU.tm) <- c('N1', 'N2', 'N3', 'N4')
names(sad.NU.stm) <- c('N1', 'N2', 'N3', 'N4')

sad.NU <- sad.NU.stm %>% 
  mutate(method = 'stm',
         alpha = c(alpha[1], alpha[1], alpha[1], alpha[1],
                   alpha[2], alpha[2], alpha[2],
                   alpha[3], alpha[3], alpha[3],
                   alpha[4], alpha[4]),
         gamma = c(gamma,
                   gamma[2:4],
                   gamma[2:4],
                   gamma[3:4])) %>% 
  rbind(sad.NU.tm %>% 
          mutate(method = 'tm',
                 alpha = alpha,
                 gamma = NA))


sad.norm.stm <- data.frame(t(stm.norm.sad))
sad.norm.tm <- data.frame(t(tm.norm.sad))
names(sad.norm.tm) <- c('N1', 'N2', 'N3', 'N4')
names(sad.norm.stm) <- c('N1', 'N2', 'N3', 'N4')

sad.norm <- sad.norm.stm %>% 
  mutate(method = 'stm',
         alpha = c(alpha[1], alpha[1], alpha[1], alpha[1],
                   alpha[2], alpha[2], alpha[2],
                   alpha[3], alpha[3], alpha[3],
                   alpha[4], alpha[4]),
         gamma = c(gamma,
                   gamma[2:4],
                   gamma[2:4],
                   gamma[3:4])) %>% 
  rbind(sad.norm.tm %>% 
          mutate(method = 'tm',
                 alpha = alpha,
                 gamma = NA))

sad.t.stm <- data.frame(t(stm.t.sad))
sad.t.tm <- data.frame(t(tm.t.sad))
names(sad.t.tm) <- c('N1', 't', 'N3', 'N4')
names(sad.t.stm) <- c('N1', 't', 'N3', 'N4')

sad.t <- sad.t.stm %>% 
  mutate(method = 'stm',
         alpha = c(alpha[1], alpha[1], alpha[1], alpha[1],
                   alpha[2], alpha[2], alpha[2],
                   alpha[3], alpha[3], alpha[3],
                   alpha[4], alpha[4]),
         gamma = c(gamma,
                   gamma[2:4],
                   gamma[2:4],
                   gamma[3:4])) %>% 
  rbind(sad.t.tm %>% 
          mutate(method = 'tm',
                 alpha = alpha,
                 gamma = NA))


sad.chi.stm <- data.frame(t(stm.chi.sad))
sad.chi.tm <- data.frame(t(tm.chi.sad))
names(sad.chi.tm) <- c('N1', 'chi', 'N3', 'N4')
names(sad.chi.stm) <- c('N1', 'chi', 'N3', 'N4')

sad.chi <- sad.chi.stm %>% 
  mutate(method = 'stm',
         alpha = c(alpha[1], alpha[1], alpha[1], alpha[1],
                   alpha[2], alpha[2], alpha[2],
                   alpha[3], alpha[3], alpha[3],
                   alpha[4], alpha[4]),
         gamma = c(gamma,
                   gamma[2:4],
                   gamma[2:4],
                   gamma[3:4])) %>% 
  rbind(sad.chi.tm %>% 
          mutate(method = 'tm',
                 alpha = alpha,
                 gamma = NA))



sad.N3.stm <- data.frame(t(stm.N3.sad))
sad.N3.tm <- data.frame(t(tm.N3.sad))
names(sad.N3.tm) <- c('N1', 'N2', 'N3', 'N4')
names(sad.N3.stm) <- c('N1', 'N2', 'N3', 'N4')

sad.N3 <- sad.N3.stm %>% 
  mutate(method = 'stm',
         alpha = c(alpha[1], alpha[1], alpha[1], alpha[1],
                   alpha[2], alpha[2], alpha[2],
                   alpha[3], alpha[3], alpha[3],
                   alpha[4], alpha[4]),
         gamma = c(gamma,
                   gamma[2:4],
                   gamma[2:4],
                   gamma[3:4])) %>% 
  rbind(sad.N3.tm %>% 
          mutate(method = 'tm',
                 alpha = alpha,
                 gamma = NA))

#           a1        a2        a3        a4
# N1  88.07083  75.96523  67.90297  55.83255
# N2 175.96999 155.85482 135.78807 115.68760
# N3 355.92753 315.80501 275.71551 235.58392
# N4 895.86818 795.76205 695.69456 595.55837

# a1
qt(0.975, 88)
qt(0.975, 175)
qt(0.975, 355)
qt(0.975, 895)

# a2
qt(0.975, 75)
qt(0.975, 155)
qt(0.975, 315)
qt(0.975, 795)

# a3
qt(0.975, 67)
qt(0.975, 135)
qt(0.975, 275)
qt(0.975, 695)


# a4
qt(0.975, 55)
qt(0.975, 155)
qt(0.975, 235)
qt(0.975, 595)



library(xtable)

t <- sad.N %>% 
  select(-c(N2,N3,N4)) %>% 
  mutate(n = 50) %>% 
  select(n,method,gamma,alpha,N1) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N1) %>% 
  rbind(sad.N %>% 
          select(-c(N2,N3,N1)) %>% 
          mutate(n = 500) %>% 
          select(n,method,gamma,alpha,N4) %>% 
          pivot_wider(names_from = c(alpha,method), values_from = N4))

openxlsx::write.xlsx(t, 'sad_N.xlsx')

t <- sad.N2 %>% 
  select(-c(N2,N3,N4)) %>% 
  mutate(n = 50) %>% 
  select(n,method,gamma,alpha,N1) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N1) %>% 
  rbind(sad.N2 %>% 
          select(-c(N2,N3,N1)) %>% 
          mutate(n = 500) %>% 
          select(n,method,gamma,alpha,N4) %>% 
          pivot_wider(names_from = c(alpha,method), values_from = N4))

openxlsx::write.xlsx(t, 'sad_N2.xlsx')

t <- sad.N3 %>% 
  select(-c(N2,N3,N4)) %>% 
  mutate(n = 50) %>% 
  select(n,method,gamma,alpha,N1) %>% 
  pivot_wider(names_from = c(alpha,method), values_from = N1) %>% 
  rbind(sad.N3 %>% 
          select(-c(N2,N3,N1)) %>% 
          mutate(n = 500) %>% 
          select(n,method,gamma,alpha,N4) %>% 
          pivot_wider(names_from = c(alpha,method), values_from = N4))

openxlsx::write.xlsx(t, 'sad_N3.xlsx')

### Grafiki ----
set.seed(1)
x <- replicate(50, gen.N())
ggplot()+
  geom_boxplot(aes(y = x))+
  theme_classic()
ggsave('bp_N.png', bg = 'white')

ggplot(aes(x = x), data = NULL)+
  geom_histogram(aes(y = ..density..), fill = 'white', col = 'black')+
  theme_classic()
ggsave('hist_N.png', bg = 'white')
getwd()
set.seed(1)
x <- replicate(50, gen.N2())
ggplot()+
  geom_boxplot(aes(y = x))+
  theme_classic()
ggsave('bp_N2.png', bg = 'white')

ggplot(aes(x = x), data = NULL)+
  geom_histogram(aes(y = ..density..), fill = 'white', col = 'black')+
  theme_classic()
ggsave('hist_N2.png', bg = 'white')


set.seed(1)
x <- replicate(50, gen.N3())
ggplot()+
  geom_boxplot(aes(y = x))+
  theme_classic()
ggsave('bp_N3.png', bg = 'white')

ggplot(aes(x = x), data = NULL)+
  geom_histogram(aes(y = ..density..), fill = 'white', col = 'black')+
  theme_classic()
ggsave('hist_N3.png', bg = 'white')
source('funkcijas.R')
library(tidyverse)
library(rstatix)


# LOS dati ----
los.dati0 <- openxlsx::read.xlsx('datu_kopas.xlsx', sheet = 'LOS')

belg <- NULL
switz <- NULL
for(i in 1:dim(los.dati0)[1]){
  belg <- c(belg, rep(los.dati0$LOS[i], los.dati0$Belguim[i]))
  switz <- c(switz, rep(los.dati0$LOS[i], los.dati0$Switzerland[i]))
}

los.dati <- data.frame(los = c(belg, switz), gr = as.character(c(rep('Belgium', length(belg)),
                                                    rep('Switzerland', length(switz)))))

los.dati %>% 
  ggplot(aes(x = gr, y = los))+
  geom_boxplot()+
  theme_minimal()+
  xlab('')+
  ylab('Length of stay')

ggsave('bp_los.png', bg = 'white')


los.dati %>% 
  filter(gr == 'Belgium') %>% 
  ggplot(aes(x = los)) +
  geom_histogram(aes(y = ..density..), col = 'black', fill = 'white')+
  theme_minimal()+
  xlab('Belguim')

ggsave('hist_los_belg.png', bg = 'white')


los.dati %>% 
  filter(gr == 'Switzerland') %>% 
  ggplot(aes(x = los)) +
  geom_histogram(aes(y = ..density..), col = 'black', fill = 'white')+
  theme_minimal()+
  xlab('Switzerland')

ggsave('hist_los_switz.png', bg = 'white')


alpha <- c(0.05, 0.1, 0.15, 0.2)
gamma <- c(0.1, 0.2, 0.3, 0.4)

ci.stm.el<- data.frame(ub = c(
                            -1*EL.stm(belg, switz, alpha[2], gamma[2])$conf.int[1],
                            -1*EL.stm(belg, switz, alpha[2], gamma[3])$conf.int[1],
                            -1*EL.stm(belg, switz, alpha[2], gamma[4])$conf.int[1],
                            -1*EL.stm(belg, switz, alpha[3], gamma[2])$conf.int[1],
                            -1*EL.stm(belg, switz, alpha[3], gamma[3])$conf.int[1],
                            -1*EL.stm(belg, switz, alpha[3], gamma[4])$conf.int[1],
                            -1*EL.stm(belg, switz, alpha[4], gamma[3])$conf.int[1],
                            -1*EL.stm(belg, switz, alpha[4], gamma[4])$conf.int[1]),
                     
                     lb = c(
                       -1*EL.stm(belg, switz, alpha[2], gamma[2])$conf.int[2],
                       -1*     EL.stm(belg, switz, alpha[2], gamma[3])$conf.int[2],
                       -1*     EL.stm(belg, switz, alpha[2], gamma[4])$conf.int[2],
                       -1*     EL.stm(belg, switz, alpha[3], gamma[2])$conf.int[2],
                       -1*     EL.stm(belg, switz, alpha[3], gamma[3])$conf.int[2],
                       -1*     EL.stm(belg, switz, alpha[3], gamma[4])$conf.int[2],
                       -1*     EL.stm(belg, switz, alpha[4], gamma[3])$conf.int[2],
                       -1*     EL.stm(belg, switz, alpha[4], gamma[4])$conf.int[2]),
                     
                     est = c(
                       -1* EL.stm(belg, switz, alpha[2], gamma[2])$estimate,
                       -1*   EL.stm(belg, switz, alpha[2], gamma[3])$estimate,
                       -1*     EL.stm(belg, switz, alpha[2], gamma[4])$estimate,
                       -1*     EL.stm(belg, switz, alpha[3], gamma[2])$estimate,
                       -1*     EL.stm(belg, switz, alpha[3], gamma[3])$estimate,
                       -1*     EL.stm(belg, switz, alpha[3], gamma[4])$estimate,
                       -1*     EL.stm(belg, switz, alpha[4], gamma[3])$estimate,
                       -1*     EL.stm(belg, switz, alpha[4], gamma[4])$estimate),
                     alpha = c(alpha[2],alpha[2],alpha[2],
                               alpha[3],alpha[3],alpha[3],
                               alpha[4],alpha[4]),
                     mean = 'STM',
                     method = 'el',
                     gamma = c(gamma[2],gamma[3],gamma[4],
                               gamma[2],gamma[3],gamma[4],
                               gamma[3],gamma[4]))


ci.stm.y<- data.frame(lb = c(
  yuen.stm(belg, switz, alpha[2], gamma[2])$conf.int[1],
  yuen.stm(belg, switz, alpha[2], gamma[3])$conf.int[1],
  yuen.stm(belg, switz, alpha[2], gamma[4])$conf.int[1],
  yuen.stm(belg, switz, alpha[3], gamma[2])$conf.int[1],
  yuen.stm(belg, switz, alpha[3], gamma[3])$conf.int[1],
  yuen.stm(belg, switz, alpha[3], gamma[4])$conf.int[1],
  yuen.stm(belg, switz, alpha[4], gamma[3])$conf.int[1],
  yuen.stm(belg, switz, alpha[4], gamma[4])$conf.int[1]),
  
  ub = c(
    yuen.stm(belg, switz, alpha[2], gamma[2])$conf.int[2],
    yuen.stm(belg, switz, alpha[2], gamma[3])$conf.int[2],
    yuen.stm(belg, switz, alpha[2], gamma[4])$conf.int[2],
    yuen.stm(belg, switz, alpha[3], gamma[2])$conf.int[2],
    yuen.stm(belg, switz, alpha[3], gamma[3])$conf.int[2],
    yuen.stm(belg, switz, alpha[3], gamma[4])$conf.int[2],
    yuen.stm(belg, switz, alpha[4], gamma[3])$conf.int[2],
    yuen.stm(belg, switz, alpha[4], gamma[4])$conf.int[2]),
  
  est = c(
    -1* yuen.stm(belg, switz, alpha[2], gamma[2])$estimate,
    -1*yuen.stm(belg, switz, alpha[2], gamma[3])$estimate,
    -1*yuen.stm(belg, switz, alpha[2], gamma[4])$estimate,
    -1*yuen.stm(belg, switz, alpha[3], gamma[2])$estimate,
    -1*yuen.stm(belg, switz, alpha[3], gamma[3])$estimate,
    -1*yuen.stm(belg, switz, alpha[3], gamma[4])$estimate,
    -1*yuen.stm(belg, switz, alpha[4], gamma[3])$estimate,
    -1*yuen.stm(belg, switz, alpha[4], gamma[4])$estimate),
  alpha = c(alpha[2],alpha[2],alpha[2],
            alpha[3],alpha[3],alpha[3],
            alpha[4],alpha[4]),
  mean = 'STM',
  method = 't',
  gamma = c(gamma[2],gamma[3],gamma[4],
            gamma[2],gamma[3],gamma[4],
            gamma[3],gamma[4]))

ci.tm.y<- data.frame(lb = c(
  yuen.tm(los~gr, data = los.dati, alpha[2])$conf.int[1],
  yuen.tm(los~gr, data = los.dati, alpha[3])$conf.int[1],
  yuen.tm(los~gr, data = los.dati, alpha[4])$conf.int[1]),
  
  ub = c(
    yuen.tm(los~gr, data = los.dati, alpha[2])$conf.int[2],
    yuen.tm(los~gr, data = los.dati, alpha[3])$conf.int[2],
    yuen.tm(los~gr, data = los.dati, alpha[4])$conf.int[2]),
  
  est = c(
    yuen.tm(los~gr, data = los.dati, alpha[2])$diff,
    yuen.tm(los~gr, data = los.dati, alpha[3])$diff,
    yuen.tm(los~gr, data = los.dati, alpha[4])$diff),
  alpha = c(alpha[2],alpha[3],alpha[4]),
  mean = 'TM',
  method = 't',
  gamma = NA)

ci.tm.el <- data.frame(lb = c(
  EL.tm(belg, switz, alpha = alpha[2],beta = alpha[2])$conf.int[1],
  EL.tm(belg, switz, alpha = alpha[3],beta= alpha[3])$conf.int[1],
  EL.tm(belg, switz, alpha = alpha[4],beta = alpha[4])$conf.int[1]),
  ub = c(
    EL.tm(belg, switz, alpha = alpha[2],beta = alpha[2])$conf.int[2],
    EL.tm(belg, switz, alpha = alpha[3],beta = alpha[3])$conf.int[2],
    EL.tm(belg, switz, alpha = alpha[4],beta = alpha[4])$conf.int[2]),
  
  est = c(
    EL.tm(belg, switz, alpha = alpha[2],beta = alpha[2])$estimate,
    EL.tm(belg, switz, alpha = alpha[3],beta = alpha[3])$estimate,
    EL.tm(belg, switz, alpha = alpha[4],beta = alpha[4])$estimate),
  alpha = c(alpha[2],alpha[3],alpha[4]),  
  mean = 'TM',
  method = 'el',
  gamma = NA)

ci.t <- data.frame(lb= t.test(belg, switz)$conf.int[1],
                   ub = t.test(belg, switz)$conf.int[2],
                   est = mean(switz) - mean(belg))



ci.los <- rbind(ci.stm.el,
                ci.stm.y,
                ci.tm.el,
                ci.tm.y)


ci.los <- ci.los %>% 
  arrange(alpha, method) %>% 
  mutate(ci_method = ifelse(mean == 'STM', 
                            paste0(method, "~", mean, "~alpha==", alpha, "*','~gamma==", gamma),
                            paste0(method, "~", mean, "~alpha==", alpha)))


ci.los %>% 
  # arrange(alpha, method) %>% 
  ggplot() +
  geom_point(aes(x = ci_method, y = est)) +
  geom_errorbar(aes(x = ci_method, ymin = lb, ymax = ub, col = as.character(alpha))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none", panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "gray90")) +
  ylab('Confidence intervals') +
  xlab('Method')+
  scale_x_discrete(labels = function(x) parse(text = x))+
  geom_hline(yintercept = 0, lty = 2, size = 1)

ggsave('ci_los.png', bg = 'white')


ci.los %>% 
  rbind(data.frame(lb = t.test(switz, belg)$conf.int[1],
                   ub = t.test(switz, belg)$conf.int[2],
                   est = mean(switz) - mean(belg),
                   alpha =NA,
                   gamma =NA,
                   method = NA,
                   mean = NA,
                   ci_method = 't-test')) %>% 
  ggplot() +
  geom_point(aes(x = ci_method, y = est)) +
  geom_errorbar(aes(x = ci_method, ymin = lb, ymax = ub, col = as.character(alpha))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none", panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "gray90")) +
  ylab('Confidence intervals') +
  xlab('Method')+
  scale_x_discrete(labels = function(x) parse(text = x))+
  geom_hline(yintercept = 0, lty = 2, size = 1)

ggsave('ci_los_art.png', bg = 'white')





los.dati %>% 
  group_by(gr) %>% 
  summarise(n = n(),
            mean = mean(los),
            sd = sd(los))


# Rat dati ----

rat.dati0 <- openxlsx::read.xlsx('datu_kopas.xlsx', sheet = 'Rat')
rat.dati <- na.omit(data.frame(data =c(rat.dati0$Control, rat.dati0$Ozone), gr = c(rep('Control', length(rat.dati0$Control)), rep('Ozone', length(rat.dati0$Ozone)))))


rat.dati %>% 
  ggplot(aes(x = gr, y = data))+
  geom_boxplot()+
  theme_minimal()+
  xlab('')+
  ylab('Rat weight gain')

ggsave('bp_rat.png', bg = 'white')


rat.dati %>% 
  filter(gr == 'Control') %>% 
  ggplot(aes(x = data)) +
  geom_histogram(aes(y = ..density..), col = 'black', fill = 'white')+
  theme_minimal()+
  xlab('Control')

ggsave('hist_rat_control.png', bg = 'white')


rat.dati %>% 
  filter(gr == 'Ozone') %>% 
  ggplot(aes(x = data)) +
  geom_histogram(aes(y = ..density..), col = 'black', fill = 'white')+
  theme_minimal()+
  xlab('Ozone')

ggsave('hist_rat_ozone.png', bg = 'white')




ci.stm.el <- data.frame(ub = c(
  -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[2])$conf.int[1],
  -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[3])$conf.int[1],
  -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[4])$conf.int[1],
  -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[2])$conf.int[1],
  -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[3])$conf.int[1],
  -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[4])$conf.int[1],
  -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[4], gamma[3])$conf.int[1],
  -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[4], gamma[4])$conf.int[1]),
  
  lb = c(
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[2])$conf.int[2],
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[3])$conf.int[2],
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[4])$conf.int[2],
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[2])$conf.int[2],
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[3])$conf.int[2],
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[4])$conf.int[2],
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[4], gamma[3])$conf.int[2],
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[4], gamma[4])$conf.int[2]),
  
  est = c(
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[2])$estimate,
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[3])$estimate,
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[2], gamma[4])$estimate,
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[2])$estimate,
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[3])$estimate,
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[3], gamma[4])$estimate,
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[4], gamma[3])$estimate,
    -1*EL.stm(rat.dati0$Control, rat.dati0$Ozone, alpha[4], gamma[4])$estimate),
  alpha = c(alpha[2],alpha[2],alpha[2],
            alpha[3],alpha[3],alpha[3],
            alpha[4],alpha[4]),
  mean = 'STM',
  method = 'el',
  gamma = c(gamma[2],gamma[3],gamma[4],
            gamma[2],gamma[3],gamma[4],
            gamma[3],gamma[4]))



ci.stm.y<- data.frame(lb = c(
  yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[2])$conf.int[1],
  yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[3])$conf.int[1],
  yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[4])$conf.int[1],
  yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[2])$conf.int[1],
  yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[3])$conf.int[1],
  yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[4])$conf.int[1],
  yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[4], gamma[3])$conf.int[1],
  yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[4], gamma[4])$conf.int[1]),
  
  ub = c(
    yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[2])$conf.int[2],
    yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[3])$conf.int[2],
    yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[4])$conf.int[2],
    yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[2])$conf.int[2],
    yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[3])$conf.int[2],
    yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[4])$conf.int[2],
    yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[4], gamma[3])$conf.int[2],
    yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[4], gamma[4])$conf.int[2]),
  
  est = c(
    -1*yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[2])$estimate,
    -1*yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[3])$estimate,
    -1*yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[2], gamma[4])$estimate,
    -1*yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[2])$estimate,
    -1*yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[3])$estimate,
    -1*yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[3], gamma[4])$estimate,
    -1*yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[4], gamma[3])$estimate,
    -1*yuen.stm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha[4], gamma[4])$estimate),
  alpha = c(alpha[2],alpha[2],alpha[2],
            alpha[3],alpha[3],alpha[3],
            alpha[4],alpha[4]),
  mean = 'STM',
  method = 't',
  gamma = c(gamma[2],gamma[3],gamma[4],
            gamma[2],gamma[3],gamma[4],
            gamma[3],gamma[4]))


ci.tm.y<- data.frame(lb = c(
  yuen.tm(data~gr, data = rat.dati, alpha[2])$conf.int[1],
  yuen.tm(data~gr, data = rat.dati, alpha[3])$conf.int[1],
  yuen.tm(data~gr, data = rat.dati, alpha[4])$conf.int[1]),
  
  ub = c(
    yuen.tm(data~gr, data = rat.dati, alpha[2])$conf.int[2],
    yuen.tm(data~gr, data = rat.dati, alpha[3])$conf.int[2],
    yuen.tm(data~gr, data = rat.dati, alpha[4])$conf.int[2]),
  
  est = c(
    yuen.tm(data~gr, data = rat.dati, alpha[2])$diff,
    yuen.tm(data~gr, data = rat.dati, alpha[3])$diff,
    yuen.tm(data~gr, data = rat.dati, alpha[4])$diff),
  alpha = c(alpha[2],alpha[3],alpha[4]),
  mean = 'TM',
  method = 't',
  gamma = NA)

ci.tm.el <- data.frame(lb = c(
  EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[2],beta = alpha[2])$conf.int[1],
  EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[3],beta= alpha[3])$conf.int[1],
  EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[4],beta = alpha[4])$conf.int[1]),
  ub = c(
    EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[2],beta = alpha[2])$conf.int[2],
    EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[3],beta = alpha[3])$conf.int[2],
    EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[4],beta = alpha[4])$conf.int[2]),
  
  est = c(
    EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[2],beta = alpha[2])$estimate,
    EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[3],beta = alpha[3])$estimate,
    EL.tm(rat.dati0$Control,na.omit(rat.dati0$Ozone), alpha = alpha[4],beta = alpha[4])$estimate),
  alpha = c(alpha[2],alpha[3],alpha[4]),  
  mean = 'TM',
  method = 'el',
  gamma = NA)


ci.rat <- rbind(ci.stm.el,
                ci.stm.y,
                ci.tm.el,
                ci.tm.y)


ci.rat <- ci.rat %>% 
  arrange(alpha, method) %>% 
  mutate(ci_method = ifelse(mean == 'STM', 
                            paste0(method, "~", mean, "~alpha==", alpha, "*','~gamma==", gamma),
                            paste0(method, "~", mean, "~alpha==", alpha)))


# ci.rat %>% 
#   # arrange(alpha, method) %>% 
#   ggplot() +
#   geom_point(aes(x = ci_method, y = est)) +
#   geom_errorbar(aes(x = ci_method, ymin = lb, ymax = ub, col = as.character(alpha))) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
#         legend.position = "none", panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "gray90")) +
#   ylab('Confidence intervals') +
#   xlab('Method')+
#   scale_x_discrete(labels = function(x) parse(text = x))+
#   geom_hline(yintercept = 0, lty = 2, size = 1)
# 
# ggsave('ci_rat.png')


ci.rat %>% 
  rbind(data.frame(lb = t.test(rat.dati0$Control,na.omit(rat.dati0$Ozone))$conf.int[1],
                   ub = t.test(rat.dati0$Control,na.omit(rat.dati0$Ozone))$conf.int[2],
                   est = -mean(na.omit(rat.dati0$Ozone)) + mean(rat.dati0$Control),
                   alpha =NA,
                   gamma =NA,
                   method = NA,
                   mean = NA,
                   ci_method = 't-test')) %>% 
  ggplot() +
  geom_point(aes(x = ci_method, y = est)) +
  geom_errorbar(aes(x = ci_method, ymin = lb, ymax = ub, col = as.character(alpha))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none", panel.background = element_rect(fill = "white"),panel.grid.major = element_line(colour = "gray90")) +
  ylab('Confidence intervals') +
  xlab('Method')+
  scale_x_discrete(labels = function(x) parse(text = x))+
  geom_hline(yintercept = 0, lty = 2, size = 1)

ggsave('ci_rat_art.png', bg = 'white')





los.dati %>% 
  group_by(gr) %>% 
  summarise(n = n(),
            mean = mean(los),
            sd = sd(los)) %>% 
  rbind(rat.dati %>% 
          group_by(gr) %>% 
          summarise(n = n(),
                    mean = mean(data),
                    sd = sd(data)) ) %>% 
  xtable::xtable()





# ANOVA ----
library(rrcov)
data(OsloTransect)
# glimpse(OsloTransect)
# X.FLITHO  

# grp <- OsloTransect$X.FLITHO
# ind <- which(grp =="CAMSED" | grp == "GNEIS_O" |
#                grp == "GNEIS_R" | grp=="MAGM")

OsloTransect <- OsloTransect %>% 
  filter(X.FLITHO %in% c('CAMSED', 'GNEIS_O', 'GNEIS_R', 'MAGM'))
OsloTransect$X.FLITHO <- as.character(OsloTransect$X.FLITHO)

OsloTransect <- OsloTransect %>% 
  select( FILTHO = X.FLITHO , Ag_ppb,  B,    Ba,    Ca,   Cd,   Co,  Cr,   Cu,  Fe, Hg_ppb,     
         K   ,La  ,Mg   ,Mn   ,Mo   ,Ni    ,P ,Pb    ,S   ,Sb    ,Sr ,Ti    ,Zn) %>% 
  na.omit()


Cd <- OsloTransect %>% 
  group_by(FILTHO) %>% 
  summarise(`STM 0.05 0.3` = ST_mean(Cd, alpha = 0.05, gamma = 0.3),
            `STM 0.05 0.4` = ST_mean(Cd, alpha = 0.05, gamma = 0.4),
            `TM 0.05` = mean(Cd, trim = 0.05, na.rm = TRUE),
            `STM 0.1 0.3` = ST_mean(Cd, alpha = 0.1, gamma = 0.3),
            `STM 0.1 0.4` = ST_mean(Cd, alpha = 0.1, gamma = 0.4),
            `TM 0.1` = mean(Cd, trim = 0.1, na.rm = TRUE),
            `STM 0.2 0.3` = ST_mean(Cd, alpha = 0.2, gamma = 0.3),
            `STM 0.2 0.4` = ST_mean(Cd, alpha = 0.2, gamma = 0.4),
            `TM 0.2` = mean(Cd, trim = 0.2, na.rm = TRUE),
            Mean = mean(Cd,na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = 2:11) %>% 
  ggplot(aes(x = FILTHO, y = value, group = name, col = name))+
  geom_point()+
  geom_line()+
  # scale_color_discrete(labels = c(expression("STM"~alpha == 0.05~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.05~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.05),
  #                                 expression("STM"~alpha == 0.1~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.1~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.15),
  #                                 expression("STM"~alpha == 0.2~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.2~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.2), 
  #                                 'Mean'))+
  ylab('Cd')+
  xlab('')+
  ylim(0.1,0.25)+
  theme_minimal()

# ggsave('Cd.png', bg = 'white')


Mo <- OsloTransect %>% 
  group_by(FILTHO) %>% 
  summarise(`STM 0.05 0.3` = ST_mean(Mo, alpha = 0.05, gamma = 0.3),
            `STM 0.05 0.4` = ST_mean(Mo, alpha = 0.05, gamma = 0.4),
            `TM 0.05` = mean(Mo, trim = 0.05, na.rm = TRUE),
            `STM 0.1 0.3` = ST_mean(Mo, alpha = 0.1, gamma = 0.3),
            `STM 0.1 0.4` = ST_mean(Mo, alpha = 0.1, gamma = 0.4),
            `TM 0.1` = mean(Mo, trim = 0.1, na.rm = TRUE),
            `STM 0.2 0.3` = ST_mean(Mo, alpha = 0.2, gamma = 0.3),
            `STM 0.2 0.4` = ST_mean(Mo, alpha = 0.2, gamma = 0.4),
            `TM 0.2` = mean(Mo, trim = 0.2, na.rm = TRUE),
            Mean = mean(Mo,na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = 2:11) %>% 
  ggplot(aes(x = FILTHO, y = value, group = name, col = name))+
  geom_point()+
  geom_line()+
  # scale_color_discrete(labels = c(expression("STM"~alpha == 0.05~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.05~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.05),
  #                                 expression("STM"~alpha == 0.1~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.1~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.15),
  #                                 expression("STM"~alpha == 0.2~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.2~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.2), 
  #                                 'Mean'))+
  ylab('Mo')+
  xlab('')+
  # ylim(0.025,0.13)+
  theme_minimal()

# ggsave('Mo.png', bg = 'white')


Ti <- OsloTransect %>% 
  group_by(FILTHO) %>% 
  summarise(`STM 0.05 0.3` = ST_mean(Ti, alpha = 0.05, gamma = 0.3),
            `STM 0.05 0.4` = ST_mean(Ti, alpha = 0.05, gamma = 0.4),
            `TM 0.05` = mean(Ti, trim = 0.05, na.rm = TRUE),
            `STM 0.1 0.3` = ST_mean(Ti, alpha = 0.1, gamma = 0.3),
            `STM 0.1 0.4` = ST_mean(Ti, alpha = 0.1, gamma = 0.4),
            `TM 0.1` = mean(Ti, trim = 0.1, na.rm = TRUE),
            `STM 0.2 0.3` = ST_mean(Ti, alpha = 0.2, gamma = 0.3),
            `STM 0.2 0.4` = ST_mean(Ti, alpha = 0.2, gamma = 0.4),
            `TM 0.2` = mean(Ti, trim = 0.2, na.rm = TRUE),
            Mean = mean(Ti,na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_longer(cols = 2:11) %>% 
  ggplot(aes(x = FILTHO, y = value, group = name, col = name))+
  geom_point()+
  geom_line()+
  # scale_color_discrete(labels = c(expression("STM"~alpha == 0.05~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.05~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.05),
  #                                 expression("STM"~alpha == 0.1~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.1~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.15),
  #                                 expression("STM"~alpha == 0.2~","~gamma == 0.3), 
  #                                 expression("STM"~alpha == 0.2~","~gamma == 0.4), 
  #                                 expression("TM"~alpha == 0.2), 
  #                                 'Mean'))+
  ylab('Ti')+
  xlab('')+
  # ylim(0.025,0.13)+
  theme_minimal()

# ggsave('Ti.png', bg = 'white')

ggpubr::ggarrange(Cd, Mo, Ti, common.legend = TRUE, legend = 'bottom')
ggsave('elementi.png', bg = 'white')

Ag <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ag_ppb))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Ag_ppb')

B <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = B))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('B')

Ba <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ba))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('BA')

Ca <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ca))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Ca')

Cd <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = B))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Cd')

Co <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Co))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Co')

Cr <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Cr))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Cr')

Cu <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Cu))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Cu')

Fe <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Fe))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Fe')

Hg <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Hg_ppb))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Hg_ppb')

K <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = K))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('K')

La <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = La))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('La')

Mg <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Mg))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Mg')

Mn <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Mn))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Mn')

Mo <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Mo))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Mo')

Ni <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ni))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Ni')

P <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = P))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('P')

Pb <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Pb))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Pb')

S <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = S))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('S')

Sb <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Sb))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Sb')

Sr <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Sr))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Sr')

Ti <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Ti))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Ti')

Zn <- OsloTransect %>% 
  ggplot(aes(x = FILTHO, y = Zn))+
  geom_boxplot()+
  theme_minimal()+
  ylab('')+
  xlab('')+
  ggtitle('Zn')


names(OsloTransect)
ggpubr::ggarrange(Ag, B, Ba, Ca, Cd, Co, Cu, Fe, Hg, K, La)
ggsave('bp_anova1.png', width = 13, bg = 'white')

ggpubr::ggarrange(Mg, Mn, Mo, Ni, P, Pb, S, Sb, Sr, Ti, Zn)
ggsave('bp_anova2.png', width = 13, bg = 'white')

rez1 <- OsloTransect %>% 
  pivot_longer(cols = 2:24) %>% 
  group_by(FILTHO, name) %>% 
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(mean = paste(round(mean,3), ' (', round(sd,3), ')', sep = '')) %>% 
  select(-sd) %>% 
  pivot_wider(names_from = 1, values_from = 3)

rez2 <- OsloTransect %>% 
  select(-FILTHO) %>% 
  pivot_longer(cols = 1:23) %>% 
  group_by(name) %>% 
  summarise(`Whole cohort` = paste(round(mean(value, na.rm = TRUE),3), ' (', round(sd(value, na.rm = TRUE),3), ')', sep = ''))

rez <- left_join(rez1, rez2)

OsloTransect %>% 
  group_by(FILTHO) %>% 
  summarise(n())

names(rez) <- c('Element', 'Camsed (n = 99)', 'Geneis_0 (n = 90)', 'Geneis_R (n = 36)', 'Magm (n = 117)', 'Whole data set (n = 342)')



alpha <- c(0.05, 0.1, 0.2)
gamma <- c(0.3, 0.4)


welch_p <- data.frame(p_w = c(welch_anova_test(data = OsloTransect, Ag_ppb ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, B ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Ba ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Ca ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Cd ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Co ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Cr ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Cu ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Fe ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Hg_ppb ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, K ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, La ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Mg ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Mn ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Mo ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Ni ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, P ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Pb ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, S ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Sb ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Sr ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Ti ~ FILTHO)$p,
welch_anova_test(data = OsloTransect, Zn ~ FILTHO)$p), 
Element = names(OsloTransect)[2:length(names(OsloTransect))])

f_p <- data.frame(p_f = c(summary(aov( Ag_ppb ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( B ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Ba ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Ca ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Cd ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Co ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Cr ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Cu ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Fe ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Hg_ppb ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( K ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( La ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Mg ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Mn ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Mo ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Ni ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( P ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Pb ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( S ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Sb ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Sr ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Ti ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1],
                        summary(aov( Zn ~ FILTHO, data = OsloTransect))[[1]]$`Pr(>F)`[1]),
                  Element = names(OsloTransect)[2:length(names(OsloTransect))])

y_p <- data.frame(py_005 = c(t1way( Ag_ppb ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( B ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Ba ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Ca ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Cd ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Co ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Cr ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Cu ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Fe ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Hg_ppb ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( K ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( La ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Mg ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Mn ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Mo ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Ni ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( P ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Pb ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( S ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Sb ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Sr ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Ti ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value,
                            t1way( Zn ~ FILTHO, data = OsloTransect, tr = 0.05, nboot = 1)$p.value),
                  py_01 = c(t1way( Ag_ppb ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( B ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Ba ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Ca ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Cd ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Co ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Cr ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Cu ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Fe ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Hg_ppb ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( K ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( La ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Mg ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Mn ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Mo ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Ni ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( P ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Pb ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( S ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Sb ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Sr ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Ti ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value,
                            t1way( Zn ~ FILTHO, data = OsloTransect, tr = 0.1, nboot = 1)$p.value),
                  py_02 = c(t1way( Ag_ppb ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( B ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Ba ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Ca ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Cd ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Co ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Cr ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Cu ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Fe ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Hg_ppb ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( K ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( La ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Mg ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Mn ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Mo ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Ni ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( P ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Pb ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( S ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Sb ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Sr ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Ti ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value,
                            t1way( Zn ~ FILTHO, data = OsloTransect, tr = 0.2, nboot = 1)$p.value),
                  Element = names(OsloTransect)[2:length(names(OsloTransect))])




ely_p <- data.frame(pel_005 = c(EL.anova.tm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value,
                              EL.anova.tm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.05, beta = 0.05)$p.value),
                    
                    pel_01 = c(EL.anova.tm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                             EL.anova.tm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value,
                              EL.anova.tm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.1, beta = 0.1)$p.value),
                    pel_02 = c(EL.anova.tm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                             EL.anova.tm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value,
                              EL.anova.tm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.2, beta = 0.2)$p.value),
                    Element = names(OsloTransect)[2:length(names(OsloTransect))])


stm_p <- data.frame(pel_005_g1 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value,
                              EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.3)$p.value),
                    pel_005_g2 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.05, gamma = 0.4)$p.value),
                    pel_01_g1 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.3)$p.value),
                    pel_01_g2 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.1, gamma = 0.4)$p.value),
                    pel_02_g1 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value,
                                 EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.3)$p.value),
                    pel_02_g2 = c(EL.anova.stm(split(OsloTransect$Ag_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$B, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ba, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ca, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cd, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Co, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Cu, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Fe, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Hg_ppb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$K, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$La, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mg, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Mo, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ni, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$P, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Pb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$S, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sb, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Sr, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Ti, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value,
                                 EL.anova.stm(split(OsloTransect$Zn, OsloTransect$FILTHO), alpha = 0.2, gamma = 0.4)$p.value),
                    Element = names(OsloTransect)[2:length(names(OsloTransect))])


tab.anova <- left_join(welch_p, left_join(f_p,left_join(y_p,left_join(ely_p,stm_p))))

openxlsx::write.xlsx(tab.anova, 'ANOVA_rez.xlsx')

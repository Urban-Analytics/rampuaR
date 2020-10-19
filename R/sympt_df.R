#based on data from: 
#https://www.nature.com/articles/s41591-020-0962-9
#figure 3a


df <- data.frame(age = 0:120, sympt_prob = c(rep(0.21, 19), approx(y = c(0.21, 0.69), x = c(19, 70), xout = 19:70)$y, rep(0.69, 50)))


# Estimação do Tempo Médio de Espera por cliente


set.seed(12345)
tme.soma <- mm1.tme()
k <- 100
while(sd(tme.soma)/k > 10^(-2)){
  append(tme.soma, mm1.tme)
  k <- length(tme.soma)
}
tme <- tme.soma/k
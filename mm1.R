# Estimação do Tempo Médio de Espera por cliente


set.seed(12345)
tme.vetor <- mm1.tme()
k <- 100
while(sd(tme.vetor)/k > 10^(-1)){
  append(tme.vetor, mm1.tme())
  k <- length(tme.vetor)
}
tme <- mean(tme.vetor)
# TME = 5.087305

while(2*pnorm(0.975)*sd(tme.vetor)/sqrt(k) > 1){
  append(tme.vetor, mm1.tme())
  k <- length(tme.vetor)
}
pnorm(0.975)*sd(tme.vetor)/sqrt(k)

set.seed(12345)
to.vetor <- mm1.ppo()
k <- 100
while(sd(to.vetor)/k > 10^(-1)){
  append(to.vetor, mm1.ppo())
  k <- length(to.vetor)
}
to <- mean(to.vetor)
to
# to = 0.5940587

while(2*pnorm(0.975)*sd(to.vetor)/sqrt(k) > 10^(-1)){
  append(to.vetor, mm1.ppo())
  k <- length(to.vetor)
}
pnorm(0.975)*sd(to.vetor)/sqrt(k)

set.seed(12345)
tmf.vetor <- mm1.tmf()
k <- 100
while(sd(tmf.vetor)/k > 10(-1)){
  append(tmf.vetor,mm1.tmf())
  k<-length(tmf.vetor)
}
tmf <- mean(tmf.vetor)
tmf
# TMF = 0.007997178

while(2*pnorm(0.975)*sd(tmf.vetor)/sqrt(k) > 10^(-1)){
  append(tmf.vetor, mm1.tmf())
  k <- length(tmf.vetor)
}
pnorm(0.975)*sd(tmf.vetor)/sqrt(k)

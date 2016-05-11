# Simulação de fila M/M/1
lambda <- 20 #taxa de chegada
mu <- 5000 #taxa de atendimento
tempo <- 1000 # tempo de simulação
t <- 0 # instante atual da simulação
fila <- 0 # fila inicialmente com tamanho zero
s <- 0 # soma acumulada para calcular o tamanho médio da fila
i <- 1

#Primeira chegada
t1 <- rexp(1, rate = lambda)
fila_atual <- 1
t_evento <- t1 #"tempo" do evento inicial
t <- t1
n_evento <- 1 #Número do evento inicial

while(t < tempo){
  n_evento <- n_evento + 1
  if(fila_atual > 0){ # A fila não está vazia
    t1 <- rexp(1, rate = lambda + mu) # Tempo até o próximo evento
    #o evento é chegada ou atendimento?
    p <- runif(1)
    fila[n_evento] <- fila_atual #Atualiza a fila pro n-esimo evento/indice
    fila_atual <- ifelse(p<lambda/(lambda + mu),
                         fila_atual+1, # chegada
                         fila_atual-1) # partida
  }
  else{ # a fila esta vazia
    t1 <- rexp(1, rate = lambda)
    fila[n_evento] <- fila_atual
    fila_atual <- 1
  }
  t <- t + t1
  t_evento[n_evento] <- t1
  s <- s+t1*fila[n_evento]
}

plot(cumsum(t_evento),fila,type="s", xlab="Tempo",ylab="Tamanho da fila",
     main=paste("Simulação de fila M/M/1\nTamanho Médio da Fila ",s/t))

# lambda = 50
# mu = 150
# t = 0
# i = 1
# T = 1000
# tEvento <- vector(length=T+2)
# while(t<=T){
#   tec <- -log(1-runif(1))/lambda
#   tEvento[i] <- t + tec
#   t = tEvento[i]
#   i <- i + 1
# }
# 
# t_0 <- tEvento[1]
# te <- 0
# i = 1
# t <- tEvento[1]
# ta <- vector(length = T)
# 
# while(i <= length(tEvento)-1){
#   if(t + ta[i] < tEvento[i+1]){
#     t_0 = t_0 + tEvento[i+1] - (t + ta[i])
#     t = tEvento[i+1]
#   }
#   else{
#     te <- te + (t + ta[i]) - tEvento[i+1]
#     t <- t + ta[i]
#   }
#   i <- i + 1
# }
# 
# # Tempo médio de Espera:
# te/(length(tEvento)-1)
# 
# 
# #Proporção de ociosidde
# t_0/T
# 
# #Tamanho médio da fila
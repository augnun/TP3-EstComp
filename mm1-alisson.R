#número dw clientes que entram no sistema a cada unidade de tempo (poison)
#tempo entre clientes (Exponencial)
#mi - Numero de atendimentos (Poisson)
#1/mi - Tempo de cada atendimento (Exponencial)


t.exp=function(){
  t=0;i=1;tc=NULL;tec=NULL #com tc e tec nulos
  T=20
  lambda=5
  while(t < T){
    tec[i] = (-(1/lambda))*log(runif(1))
    tc[i] = sum(tc[i-1]) + tec[i]
    t=tc[i]
    t
    i=i+1
    return(tc)
  }
}



##### INSERIR CODIGO MM1.R


# t=0;i=1;lambda = 5;T=20;tc=NULL;tec=NULL
# while (t < T){
#   tec[i] = (-(1/lambda))*log(runif(1))
#   tc[i] = sum(tc[i-1]) + tec[i]
#   t=tc[i]
#   t
#   i=i+1
# }
# 
# ta=NULL
# ta=tec
# 
# ta
# tc


#TO TEMPO OCIOSO
to = tc[1]
te = 0
i = 1
t = tc[1]
length(tc)
n=100

while (i < n){
  if((t+ta[i]) < tc[i+1]){
    to=to+tc[i+1]-(t+ta[i])
    t=tc[i+1]
  }
  else{
    te=te+(t+ta[i])-tc[i+1]
    t=t+ta[i]
  }
  i=i+1
}
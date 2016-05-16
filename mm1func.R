mm1.tme <- function(lambda = 20,
                    mu = 50,
                    tempo = 1000,
                    n=100) {
  tme <- vector()
  a <- 1
  while (a < n)
  {
    t <- 0
    i <- 1
    tc <- vector()
    ta <- vector()
    while (t < tempo) {
      tec <- rexp(1, rate = lambda)
      tc[i] <- t + tec
      t <- tc[i]
      i <- i + 1
    }
    to <- tc[1]
    te <- 0
    j <- 1
    t <- tc[1]
    while (j <= length(tc) - 1) {
      ta[j] <- rexp(1, rate = lambda) #tempo de atendimento exp com taxa mu
      if (t + ta[j] < tc[j + 1]) {
        to <- to + tc[j + 1] - (t + ta[j])
        t <- tc[j + 1]
      }
      else{
        te <- te + (t + ta[j]) - tc[j + 1]
        t <- t + ta[j]
      }
      j <- j + 1
    }
    tme[a] <- (te*length(tc))/ tempo
    a <- a + 1
  }
  return(tme)
}

mm1.ppo <- function(lambda = 20,
                    mu = 50,
                    tempo = 1000,
                    n=100) {
  ppo <- vector(length = n)
  a <- 1
  while (a < n)
  {
    t <- 0
    i <- 1
    tc <- vector()
    ta <- vector()
    while (t < tempo) {
      tec <- rexp(1, rate = lambda)
      tc[i] <- t + tec
      t <- tc[i]
      i <- i + 1
    }
    to <- tc[1]
    te <- 0
    j <- 1
    t <- tc[1]
    while (j <= length(tc) - 1) {
      ta[j] <- rexp(1, rate = mu) #tempo de atendimento exp com taxa mu
      if (t + ta[j] < tc[j + 1]) {
        to <- to + tc[j + 1] - (t + ta[j])
        t <- tc[j + 1]
      }
      else{
        te <- te + (t + ta[j]) - tc[j + 1]
        t <- t + ta[j]
      }
      j <- j + 1
    }
    ppo[a] <- to / tempo
    a <- a + 1
  }
  return(ppo)
}

mm1.tmf <- function(lambda = 20,
                    mu = 50,
                    tempo = 1000,
                    n=100) {
  tmf <- vector(length = n)
  a <- 1
  while (a <= n)
  {
    t <- 0
    i <- 1
    tc <- vector()
    ta <- vector()
    
    s <- 0
    while (t < tempo) {
      tec <- rexp(1, rate = lambda)
      tc[i] <- t + tec
      t <- tc[i]
      i <- i + 1
    }
    fila <- vector(length=length(tc))
    to <- tc[1]
    te <- 0
    j <- 1
    t <- tc[1]
    while (j <= length(tc) - 1) {
      ta[j] <- rexp(1, rate = mu) #tempo de atendimento exp com taxa mu
      if (t + ta[j] < tc[j + 1]) {
        to <- to + tc[j + 1] - (t + ta[j])
        t <- tc[j + 1]
      }
      else{
        te <- te + (t + ta[j]) - tc[j + 1]
        t <- t + ta[j]
        fila[j] <- fila[j] + 1
        s <- s + fila[j]*(tc[j+1]-tc[j])
        
      }

      j <- j + 1

    }
    tmf[a] <- s / length(tc)
    a <- a + 1
  }
  return(tmf)
}
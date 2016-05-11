mm1.tme <- function(n = 1000,
                    lambda = 20,
                    tempo = 1000) {
  tme <- vector(length = n)
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
      ta[j] <- rexp(1, rate = lambda)
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
    tme[a] <- te / (length(tc) - 1)
    a <- a + 1
  }
  return(tme)
}

mm1.ppo <- function(n = 1000,
                    lambda = 20,
                    tempo = 1000) {
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
      ta[j] <- rexp(1, rate = lambda)
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
    ppo[a] <- to / tail(te,1)
    a <- a + 1
  }
  return(ppo)
}

mm1.tmf <- function(n = 1000,
                    lambda = 20,
                    tempo = 1000) {
  tmf <- vector(length = n)
  a <- 1
  while (a < n)
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
    to <- tc[1]
    te <- 0
    j <- 1
    t <- tc[1]
    while (j <= length(tc) - 1) {
      ta[j] <- rexp(1, rate = lambda)
      if (t + ta[j] < tc[j + 1]) {
        to <- to + tc[j + 1] - (t + ta[j])
        t <- tc[j + 1]
      }
      else{
        te <- te + (t + ta[j]) - tc[j + 1]
        t <- t + ta[j]
        s <- s + te
      }

      j <- j + 1

    }
    tmf[a] <- te / length(tc)
    a <- a + 1
  }
  return(tmf)
}
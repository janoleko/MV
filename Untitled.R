g = function(k,p,q){
  choose(2*k,k)*((p*q)^k)/(2*k-1)
}

v = rep(NA, 200)

for (i in 1:200){
  v[i] = g(i, 0.2, 0.8)
}


G = function(s,n,p){
  q = 1-p
  k = seq(from = 1, to = n, by = 1)
  P = choose(2*k,k)*((p*q)^k)/(2*k-1)
  S = s^(2*k)
  sum(S*P)
}

G(1,1:500, 0.7)

A = rep(NA, 500)

for (i in 1:500){
  A[i] = G(.9,i,0.5)
}
plot(A, ylim = c(0,1), type = "l")



# Fred the frog ----------------------------------------------------------

MK = function(start, alpha, beta, n){
  mk = rep(NA, n)
  mk[1] = start
  for (i in 2:n){
    if (mk[i-1] == 0){
      mk[i] = sample(c(0,1), 1, prob = c(1-alpha, alpha))
    }
    else{
      mk[i] = sample(c(0,1), 1, prob = c(beta, 1-beta))
    }
  }
  mk
}


n = 100

mk = MK(1, .5, .5, n)

plot(mk[1], pch = 19, xlim = c(0,n), ylim = c(0,1))

for (i in 2:n){
  Sys.sleep(.2)
  points(i, mk[i], pch = 19)
}


store = rep(NA, 10000)
for(i in 1:10000){
  store[i] = sum(sample(1:100, rep = TRUE)==4) > 0
}
mean(store)

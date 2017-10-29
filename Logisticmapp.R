minr<- 2.5 # valor minim de r
maxr<- 4.0 # valor maxim de r
inc<- 0.001 # increment
rd<- seq(minr, maxr, inc) # vector r
ttransients<- 100 # punt a partir del qual el sistema te equilibri
trange<- 400 # generacions en les que prendrem valors depres de ttransients

plot(c(minr,maxr), c(0,1), type = "n", pch=".", xlab="r", ylab="x*")

for(r in rd){x<- 0.1
for (i in 1:ttransients){x<- r*x*(1-x)}
for (i in 1:trange) {
x<- r*x*(1-x)
points(r,x, pch=".")
  
}
}

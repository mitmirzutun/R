data <- read.csv("data.csv")
print(data)
summe <- sum(rowSums(data))
laenge <- (summe/(2^.5))^.5
print(laenge)
plot(c(0, 100*2^.5), c(0, 100), type= "n", xlab = "", ylab = "")
rowSum = rowSums(data)
for (i in seq(1,nrow(data),by=1)) {
  rowSum[i] = rowSum[i]/(laenge^2)*100
}
summe = sum(data[1])
iLaenge=0
meta = matrix(c("green","red","green","red"),ncol=2,nrow=2)
for (i in seq(1,nrow(data),by=1)) {
  jHoehe = 0
  summe = sum(data[i])
  for (j in seq(ncol(data),1,-1)) {
    jHoehe1 =jHoehe+(data[j,i]*100.0)/summe
    rect(iLaenge,jHoehe,iLaenge+rowSum[i],jHoehe1,density = -1,col = meta[j,i])
    text(iLaenge+rowSum[i]*.5, jHoehe++(data[j,i]*50.0)/summe, data[j,i], adj = c(0,0))
    jHoehe = jHoehe1
  }
  iLaenge = iLaenge+rowSum[i]
}
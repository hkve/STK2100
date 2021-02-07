wdbc = read.table("data/wdbc.data", sep=",")
wdbc = wdbc[,-1]

norm = function(x) {
  (x-min(x))/(max(x)-min(x))
}

wdbc_norm = as.data.frame(lapply(wdbc[,-1], norm))

split = 450
wdbc_train = wdbc_norm[1:split,]
wdbc_test = wdbc_norm[split+1:nrow(wdbc_norm),]



library(class)
wdbc_pred = knn(wdbc_train, wdbc_test, wdbc[1:split,1], k=21)
table(wdbc_pred, wdbc[split+1:nrow(wdbc),1])

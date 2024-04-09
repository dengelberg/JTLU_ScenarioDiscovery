source('~/DUSP/FirstYearPaper/convexHullAnalysis.R')
ch.failures <- failures
source('~/DUSP/FirstYearPaper/mockExpScn.R')
me.failures <- failures

coreHH <- c(35, 50, 63, 2, 50)
innerHH <- c(24,1,18,95,62)
outerHH <- c(3,3,31,86,77)
beyondHH <- c(11,6,39,71,73)
teaHH <- c(49,56,15,63,17)
haHH <- c(81,55,4,58,2)
burdenHH <- c(0,0,96,5,99)
median.unit.price <- c(52,6,32,49,55)

sd.failures <- data.frame(coreHH, innerHH, outerHH, beyondHH,
                          teaHH, haHH, burdenHH, median.unit.price)
row.names(sd.failures) <- c("base",
                            "TOD",
                            "TOD21",
                            "PGAS",
                            "PGAS2")


cHull.probs <- sd.failures
for(i in 1:nrow(sd.failures)){
      for(j in 1:ncol(sd.failures)){
            t.test <- prop.test(c(sd.failures[i,j],ch.failures[i,j]),c(100,9),
                      p = NULL, alternative="two.sided")
            print(t.test)
            cHull.probs[i,j] <- t.test$p.value
      }
}

hist(array(data.matrix(cHull.probs)), na.rm = T)

me.probs <- sd.failures
for(i in 1:nrow(sd.failures)){
      for(j in 1:ncol(sd.failures)){
            t.test <- prop.test(c(sd.failures[i,j],me.failures[i,j]),c(100,8),
                                p = NULL, alternative="two.sided")
            print(t.test)
            me.probs[i,j] <- t.test$p.value
      }
}

hist(array(data.matrix(me.probs)), na.rm = T)



plot(ecdf(array(data.matrix(me.probs))), main = "CDF of P Values Comparing Alternatives to Scn. Discovery")
lines(ecdf(array(data.matrix(cHull.probs))), col = "red")
legend("topleft", legend = c("Convex Hull", "Expl. Scenarios"),
       col = c("red", "black"), pch = c(19,19))

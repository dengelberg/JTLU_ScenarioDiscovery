setwd("C:/Users/denge/OneDrive/Documents/DUSP/FirstYearPaper/Baseline_2.24")

SCN_COUNT <- 100

getCapLeft <- function(scnNumber){
      scnDir <- paste("scn_", scnNumber, sep = "")
      read.table(paste(scnDir, "/resultFile_2030.csv", sep = ""),
                 header = FALSE, skip = 191,
                 nrows = 31, sep = ",")
}

allCap <- data.frame()
for(i in 1:SCN_COUNT){
      capTable <- cbind(i, getCapLeft(i))
      names(capTable) <- c("scn","region", "devCap")
      allCap <- rbind(allCap, capTable)
}

regions <- read.csv("C:/Users/denge/OneDrive/Documents/DUSP/FirstYearPaper/regions.csv")

rSMZ <- data.frame(smz = 1:max(regions$End.SMZ))
for(scenario in 1:nrow(regions)){
      for(j in regions$Start.SMZ[scenario]:regions$End.SMZ[scenario]){
            rSMZ[j,2:6] <- regions[scenario,1:5]
      }
}

noSMZ <- which(is.na(rSMZ$FIPS))
rSMZ <- rSMZ[-noSMZ,]

allGeoResults <- data.frame()
for(i in 1:SCN_COUNT){
      scnDir <- paste("scn_", i, sep = "")
      rfSpatial <- read.csv(paste(scnDir, "/resultFileSpatial_2030.csv", sep = ""))
      rfSpatial <- cbind(i, rSMZ, rfSpatial)
      allGeoResults <- rbind(allGeoResults, rfSpatial)
}

names(allGeoResults)[1] <- "scenario"

scnInputs <- read.csv("scnInputTable.csv")
trueInputs <- scnInputs[1:100,10:15]
inputCorrelation <- cor(trueInputs)


hhTotRegion <- aggregate(households ~ Subregion + scenario,
                      allGeoResults, FUN = sum)

coreHH <- cbind(hhTotRegion[which(hhTotRegion$Subregion == "Core"),],
                trueInputs)
png("coreHH.png")
hist(coreHH$households, breaks = 20,
     main = "households in core")
dev.off()

coreOLS <- lm(households ~ growth.capacity.file +
         auto.accessibility.beta + auto.operating.costs +
         hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
   coreHH)

innerHH <- cbind(hhTotRegion[which(hhTotRegion$Subregion == "Inner"),],
                trueInputs)
png("inHH.png")
hist(innerHH$households, breaks = 20,
     main = "households in inner")
dev.off()
innerOLS <- lm(households ~ growth.capacity.file +
                    auto.accessibility.beta + auto.operating.costs +
                    hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
              innerHH)

OutsideHH <- cbind(hhTotRegion[which(hhTotRegion$Subregion == "Outer"),],
                 trueInputs)
hist(OutsideHH$households, breaks = 20)
OutsideOLS <- lm(households ~ growth.capacity.file +
                     auto.accessibility.beta + auto.operating.costs +
                     hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
               OutsideHH)

beyondHH <- cbind(hhTotRegion[which(hhTotRegion$Subregion == ""),],
                 trueInputs)
hist(beyondHH$households, breaks = 20)
beyondOLS <- lm(households ~ growth.capacity.file +
                     auto.accessibility.beta + auto.operating.costs +
                     hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
               beyondHH)

scnHH <- cbind(aggregate(households ~ scenario, 
                   allGeoResults, FUN = sum), trueInputs)
hist(scnHH$households, breaks = 20)
scnHhOLS <- lm(households ~ growth.capacity.file +
                      auto.accessibility.beta + auto.operating.costs +
                      hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
               scnHH)



hhTotPrice <- aggregate(households*avePrice ~ scenario,
                         allGeoResults, FUN = sum)
names(hhTotPrice)[2] <- "totalCost"
hhTotPrice$households <- scnHH$households
hhTotPrice$average <- hhTotPrice$totalCost/hhTotPrice$households
hist(hhTotPrice$average, breaks = 20)
hhTotPrice <- cbind(hhTotPrice, trueInputs)
priceOLS <- lm(average ~ growth.capacity.file +
                     auto.accessibility.beta + auto.operating.costs +
                     hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
               hhTotPrice)

geoFactors <- read.csv("C:/Users/denge/OneDrive/Documents/DUSP/FirstYearPaper/locFactors.csv")
factors <- data.frame(geoFactors)
for(i in 2:100){
      factors <- rbind(factors, geoFactors)
}
resultsFact <- cbind(allGeoResults, factors)
resultsFact$tod <- (resultsFact$Old.METRO + resultsFact$Balt.METRO +
                       resultsFact$Existing.Light.Rail > 0)
hhTOD <- aggregate(households ~ scenario + tod,
                   resultsFact, FUN = sum)
hhInTOD <- cbind(hhTOD[which(hhTOD$tod == TRUE),],
                  trueInputs)
hist(hhInTOD$households, breaks = 20)
todOLS <- lm(households ~ growth.capacity.file +
                     auto.accessibility.beta + auto.operating.costs +
                     hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
             hhInTOD)

hhTown<- aggregate(households ~ scenario + Town,
                   resultsFact, FUN = sum)
hhInTown <- cbind(hhTOD[which(hhTown$Town == 1),],
                 trueInputs)
hist(hhInTown$households, breaks = 20)
townOLS <- lm(households ~ growth.capacity.file +
                   auto.accessibility.beta + auto.operating.costs +
                   hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
              hhInTown)

resultsFact$tea.25 <- (resultsFact$Percent.TEA > .25)
resultsFact$tea.5 <- (resultsFact$Percent.TEA > .5)
resultsFact$tea.75 <- (resultsFact$Percent.TEA > .75)
teaDev <- aggregate(households ~ scenario + tea.25,
                   resultsFact, FUN = sum)
names(teaDev)[2:3] <- c("inTea", "tea.25")
tea.5Dev <- aggregate(households ~ scenario + tea.5,
                      resultsFact, FUN = sum)
tea.75Dev <- aggregate(households ~ scenario + tea.75,
                       resultsFact, FUN = sum)
teaDev$tea.5 <- tea.5Dev$households
teaDev$tea.75 <- tea.75Dev$households

teaInDev <- cbind(teaDev[which(teaDev$inTea == TRUE),],
                  trueInputs)

hist(teaInDev$tea.25, breaks = 20)
hist(teaInDev$tea.5, breaks = 20)
hist(teaInDev$tea.75, breaks = 20)
teaOLS.25 <- lm(tea.25 ~ growth.capacity.file +
                    auto.accessibility.beta + auto.operating.costs +
                    hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
             teaInDev)
teaOLS.5 <- lm(tea.5 ~ growth.capacity.file +
                      auto.accessibility.beta + auto.operating.costs +
                      hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
                teaInDev)
teaOLS.75 <- lm(tea.75 ~ growth.capacity.file +
                     auto.accessibility.beta + auto.operating.costs +
                     hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
               teaInDev)

priceResults <- data.frame()
for(i in 1:SCN_COUNT){
      scnDir <- paste("scn_", i, sep = "")
      rf <- read.csv(paste(scnDir, "/resultFile_2030.csv", sep = ""),
                            skip = 223, nrows = 10)
      pricetable <- cbind(i, rf)
      priceResults <- rbind(priceResults, pricetable)
}
names(priceResults)[1] <- "scenario"

for(i in 1:nrow(priceResults)){
      priceResults$hh[i] <- sum(priceResults[i,3:12])
}
priceResults$totalRent <- priceResults$averageRent * 
      priceResults$hh

lowestRent <- priceResults[which(priceResults$Income == 10000),]
lowestRent <- cbind(lowestRent, trueInputs)
hist(lowestRent$averageRent, breaks = 20)
lowrentOLS <- lm(averageRent ~ growth.capacity.file +
                     auto.accessibility.beta + auto.operating.costs +
                     hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
               lowestRent)

priceResults$monthInc <- priceResults$Income/12
incBreaks <- seq(250, 2500, by = 250)
monthMin <- incBreaks * 1/.35
monthInc <- priceResults$monthInc[1:10]

afford <- data.frame(monthInc)
affcount <- 10 - sapply(1:10, FUN = function(i){
      countAfford <- 0
      for(j in 1:10){
            if(monthInc[i] < monthMin[j]){
                  countAfford <- countAfford + 1
            }
      }
      countAfford
})
priceResults$affCount <- affcount

for(i in 1:nrow(priceResults)){
      if(priceResults$affCount[i] < 10){
            count <- priceResults$affCount[i]
            burden <- sum(priceResults[i,(3 + count):12])
            priceResults$burden[i] <- burden
      } else{
            priceResults$burden[i] <- 0 
      }
}

hhBurdened <- aggregate(burden ~ scenario,
                        priceResults, FUN = sum)
hhBurdened <- cbind(hhBurdened, trueInputs)
hist(hhBurdened$burden, breaks = 20)
burdenedOLS <- lm(burden ~ growth.capacity.file +
                      auto.accessibility.beta + auto.operating.costs +
                      hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
                     hhBurdened)


transAcc <- sapply(1:100, FUN = function(i){
      scnTable <- allGeoResults[which(allGeoResults$scenario == i),]
      indAcc <- with(scnTable, rep.int(transitAccessibility, households))
      sum(indAcc > 0)
})
hist(transAcc, breaks = 20)

highAcc <- sapply(1:100, FUN = function(i){
      scnTable <- allGeoResults[which(allGeoResults$scenario == i),]
      indAcc <- with(scnTable, rep.int(transitAccessibility, households))
      sum(indAcc > .5)
})
hist(highAcc, breaks = 20)


autoAccMed <- sapply(1:100, FUN = function(i){
      scnTable <- allGeoResults[which(allGeoResults$scenario == i),]
      medAcc <- with(scnTable, median(rep.int(autoAccessibility, households)))
})
hist(autoAccMed)

priceMed <- sapply(1:100, FUN = function(i){
      scnTable <- allGeoResults[which(allGeoResults$scenario == i),]
      pMed <- with(scnTable, median(rep.int(avePrice, households)))
})
hist(priceMed)

transAcc <- cbind(transAcc, trueInputs)
highAcc <- cbind(highAcc, trueInputs)
autoAccMed <- cbind(autoAccMed, trueInputs)
priceMed <- cbind(priceMed, trueInputs)


tAccOLS <- lm(transAcc ~ growth.capacity.file +
                        auto.accessibility.beta + auto.operating.costs +
                        hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
              transAcc)

hAccOLS <- lm(highAcc ~ growth.capacity.file +
                    auto.accessibility.beta + auto.operating.costs +
                    hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
              highAcc)

aaMedOLS <- lm(autoAccMed ~ growth.capacity.file +
                    auto.accessibility.beta + auto.operating.costs +
                    hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
              autoAccMed)

pMedOLS <- lm(priceMed ~ growth.capacity.file +
                    auto.accessibility.beta + auto.operating.costs +
                    hts.work.tlfd + HH.Moves.UEC.FileName + auto.peak.sov.skim.2015,
              priceMed)

myModels <- list(coreOLS, innerOLS, OutsideOLS,
              beyondOLS, hAccOLS, teaOLS.75,
              burdenedOLS, priceOLS)
indicators <- c("core", "inner", "outer", "beyond", "trans.acc",
           "tea", "burden", "price")

coefficients <- data.frame(row.names = names(coreOLS$coefficients))
standardErrors <- data.frame(row.names = names(coreOLS$coefficients))
for(i in 1:length(myModels)){
      coefficients <- cbind(coefficients, summary(myModels[[i]])$coefficients[,1])
      standardErrors <- cbind(standardErrors, summary(myModels[[i]])$coefficients[,2])
}
names(coefficients) <- indicators
names(standardErrors) <- indicators
confInt <- (qt(.975, 99) * standardErrors)/100
lowBound <- coefficients - confInt
upBound <- coefficients + confInt


parNames <- names(scnInputs[10:15])
parMins <- sapply(10:15, FUN = function(i){min(scnInputs[,i])})
parMaxs <- sapply(10:15, FUN = function(i){max(scnInputs[,i])})
parRange <- data.frame(parNames, parMaxs - parMins)

coefScaled <- coefficients
for(i in nrow(parRange)){
      for(j in ncol(coefScaled)){
            coefScaled[i + 1,j] <- coefficients[i + 1,j] * parRange[i,2]
      }
}

colors <- c("black","red", "green", "blue",
            "orange", "brown", "purple")

png("baselineHH.png")
plot(x = 1:7, y = coefScaled[2,1:7], pch = 19,
     xaxt='n', ylim = c(min(coefScaled[2:7,]),
                        max(coefScaled[2:7,])),
     ylab = "households",
     xlab = "", cex.axis = 1.5, cex.lab = 1.5)
for(i in 1:6){
      segments(i, coefScaled[2,i], 
               x1 = i + 1, y1 = coefScaled[2,i + 1])
}
for(i in 3:7){
      points(x = 1:7, y = coefScaled[i,1:7], pch = 19,
             col = colors[i - 1])
      for(j in 1:6){
            segments(j, coefScaled[i,j], 
                  x1 = j + 1, y1 = coefScaled[i,j + 1],
                  col = colors[i - 1])
      }
}
axis(1, at = 1:ncol(coefScaled), labels = names(coefScaled), las=3)
legend("topright", legend = c("Infill capacity",
                              "Accessibility beta",
                              "Auto operating cost",
                              "Distance to work parameter",
                              "Relative value of access in moves",
                              "Travel times skim file"), col = colors,
       cex = 1.25, pch = 19)
dev.off()

png("baselinePrice.png", width = 360)
plot(x = 1, y = coefScaled[2,8], pch = 19,
     xaxt='n', ylim = c(-40,
                        10),
     ylab = "median unit price",
     xlab = "", cex.axis = 1.5, cex.lab = 1.5)
segments(1 - .25, coefScaled[2,8], 
      x1 = 1 + .25, y1 = coefScaled[2,8])
for(i in 3:7){
      points(x = 1, y = coefScaled[i,8], pch = 19,
             col = colors[i - 8])
      segments(.75, coefScaled[i,8],
               x1 = 1.25, y1 = coefScaled[i,8],
               col = colors[i - 1])
}
axis(1, at = 1, labels = "median housing price", las=3)
dev.off()

for(i in 3:7){
      points(x = 1, y = coefScaled[i,8], pch = 19,
             col = colors[i - 1])
      segments(.75, coefScaled[i,8],
               x1 = 1.25, y1 = coefScaled[i,8],
               col = colors[i - 1])
}
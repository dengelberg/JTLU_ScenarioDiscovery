## This file is for analyzing the baseline against all scenario files.

#Set working directory and input needed libraries

setwd("C:/Users/denge/OneDrive/Documents/DUSP/FirstYearPaper/")
library(prim)
library(stargazer)
library(gplots)

region.Smz <- function(){
      regions <- read.csv("C:/Users/denge/OneDrive/Documents/DUSP/FirstYearPaper/regions.csv")
      
      rSMZ <- data.frame(smz = 1:max(regions$End.SMZ))
      for(scenario in 1:nrow(regions)){
            for(j in regions$Start.SMZ[scenario]:regions$End.SMZ[scenario]){
                  rSMZ[j,2:6] <- regions[scenario,1:5]
            }
      }
      
      noSMZ <- which(is.na(rSMZ$FIPS))
      rSMZ <- rSMZ[-noSMZ,]
}

store.geo.results <- function(SCN_COUNT, scnName, region.file, folder){
      geoSpatial <- data.frame()
      for(i in 1:SCN_COUNT){
            scnDir <- paste(scnName, "/scn_", i, folder, sep = "")
            #print(paste(scnDir, "/resultFileSpatial_2030.csv", sep = ""))
            rfSpatial <- read.csv(paste(scnDir, "/resultFileSpatial_2030.csv", sep = ""))
            rfSpatial <- cbind(i, region.file, rfSpatial)
            geoSpatial <- rbind(geoSpatial, rfSpatial)
      }
      names(geoSpatial)[1] <- "scenario"
      geoSpatial
}

hh.burdened <- function(SCN_COUNT, scnName, folder){
      priceResults <- data.frame()
      for(i in 1:SCN_COUNT){
            scnDir <- paste(scnName, "/scn_", i, folder, sep = "")
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
}

input.cor <- function(){
      scnInputs <- read.csv("scnInputTable.csv")
      trueInputs <- scnInputs[1:100,9:15]
      inputCorrelation <- cor(trueInputs)
}


sub.regions <- function(geoTable, trueInputs){
      hhTotRegion <- aggregate(households ~ Subregion + scenario,
                               geoTable, FUN = sum)
      coreHH <- hhTotRegion[which(hhTotRegion$Subregion == "Core"),]
      innerHH <- hhTotRegion[which(hhTotRegion$Subregion == "Inner"),]
      OutsideHH <- hhTotRegion[which(hhTotRegion$Subregion == "Outer"),]
      beyondHH <- hhTotRegion[which(hhTotRegion$Subregion == ""),]
      hh.dist <- data.frame(coreHH$households,innerHH$households,
                            OutsideHH$households,beyondHH$households,
                            trueInputs)
      hh.dist
}

factor.table <- function(geo.results){
      geoFactors <- read.csv("C:/Users/denge/OneDrive/Documents/DUSP/FirstYearPaper/locFactors.csv")
      factors <- data.frame(geoFactors)
      for(i in 2:100){
            factors <- rbind(factors, geoFactors)
      }
      resultsFact <- cbind(geo.results, factors)
}

calc.tea.75 <- function(result.factors){
      result.factors$tea.75 <- (result.factors$Percent.TEA > .75)
      tea.75Dev <- aggregate(households ~ scenario + tea.75,
                             result.factors, FUN = sum)
      names(tea.75Dev)[2:3] <- c("inTea", "tea.75")
      tea.75Dev[101:200,3]
}

SCN_COUNT <- 100
POL_COUNT <- 5

rfile <- region.Smz()
scnInputs <- read.csv("scnInputTable.csv")
myInputs <- scnInputs[1:100,2:8]
scn.names <- c("Baseline_2.24", "TOD_3.5", "TOD2_3.17", "COST_3.29", "COST2_4.12")
folder.ext <- c("", "TOD", "TOD2", "COST", "COST2")
offical.names <- c("base", "TOD", "TOD21", "PGAS", "PGAS2")
indic.names <- c("coreHH", "innerHH", "outerHH", "beyondHH",
                 "teaHH", "haHH", "burdenHH", "median unit price")

#pulls in all results into a table for each policy
scn.geo <- lapply(1:POL_COUNT, FUN = function(i){
      store.geo.results(SCN_COUNT, scn.names[i], rfile, folder.ext[i])
})
names(scn.geo) <- offical.names
saveRDS(scn.geo, file = "r_data_objects/scn_geo.rds")

#calculates subregion population totals for each future
all.sregion <- lapply(1:length(scn.geo), FUN = function(i){
      sub.regions(scn.geo[[i]], myInputs)
})
names(all.sregion) <- offical.names
saveRDS(all.sregion, file = "r_data_objects/all_sregions.rds")

#attaches the zonal characteristics to the results file
all.factors <- lapply(1:length(all.sregion), FUN = function(i){
      factor.table(scn.geo[[i]])
})
names(all.factors) <- offical.names
saveRDS(all.factors, file = "r_data_objects/scn_smz_withFactors.rds")

#determines development in high targeted ecological areas for each scenario
all.tea <- lapply(1:length(all.factors), FUN = function(i){
      calc.tea.75(all.factors[[i]])
})
names(all.tea) <- offical.names

#these are the true lhs pulls for each scenario
parameter <- all.sregion[[1]][-1:-4]

#adds tea development units level to the data
for(i in 1:POL_COUNT){
      all.sregion[[i]] <- cbind(all.sregion[[i]],
                                calc.tea.75(all.factors[[i]]))
      names(all.sregion[[i]])[12] <- "tea.dev"
}

# Calculates households in high accessibility areas
high.access <- lapply(1:POL_COUNT, FUN = function(i){
      high.acc.table <- sapply(1:100, FUN = function(j){
            scnTable <- scn.geo[[i]][which(scn.geo[[i]]$scenario == j),]
            indAcc <- with(scnTable, rep.int(transitAccessibility, households))
            sum(indAcc > .5)
      })
})

#calculates cost burdened households
hh.burden <- lapply(1:POL_COUNT, FUN = function(i){
      hh.burdened(SCN_COUNT, scn.names[i], folder.ext[i])
})
names(scn.geo) <- offical.names

#caculates median price for each future
price.med <- sapply(1:POL_COUNT, FUN = function(i){
      price.med.scn <- sapply(1:100, FUN = function(j){
            scnTable <- scn.geo[[i]][which(scn.geo[[i]]$scenario == j),]
            pMed <- with(scnTable, median(rep.int(avePrice, households)))
      })
})
price.med <- data.frame(price.med)
names(price.med) <- offical.names

#gets the eight measures results for each of the scenarios
all.res.tables <- list()
for(i in 1:POL_COUNT){
      all.res.tables[[i]] <- cbind(all.sregion[[i]][c(1:4,12)],
                                   high.access[[i]],
                                   hh.burden[[i]]$burden,
                                   price.med[[i]])
}

#collects measures them into one table
all.res.one <- do.call("rbind", all.res.tables)
policy <- array(sapply(1:POL_COUNT, function(i){rep(i,100)}))
scenario <- array(sapply(1:POL_COUNT, function(i){1:100}))
all.res.one <- cbind(policy, scenario, all.res.one)

#get table of the maximum and minimum value from each scenario
max.table <- do.call("pmax", all.res.tables)
min.table <- do.call("pmin", all.res.tables)

# determines the regret if measured from a maximizing or minimizing framework
max.regret <- list()
min.regret <- list()
for(i in 1:POL_COUNT){
      max.regret[[i]] <- max.table - all.res.tables[[i]]
      min.regret[[i]] <- all.res.tables[[i]] - min.table 
}

#collects the max-relative regret
comb.max.reg <- do.call("rbind", max.regret)
comb.max.reg <- cbind(policy, scenario, comb.max.reg)

#collects the min-relative regret
comb.min.reg <- do.call("rbind", min.regret)
comb.min.reg <- cbind(policy, scenario, comb.min.reg)


#############################
# EMAT
policies <- c("baseline", "TOD", "TOD_21", "gas.price", "gas.price_21")
policy.emat <- array(sapply(1:POL_COUNT, function(i){rep(policies[i],100)}))
param.emat <- rbind(parameter, parameter, parameter, parameter, parameter)[,-1]

all.res.emat <- cbind(param.emat, policy.emat, do.call("rbind", all.res.tables))
names(all.res.emat) <- c("inf.cap", "access.beta", "operating.cost", "dist.to.wrk", "val.of.access", "tr.times",
                         "policy", "core.hh", "inner.hh", "outer.hh", "beyond.hh", "env.area.hh", "high.access.hh",
                         "cost.burdened.hh", "median.hh.price")
all.res.emat$tr.times <- ifelse(all.res.emat$tr.times > .5, 1, 0)


write.csv(all.res.emat, "emat_inputs.csv")


#############################
max.fail <- comb.max.reg
min.fail <- comb.min.reg
med.max.regret <- array()
med.min.regret <- array()
quant <- 1 - (POL_COUNT - 1)/(2 * POL_COUNT)
for(i in 3:ncol(max.fail)){                               #failure > median (reg > 0)
      med.max.regret[i - 2] <- quantile(comb.max.reg[,i], quant)
      med.min.regret[i - 2] <- quantile(comb.min.reg[,i], quant)
      max.fail[,i] <- comb.max.reg[i] > med.max.regret[i - 2]
      min.fail[,i] <- comb.min.reg[i] > med.min.regret[i - 2]
}

#parameters for all 500 runs
param.binder <- parameter
for(i in 2:POL_COUNT){
      param.binder <- rbind(param.binder, parameter)
}

#connects parameters to results
max.reg.p <- cbind(comb.max.reg, param.binder)[-11]
min.reg.p <- cbind(comb.min.reg, param.binder)[-11]
max.fail.p <- cbind(max.fail, param.binder)[-11]
min.fail.p <- cbind(min.fail, param.binder)[-11]
saveRDS(max.reg.p, file = "r_data_objects/max_regret.rds")
saveRDS(min.reg.p, file = "r_data_objects/min_regret.rds")
saveRDS(max.fail.p, file = "r_data_objects/max_fail.rds")
saveRDS(min.fail.p, file = "r_data_objects/min_fail.rds")



#runs prim on max regret
prim.max <- lapply(1:POL_COUNT, FUN = function(i){
      prim.max.scn <- lapply(3:10, FUN = function(j){
            min <- (i * 100) - 99
            max <- (i * 100)
            prim.y <- max.fail.p[min:max, j]
            prim.x <- max.fail.p[min:max,11:16]
            var.box <- prim.box(prim.x, prim.y, threshold.type = 1)
      })
      names(prim.max.scn) <- indic.names
      prim.max.scn
})
names(prim.max) <- offical.names

#runs prim on min regret
prim.min <- lapply(1:POL_COUNT, FUN = function(i){
      prim.min.scn <- lapply(3:10, FUN = function(j){
            min <- (i * 100) - 99
            max <- (i * 100)
            prim.y <- min.fail.p[min:max, j]
            prim.x <- min.fail.p[min:max,11:16]
            var.box <- prim.box(prim.x, prim.y, threshold.type = 1)
      })
      names(prim.min.scn) <- indic.names
      prim.min.scn
})
names(prim.min) <- offical.names

saveRDS(prim.max, file = "r_data_objects/prim_max.rds")
saveRDS(prim.min, file = "r_data_objects/prim_min.rds")



for(i in 1:length(indic.names)){
      png(paste("regret_unordered/", indic.names[i], "_max.png", sep = ""))
      plot(max.fail.p$scenario,
           max.reg.p[, i + 2], main = paste("Regret score:", indic.names[i]),
           col = max.fail.p$policy, xlab = "scenario", ylab = "regret",
           pch = max.fail.p[, i + 2] * 15 + 1,
           ylim = c(0, max(max.reg.p[c(-1,-101,-201), i + 2])),
           cex.lab = 1.5, cex.axis = 1.25)
      dev.off()
}

for(i in 1:length(indic.names)){
      png(paste("regret_unordered/", indic.names[i], "_min.png", sep = ""))
      plot(min.fail.p$scenario,
           min.reg.p[, i + 2], main = paste("Regret score:", indic.names[i]),
           col = min.fail.p$policy, xlab = "scenario", ylab = "regret",
           pch = min.fail.p[, i + 2] * 15 + 1,
           ylim = c(0, max(min.reg.p[c(-1,-101,-201), i + 2])),
           cex.lab = 1.5, cex.axis = 1.25)
      dev.off()
}

png("regret_unordered/legend1.png")
plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('Base', 'Base - Fail', 'TOD', 'TOD - Fail',
                            'TOD21', 'TOD21 - Fail', "PGAS", "PGAS - Fail",
                            "PGAS2", "PGAS2 - Fail"),
       pt.cex=3, cex=1.5, bty='n',
       col = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
       pch = c(1,16,1,16,1,16,1,16))
dev.off()

param.names <- c("Infill capacity",
                 "Accessibility beta",
                 "Auto operating cost",
                 "Distance to work parameter",
                 "Relative value of access in moves",
                 "Travel times skim file")

for(i in 1:8){
      png(paste("box6/PrimMaxBox_", indic.names[i], ".png", sep = ""), height = 600)
      boxes <- lapply(1:POL_COUNT, FUN = function(j){
            prim.max[[j]][[i]]$box[[1]]
      })
      par(mar=c(15,6,4,2)+0.1)
      plot(1:6 - .1, y = boxes[[1]][1,], ylim = c(-.1, 1.1),
           xlab = "", ylab = "latin hypercube value",
           cex.lab = 1.5, cex.axis = 1.25, xaxt='n', xlim = c(.5,6.5),
           main = paste("PRIM box for", indic.names[i]))
      points(1:6 - .1, y = boxes[[1]][2,])
      segments(x0 = c(1:6 - .1), y0 = boxes[[1]][1,],
               x1 = c(1:6 - .1), y1 = boxes[[1]][2,])
      for(j in 2:length(boxes)){
            nudge <- (-.1 + .1*j)
            points(1:6 + nudge, y = boxes[[j]][1,], col = j)
            points(1:6 + nudge, y = boxes[[j]][2,], col = j)
            segments(x0 = c(1:6) + nudge, y0 = boxes[[j]][1,],
               x1 = c(1:6) + nudge, y1 = boxes[[j]][2,], col = j)
      }
      dev.off()

      png(paste("box6/PrimMinBox_", indic.names[i], ".png", sep = ""), height = 600)
      boxes <- lapply(1:POL_COUNT, FUN = function(j){
            prim.min[[j]][[i]]$box[[1]]
      })
      par(mar=c(15,6,4,2)+0.1)
      plot(1:6 - .1, y = boxes[[1]][1,], ylim = c(-.1, 1.1),
           xlab = "", ylab = "latin hypercube value",
           cex.lab = 1.5, cex.axis = 1.25, xaxt='n', xlim = c(.5,6.5),
           main = paste("PRIM box for", indic.names[i]))
      points(1:6 - .1, y = boxes[[1]][2,])
      segments(x0 = c(1:6 - .1), y0 = boxes[[1]][1,],
               x1 = c(1:6 - .1), y1 = boxes[[1]][2,])
      for(j in 2:length(boxes)){
            nudge <- (-.1 + .1*j)
            points(1:6 + nudge, y = boxes[[j]][1,], col = j)
            points(1:6 + nudge, y = boxes[[j]][2,], col = j)
            segments(x0 = c(1:6) + nudge, y0 = boxes[[j]][1,],
                     x1 = c(1:6) + nudge, y1 = boxes[[j]][2,], col = j)
      }
      dev.off()
}

for(i in 1:8){
      boxes <- lapply(1:POL_COUNT, FUN = function(j){
            prim.max[[j]][[i]]$box[[1]]
      })
      for(j in 1:POL_COUNT){
            my.box <- boxes[[j]]
            for(k in 1:(ncol(my.box) - 1)){
                  for(l in ((k + 1):ncol(my.box))){
                        png(paste("2d_boxes/max_", strtrim(offical.names[j], 5), "_"
                                  , strtrim(indic.names[i], 5), "_", strtrim(param.names[k], 5), "_",
                                  strtrim(param.names[l], 5), ".png", sep = ""))
                        colfunc <- colorRampPalette(c("green", "yellow", "orange", "red"))
                        palette(colfunc(100))
                        min <- (j * 100) - 99
                        max <- (j * 100)
                        plot(max.fail.p[[k + 10]][min:max],
                              max.fail.p[[l + 10]][min:max],
                              col = rank(max.reg.p[[i + 2]][min:max]),
                              pch = max.fail.p[[i + 2]][min:max] * 15 + 1,
                              xlab = param.names[k],
                              ylab = param.names[l],
                              main = paste(offical.names[j], " regret box:", indic.names[i]),
                              cex.lab = 1.5, cex.axis = 1.25)
                        
                        rect(my.box[1,k], my.box[1,l],
                              my.box[2,k], my.box[2,l])
                        dev.off()
                  }
            }
      }
}

for(i in 1:8){
      boxes <- lapply(1:POL_COUNT, FUN = function(j){
            prim.min[[j]][[i]]$box[[1]]
      })
      for(j in 1:POL_COUNT){
            my.box <- boxes[[j]]
            for(k in 1:(ncol(my.box) - 1)){
                  for(l in ((k + 1):ncol(my.box))){
                        png(paste("2d_boxes/min_", strtrim(offical.names[j], 5)
                                  , strtrim(indic.names[i], 5), strtrim(param.names[k], 5),
                                  strtrim(param.names[l], 5), ".png", sep = ""))
                        colfunc <- colorRampPalette(c("green", "yellow", "orange", "red"))
                        palette(colfunc(100))
                        min <- (j * 100) - 99
                        max <- (j * 100)
                        plot(min.fail.p[[k + 10]][min:max],
                             min.fail.p[[l + 10]][min:max],
                             col = log(min.reg.p[[i + 2]][min:max])/log(max(min.reg.p[[i + 2]][min:max])) * 99 + 1,
                             pch = min.fail.p[[i + 2]][min:max] * 15 + 1,
                             xlab = param.names[k],
                             ylab = param.names[l],
                             main = paste(offical.names[j], " regret box:", indic.names[i]),
                             cex.lab = 1.5, cex.axis = 1.25)
                        rect(my.box[1,k], my.box[1,l],
                             my.box[2,k], my.box[2,l])
                        dev.off()
                  }
            }
      }
}

#sets which indicators should be maximize and which minimized
maxs.ind <- c(1, 2, 6)
mins.ind <- c(3,4,5,7,8)

palette("default")
for(i in 1:8){
      if(i %in% maxs.ind){
            boxes <- lapply(1:POL_COUNT, FUN = function(j){
                  prim.max[[j]][[i]]$box[[1]]
            })
      }
      if(i %in% mins.ind){
            boxes <- lapply(1:POL_COUNT, FUN = function(j){
                  prim.min[[j]][[i]]$box[[1]]
            })
      }
      for(j in 1:6){
            png(paste("1d_box/reg_", strtrim(indic.names[i], 3), "_",
                      strtrim(param.names[j], 3), ".png", sep = ""))
            if(i %in% maxs.ind){
                  max.reg <- max(max.reg.p[, i + 2])
                  plot(max.fail.p[,j + 10],
                       max.reg.p[, i + 2], main = paste("Regret score:", indic.names[i]),
                       col = max.fail.p$policy, xlab = param.names[j], ylab = "regret",
                       pch = max.fail.p[, i + 2] * 15 + 1,
                       ylim = c(-max.reg/10, max.reg),
                       cex.lab = 1.5, cex.axis = 1.25)
            }
            if(i %in% mins.ind){
                  max.reg <- max(min.reg.p[, i + 2])
                  plot(min.fail.p[,j + 10],
                       min.reg.p[, i + 2], main = paste("Regret score:", indic.names[i]),
                       col = min.fail.p$policy, xlab = param.names[j], ylab = "regret",
                       pch = min.fail.p[, i + 2] * 15 + 1,
                       ylim = c(-max.reg/10, max.reg),
                       cex.lab = 1.5, cex.axis = 1.25)
            }
            height <- -max.reg/35 * 1:length(boxes)
            for(k in 1:length(boxes)){
                  segments(x0 = max(0,boxes[[k]][1,j]), y0 = height[k],
                     x1 = min(1,boxes[[k]][2,j]), y1 = height[k],
                     col = k, lwd = 2)
            }
            dev.off()
            png(paste("1d_box/value_", strtrim(indic.names[i], 3), "_",
                      strtrim(param.names[j], 3), ".png", sep = ""))
            if(i %in% maxs.ind){
                  #max.reg <- max(max.reg.p[, i + 2])
                  plot(max.fail.p[,j + 10],
                       all.res.one[, i + 2], main = paste("Regret score:", indic.names[i]),
                       col = all.res.one$policy, xlab = param.names[j], ylab = indic.names[i],
                       pch = max.fail.p[, i + 2] * 15 + 1,
                       #ylim = c(-max.reg/10, max.reg),
                       cex.lab = 1.5, cex.axis = 1.25)
            }
            if(i %in% mins.ind){
                  max.reg <- max(min.reg.p[, i + 2])
                  plot(min.fail.p[,j + 10],
                       all.res.one[, i + 2], main = paste("Regret score:", indic.names[i]),
                       col = all.res.one$policy, xlab = param.names[j], ylab = indic.names[i],
                       pch = min.fail.p[, i + 2] * 15 + 1,
                       #ylim = c(-max.reg/10, max.reg),
                       cex.lab = 1.5, cex.axis = 1.25)
            }
            #height <- -max.reg/35 * 1:length(boxes)
            #for(k in 1:length(boxes)){
            #      segments(x0 = max(0,boxes[[k]][1,j]), y0 = height[k],
            #               x1 = min(1,boxes[[k]][2,j]), y1 = height[k],
            #               col = k, lwd = 2)
            #}
            dev.off()
      }
}



failures <- data.frame(sapply(1:8, FUN = function(i){
      if(i %in% maxs.ind){
            fails <- aggregate(max.fail[,i + 2] ~ max.fail$policy, max.fail, sum)
      }
      if(i %in% mins.ind){
            fails <- aggregate(min.fail[,i + 2] ~ min.fail$policy, min.fail, sum)
      }
      fails[,2]
}))
names(failures) <- indic.names
row.names(failures) <- offical.names
stargazer(failures, title = "Failed Futures for Each Indicator",
          summary = FALSE)
png("heatmaps/lhsRobust.png")
heatmap.2(as.matrix(failures), cellnote = as.matrix(failures), notecol="black",
          Colv=NA, Rowv=NA, scale='none', density.info = 'none', trace = 'none',
          #key = F,
          lhei=c(3,10),margins=c(10,8))
dev.off()

match <- lapply(1:8, FUN = function(i){
      max.boxes <- lapply(1:POL_COUNT, FUN = function(j){
            prim.max[[j]][[i]]$box[[1]]
      })
      min.boxes <- lapply(1:POL_COUNT, FUN = function(j){
            prim.min[[j]][[i]]$box[[1]]
      })
      sapply(1:length(boxes), FUN = function(j){
            if(i %in% maxs.ind){
                  my.box <- max.boxes[[j]]
                  in.fail <- sum(prim.max[[j]][[i]]$y[[1]])
                  out.fail <- failures[j,i] - in.fail
                  in.pass <- length(prim.max[[j]][[i]]$y[[1]]) - in.fail
                  out.pass <- 100 - sum(in.fail, out.fail, in.pass)
                  match.in <- in.fail/length(prim.max[[j]][[i]]$y[[1]])
                  match.out <- out.fail/(100 - length(prim.min[[j]][[i]]$y[[1]]))
                  fail.capture <- in.fail/(in.fail + out.fail)
                  pass.capture <- in.pass/(in.pass + out.pass)
                  true.pos <- in.fail/(in.fail + in.pass)
                  false.pos <- in.pass/(in.fail + in.pass)
                  pass.miss <- out.pass/(in.pass + out.pass)
                  fail.miss <- out.fail/(in.fail + out.fail)
                  true.neg <- out.pass/(out.pass + out.fail)
                  false.neg <- out.fail/(out.pass + out.fail)
                  total.fail <- in.fail + out.fail
                  mass <- prim.max[[j]][[i]]$mass[[1]]
            }
            if(i %in% mins.ind){
                  in.fail <- sum(prim.min[[j]][[i]]$y[[1]])
                  out.fail <- failures[j,i] - in.fail
                  in.pass <- length(prim.min[[j]][[i]]$y[[1]]) - in.fail
                  out.pass <- 100 - sum(in.fail, out.fail, in.pass)
                  match.in <- in.fail/length(prim.min[[j]][[i]]$y[[1]])
                  match.out <- out.fail/(100 - length(prim.min[[j]][[i]]$y[[1]]))
                  fail.capture <- in.fail/(in.fail + out.fail)
                  pass.capture <- in.pass/(in.pass + out.pass)
                  true.pos <- in.fail/(in.fail + in.pass)
                  false.pos <- in.pass/(in.fail + in.pass)
                  pass.miss <- out.pass/(in.pass + out.pass)
                  fail.miss <- out.fail/(in.fail + out.fail)
                  true.neg <- out.pass/(out.pass + out.fail)
                  false.neg <- out.fail/(out.pass + out.fail)
                  total.fail <- in.fail + out.fail
                  mass <- prim.min[[j]][[i]]$mass[[1]]
            }
            c(match.in, match.out, fail.capture, pass.capture, fail.miss,
              pass.miss, true.pos, false.pos, true.neg, false.neg, 
              total.fail, mass)
      })
})
names(match) <- indic.names
match.table <- data.frame(t(do.call("cbind", match)))
match.table$policy <- array(offical.names, dim = nrow(match.table))
match.table$indicator <- array(sapply(1:length(indic.names), FUN = function(i){
      rep(indic.names[i], POL_COUNT)
}))
names(match.table)[1:12] <- c("match.in", "match.out", "fail.capture", "pass.capture",
                             "fail.miss", "pass.miss", "true.pos", "false.pos",
                             "true.neg", "false.neg", "total.fail", "mass")

match.table2 <- subset(match.table, 95 > total.fail & total.fail > 5)
match.table3 <- subset(match.table2, match.table2$true.pos/match.table2$false.neg > 2)
fc.med <- median(match.table$fail.capture, na.rm = T)
tp.med <- median(match.table$true.pos, na.rm = T)
match.table4 <- subset(match.table3, fail.capture > fc.med &
                             true.pos > .5)
match.table4 <- cbind(match.table4[,13:14], match.table4[1:12])


colfunc <- colorRampPalette(c("green", "yellow", "orange", "red"))
palette(colfunc(100))
plot(match.table$fail.capture, match.table$true.pos, xlim = c(0,1), pch = 19,
     col = (100 * 1 + (95 > match.table$total.fail & match.table$total.fail > 5)),
     ylim = c(0,1))
abline(a = 0, b = 1, lty = 2)

stargazer(match.table4[,c(1:2,5,9,12:13)], summary = FALSE,
          rownames = FALSE)

select.boxes <- list(prim.max$base$coreHH$box[[1]],
                     prim.max$TOD$coreHH$box[[1]],
                     prim.max$base$innerHH$box[[1]],
                     prim.max$TOD21$innerHH$box[[1]],
                     prim.min$base$beyondHH$box[[1]],
                     prim.min$TOD21$teaHH$box[[1]],
                     prim.min$PGAS2$teaHH$box[[1]],
                     prim.min$PGAS$`median unit price`$box[[1]])
all.select.boxes <- round(data.frame(do.call("rbind", select.boxes)), 2)
names(all.select.boxes) <- c("Inf. cap", "Acc. beta", "Op. cost", "Dist. to wrk", "Val. of acc.", "Tr. Times")
policy <- c("base", "", "TOD", "", "base", "", "TOD21", "", "base", "", "TOD21", "", "PGAS2", "", "PGAS", "")
indicator <- c("core HH", "", "core HH", "", "inner HH", "", "inner HH", "", "Bey. HH","", "TEA HH", "", "TEA HH", "", "unit price", "")
bound <- rep(c("low", "high"), 8)
all.select.boxes <- cbind(policy, indicator, bound, all.select.boxes)
stargazer(all.select.boxes,summary = FALSE, rownames = FALSE)


colfunc <- colorRampPalette(c("green", "yellow", "orange", "red"))
palette("default")
pdf("prim_measures.pdf")
par(mar=c(5,6,4,1)+.1)
plot(match.table$fail.capture, match.table$true.pos, pch = 19,
     ylim = c(0,1), xlim = c(0,1), xlab = "Coverage",
     ylab = "Density", cex = 2, cex.axis = 1.5, cex.lab = 2)
points(match.table4$fail.capture, match.table4$true.pos, pch = 19,
     ylim = c(0,1), xlim = c(0,1), col = "red", cex = 2)
dev.off()
abline(a = 0, b = 1, lty = 2)

colfunc <- colorRampPalette(c("green", "yellow", "orange", "red"))
palette(colfunc(100))
plot(match.table$false.pos, match.table$false.neg, pch = 19,
     ylim = c(0,1), xlim = c(0,1))
abline(a = 0, b = 1, lty = 2)

png("legendPRIM.png")
plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
colfunc <- colorRampPalette(c("green", "yellow", "orange", "red"))
palette(colfunc(100))
legend("topleft", legend =c('Included in Analysis', 'Expcluded from Analysis'),
       pt.cex=3, cex=2, bty='n',
       col = c("black", "red"),
       pch = c(19,19))
dev.off()

png("legend2.png")
plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
colfunc <- colorRampPalette(c("green", "yellow", "orange", "red"))
palette(colfunc(100))
legend("topleft", legend =c('Low Regret', '', '',
                            'Failure', 'High Regret', 'PRIM box'),
       pt.cex=3, cex=1.5, bty='n',
       col = c(1, 20, 40, 80, 100, "black"),
       lty = c(0,0,0,0,0,1),
       pch = c(1,1,1,16,16,NA))
dev.off()



box.size <- lapply(1:8, FUN = function(i){
      box.diff <- sapply(1:length(boxes), FUN = function(j){
            if(i %in% maxs.ind){
                  my.box <- prim.max[[j]][[i]]$box[[1]]
            }
            if(i %in% mins.ind){
                  my.box <- prim.min[[j]][[i]]$box[[1]]
            }
            my.box[2,] - my.box[1,]
      })
      box.diff
})
boxtable <- data.frame(t(do.call("cbind", box.size)))
boxtable$policy <- array(offical.names, dim = nrow(boxtable))
boxtable$indicator <- array(indic.names, dim = nrow(boxtable))


png("legend3.png")
plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend = c('Base PRIM Box',
                             'TOD PRIM Box',
                             'TOD-21 PRIM Box',
                             'PGAS PRIM Box',
                             'PGAS2 PRIM Box'),
       pt.cex=3, cex=1.5, bty='n',
       col = c(1:5),
       lty = c(1,1,1,1,1), lwd = c(2,2,2,2,2),
       pch = c(NA,NA,NA,NA,NA))
dev.off()


for(i in 1:length(select.boxes)){
      current.box <- select.boxes[[i]]
      lows <- current.box[1,]
      highs <- current.box[2,]
      current.box[1,c(2,3,5,6)] <- 1 - highs[c(2,3,5,6)]
      current.box[2,c(2,3,5,6)] <- 1 - lows[c(2,3,5,6)]
      plot(x = rep(1:6,2), y = t(current.box), ylim = c(-.1,1.1))
      for(j in 1:6){
            points(x = c(j,j), y = current.box[,j], type = "l")
      }
      print(i)
}

for(i in 1:8){
      if(j %in% mins.ind){
            boxes <- lapply(1:POL_COUNT, FUN = function(j){
                  prim.min[[j]][[i]]$box[[1]]
            })
      }
      if(j %in% maxs.ind){
            boxes <- lapply(1:POL_COUNT, FUN = function(j){
                  prim.max[[j]][[i]]$box[[1]]
            })
      }
      for(j in 1:POL_COUNT){
            my.box <- boxes[[j]]
            lows <- my.box[1,]
            highs <- my.box[2,]
            my.box[1,c(2,3,5,6)] <- 1 - highs[c(2,3,5,6)]
            my.box[2,c(2,3,5,6)] <- 1 - lows[c(2,3,5,6)]
            png(paste("box6/", indic.names[i], "_", offical.names[j],
                      ".png", sep = ""))
            plot(x = rep(1:6,2), y = t(my.box), ylim = c(-.1,1.1),
                 xaxt = 'n')
            for(k in 1:ncol(my.box)){
                  palette("default")
                  points(x = c(k,k), y = my.box[,k], type = "l")
                  colfunc <- colorRampPalette(c("green", "yellow", "orange", "red"))
                  palette(colfunc(100))
                  min <- (j * 100) - 99
                  max <- (j * 100)
                  points(x = rep(k + .1,100), y = min.fail.p[[k + 10]][min:max],
                         pch = min.fail.p[[i + 2]][min:max] * 15 + 1)
            }
            axis(side = 1, at = 1:6,
                 labels = c("Inf. cap", "Acc. beta", "Op. cost", "Dist. to wrk", "Val. of acc.", "Tr. Times"))
            dev.off()
      }
}

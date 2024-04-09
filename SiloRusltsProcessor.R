prepareSiloResults <- function(year, prestoScn, sensitivity, value){
  
  setwd("C:/Users/dengelbe/Documents/NCSG/PRESTO/Sensitivity_Full/ScenariosReuslts_Raw/")
  destination <- "C:/Users/dengelbe/Documents/NCSG/PRESTO/Sensitivity_Full/R_Analysis/"
  #year <- 2040
  #prestoScn <- "lco"
  #sensitivity <- "AOC"
  #value <- "1.25"
  destination <- paste(destination, sensitivity, sep = "")
  if(!dir.exists(destination)){
    dir.create(file.path(destination))
  }
  destination <- paste(destination, "/SILO_", prestoScn, "_", sensitivity, "_", value, sep = "")
  if(!dir.exists(destination)){
    dir.create(file.path(destination))
  }
  
  setwd(paste("./", sensitivity, "_", value, sep = ""))
  setwd(paste("./SILO_", prestoScn, "_", sensitivity, "_", value, sep = ""))
  
  siloInput <- paste("./resultFile_", as.character(year), ".csv", sep = "")
  siloTables <- list()
  
  siloTables[[1]] <- read.table(siloInput, nrows = 101, header = TRUE, sep = ",")
  
  siloTables[[2]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  
  sizeInc <- read.table(siloInput, skip = 107, nrows = 16, header = TRUE, sep = ",")
  hhSize <- c("size1","size2","size3","size4")
  hhInc <- c("inc1", "inc2", "inc3", "inc4")
  sizeInc <- as.array(sizeInc[,2])
  siloTables[[3]] <- data.frame(matrix(sizeInc, nrow = 4, ncol = 4), row.names = hhSize)
  names(siloTables[[3]]) <- hhInc
  
  siloTables[[4]] <- read.table(siloInput, skip = 124, nrows = 4, header = TRUE, sep = ",")
  
  hhSize <- read.table(siloInput, skip = 129, nrows = 1, header = FALSE, sep = ",")
  siloTables[[5]] <- data.frame(matrix(hhSize, hhSize, nrow = 10, ncol = 1), row.names = as.array(1:10))
  
  #names(siloTables[[5]]) <- hhBySize
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[6]]) <- AveHHSize
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[7]]) <- AveHHInc
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[8]]) <- MedianHHInc
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[9]]) <- laborParticipationRateByAge
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[10]]) <- aveCommuteDistByRegion
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[11]]) <- QualityLevel
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[12]]) <- CountOfDD
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[13]]) <- AveMonthlyPrice
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[14]]) <- AveVacancy
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[15]]) <- landForConst
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[16]]) <- hCostIncGroup
  #siloTables[[1]] <- read.table(siloInput, skip = 102, nrows = 4, header = TRUE, sep = ",")
  #names(siloTables[[17]]) <- jobByRegion
  names(siloTables) <- c("age", "ppByRace", "hhByType", "hhByRace")
  print(siloTables)
  
  
  
  
}
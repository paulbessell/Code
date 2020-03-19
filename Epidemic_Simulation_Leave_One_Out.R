library(igraph)

####*****************************************************************
####
#### This is the code for running epidemics with a node left out
####*****************************************************************

## Before you start
##Load the require modules 
# apps/gcc/R/3.0.0  
# compilers/gcc/4.9.2
# compilers/python/2.7.2
# 
# Start and R session
# Load: library Epinet and Igraph
# Run the modified function SEIR.simulater
######

SEIR.simulator2 <- function (M, N, beta, ki, thetai, ke = ki, thetae = thetai, latencydist = "fixed", 
                           latencyperiod = 0, init) 
{
  if (!is.matrix(M)) 
    stop("Input error: Network M must be an edgelist matrix.")
  if ((length(dim(M)) != 2) || (dim(M)[2] != 2)) 
    stop("Input error: Network M must an a 2-dimensional edgelist matrix.")
  t.time = array(dim = N)
  r.time = array(dim = N)
  t.time[init] <- ifelse(latencydist == "fixed", latencyperiod, 
                         rgamma(1, ke, scale = thetae))
  r.time[init] = rgamma(1, ki, scale = thetai) + t.time[init]
  nextrec = init
  inf.list <- matrix(c(init, NA, 0, t.time[init], NA), nrow = 1)
  time <- cm.time <- t.time[init]
  nexttrans = init
  t.time[init] <- Inf
  s.list <- (1:N)[-init]
  e.list <- NULL
  i.list <- init
  inf <- list(i.list)
  susc <- list(s.list)
  expo <- list(e.list)
  for (i in 2:(N * 3)) {
    s.list <- array(susc[[i - 1]])
    i.list <- array(inf[[i - 1]])
    si.ex <- ((M[, 1] %in% i.list) & (M[, 2] %in% s.list)) | 
      ((M[, 2] %in% i.list) & (M[, 1] %in% s.list))
    n.si <- sum(si.ex)
    dwt <- ifelse(length(inf[[i - 1]]) > 0, r.time[nextrec] - 
                    cm.time, Inf)
    bwt <- ifelse(n.si != 0, rexp(1, beta * n.si), Inf)
    twt <- t.time[nexttrans] - cm.time
    ewt <- min(bwt, dwt, twt, na.rm = TRUE)
    time <- c(time, ewt)
    cm.time <- cm.time + ewt
    if (ewt == bwt) 
      test <- "Infect"
    else if (ewt == dwt) 
      test <- "removal"
    else test <- "transition"
    if (test == "Infect") {
      is.pairs <- which(si.ex == 1)
      smp.ind <- ifelse(length(is.pairs) == 1, is.pairs, 
                        sample(is.pairs, 1))
      parentindex <- which(M[smp.ind, ] %in% i.list)
      new.inf <- M[smp.ind, 3 - parentindex]
      parent <- M[smp.ind, parentindex]
      lat <- ifelse(latencydist == "fixed", latencyperiod, 
                    rgamma(1, ke, scale = thetae))
      t.time[new.inf] <- cm.time + lat
      susc <- append(susc, list(susc[[i - 1]][-which(susc[[i - 
                                                             1]] == new.inf)]))
      expo <- append(expo, list(c(expo[[i - 1]], new.inf)))
      inf <- append(inf, list(inf[[i - 1]]))
      inf.list <- rbind(inf.list, c(new.inf, parent, cm.time, 
                                    NA, NA))
      nexttrans <- which(t.time == min(t.time, na.rm = TRUE))
    }
    else if (test == "removal") {
      if (i == 2) {
        inf.list[1, 5] <- cm.time
        break
      }
      new.rec <- nextrec
      susc <- append(susc, list(susc[[i - 1]]))
      expo <- append(expo, list(expo[[i - 1]]))
      inf <- append(inf, list(inf[[i - 1]][-which(inf[[i - 
                                                         1]] == new.rec)]))
      inf.list[which(inf.list[, 1] == new.rec), 5] <- cm.time
      r.time[nextrec] <- NA
      if (length(inf[[i]]) > 0) 
        nextrec <- which(r.time == min(r.time, na.rm = TRUE))
      else if (length(expo[[i]]) > 0) {
        nextrec <- which(t.time == min(t.time, na.rm = TRUE))
        r.time[nextrec] <- Inf
      }
    }
    else {
      new.trans <- nexttrans
      susc <- append(susc, list(susc[[i - 1]]))
      expo <- append(expo, list(expo[[i - 1]][-which(expo[[i - 
                                                             1]] == new.trans)]))
      inf <- append(inf, list(c(inf[[i - 1]], new.trans)))
      inf.list[which(inf.list[, 1] == new.trans), 4] <- cm.time
      t.time[nexttrans] <- NA
      nexttrans <- which(t.time == min(t.time, na.rm = TRUE))
      r.time[new.trans] <- cm.time + rgamma(1, ki, scale = thetai)
      if (r.time[new.trans] < r.time[nextrec]) 
        nextrec <- new.trans
    }
    if (length(inf[[i]]) + length(expo[[i]]) == 0) {
      break
    }
  }
  inf.list[, 3:5] <- inf.list[, 3:5] - min(inf.list[, 5])
  if (length(s.list) > 0) {
    for (i in 1:length(s.list)) inf.list <- rbind(inf.list, 
                                                  c(s.list[i], NA, NA, NA, NA))
  }
  colnames(inf.list) <- c("Node ID", "Parent", "Etime", "Itime", 
                          "Rtime")
  return(inf.list)
}

#### *****************************************************
#CODE FOR SIMULATING EPIDEMIC ON ALL 6 NETWORKS WITH VARYING Beta and Ki 
#on seeding all 20 nodes repeating the process 200 times
####**********************************************************************

curateMyNetwork <- function(examplenet) {
  examplenetLine <- c(examplenet[, 1], examplenet[, 2])
  examplenetLineNum <- as.numeric(factor(examplenetLine, levels = sort(unique(examplenetLine))))
  translationTable <<- data.frame(city=unique(examplenetLine), code=unique(examplenetLineNum))### You will use this table to march numbers in the randome network to city names # Created this is a global variable. Not widely recommended
  examplenet <- cbind(examplenetLineNum[1 : (length(examplenetLineNum) / 2)], examplenetLineNum[(length(examplenetLineNum) / 2 + 1):length(examplenetLineNum)])
  return(examplenet)
}
#examplenet[,1] <- as.numeric(factor(examplenet[,1], levels = sort(unique(examplenet[,1]))))
#examplenet[,2] <- as.numeric(factor(examplenet[,2], levels = sort(unique(examplenet[,2]))))
# head(examplenet)
# eee <- as.numeric(as.character(examplenet[,1]))
# eee <- cbind(as.numeric(as.character(examplenet[,1])), as.numeric(as.character(examplenet[,2])))
# eee
# examplenet=eee

#average epidemic and plot tree of that epdemic


runSimulation <- function(examplenet, k = 0.3, p = 2) {
  dflist <- list()
  dflist_stats <- list()
  epioutList <- list()
  N <- length(unique(c(examplenet[,1], examplenet[,2]))) # Added by PB represents 19 initial seedings - each seeding in turn is tested
  for(ii in 1:N){
    # run a simulation over these edges in epinet(Change latencyperiod=0 to converts this to SIR model)
    
    # run a simulation over these edges in epinet(Change latencyperiod=0 to converts this to SIR model)
    
    epiout 	<- SEIR.simulator2(examplenet, N = N, 
                               beta = k, ki = p, thetai = 5, latencydist="gamma", latencyperiod = 0,
                               init = ii)# init is specifies the number of nodes you want to seed the infection this allows us to seed disease on each node rather than random(view the code behind SEIR.simulator2 Ian tweaked it )
    
    
    #print(sum(!is.na(epiout[,4])))# print out the infected
    
    # shift times so that first I is at time 0
    firstE 				<- epiout[1,3]
    epiout[,c(3,4,5)]  		<- epiout[,c(3,4,5)] - firstE 
    
    # total number of infecteds
    infecteds	<- which(is.finite(epiout[,4]))
    totalI	<- length(infecteds)
    
    # time of last recovery
    last_R_time	<- max(epiout[infecteds,5])
    
    # number of infections over time (increment 1 day)
    mids		<- seq(0, ceiling(last_R_time), 1)
    tempmap	<- matrix(0, totalI, length(mids))
    rownames(tempmap) <- infecteds
    colnames(tempmap) <- mids
    estart <- round(epiout[infecteds,3])+1
    istart <- round(epiout[infecteds,4])+1
    rstart <- round(epiout[infecteds,5])+1
    for (j in 1:totalI) {
      tempmap[j,estart[j]:istart[j]] <- 1
      tempmap[j,istart[j]:rstart[j]] <- 2
      tempmap[j,rstart[j]:length(mids)]   <- 3
    }
    
    numE <- apply( (tempmap==1), 2, sum)
    numI <- apply( (tempmap==2), 2, sum)
    numR <- apply( (tempmap==3), 2, sum)
    numS <- N - (numE+numI+numR)
    
    maxI		<- max(numI)
    maxI_time 	<- mids[which.max(numI)]
    stats 	<- c(paste("TotalI =",totalI),paste("MaxI =",maxI),paste("TimeOfMaxI =",maxI_time))		
    #outDF <- data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("TotalI", "MaxI", "TimeOfMaxI"))))
    
    dflist[[ii]] <- rbind(
      data.frame(mids = mids, num = numE, status = "E", node = ii, stats = paste(stats, collapse="\n"), stringsAsFactors = FALSE),
      data.frame(mids = mids, num = numI, status = "I", node = ii, stats = paste(stats, collapse="\n"), stringsAsFactors = FALSE),
      data.frame(mids = mids, num = numR, status = "R", node = ii, stats = paste(stats, collapse="\n"), stringsAsFactors = FALSE),
      data.frame(mids = mids, num = numS, status = "S", node = ii, stats = paste(stats, collapse="\n"), stringsAsFactors = FALSE)
    )
    dflist_stats[[ii]] <- stats
    #   plot(mids, numS, type="l", col="black", ylim=c(0,N),xlab="Time",ylab="Number", main = paste("start node:", ii))
    #   lines(mids, numE, col="orange")
    #   lines(mids, numI, col="red")
    #   lines(mids, numR, col="blue")
    #   legend("left",c("S","E","I","R"),lty=1,col=c("black","orange","red","blue"),bty="n")
    #   legend("topright",stats,pch=NA,bty="n")
    
    epioutList[[ii]] <- epiout
    
  }
  
  epioutListMod <- lapply(
    epioutList,
    function(x) {
      myTable <- merge(data.frame(x, stringsAsFactors = FALSE), translationTable, by.x = "Node.ID", by.y = "code")
      names(myTable) <- c(names(myTable)[1:5], "label")
      return(myTable)
    }
  )
  
  n_seeds <- length(epioutListMod)
  epioutListMod <- bind_rows(epioutListMod)
  
  # print("HERE epioutListMod$seeding")
  epioutListMod$seeding <- rep(1:n_seeds, each = dim(epioutListMod)[[1]] / n_seeds)
  # print("HERE epioutListMod")
  return(epioutListMod)
}

run200Simulation <- function(examplenet, k, p) {
  return(
    bind_rows(
      lapply(
        1:200,
        function(x) {
          myOutput <- runSimulation(examplenet, k=k, p=p)
          myOutput$run <- x
          return(myOutput)
        }
      )
    )
  )
}

runSimulationsForPs <- function(examplenet, k, ps = c(0.1, 0.5, 1, 1.5, 2)) {
  return(
    bind_rows(
      lapply(
        ps,
        function(x) {
          message(paste("Running p =", x))
          myOutput <- run200Simulation(examplenet, k=k, p=x)
          myOutput$p <- x
          return(myOutput)
        }
      )
    )
  )
}

runSimulationsForKs <- function(examplenet, ks = c(0.01, 0.1, 0.2, 0.3), ps = c(0.1, 0.5, 1, 1.5, 2)) {
  return(
    bind_rows(
      lapply(
        ks,
        function(x) {
          message(paste("Running k =", x))
          myOutput <- runSimulationsForPs(examplenet, k=x, ps=ps)
          myOutput$k <- x
          return(myOutput)
        }
      )
    )
  )
}

t0 <- Sys.time()
# test1 <- get.edgelist(gve)
# test2 <- curateMyNetwork(test1)
# examplenet <- test2
# finalTable_empirical   <- runSimulationsForKs(test2)
gve <- get(load("..//RImages//EmpiricalObject.RData"))

nodes <- sort(unique(c(get.edgelist(gve)[,1], get.edgelist(gve)[,2])))

for(i in 0:length(nodes)){
  
  print(nodes[i])
  gveR <- get.edgelist(gve)
  if(i > 0){
    c1 <- grepl(nodes[i], gveR[,1])
    c2 <- grepl(nodes[i], gveR[,2])
  
    gveR <- gveR[!c1 & !c2,]
  }
  finalTable_empirical   <- gveR %>% curateMyNetwork %>% runSimulationsForKs
  
  outN <- ifelse(i == 0, "Base", nodes[i])
  finalTable_empirical$nodeD <- outN
  finalTable_empirical <- finalTable_empirical[!is.na(finalTable_empirical$Etime),]
  
  if(exists("finalTableEOut")) finalTableEOut <- rbind(finalTableEOut, finalTable_empirical)
  if(!exists("finalTableEOut")) finalTableEOut <- finalTable_empirical
  
}

gv2 <- get(load("..//RImages//GravityObject.RData"))

# Gravity network
for(i in 0:length(nodes)){
  
  print(nodes[i])
  gveR <- get.edgelist(gv2)
  if(i > 0){
  c1 <- grepl(nodes[i], gveR[,1])
  c2 <- grepl(nodes[i], gveR[,2])
  
  gveR <- gveR[!c1 & !c2,]
  }
  finalTable_empirical   <- gveR %>% curateMyNetwork %>% runSimulationsForKs

  outN <- ifelse(i == 0, "Base", nodes[i])
  finalTable_empirical$nodeD <- outN
  
#  finalTable_empirical$nodeD <- nodes[i]
  finalTable_empirical <- finalTable_empirical[!is.na(finalTable_empirical$Etime),]
  if(exists("finalTableGOut")) finalTableGOut <- rbind(finalTableGOut, finalTable_empirical)
    if(!exists("finalTableGOut")) finalTableGOut <- finalTable_empirical
  
}

g2 <- get(load("..//RImages//MolecularObject.RData"))

# Molecular
for(i in 0:length(nodes)){
  
  print(nodes[i])
  gveR <- get.edgelist(g2)
  if(i > 0){
  c1 <- grepl(nodes[i], gveR[,1])
  c2 <- grepl(nodes[i], gveR[,2])
  
  gveR <- gveR[!c1 & !c2,]
  }
  finalTable_empirical   <- gveR %>% curateMyNetwork %>% runSimulationsForKs

  outN <- ifelse(i == 0, "Base", nodes[i])
  finalTable_empirical$nodeD <- outN
  
#  finalTable_empirical$nodeD <- nodes[i]
  finalTable_empirical <- finalTable_empirical[!is.na(finalTable_empirical$Etime),]
  if(exists("finalTableMOut")) finalTableMOut <- rbind(finalTableMOut, finalTable_empirical)
    if(!exists("finalTableMOut")) finalTableMOut <- finalTable_empirical
  
}


finalTable_random_emp  <- get.edgelist(random.empirical)     %>% curateMyNetwork %>% runSimulationsForKs
finalTable_random_emp$label <- translationTable$city[match(finalTable_random_emp$seeding, translationTable$code)]
finalTable_random_emp$file  <- "Random_emp_xtx"

finalTable_random_grav <- get.edgelist(random.gravity)    %>% curateMyNetwork %>% runSimulationsForKs
finalTable_random_grav$label <- translationTable$city[match(finalTable_random_grav$seeding, translationTable$code)]
finalTable_random_grav$file <- "Random_grav_xtx"

finalTable_random_mol  <- get.edgelist(random.molecular)     %>% curateMyNetwork %>% runSimulationsForKs
finalTable_random_emp$label <- translationTable$city[match(finalTable_random_emp$seeding, translationTable$code)]
finalTable_random_mol$file  <- "Random_mole_xtx"


finalTable_random_grav_sq <- get.edgelist(random.gravity.sq)    %>% curateMyNetwork %>% runSimulationsForKs
finalTable_random_grav_sq$label <- translationTable$city[match(finalTable_random_grav_sq$seeding, translationTable$code)]
finalTable_random_grav_sq$file <- "Random_grav_sq"


##### testing code
test1 <- get.edgelist(gve)
test2 <- curateMyNetwork(test1)
test3 <- runSimulation(test2, k = 0.3, p = 1)
summary(test3)

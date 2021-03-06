PART I


txt = readLines("offline.final.trace.txt")



processLine = function(x){
  fields = unlist(strsplit(x, split = ";"))
  n = length(fields)
  if(n == 1){
    return(NULL)
  }
  t = gsub("t=", "", fields[1])
  scanMac = gsub("id=","", fields[2])
  pos = unlist(strsplit(gsub("pos=","",fields[3]), split = ","))
  posX = pos[1]
  posY = pos[2]
  posZ = pos[3]
  orientation = gsub("degree=", "", fields[4])
  if(n == 4){
    return(NULL)
  }
  mac0 = sapply(fields[5:n], function(x) strsplit(x, split = "="))
  mac = sapply(mac0, function(x) x[1])
  names(mac) = NULL
  mac_de = sapply(mac0, function(x) strsplit(x[2], split = ","))
  signal = sapply(mac_de, function(x) x[1])
  names(signal) = NULL
  channel = sapply(mac_de, function(x) x[2])
  names(channel) = NULL
  type = sapply(mac_de, function(x) x[3])
  names(type) = NULL
  set = matrix(c(rep(t,n-4), rep(scanMac,n-4), rep(posX,n-4), rep(posY,n-4), 
                 rep(posZ,n-4), rep(orientation,n-4), mac, signal, channel, type), 
               byrow = FALSE, ncol = 10)
  
  return(set)
}

tmp = lapply(txt, processLine)
offline = as.data.frame(do.call("rbind", tmp))
names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                   "orientation", "mac", "signal", "channel", "type")

save(offline, file = "offline.rda")


summary(offline)
summary(offline$mac)
hist(as.numeric(as.character(offline$signal[as.character(offline$mac) == "00:0f:a3:39:dd:cd"]))
     , main = "Signal Strength of 39:dd:cd", xlab = "signal strength")
hist(as.numeric(as.character(offline$signal[as.character(offline$mac) == "00:0f:a3:39:e1:c0"]))
     , main = "Signal Strength of 39:e1:c0", xlab = "signal strength")
summary((offline$channel[as.character(offline$mac) == "00:0f:a3:39:e1:c0"]))
summary((offline$channel[as.character(offline$mac) == "00:14:bf:3b:c7:c6"]))
summary((offline$channel[as.character(offline$mac) == "00:14:bf:b1:97:81"]))
summary((offline$channel[as.character(offline$mac) == "00:14:bf:b1:97:8a"]))
summary((offline$channel[as.character(offline$mac) == "00:14:bf:b1:97:8d"]))
summary((offline$channel[as.character(offline$mac) == "00:14:bf:b1:97:90"]))


keepMac = c("00:14:bf:b1:97:90", "00:14:bf:b1:97:8d", 
            "00:14:bf:b1:97:8a", "00:14:bf:b1:97:81",
            "00:14:bf:3b:c7:c6", "00:0f:a3:39:e1:c0")

cleanData = function(data, keepMacs = c("00:14:bf:b1:97:90", "00:14:bf:b1:97:8d", 
                                        "00:14:bf:b1:97:8a", "00:14:bf:b1:97:81",
                                        "00:14:bf:3b:c7:c6", "00:0f:a3:39:e1:c0")) {
  data$time = as.numeric(as.character(data$time))
  data$orientation = as.numeric(as.character(data$orientation))
  data$orientation0 = data$orientation
  data$orientation = (round(data$orientation/45) * 45)  %% 360
  data = data[!(data$type == 1),]
  dataframe = subset(data, select = -c(scanMac, posZ, type, channel))
  dataframe = dataframe[dataframe$mac %in% keepMacs,]
  levels(dataframe$mac)[!(levels(dataframe$mac) %in% keepMacs)] = NA
  dataframe$signal = as.numeric(as.character(dataframe$signal))
  return(dataframe)
}

offline2 = cleanData(offline)
summary(offline2)


PART II


diff_avg_median = aggregate(offline2$signal, by = list(posX = offline2[,2], posY = offline2[,3], 
                                                       orientation = offline2[,4]), FUN = mean)[,4] - 
  aggregate(offline2$signal, by = list(posX = offline2[,2], posY = offline2[,3], 
                                       orientation = offline2[,4]), FUN = median)[,4]
par(mfrow=c(1,1))
hist(diff_avg_median, freq = FALSE, main = "Difference between Mean and Median for each Position Orientation", xlab = "Difference")

comb_mac = function(data){
  macp = data[data$mac == keepMac[1],]
  maclis = aggregate(macp$signal, by = list(posX = macp[,2], posY = macp[,3], 
                                            orientation = macp[,4]), 
                     FUN = mean)
  for(i in 2:6){
    macp = data[data$mac == keepMac[i],]
    macal = aggregate(macp$signal, by = list(posX = macp[,2], posY = macp[,3], 
                                             orientation = macp[,4]), 
                      FUN = mean)
    maclis = cbind(maclis, macal[,4])
  }
  names(maclis) = c('posX', 'posY', 'orientation', "S1", "S2", "S3", "S4", "S5", "S6")
  maclis$posX = as.numeric(as.character(maclis$posX))
  maclis$posY = as.numeric(as.character(maclis$posY))
  return(maclis)
}

offline3 = comb_mac(offline2)
save(offline3, file = "offline3.rda")




max.pos = lapply(1:6, function(i) offline3[ offline3[, i + 3] == max(offline3[, i + 3]), 1:2])
max.pos


library(gplots)
library(plot3D)

for(i in 1:6){
  par(mfrow=c(1,2))
  scatter2D(x = jitter(as.numeric(as.character(offline2$posX[(offline2$mac == keepMac[i])]))), 
    y = jitter(as.numeric(as.character(offline2$posY[offline2$mac == keepMac[i]]))), 
    colvar = offline2$signal[offline2$mac == keepMac[i]], 
    pch = 19, cex = 0.2, col = rev(redblue(100)), ylab = "posY", 
    xlab = "posX", main = paste("Signal Strength of S", i, " at each Position", sep = ""), 
    clab = c("", "", "Signal Strength") , colkey = list(cex.clab = 0.7))
  points(x = max.pos[[i]][[1]], y = max.pos[[i]][[2]], pch = 8)
  legend(x = 10, y = 11, pch = 8, legend = "Access Point", bty = "n")
  offline.plot = offline3
  offline.plot$dis = sqrt((offline.plot$posX - max.pos[[i]][[1]])^2 + 
    (offline.plot$posY - max.pos[[i]][[2]])^2)
  color = redblue(length(offline.plot[,i + 3]))[round(rank(-offline.plot[, i + 3]))]
  plot(offline.plot$dis ~ offline.plot$orientation, cex = 4, pch = 15, col = color
    ,xlab = "Orientation
    (color is signal strength as left graph)", 
    ylab = "Distance From Access Point", 
    main = paste("  Distance vs Orientation 
    vs Signal Strength(S", i, ")", sep = ""))
  points(offline.plot$dis ~ offline.plot$orientation, cex = 0.8, pch = 18)
}
par(mfrow=c(1,1))



PART III

calDis = function(data = offline3){
sig = t(as.matrix(data[,4:9]))
ori_sig = t(as.matrix(offline3[,4:9]))
dis = apply(sig, 2, function(x) sqrt(colSums((ori_sig - x)^2)) )
return(t(dis))
}

disSig = calDis()

predNeighbor = function(k, disMat, pos_ori, nangles, angle){
  row = dim(disMat)[1]
  anglediff = 45 * floor(1:nangles - nangles/2)
  angles = lapply(angle, function(x) (x + anglediff) %% 360)
  pos_guess = lapply(1:row, 
    function(i) pos_ori[pos_ori$orientation %in% angles[[i]], 1:2][
      order(disMat[i, pos_ori$orientation %in% angles[[i]]])[1:k], ])
      pos_est = sapply(pos_guess, colMeans) 
  return(pos_est)
}



cvKnn = function(distances, pos_ori, k, nangles, vfold = 5){
  row = dim(distances)[1]
  fold = split(sample(1:row), rep(1:vfold, length.out = row))
  pred = function(k){
    ori = lapply(fold, function(x) predNeighbor(k, distances[x,-x], pos_ori[-x,], 
    nangles, angle = pos_ori$orientation[x]))
    names(ori) = NULL
    ori = as.data.frame(do.call("cbind", ori))
    ori = ori[,order(unlist(fold))]
    names(ori) = 1:row
    return(ori)
  }
  set = sapply(k, pred)
  return(set)
}


offline_tr1 = cvKnn(disSig, offline3[,1:3], k = 1:20, nangles = 1, vfold = 5)
offline_tr2 = cvKnn(disSig, offline3[,1:3], k = 1:20, nangles = 2, vfold = 5)
offline_tr3 = cvKnn(disSig, offline3[,1:3], k = 1:20, nangles = 3, vfold = 5)
offline_tr4 = cvKnn(disSig, offline3[,1:3], k = 1:20, nangles = 4, vfold = 5)
offline_tr5 = cvKnn(disSig, offline3[,1:3], k = 1:20, nangles = 5, vfold = 5)
offline_tr6 = cvKnn(disSig, offline3[,1:3], k = 1:20, nangles = 6, vfold = 5)
offline_tr7 = cvKnn(disSig, offline3[,1:3], k = 1:20, nangles = 7, vfold = 5)
offline_tr8 = cvKnn(disSig, offline3[,1:3], k = 1:20, nangles = 8, vfold = 5)

save(offline_tr1, file = "offline_tr1.rda")
save(offline_tr2, file = "offline_tr2.rda")
save(offline_tr3, file = "offline_tr3.rda")
save(offline_tr4, file = "offline_tr4.rda")
save(offline_tr5, file = "offline_tr5.rda")
save(offline_tr6, file = "offline_tr6.rda")
save(offline_tr7, file = "offline_tr7.rda")
save(offline_tr8, file = "offline_tr8.rda")



plotErrorDist = function(posPred, posTruth = offline3[, 1:2], k, ...){
  row = dim(posPred)[1]
  err = function(x){
    error = sum(sapply(1:row, function(i) 
      sqrt((x[[i]][1] - posTruth[i, 1])^2 + (x[[i]][2] - posTruth[i, 2])^2)))
    return(error)
  }
  errors = apply(posPred, 2, err)
  plot(y = errors, x = k, type = 'l', 
       xlab = 'k Values', ylab = 'Errors',  ...)
  segments(x0 = 0, y0 = min(errors), x1 = k[errors == min(errors)], y1 = min(errors), lty = 2)
  segments(x0 = k[errors == min(errors)], y0 = 0, 
           x1 = k[errors == min(errors)], y1 = min(errors), lty = 2)
  text(x = k[errors == min(errors)], y = min(errors) + 100, 
       labels = paste(c("error is ", min(errors), ", optimal k is ", 
                        k[errors == min(errors)]), sep = "", collapse = "", main = " "), cex = 0.7)
  return(k[errors == min(errors)])
}

par(mfrow = c(2,2))
plotErrorDist(offline_tr1, k = 1:20, main = "Errors VS k Values(nangle = 1)")
plotErrorDist(offline_tr2, k = 1:20, main = "Errors VS k Values(nangle = 2)")
plotErrorDist(offline_tr3, k = 1:20, main = "Errors VS k Values(nangle = 3)")
plotErrorDist(offline_tr4, k = 1:20, main = "Errors VS k Values(nangle = 4)")
plotErrorDist(offline_tr5, k = 1:20, main = "Errors VS k Values(nangle = 5)")
plotErrorDist(offline_tr6, k = 1:20, main = "Errors VS k Values(nangle = 6)")
plotErrorDist(offline_tr7, k = 1:20, main = "Errors VS k Values(nangle = 7)")
plotErrorDist(offline_tr8, k = 1:20, main = "Errors VS k Values(nangle = 8)")
par(mfrow = c(1,1))
optimal_k = plotErrorDist(offline_tr4, k = 1:20)



library(plotrix)

online = readLines("online.final.trace.txt")
tmponline = lapply(online, processLine)
online = as.data.frame(do.call("rbind", tmponline))
names(online) = c("time", "scanMac", "posX", "posY", "posZ",
  "orientation", "mac", "signal", "channel", "type")
online2 = cleanData(online)
online3 = comb_mac(online2)
disSigonline = calDis(online3)

online_prd = predNeighbor(k = optimal_k, disMat = disSigonline, 
pos_ori = offline3[, 1:3],  nangles = 3, angle = online3$orientation)

error_online = sqrt((online_prd[ 1,] - online3[, 1])^2 + (online_prd[ 2,] - online3[, 2])^2)

scatter2D(x = offline3$posX , y = offline3$posY, col = "grey", pch = 15, cex = 0.8, type = "p",
  asp = 1, xlab = "posX", ylab = "posY", main = "Real VS Predicted Positions")
  legend(x = 20, y = 2, legend = c("Real Position", "Predicted Position"), 
  pch = c(19, 8),
  bty = "n", cex = 0.7, col = c("black", "firebrick2", "red"))
legend(x = 19.5, y = 0.5, lty = 1, lwd = 2, legend = "Connecting Corresponding Points", 
  bty = "n", cex = 0.7, col = "red")
for(i in 1:60){
  draw.circle(x = online3[i, 1], y = online3[i, 2], radius = error_online[i], 
    col = adjustcolor("olivedrab2", alpha.f = 0.1), 
    border = adjustcolor("olivedrab2", alpha.f = 0.5))
  points(x = online3[i, 1], y = online3[i, 2], cex = 0.7, pch = 19)
  points(x = online_prd[1,i], y = online_prd[2,i], cex = 0.7, pch = 8, col = "firebrick2")
  lines(x = c(online3[i, 1], online_prd[1,i]), y = c(online3[i, 2], online_prd[2,i]), 
  col = "red", lwd = 2)
}


hist(error_online, freq = F, xlab = "Error in Euclidean Distance", 
main = "Errors of Predictions")
summary(error_online)

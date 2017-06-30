#### calculates aggregated attribute values given a discount rate

get_scores <- function(d = 0, dt0, dt5, dt10){
  
  # calculate time weights
  tw <- (1-d)^c(0,5,10)
  tw <- tw / sum(tw)
  
  # generate data aggregated over time
  aggdata <- tw[1] * dt0 + tw[2] * dt5 + tw[3] * dt10
  
  return(aggdata)
  
}

#### copies an attribute matrix N times to give SMAA input

smaa_copy <- function(data,N){
  newdata <- array(rep(as.matrix(data),each=N),c(N,nrow(data),ncol(data)))
}

#### takes (n * m) attribute matrix and SMAA wts, and returns plot of RAI's

do_smaa <- function(meas,attnames,altnames){
  pref <- simplex.sample(ncol(meas[1,,]), length(meas[,1,1]))$samples
  result <- smaa(meas, pref)
  
  # plotting rank acceptability results
  ras <- as.data.frame(unclass(result$ra))
  names(ras) <- c("R1","R2","R3","R4","R5","R6","R7")
  ras <- melt(cbind(ras, option = altnames), id.vars = c('option'))
  ras$option <- factor(ras$option,levels=c("MPA-closed-R20M","MPA-closed","Open-1-add",
                                           "Open-1","Open-3-add","Open-3","MPA-open"))
  g1 <- ggplot(ras,aes(x = option, y = value,fill = variable)) + 
    geom_bar(position = "fill",stat = "identity") +
    scale_fill_brewer(direction = -1, palette = 1) +
    theme_bw(base_size=24) +
    theme(legend.text=element_text(size=20),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 20), 
          axis.title = element_blank(),
          legend.key.size = unit(2, 'lines'))
  
  # plotting central weight results
  
  cws <- as.data.frame(unclass(result$cw))
  names(cws) <- attnames
  
  # aggregate some criteria for neater plot
  cws_red <- data.frame(Environment = apply(cws[,1:6],1,sum),
                        Economy = apply(cws[,7:11],1,sum),
                        Equity = apply(cws[,12:16],1,sum),
                        Political = apply(cws[,17:20],1,sum))
  
  # put full cws into long form
  cws <- melt(cbind(cws, option = altnames), id.vars = c('option'))
  
  # reorder options
  cws$option <- factor(cws$option,levels=c("MPA-closed-R20M","MPA-closed","Open-1-add",
                                           "Open-1","Open-3-add","Open-3","MPA-open"))
  # plot
  g2 <- ggplot(cws,aes(x = option, y = value,fill = variable)) + 
    geom_bar(position = "fill",stat = "identity") +
    scale_fill_viridis(discrete = TRUE, direction = -1) + 
    theme_bw(base_size=24) +
    theme(legend.position = "bottom",
          legend.text=element_text(size=20),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 20), 
          axis.title = element_blank())
  
  # put reduced cws into long form
  cws_red <- melt(cbind(cws_red, option = altnames), id.vars = c('option'))
  
  # reorder options
  cws_red$option <- factor(cws_red$option,levels=c("MPA-closed-R20M","MPA-closed","Open-1-add",
                                                   "Open-1","Open-3-add","Open-3","MPA-open"))
  # plot
  g3 <- ggplot(cws_red,aes(x = option, y = value,fill = variable)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_fill_viridis(discrete = TRUE, direction = -1) + 
    theme_bw(base_size=24) +
    theme(legend.position = "bottom",
          legend.text=element_text(size=20),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 20), 
          axis.title = element_blank())
  
  return(list(ra = result$ra, cw = result$cw, 
              plotra = g1, plotcw = g2, plotcw_red = g3))
  
}

#### applies some uncertainty to attribute evaluations

apply_uncertainty <- function(meas,alts,envu=c(0,0),ecou=c(0,0),socu=c(0,0),polu=c(0,0)){
  
  # environmental uncertainty
  meas[,alts,1:6] <- meas[,alts,1:6] + runif(prod(dim(meas[,alts,1:6])), min = envu[1], max = envu[2])
  meas[,,1:6] <- (meas[,,1:6] - min(meas[,,1:6])) / (max(meas[,,1:6]) - min(meas[,,1:6]))
  
  # economic uncertainty  
  meas[,alts,7:11] <- meas[,alts,7:11] + runif(prod(dim(meas[,alts,7:11])), min = ecou[1], max = ecou[2])
  meas[,,7:11] <- (meas[,,7:11] - min(meas[,,7:11])) / (max(meas[,,7:11]) - min(meas[,,7:11]))
  
  # equity (social) uncertainty
  meas[,alts,12:16] <- meas[,alts,12:16] + runif(prod(dim(meas[,alts,12:16])), min = socu[1], max = socu[2])
  meas[,,12:16] <- (meas[,,12:16] - min(meas[,,12:16])) / (max(meas[,,12:16]) - min(meas[,,12:16]))
  
  # equity (political) uncertainty
  meas[,alts,17:20] <- meas[,alts,17:20] + runif(prod(dim(meas[,alts,17:20])), min = polu[1], max = polu[2])
  meas[,,17:20] <- (meas[,,17:20] - min(meas[,,17:20])) / (max(meas[,,17:20]) - min(meas[,,17:20]))
  
  return(meas)
}

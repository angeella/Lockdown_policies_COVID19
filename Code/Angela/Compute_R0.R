###################################################################################
##############################R0 Index###################################
###################################################################################


R0_compute <- function(state, dataset){
  
  #dat_c <- dataset %>% filter(id == paste0(state)) 
  dat_c <- dataset[which(dataset$id== paste0(state)),]
  dat_c <- dat_c[which(dat_c$confirmed!=0),]
  days <- dim(dat_c)[1]
  dat_c$t <- c(1:days)
  
  #calculate r0 based with a mobile window of 5 days
  #vector for beta and standard deviation
  duration<-18
  beta1<-NULL
  sd_beta1<-NULL
  #for cycle for R0 estimates from days-2 to days+2
  for (i in 3:(days-2)){
    fit <- lm(log(confirmed)~t,data=dat_c[(i-2):(i+2),])
    beta1<-c(beta1,coef(fit)[2])
    sd_beta1<-c(sd_beta1,coef(summary(fit))[2,2])
  }
  
  mean  <- 1+(beta1*duration)
  lower <- 1+((beta1-1.96*sd_beta1)*duration)
  upper <- 1+((beta1+1.96*sd_beta1)*duration)
  date<-as.Date(substr(dat_c$date,1,10))[3:(days-2)]
  
  
  df <- data.frame(date, mean, lower, upper)
  #colnames(df) <- c("date", paste0("mean.", state),paste0("lower.", state),paste0("upper.", state))
  return(df)
}

#JOIN ALL
add_R0 <- function(dataset){

  dat <- dataset %>% filter(id != "ASM")
  id_states <- unique(dat$id)
  db2 <- list()
  for(i in 1:length(id_states)){
    
    db <- dat %>% filter(id == id_states[i]) 
    db1 <- R0_compute(state = id_states[i], dataset = dat %>% filter(id == id_states[i]))[,c("date", "mean")]
    colnames(db1) <- c("date", "R0mean")
    db2[[i]] <-full_join(db, db1, by = "date")
  }
  
  df <- plyr::ldply(db2, data.frame)
  
  
  return(df)  
}


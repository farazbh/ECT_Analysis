read.table("aseg_stats.txt", sep = "\t")

for (grid in c('13900', '13903', '14071', '14160')){
  filename = paste0("aseg_stats_", grid,".txt")
  voldf <- read.table(filename, sep = "\t", header = T)
  voldf$time <- 1:dim(voldf)[1]
  voldf$grid <- as.numeric(grid)
  if (grid=='13900')
  {
    fvoldf <- voldf
  }else
  {
    fvoldf <- rbind(fvoldf, voldf)
  }
}
  

library(ggplot2)

#Establishing net change response variable for right hippocampus volume
fvoldf['NRhip'] <- 0
for (grid in c('13900', '13903', '14071', '14160')){
  ix <- which(fvoldf$grid==grid)
  ix1 <- which((fvoldf$grid==grid) & (fvoldf$time==1))
  fvoldf$NRhip[ix]<-  (fvoldf$Right.Hippocampus[ix]-fvoldf$Right.Hippocampus[ix1])/fvoldf$Right.Hippocampus[ix1]
}

#Establishing net change response variable for left hippocampus volume
fvoldf['NLhip'] <- 0
for (grid in c('13900', '13903', '14071', '14160')){
  ix <- which(fvoldf$grid==grid)
  ix1 <- which((fvoldf$grid==grid) & (fvoldf$time==1))
  fvoldf$NLhip[ix]<-  (fvoldf$Left.Hippocampus[ix]-fvoldf$Left.Hippocampus[ix1])/fvoldf$Left.Hippocampus[ix1]
}

#Establishing net change response variable for right amygdala volume
fvoldf['NRami'] <- 0
for (grid in c('13900', '13903', '14071', '14160'))
{
  ix <- which(fvoldf$grid==grid)
  ix1 <- which((fvoldf$grid==grid) & (fvoldf$time==1))
  fvoldf$NRami[ix] <-  (fvoldf$Right.Amygdala[ix]-fvoldf$Right.Amygdala[ix1])/fvoldf$Right.Amygdala[ix1]
}

#Establishing net change response variable for left amygdala volume
fvoldf['NLami'] <- 0
for (grid in c('13900', '13903', '14071', '14160'))
{
  ix <- which(fvoldf$grid==grid)
  ix1 <- which((fvoldf$grid==grid) & (fvoldf$time==1))
  fvoldf$NLami[ix] <-  (fvoldf$Left.Amygdala[ix]-fvoldf$Left.Amygdala[ix1])/fvoldf$Left.Amygdala[ix1]
}

#plot for NRhip over time for 4 subjects
ggplot(data=fvoldf, aes(x=time, y=NRhip, colour=factor(grid)))+
  geom_boxplot()+
  geom_line()


#loading ect data
install.packages("dplyr")
library("dplyr")
iper <- read.csv("IPERSR_DATA_2023-06-21_1423.csv")

iper <- iper %>% filter(record_id %in% c(58,59,61,62))

for(x in 1:nrow(iper)){
  iper$ect_level2[x] <- iper$frequency[x] * iper$stim_duration[x]
}
for(x in 1:nrow(iper)){
  iper$ect_level3[x] <- (iper$frequency_2[x] * iper$stim_duration_2[x]) + (iper$frequency[x] * iper$stim_duration[x])
}

for(x in 1:nrow(iper)){
  if (is.na(iper$ect_level3[x]) == FALSE) {
    iper$ect_level2[x] <- iper$ect_level3[x]
  }
}

library(tibble)
# redcap instance, instrument, grid number, frquency, stim, frequency 2, stim2, pre_mri ect

test_1 <- data.frame(iper$record_id, iper$redcap_event_name, iper$gridnumber, iper$redcap_repeat_instance, iper$frequency, iper$ect_completed_pre_mri, iper$stim_duration, iper$frequency_2, iper$stim_duration_2, iper$ect_level2) 

test_1 <- test_1 %>%
  add_column(ect_total = 0)

for (x in 2:nrow(test_1)){
  if (test_1$iper.redcap_event_name[x] == 'ect_arm_1' ){
    test_1$ect_total[x] <- as.numeric(test_1$iper.ect_level2[x])+as.numeric(test_1$ect_total[x-1])
  }
}

ect_1 <- select(filter(test_1, iper.redcap_event_name == 'ect_arm_1' & iper.record_id == 58 ),c(iper.redcap_repeat_instance, ect_total))
ect_2 <- select(filter(test_1, iper.redcap_event_name == 'ect_arm_1' & iper.record_id == 59 ),c(iper.redcap_repeat_instance, ect_total))
ect_3 <- select(filter(test_1, iper.redcap_event_name == 'ect_arm_1' & iper.record_id == 61 ),c(iper.redcap_repeat_instance, ect_total))
ect_4 <- select(filter(test_1, iper.redcap_event_name == 'ect_arm_1' & iper.record_id == 62 ),c(iper.redcap_repeat_instance, ect_total))

test_1$iper.ect_completed_pre_mri[15] <- 13

for (x in 2:nrow(test_1)){
  if (test_1$iper.redcap_event_name[x] == 'mri_arm_1' & test_1$iper.record_id[x] == 58){
    i <- as.numeric(test_1$iper.ect_completed_pre_mri[x])
    if (i == 0) {
      test_1$ect_total[x] <- 0
    }
    else {
      test_1$ect_total[x] <- as.numeric(ect_1$ect_total[i])
    }
  }
  else if (test_1$iper.redcap_event_name[x] == 'mri_arm_1' & test_1$iper.record_id[x] == 59){
    i <- as.numeric(test_1$iper.ect_completed_pre_mri[x])
    if (i == 0) {
      test_1$ect_total[x] <- 0
    }
    else {
      test_1$ect_total[x] <- as.numeric(ect_2$ect_total[i])
    }
  }
  else if (test_1$iper.redcap_event_name[x] == 'mri_arm_1' & test_1$iper.record_id[x] == 61){
    i <- as.numeric(test_1$iper.ect_completed_pre_mri[x])
    if (i == 0) {
      test_1$ect_total[x] <- 0
    }
    else {
      test_1$ect_total[x] <- as.numeric(ect_3$ect_total[i])
    }
  }
  else if (test_1$iper.redcap_event_name[x] == 'mri_arm_1' & test_1$iper.record_id[x] == 62){
    i <- as.numeric(test_1$iper.ect_completed_pre_mri[x])
    if (i == 0) {
      test_1$ect_total[x] <- 0
    }
    else {
      test_1$ect_total[x] <- as.numeric(ect_4$ect_total[i])
    }
  }
}


ect <- select(filter(test_1, iper.redcap_event_name == 'mri_arm_1' ),c(iper.record_id, iper.gridnumber, iper.ect_completed_pre_mri, iper.redcap_repeat_instance, ect_total))
for (x in 1:nrow(ect)){
  if (ect$iper.record_id[x] == 58){
    ect$iper.gridnumber[x] <- 13900
  }
  else if (ect$iper.record_id[x] == 59){
    ect$iper.gridnumber[x] <- 13903
  }
  else if (ect$iper.record_id[x] == 61){
    ect$iper.gridnumber[x] <- 14071
  }
  else if (ect$iper.record_id[x] == 62){
    ect$iper.gridnumber[x] <- 14160
  }
}



#NRhip over ect levels
fvoldf <- cbind(fvoldf, ect$ect_total)
colnames(fvoldf)[which(colnames(fvoldf)=='ect$ect_total')] <- 'ect_total'

ggplot(data=fvoldf, aes(x=ect_total, y=NRhip, colour=factor(grid)))+
  geom_line()+
  geom_point()

#NLhip over ect levels
ggplot(data=fvoldf, aes(x=ect_total, y=NLhip, colour=factor(grid)))+
  geom_line()+
  geom_point()

#NLami over ect levels
ggplot(data=fvoldf, aes(x=ect_total, y=NLami, colour=factor(grid)))+
  geom_line()+
  geom_point()

#NRami over ect levels
ggplot(data=fvoldf, aes(x=ect_total, y=NRami, colour=factor(grid)))+
  geom_line()+
  geom_point()



#loading aparc data
for (grid in c('13900', '13903', '14071', '14160')){
    filename = paste0("aparc_stats_",grid,"_LH.txt")
    lvol <- read.table(filename, sep = "\t", header = T)
    lvol$time <- 1:dim(voldf)[1]
    lvol$grid <- as.numeric(grid)
    if (grid=='13900'){
        lvoldf <- lvol
        }
    else{
    lvoldf <- rbind(lvoldf, lvol)
    }
}

for (grid in c('13900', '13903', '14071', '14160')){
  filename = paste0("aparc_stats_",grid,"_RH.txt")
  rvol <- read.table(filename, sep = "\t", header = T)
  rvol$time <- 1:dim(voldf)[1]
  rvol$grid <- as.numeric(grid)
    if (grid=='13900')
    {
      rvoldf <- rvol
      }
  else
        {
          rvoldf <- rbind(rvoldf, rvol)
          }
}
aparc_vol <- cbind(lvoldf, rvoldf)

#establishing percentage change response variable for left and right insula
aparc_vol['NRins'] <- 0
for (grid in c('13900', '13903', '14071', '14160')){
   ix <- which(aparc_vol$grid==grid)
   ix1 <- which((aparc_vol$grid==grid) & (aparc_vol$time==1))
   aparc_vol$NRins[ix]<-  (aparc_vol$rh_insula_thickness[ix]-aparc_vol$rh_insula_thickness[ix1])/aparc_vol$rh_insula_thickness[ix1]
}
aparc_vol['NLins'] <- 0
for (grid in c('13900', '13903', '14071', '14160')){
  ix <- which(aparc_vol$grid==grid)
  ix1 <- which((aparc_vol$grid==grid) & (aparc_vol$time==1))
  aparc_vol$NLins[ix]<-  (aparc_vol$lh_insula_thickness[ix]-aparc_vol$lh_insula_thickness[ix1])/aparc_vol$lh_insula_thickness[ix1]
}

#binding ect values with aparc_vol data
aparcB <- cbind(aparc_vol, ect$ect_total)
colnames(aparcB)[which(colnames(aparcB)=='ect$ect_total')]<-'ecttotal'
aparcB
#NLins and NRin over total ect levels
ggplot(data=aparcB, aes(x=ecttotal, y=NLins, colour=factor(grid)))+
  geom_line()+
  geom_point()

ggplot(data=aparc_vol, aes(x=ect$ect_total, y=NRins, colour=factor(grid)))+
  geom_line()+
  geom_point()

#time allocated differences

test_2 <- data.frame(iper$record_id, iper$redcap_event_name, iper$gridnumber, iper$redcap_repeat_instance, iper$ect_completed_pre_mri, iper$date_mri, iper$ect_date) 
for (x in 1:nrow(test_2)){
  if (test_2$iper.record_id[x] == 58){
    test_2$iper.gridnumber[x] <- 13900
  }
  else if (test_2$iper.record_id[x] == 59){
    test_2$iper.gridnumber[x] <- 13903
  }
  else if (test_2$iper.record_id[x] == 61){
    test_2$iper.gridnumber[x] <- 14071
  }
  else if (test_2$iper.record_id[x] == 62){
    test_2$iper.gridnumber[x] <- 14160
  }
}
test_2$iper.ect_completed_pre_mri[15] <- 13
dates_1 <- select(filter(test_2, iper.redcap_event_name == 'ect_arm_1' & iper.record_id == 58 ),c(iper.redcap_repeat_instance, iper.ect_date))
dates_2 <- select(filter(test_2, iper.redcap_event_name == 'ect_arm_1' & iper.record_id == 59 ),c(iper.redcap_repeat_instance, iper.ect_date))
dates_3 <- select(filter(test_2, iper.redcap_event_name == 'ect_arm_1' & iper.record_id == 61 ),c(iper.redcap_repeat_instance, iper.ect_date))
dates_4 <- select(filter(test_2, iper.redcap_event_name == 'ect_arm_1' & iper.record_id == 62 ),c(iper.redcap_repeat_instance, iper.ect_date))


test_2['last_ect'] <- 0

for (x in 1:nrow(test_2)){
  if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 58){
    i <- test_2$iper.ect_completed_pre_mri[x]
    if (i==0){
      test_2$last_ect[x] <- as.numeric(0)
    }
    else{
      test_2$last_ect[x] <- as.integer(difftime(test_2$iper.date_mri[x], dates_1$iper.ect_date[i]))
    }
  }
  else if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 59){
    i <- test_2$iper.ect_completed_pre_mri[x]
    if(i == 0){
      test_2$last_ect[x] <- as.numeric(0)
    }
    else{
      test_2$last_ect[x] <- as.integer(difftime(test_2$iper.date_mri[x], dates_2$iper.ect_date[i]))
    }
  }
  else if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 61){
    i <- test_2$iper.ect_completed_pre_mri[x]
    if (i==0){
      test_2$last_ect[x] <- as.numeric(0)
    }
    else{
      test_2$last_ect[x] <- as.integer(difftime(test_2$iper.date_mri[x], dates_3$iper.ect_date[i]))
    }
  }
  else if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 62){
    i <- test_2$iper.ect_completed_pre_mri[x]
    if (i == 0){
      test_2$last_ect[x] <- as.numeric(0)
    }
    else{
      test_2$last_ect[x] <- as.integer(difftime(test_2$iper.date_mri[x], dates_4$iper.ect_date[i]))
    }
  }
  else {
    test_2$last_ect[x] <- NA
  }
}

time_analysis <- select(filter(test_2, iper.redcap_event_name == 'mri_arm_1' ),c(iper.record_id, iper.gridnumber, iper.redcap_repeat_instance, ect_frequency, last_ect))


#binding last ect time values with aparc_vol data
cbind(aparc_vol, time_analysis$last_ect)

aparcB <- cbind(aparcB, time_analysis$last_ect)
colnames(aparcB)[which(colnames(aparcB)=='time_analysis$last_ect')]<-'lastect'
aparcB

#NLins and NRin over total ect levels
ggplot(data=aparcB, aes(x=lastect, y=NLins, colour=factor(grid)))+
  geom_point()

ggplot(data=aparcB, aes(x=lastect, y=NRins, colour=factor(grid)))+
  stat_smooth(method='lm')+
  geom_point()

View(aparc_vol$time_analysis$last_ect)

fvoldf2 <- cbind(fvoldf2, ect$iper.ect_completed_pre_mri)
colnames(fvoldf2)[which(colnames(fvoldf2)=='ect$iper.ect_completed_pre_mri')] <- 'ect_pre_mri'
fvoldf2 <- cbind(fvoldf2, ect$ect_total)
colnames(fvoldf2)[which(colnames(fvoldf2)=='ect$ect_total')] <- 'ect_total'


ggplot(data=fvoldf2, aes(x= ect$iper.ect_completed_pre_mri, y=NLhip, colour=factor(grid)))+
  geom_line()+
  geom_point()


#ect frequency calculations

test_2['ect_frequency'] <- NA
for (x in 1:nrow(test_2)){
  if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 58){
    i <- test_2$iper.ect_completed_pre_mri[x]
    if (i < 3){
      test_2$ect_frequency[x] <- NA
    }
    else{
     j <- as.numeric(difftime(test_2$iper.date_mri[x], dates_1$iper.ect_date[i-2]))
     test_2$ect_frequency[x] <- 3/(j)
    }
  }
}

for (x in 1:nrow(test_2)){
  if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 59){
    i <- test_2$iper.ect_completed_pre_mri[x]
    if (i < 3){
      test_2$ect_frequency[x] <- NA
    }
    else{
      j <- as.numeric(difftime(test_2$iper.date_mri[x], dates_2$iper.ect_date[i-2]))
      test_2$ect_frequency[x] <- 3/(j)
    }
  }
}

for (x in 1:nrow(test_2)){
  if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 61){
    i <- test_2$iper.ect_completed_pre_mri[x]
    if (i < 3){
      test_2$ect_frequency[x] <- NA
    }
    else{
      j <- as.numeric(difftime(test_2$iper.date_mri[x], dates_3$iper.ect_date[i-2]))
      test_2$ect_frequency[x] <- 3/(j)
    }
  }
}

for (x in 1:nrow(test_2)){
  if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 62){
    i <- test_2$iper.ect_completed_pre_mri[x]
    if (i < 3){
      test_2$ect_frequency[x] <- NA
    }
    else{
      j <- as.numeric(difftime(test_2$iper.date_mri[x], dates_4$iper.ect_date[i-2]))
      test_2$ect_frequency[x] <- 3/(j)
    }
  }
}

fvoldf3 <- cbind(fvoldf2, time_analysis$ect_frequency)
colnames(fvoldf3)[which(colnames(fvoldf3)=='time_analysis$ect_frequency')] <- 'ect_frequency'
aparcC <- cbind(aparcB, time_analysis$ect_frequency)
colnames(aparcC)[which(colnames(aparcC)=='time_analysis$ect_frequency')] <- 'ect_frequency'




#organizing days past each ect from mri date
for (x in 1:14){
  test_2[paste0('e-',x)] <- NA
}

for (x in 1:nrow(test_2)){
  i <- test_2$iper.ect_completed_pre_mri[x]
  if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 58 & i > 0){
    for (j in 1:i){
    test_2[x, paste0('e-',j)] <- as.integer(difftime(test_2$iper.date_mri[x], dates_1$iper.ect_date[j]))
    }
  }
  else if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 59 & i > 0){
    for (j in 1:i){
      test_2[x, paste0('e-',j)] <- as.integer(difftime(test_2$iper.date_mri[x], dates_2$iper.ect_date[j]))
    }
  }
  else if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 61 & i > 0){
    for (j in 1:i){
      test_2[x, paste0('e-',j)] <- as.integer(difftime(test_2$iper.date_mri[x], dates_3$iper.ect_date[j]))
    }
  }
  else if (test_2$iper.redcap_event_name[x] == 'mri_arm_1' & test_2$iper.record_id[x] == 62 & i > 0){
    for (j in 1:i){
      test_2[x, paste0('e-',j)] <- as.integer(difftime(test_2$iper.date_mri[x], dates_4$iper.ect_date[j]))
    }
  }
}


time_analysis <- select(filter(test_2, iper.redcap_event_name == 'mri_arm_1' ),c(iper.record_id, iper.gridnumber, iper.redcap_repeat_instance, last_ect, ect_frequency, 'e-1', 'e-2', 'e-3', 'e-4', 'e-5', 'e-6', 'e-7', 'e-8', 'e-9', 'e-10', 'e-11', 'e-12', 'e-13', 'e-14'))
fvoldf5 <- cbind(fvoldf2, time_analysis[c('e-1', 'e-2', 'e-3', 'e-4', 'e-5', 'e-6', 'e-7', 'e-8', 'e-9', 'e-10', 'e-11', 'e-12', 'e-13', 'e-14')])
fvoldf5                        

#ect impact function used to find residuals for given exponential model
ectimpact <-function(A, k, fvoldf5, roi)
{
  fsum =0 
  for (i in 1:dim(fvoldf5)[1])
  {
    msum = 0 
    if (fvoldf5$ect_pre_mri[i]!=0)
    {
      for (j in 1:fvoldf5$ect_pre_mri[i])
      {
        msum = msum + A*exp((-1)*k*fvoldf5[i, paste0('e-',j)])
      }
     
    }
    fsum = fsum + (fvoldf5[i,roi]-msum)*(fvoldf5[i,roi]-msum)
  }
  return(fsum) 
}

#additional function created to optimize A and k (13900)
funtoopt<-function(x)
{
  A<-x[1]
  k<-x[2]
  ectimpact(A,k,fvoldf5[which(fvoldf5$grid == 13900),], 'NRhip')
}
funtoopt(c(1,1))
myparam <- optim(c(1,1), funtoopt)

# function that generates list of estimates based on model
fitcalc <-function(A, k, fvoldf5, roi)
{
  fsum =c()
  for (i in 1:dim(fvoldf5)[1])
  {
    msum = 0 
    if (fvoldf5$ect_pre_mri[i]!=0)
    {
      for (j in 1:fvoldf5$ect_pre_mri[i])
      {
        msum = msum + A*exp((-1)*k*fvoldf5[i, paste0('e-',j)])
      }
      
    }
    fsum = c(fsum, msum)
  }
  return(fsum) 
}

#nrhip estimates list and plotting
NRhipfit1 <- fitcalc(myparam$par[1],myparam$par[2],fvoldf5[which(fvoldf5$grid == 13900),], 'NRhip')
plot(fvoldf5[which(fvoldf5$grid == 13900),'NRhip'], NRhipfit1)
abline(lm(NRhipfit1 ~ fvoldf5[which(fvoldf5$grid == 13900),'NRhip']))


#above sequence repeated for 13903
funtoopt2<-function(x)
{
  A<-x[1]
  k<-x[2]
  ectimpact(A,k,fvoldf5[which(fvoldf5$grid == 13903),], 'NRhip')
}
funtoopt2(c(1,1))
myparam2 <- optim(c(1,1), funtoopt2)
NRhipfit2 <- fitcalc(myparam2$par[1],myparam2$par[2],fvoldf5[which(fvoldf5$grid == 13903),], 'NRhip')
plot(fvoldf5[which(fvoldf5$grid == 13903),'NRhip'], NRhipfit2)
abline(lm(NRhipfit2 ~ fvoldf5[which(fvoldf5$grid == 13903),'NRhip']))

#above sequence repeated for 14071
funtoopt3<-function(x)
{
  A<-x[1]
  k<-x[2]
  ectimpact(A,k,fvoldf5[which(fvoldf5$grid == 14071),], 'NRhip')
}
funtoopt3(c(1,1))
myparam3 <- optim(c(1,1), funtoopt3)
NRhipfit3 <- fitcalc(myparam3$par[1],myparam3$par[2],fvoldf5[which(fvoldf5$grid == 14071),], 'NRhip')
plot(fvoldf5[which(fvoldf5$grid == 14071),'NRhip'], NRhipfit3)
abline(lm(NRhipfit3 ~ fvoldf5[which(fvoldf5$grid == 14071),'NRhip']))

#above sequence repeated for 14160
funtoopt4<-function(x)
{
  A<-x[1]
  k<-x[2]
  ectimpact(A,k,fvoldf5[which(fvoldf5$grid == 14160),], 'NRhip')
}
funtoopt4(c(1,1))
myparam4 <- optim(c(1,1), funtoopt4)
NRhipfit4 <- fitcalc(myparam4$par[1],myparam4$par[2],fvoldf5[which(fvoldf5$grid == 14160),], 'NRhip')
plot(fvoldf5[which(fvoldf5$grid == 14160),'NRhip'], NRhipfit4)
abline(lm(NRhipfit4 ~ fvoldf5[which(fvoldf5$grid == 14160),'NRhip']))

#above sequence for NLhip 

#13900
funtooptA<-function(x)
{
  A<-x[1]
  k<-x[2]
  ectimpact(A,k,fvoldf5[which(fvoldf5$grid == 13900),], 'NLhip')
}
funtooptA(c(1,1))
myparamA <- optim(c(1,1), funtooptA)
NLhipfit1 <- fitcalc(myparamA$par[1],myparamA$par[2],fvoldf5[which(fvoldf5$grid == 13900),], 'NLhip')
plot(fvoldf5[which(fvoldf5$grid == 13900),'NLhip'], NLhipfit1)
abline(lm(NLhipfit1 ~ fvoldf5[which(fvoldf5$grid == 13900),'NLhip']))

#13903
funtooptB<-function(x)
{
  A<-x[1]
  k<-x[2]
  ectimpact(A,k,fvoldf5[which(fvoldf5$grid == 13903),], 'NLhip')
}
funtooptB(c(1,1))
myparamB <- optim(c(1,1), funtooptB)
NLhipfit2 <- fitcalc(myparamB$par[1],myparamB$par[2],fvoldf5[which(fvoldf5$grid == 13903),], 'NLhip')
plot(fvoldf5[which(fvoldf5$grid == 13903),'NLhip'], NLhipfit2)
abline(lm(NLhipfit2 ~ fvoldf5[which(fvoldf5$grid == 13903),'NLhip']))



#14071
funtooptC<-function(x)
{
  A<-x[1]
  k<-x[2]
  ectimpact(A,k,fvoldf5[which(fvoldf5$grid == 14071),], 'NLhip')
}
funtooptC(c(1,1))
myparamC <- optim(c(1,1), funtooptC)
NLhipfit3 <- fitcalc(myparamC$par[1],myparamC$par[2],fvoldf5[which(fvoldf5$grid == 14071),], 'NLhip')
plot(fvoldf5[which(fvoldf5$grid == 14071),'NLhip'], NLhipfit3)
abline(lm(NLhipfit3 ~ fvoldf5[which(fvoldf5$grid == 14071),'NLhip']))

#14160
funtooptD<-function(x)
{
  A<-x[1]
  k<-x[2]
  ectimpact(A,k,fvoldf5[which(fvoldf5$grid == 14160),], 'NLhip')
}
funtooptD(c(1,1))
myparamD <- optim(c(1,1), funtooptD)
NLhipfit4 <- fitcalc(myparamD$par[1],myparamD$par[2],fvoldf5[which(fvoldf5$grid == 14160),], 'NLhip')
plot(fvoldf5[which(fvoldf5$grid == 14160),'NLhip'], NLhipfit4)
abline(lm(NLhipfit4 ~ fvoldf5[which(fvoldf5$grid == 14160),'NLhip']))



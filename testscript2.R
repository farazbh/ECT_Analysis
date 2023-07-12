data.csv <- read.csv('/nethome/fhassan3/IPERSR-MRISessionsAndName_DATA_2023-05-31_1058.csv')
head(data.csv)


# /data/BIDS/sub-13900/ses-24007/anat/sub-13900_ses-24007_run-01_T1w.nii.gz

# T1 image copy
for (x in 1:nrow(data.csv)){
  i = data.csv[x,7]
  j = data.csv[x,9]
  file_from <- paste0('/data/BIDS/sub-',j,'/ses-',i,'/anat','/sub-',j,'_ses-',i,'_run-01_T1w.nii.gz')
  
  k = sprintf("%02d", data.csv[x,4])
  file_to <- paste0('/nethome/fhassan3/LongFreesurfer/sub-',j,'_ses-',i,'_tp-', k,'_T1w.nii.gz')
  print(file_to)
  file.copy(file_from, file_to)
}

# T2 image copy
for (x in 1:nrow(data.csv)){
  i = data.csv[x,7]
  j = data.csv[x,9]
  file_from <- paste0('/data/BIDS/sub-',j,'/ses-',i,'/anat','/sub-',j,'_ses-',i,'_run-01_T2w.nii.gz')
  
  k = sprintf("%02d", data.csv[x,4])
  file_to <- paste0('/nethome/fhassan3/LongFreesurfer/sub-',j,'_ses-',i,'_tp-', k,'_T2w.nii.gz')
  print(file_to)
  file.copy(file_from, file_to)
}

file_path = '/nethome/fhassan3/recon.txt' 
base_path = '/nethome/fhassan3/recon_base.txt'
long_path = '/nethome/fhassan3/recon_long.txt'


# recon of T1 and T2 images using docker
file_path = '/nethome/fhassan3/recon.txt' 
for (x in 1:nrow(data.csv)){
  i = data.csv[x,7]
  j = data.csv[x,9]
  k = sprintf("%02d", data.csv[x,4])
  new_line <- paste0('echo -n "docker run -i --rm -v /nethome/fhassan3/LongFreesurfer/:/data -v /nethome/fhassan3/LongFreesurfer/OUTPUT/:/usr/local/freesurfer/subjects -v /usr/local/opt/freesurfer7.1/.license:/usr/local/freesurfer/.license freesurfer/freesurfer:7.1.1 /bin/bash -c ',"'",'recon-all -s ',j,'_',k, ' -i /data/sub-',j,'_ses-',i,'_tp-',k,'_T1w.nii.gz -T2 /data/sub-',j,'_ses-',i,'_tp-',k,'_T2w.nii.gz -T2pial -all',"'",'"','| qsub -o /nethome/amiklos/QSUB_OUTPUT/test_0',i,' -e /nethome/amiklos/QSUB_OUTPUT/teste_',i)
  write(new_line, file_path, append=TRUE)
}



# recon base
s <- unique(data.csv$grid)
v <- as.list(s)
base_path = '/nethome/fhassan3/recon_base.txt'
for(x in v) {
  if (x < 14160) {
    tp <- ''
    for (y in 1:10){
      k = sprintf("%02d", y)
      new_line1 <- paste0(' -tp /usr/local/freesurfer/subjects/',x,'_', k)
      tp <- paste0(tp, new_line1)
    }
    new_line2 <- paste0('echo -n "docker run -i --rm -v /nethome/fhassan3/LongFreesurfer/:/data -v /nethome/fhassan3/LongFreesurfer/OUTPUT/:/usr/local/freesurfer/subjects -v /usr/local/opt/freesurfer7.1/.license:/usr/local/freesurfer/.license freesurfer/freesurfer:7.1.1 /bin/bash -c ',"'",'recon-all -base ', x, '_base',tp,' -all',"'",'"','| qsub -o /nethome/amiklos/QSUB_OUTPUT/test_0',x,' -e /nethome/amiklos/QSUB_OUTPUT/teste_',x)
    write(new_line2, base_path, append=TRUE)
  }
  else {
    tp <- ''
    for (y in 1:5){
      k = sprintf("%02d", y)
      new_line3 <- paste0(' -tp /usr/local/freesurfer/subjects/',x,'_', k)
      tp <- paste0(tp, new_line3)
    }
    new_line4 <- paste0('echo -n "docker run -i --rm -v /nethome/fhassan3/LongFreesurfer/:/data -v /nethome/fhassan3/LongFreesurfer/OUTPUT/:/usr/local/freesurfer/subjects -v /usr/local/opt/freesurfer7.1/.license:/usr/local/freesurfer/.license freesurfer/freesurfer:7.1.1 /bin/bash -c ',"'",'recon-all -base ', x, '_base',tp,' -all',"'",'"','| qsub -o /nethome/amiklos/QSUB_OUTPUT/test_0',x,' -e /nethome/amiklos/QSUB_OUTPUT/teste_',x)
    write(new_line4, base_path, append=TRUE)
  }
} # recon-all -base OAS2_0001 -tp OAS2_0001_MR1 -tp OAS2_0001_MR2 -all

# recon_long
long_path = '/nethome/fhassan3/recon_long.txt'
for (x in 1:nrow(data.csv)){
  i = data.csv[x,9]
  j = sprintf("%02d", data.csv[x,4])
  new_line <- paste0('echo -n "docker run -i --rm -v /nethome/fhassan3/LongFreesurfer/:/data -v /nethome/fhassan3/LongFreesurfer/OUTPUT/:/usr/local/freesurfer/subjects -v /usr/local/opt/freesurfer7.1/.license:/usr/local/freesurfer/.license freesurfer/freesurfer:7.1.1 /bin/bash -c ',"'",'recon-all -long /usr/local/freesurfer/subjects/',i,'_', j,' /usr/local/freesurfer/subjects/', i, '_base -all',"'",'"','| qsub -o /nethome/amiklos/QSUB_OUTPUT/test_0',i,'_',j,' -e /nethome/amiklos/QSUB_OUTPUT/teste_',i,'_',j)
  write(new_line, long_path, append=TRUE)
}

unique(data.csv$grid)
s <- unique(data.csv$grid)
v <- as.list(s)
for(x in v) {
  if (x < 14160) {
    tp <- ''
    for (y in 1:10){
      k = sprintf("%02d", y)
      new_line1 <- paste0(' -tp /usr/local/freesurfer/subjects/',x,'_', k)
      tp <- paste0(tp, new_line1)
    }
    new_line2 <- paste0('echo -n "docker run -i --rm -v /nethome/fhassan3/LongFreesurfer/:/data -v /nethome/fhassan3/LongFreesurfer/OUTPUT/:/usr/local/freesurfer/subjects -v /usr/local/opt/freesurfer7.1/.license:/usr/local/freesurfer/.license freesurfer/freesurfer:7.1.1 /bin/bash -c ',"'",'recon-all -base ', x, '_base',tp,' -all',"'",'"','| qsub -o /nethome/amiklos/QSUB_OUTPUT/test_0',x,' -e /nethome/amiklos/QSUB_OUTPUT/teste_',x)
    write(new_line2, base_path, append=TRUE)
  }
  else {
    tp <- ''
    for (y in 1:5){
      k = sprintf("%02d", y)
      new_line3 <- paste0(' -tp /usr/local/freesurfer/subjects/',x,'_', k)
      tp <- paste0(tp, new_line3)
    }
    new_line4 <- paste0('echo -n "docker run -i --rm -v /nethome/fhassan3/LongFreesurfer/:/data -v /nethome/fhassan3/LongFreesurfer/OUTPUT/:/usr/local/freesurfer/subjects -v /usr/local/opt/freesurfer7.1/.license:/usr/local/freesurfer/.license freesurfer/freesurfer:7.1.1 /bin/bash -c ',"'",'recon-all -base ', x, '_base',tp,' -all',"'",'"','| qsub -o /nethome/amiklos/QSUB_OUTPUT/test_0',x,' -e /nethome/amiklos/QSUB_OUTPUT/teste_',x)
    write(new_line4, base_path, append=TRUE)
  }
}
# recon-all -long OAS2_0001_MR1 OAS2_0001 -all
# echo -n "docker run -d --rm -v /nethome/fhassan3/LongFreesurfer/:/data -v /nethome/fhassan3/LongFreesurfer/OUTPUT/:/usr/local/freesurfer/subjects -v /usr/local/opt/freesurfer7.1/.license:/usr/local/freesurfer/.license freesurfer/freesurfer:7.1.1 /bin/bash -c  'recon-all -s 13900_01 -i /data/sub-13900_ses-24007_tp-01_T1w.nii.gz -T2 /data/sub-13900_ses-24007_tp-01_T2w.nii.gz -T2pial -all'" | qsub -o test0 -e teste
# echo "recon-all -s subjectname -i /path/to/input -T2 /path/to/T2_input -T2pial -all" | qsub -o subjectname_tp_output.txt -e subjectname_tp_error.txt
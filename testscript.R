data.csv <- read.csv('/nethome/fhassan3/IPERSR-MRISessionsAndName_DATA_2023-05-31_1058.csv')
head(data.csv)


# /data/BIDS/sub-13900/ses-24007/anat/sub-13900_ses-24007_run-01_T1w.nii.gz
file_from <- list()
file_to <- list()

for (x in 1:nrow(data.csv)){
  i = data.csv[x,7]
  j = data.csv[x,9]
  new_element <- paste0('/sub-',j,'/ses-',i, '/anat')
  file_from[[length(file_from) + 1]] <- new_element
}


for (y in 1:nrow(data.csv)){
  i = data.csv[y,7]
  j = data.csv[y,9]
  k = data.csv[y,4]
  new_element <- paste0('/data/BIDS/sub-',j,'/ses-',i,'/anat','/run-0',k,'/T1w.nii.gz')
  file_to[[length(file_to) + 1]] <- new_element
}

old_files <- list.files("file_to")
new_files <- list.files(paste0("/nethome/fhassan3", file_to))


file.copy(old_files, new_files)

remove("file_from")
remove("file_to")

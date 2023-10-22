#Section 1: Combine LFS into one .csv file

files <- list.files(path="C:/820WF/LFS_2017_2023", pattern='[.]csv')

files

lfs <- as.data.frame(matrix(ncol=60, nrow=1))

for (i in files){
  
  path <- paste("C:/820WF/LFS_2017_2023/",get("i"), sep="")
  temp1 <- read.csv(path)
  
  colnames(lfs) <- colnames(temp1)
  
  lfs <- rbind(lfs, temp1)
  
  print(i)
  
}  

lfs <- lfs[-1,]

write.csv(lfs, file = "C:/820WF/LFS_2017_2023/lfs_17_23.csv")


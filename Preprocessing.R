library(BSDA)
library(stringr)
library(dplyr)
library(sjmisc)
#read csv file
df<- read.csv("C:/Users/Tanya/Desktop/laptop_data.csv", header=T)
df
summary(df)

#function to separate cpu name
func <- function(x)
{
  vector1 <-c( strsplit(x,split = " "))
  vector2 <- matrix(unlist(vector1))
  n <- length(vector2)
  cpu_name <- paste(vector2[1,1],vector2[2,1],vector2[3,1])
  if (cpu_name == 'Intel Core i7' | cpu_name == 'Intel Core i5' | cpu_name == 'Intel Core i3')
  {
    cpu_name = cpu_name
  }
  else if (vector2[1,1] == 'Intel')
  {
    cpu_name ='Other Intel Processor'
  }
  else
  {
    cpu_name ='AMD Processor'
  }
  freq <- vector2[n,1]
  df$Cpu_name <- cpu_name
  df$freq <- freq
}
#to view
data.frame(lapply(df$Cpu,func))
#result <- apply(new.array, str, sum)
df_empty=data.frame(lapply(df$Cpu,func))
#reshape df
df_empty = matrix(unlist(t(df_empty)), byrow=T, 1303, 3)
#to merge two column without anything in common
data_frame_merge <- merge(df, df_empty,by = 'row.names', all = TRUE)

#function for os_type 
fun1 <- function(x)
{
  if ( x == 'Windows 10' | x == 'Windows 7' | x == 'Windows 10 S')
  {
    return('Windows')
  }
  else if(x == 'macOS' | x == 'Mac OS X')
  {
    return('Mac')
  }
  else
  {
    return('Other/No OS/Linux')
  }
}
df_with_os1$os_type <- mapply(fun1, df$OpSys)
df_new = merge(x = data_frame_merge, y = df_with_os1, by = "OpSys")

#matrix to df
#dataframe_data=as.data.frame(matrix_data)



#for memory column
fun_12 <- function(x)
{
  if(str_contains(x, "HDD", ignore.case = T))
  {
    return(1)
  }
  else
  {
    return(0)
  }
  
}
df_with_os1$mem_HDD <- mapply(fun_12, df_with_os1$Memory)


#for HDD
fun_12 <- function(x)
{
  if(str_contains(x, "HDD", ignore.case = T)  & !str_contains(x,"+",ignore.case = T))
  {
    return(gsub("([0-9]+).*$", "\\1", x))
  }
  else if(str_contains(x, "HDD", ignore.case = T)  & str_contains(x,"+",ignore.case = T))
  {
    vector1 <-c( strsplit(x,split = " "))
    vector2 <- matrix(unlist(vector1))
    return(gsub("([0-9]+).*$", "\\1", vector2[5,1]))
  }
  else
  {
    return(0)
  }
}
df_with_os1$mem_HDD <- mapply(fun_12, df_with_os1$Memory)

#for SSD
fun_12 <- function(x)
{
  if(str_contains(x, "SSD", ignore.case = T)  & !str_contains(x,"+",ignore.case = T))
  {
    return(gsub("([0-9]+).*$", "\\1", x))
  }
  else if(str_contains(x, "SSD", ignore.case = T)  & str_contains(x,"+",ignore.case = T))
  {
    vector1 <-c( strsplit(x,split = " "))
    vector2 <- matrix(unlist(vector1))
    return(gsub("([0-9]+).*$", "\\1", vector2[1,1]))
  }
  else
  {
    return(0)
  } 
}
df_with_os1$mem_SSD <- mapply(fun_12, df_with_os1$Memory)

#for Flash Storage
fun_12 <- function(x)
{
  if(str_contains(x, "Flash Storage", ignore.case = T))
  {
    return(gsub("([0-9]+).*$", "\\1", x))
  }
  else
  {
    return(0)
  }
  
}
df_with_os1$mem_flash <- mapply(fun_12, df_with_os1$Memory)

#for memory with bytes
fun_123 <- function(x)
{
  if(x == 1)
  {
    return(1000)
  }
  else if(x == 2)
  {
    return(2000)
  }else
  {
    return(x)
  }
}
df_with_os1$mem_SSD <- mapply(fun_123, df_with_os1$mem_SSD)

#for touch screen
fun_1234 <- function(x)
{
  if(str_contains(x,"Touchscreen", ignore.case = T))
  {
    return (1)
  }
  else
  {
    return(0)
  }
}
df_with_os1$Touch <- mapply(fun_1234, df_with_os1$ScreenResolution)

#for resolution
fun_12345 <- function(x)
{
  vector1 <-c( strsplit(x,split = " "))
  vector2 <- matrix(unlist(vector1))
  n <- length(vector2)
  vector3 <-c( strsplit(vector2[n,1],split = "x"))
  vector4 <- matrix(unlist(vector3))
  print(vector4)
  return(vector4[1,1])	
}
df_with_os1$x_res <- mapply(fun_12345, df_with_os1$ScreenResolution)

#for y res
fun_123456 <- function(x)
{
  vector1 <-c( strsplit(x,split = " "))
  vector2 <- matrix(unlist(vector1))
  n <- length(vector2)
  vector3 <-c( strsplit(vector2[n,1],split = "x"))
  vector4 <- matrix(unlist(vector3))
  print(vector4)
  return(vector4[2,1])	
}
df_with_os1$y_res <- mapply(fun_123456, df_with_os1$ScreenResolution)

#to copy df
#write_xlsx(df_with_os1,"C:\\Users\\Tanya\\Desktop\\laptop_data.xlsx")

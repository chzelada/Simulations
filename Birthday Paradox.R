

check_pair <- function(vec){
  n<-length(vec)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      if( vec[i]==vec[j]){
        return(TRUE)
      } 
    }
  }
return(FALSE)
}



prob_same_dob <- function(group_size=23,nsim=10){
  x<-0
  for(step in 1:nsim){
    group <- sample(1:365,size = group_size,replace = TRUE )
    if(check_pair(group)) x <-x+1
  }
  return(x/nsim)
}

out <- c()
for(i in 2:100){
  prob_i <- prob_same_dob(group_size = i,nsim=5000)
  out <- c(out,prob_i)
}

plot(2:100,out,type='l',
     col='red', 
     xlab = "Group size",
     ylab = "Probabilidad",
     main = "Birthday Paradox")

x <- 13
y <- prob_same_dob(group_size = x,nsim=5000)
points(x,y, pch = 18 , col="blue")
text(x,y, paste0("(",x,", ",y,")"),pos = 2)


x <- 23
y <- prob_same_dob(group_size = x,nsim=5000)
points(x,y, pch = 18 , col="blue")
text(x,y, paste0("(",x,", ",y,")"),pos = 2)

x <- 30
y <- prob_same_dob(group_size = x,nsim=5000)
points(x,y, pch = 18 , col="blue")
text(x,y, paste0("(",x,", ",y,")"),pos = 2)
x <- 41
y <- prob_same_dob(group_size = x,nsim=5000)
points(x,y, pch = 18 , col="blue")
text(x,y, paste0("(",x,", ",y,")"),pos = 2)

x <- 51
y <- prob_same_dob(group_size = x,nsim=5000)
points(x,y, pch = 18 , col="blue")
text(x,y, paste0("(",x,", ",y,")"),pos = 1)

x <- 70
y <- prob_same_dob(group_size = x,nsim=5000)
points(x,y, pch = 18 , col="blue")
text(x,y, paste0("(",x,", ",y,")"),pos = 1)

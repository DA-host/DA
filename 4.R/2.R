
rainfall <- c(56,78,65,120,140,160,180,150,110,90,70,60)
sum(rainfall)
avg <- mean(rainfall)
print(avg)
month <-month.name
month[which.max(rainfall)]
month[which.min(rainfall)]
sum(rainfall > avg)

data.frame(Month=month,Rainfall=rainfall)

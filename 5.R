
library(psych)
avg_rainfall = data.frame(month = month.name , rainfall = c(10,10,10,10,10,560,
640,520,320,90,20,10))
print(avg_rainfall)
#mean
mean(avg_rainfall$rainfall)
#median
median(avg_rainfall$rainfall)
#mode
get_mode <- function(x) {
 uniq_vals <- unique(x)
 uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}
get_mode(avg_rainfall$rainfall)
#geometricmean
geometric.mean(avg_rainfall$rainfall)
#harmonicmean
harmonic.mean(avg_rainfall$rainfall)
#1stquartile
quantile(avg_rainfall$rainfall,0.25)
#56quartile

quantile(avg_rainfall$rainfall,0.56)

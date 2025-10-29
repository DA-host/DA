
transactions <- c(-5000, -3000, -2000, 8000, 12000, -1000, 15000)
for (t in transactions) {
 if (t > 0) print("Deposit") else print("Withdrawal")
}
ifelse(abs(transactions) > 10000, "Large", "Small")
i <- 1
while (i <= length(transactions) && transactions[i] < 0) {
 print(paste("Withdrawal", i, ":", transactions[i]))
 i <- i + 1
}
total <- 0; i <- 1
repeat {
 if (i > length(transactions) || total > 50000) break
 total <- total + transactions[i]
 i <- i + 1
}

total


employees <- data.frame(
 Name = c("A", "B", "C", "D", "E"),
 Age = c(30, 25, 29, 35, 32),
 Department = c("HR", "Finance", "IT", "HR", "IT"),
 Basic = c(40000, 50000, 45000, 47000, 66000),
 Bonus = c(5000, 6000, 5500, 5000, 6500)
)
# Total Salary
employees$Total <- employees$Basic + employees$Bonus
# Highest Paid
employees[which.max(employees$Total), ]
# Avg salary by dept
aggregate(Total ~ Department, data = employees, FUN = mean)
# Salary > 60000

subset(employees, Total > 60000)

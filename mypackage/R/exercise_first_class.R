print("Hello Word");
prices <- c( 6, 8, 6, 8, 6, 8, 12, 6, 8, 8, 6, 8, 8, 8, 12, 12, 8, 8, 12, 6, 8, 6, 6, 8, 12, 6, 6, 6, 6, 6)
prices_table <- table(prices)
print(prices_table)
prop_table <- prop.table(prices)
print(prop_table)
cumsum_table <- cumsum(prices_table)
print(cumsum_table)

students <- c(1500, 750, 1000, 500, 250)
names <- c("Chemistry", "Mathematics", "Physics", "Biology", "Geology")
color <- c("red", "blue", "green", "orange", "purple")
barplot(students, names.arg = names, col = color)
pie(students, names, col = color)

data <- c(7,11,11,8,12,7,6,6)
mean_data <- mean(data)
print(mean_data)

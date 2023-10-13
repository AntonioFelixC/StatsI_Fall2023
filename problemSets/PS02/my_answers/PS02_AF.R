#Problem Set 2
x <- c(14,6,7)
y <- c(7,7,1)
# correlation 
r <- cor(x,y)
n <- length(x)
t_stat <- (r*sqrt(n-2)/sqrt(1-r^2))
print(t_stat)

df <- n-2
p_value <- 2*(1-pt(abs(t_stat),df))
print(df)
print(p_value)

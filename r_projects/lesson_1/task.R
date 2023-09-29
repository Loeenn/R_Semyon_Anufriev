print("Задание 1:")
a <- c("string", "str", "String")
print(a)
b <- 1:7
print(b)
print("Задание 2:")
print(length(b))
print(b[1])
print(b[2:4])
print("Задание 3")
s1 <- c(1,2,3)
s2 <- c(4,5,6)
print(s1+s2)
print("Задание 4")
print(mean(s2))
print("Задание 5")
func <- function(x) {
  return(x / 2)
}
print(func(s2))
print("Задание c матрицей 1")

m1 <- matrix(c(1, 2, 3,
              4, 5, 6,
              7, 8, 9), nrow = 3, ncol = 3)
m2 <- cbind(seq(1,3), seq(4, 6),
             seq(7, 9))

print(m2)
print("Задание c матрицей 2")
print(m2[1,])
print(m2[,2])
print("Задание c матрицей 3")
print(m1[(ncol(m2) + 1) / 2, (nrow(m2) + 1) / 2])

print("Задание c датафреймом 1")
df <- read.csv("/Users/senya/Downloads/ДЗ2_vgsales.csv")
print(head(df))
print("Задание c датафреймом 2")
print(str(df))
print(summary(df))

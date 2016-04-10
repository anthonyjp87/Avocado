foo <- read.csv("foo.csv")
r <- data.table(foo)

print("is na")
print(sapply(r,function(x) sum(is.na(x))))

print("unique")
print(sapply(r, function(x) length(unique(x))))

N16 <- subset(foo, r$postcode=='N16')
PVC <- subset(foo, foo$postviewconversions==1)
# missmap(r,main="missing data")
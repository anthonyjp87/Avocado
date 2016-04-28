#Enter Data into Datatable
# foo <- read.csv("foo.csv")
r <- data.table(foo)

#Clear the empty shit: 
r[,c('accountid','campaignid','bidderid','clientid','externaltagid',' exchangeid','brandid','insertionorderid','inventoryid','tacticflightid','insertionorderflightid','currency','expansions','totalcost','advertiserrevenue','dealid'):=NULL]

#Rename the items in this long stupid thing to easier names:
# unique_item<-unique(r$creativeid)
#   for(i in 1:length(unique_item)){
#     r[creativeid==unique_item[i],creativeid:=as.character(i)]
# }


print("is na")
print(sapply(r,function(x) sum(is.na(x))))

print("unique")
print(sapply(r, function(x) length(unique(x))))


#1: To clense Data: 
#binomial regressions: log link function 

two_break <- function(test_hr,b1,b2){

  test_hr<-ifelse(test_hr==1,1.1,test_hr)
  for(i in 0:6){
    test_hr<-ifelse(test_hr> (b1+24*i) & test_hr< (b2+24*i), 1 ,test_hr)
  }
  test_hr<-ifelse(test_hr==1,1,0)
  return(test_hr)
}

multi_divide <- function(test_hr,b1,b2,b3){
  test_hr<-ifelse(test_hr==1,1.1,test_hr)
  test_hr<-ifelse(test_hr==2,2.1,test_hr)
  
  for(i in 0:6){
    test_hr<-ifelse(test_hr> (b1+24*i) & test_hr<= (b2+24*i), 1 ,test_hr)
    test_hr<-ifelse(test_hr> (b2+24*i) & test_hr<= (b3+24*i), 2 ,test_hr)
  }
  test_hr<-ifelse(test_hr!=1 & test_hr!=2,0,test_hr)
  return(test_hr)
}

d_n <- two_break(r$hourofweek,7,18)
t_14_17 <- two_break(r$hourofweek,14,17)
m_d <- multi_divide(r$hourofweek,6,13,20)


#2 Set up Logistic Regression of input variables: 
fit.how <- glm(r$postviewconversions~r$hourofweek, family=binomial)
display(fit.how)

fit.1 <- glm(r$postviewconversions~d_n, family=binomial)
display(fit.1)
# plot(jitter(r$postviewconversions, factor=.5), jitter(d_n, factor=.5), pch=20)
# curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), add=TRUE)
fit.14_17 <- glm(r$postviewconversions~t_14_17, family=binomial)
display(fit.14_17)

fit.2 <- glm(r$postviewconversions~d_n + m_d, family=binomial)
display(fit.2)

fit.3 <- glm(r$postviewconversions~d_n + m_d + d_n:m_d, family=binomial)
display(fit.3)

#Build iterator that increases the hour each time, builds the vector, regresses the vector on postviewconversions, and tracks coefficients
fit.it.coef<-rep(0,24)
fit.it.coef1<-rep(0,24)
for(n in 1:24){
  spec_hour <- two_break(r$hourofweek,n-1,n+1)
  fit.it <- glm(r$postviewconversions~spec_hour, family=binomial)
  fit.it.coef[n]<-fit.it$coef[1]
  fit.it.coef1[n]<-fit.it$coef[2]
}
plot(fit.it.coef1,xlab = "Hour of Day", ylab="log reg coef")



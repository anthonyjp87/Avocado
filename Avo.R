foo <- read.csv("foo.csv")
r <- data.table(foo)

#Clear the empty shit: 
r[,exchangeid:=NULL]


print("is na")
print(sapply(r,function(x) sum(is.na(x))))

print("unique")
print(sapply(r, function(x) length(unique(x))))

#1: To clense Data: 
#binomial regressions: log link function 
#hour fo week==int identifying the hour of the week based on 168 hour time. interesting.. Could be necessary
#to create model determining morning/evening hours? Also--must be linked with time zone
#time of week: Create new vector that divides the day into 2 periods, Day/Night (and expand to 3/4 etc.)

day_night <- function(x,s,e){
  test_hr<-x
  test_hr<-ifelse(test_hr==1,1.1,test_hr)
  for(i in 0:6){
    test_hr<-ifelse(test_hr> (s+24*i) & test_hr< (e+24*i), 1 ,test_hr)
  }
  test_hr<-ifelse(test_hr==1,1,0)
  return(test_hr)
}

d_n <- day_night(r$hourofweek,7,18)




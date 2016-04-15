#Enter Data into Datatable
foo <- read.csv("foo.csv")
r <- data.table(foo)

#Clear the empty shit: 
r[,c('accountid','campaignid','bidderid','clientid','exchangeid','brandid','insertionorderid','inventoryid','tacticflightid','insertionorderflightid','currency','expansions','totalcost','advertiserrevenue','dealid'):=NULL]

#Categorize Creative ID into Readable Segments: 
cid2<-as.character(r$creativeid)
cid <- replace(rup, rup==c('565d7d09507048f61cee0115', '565d7d0a507048f61cee0116'),c('a','b'))


#Rename the items in this long stupid thing to easier names:
unique_item<-unique(r$creativeid)
  for(i in 1:length(unique_item)){
    r[creativeid==unique_item[i],creativeid:=as.character(i)]
  }


print("is na")
print(sapply(r,function(x) sum(is.na(x))))

print("unique")
print(sapply(r, function(x) length(unique(x))))


#1: To clense Data: 
#binomial regressions: log link function 

day_night <- function(test_hr,b1,b2){

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



d_n <- day_night(r$hourofweek,7,18)

m_d <- multi_divide(r$hourofweek,6,13,20)

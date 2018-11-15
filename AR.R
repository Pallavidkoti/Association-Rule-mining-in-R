library(data.table)
library(arules)
library(DMwR)
library("ROSE")

train = fread('train_transactional_data_with_addr_emp_2018.csv',stringsAsFactors = T)
test = fread('test_transactional_data_apr_new_dates.csv',stringsAsFactors = T)



train$rto_flag = as.factor(train$rto_flag)
test$rto_flag = as.factor(test$rto_flag)



Features = c('cdstscrcd','cmloccode',
             'segname','cname','destination_region','regionname',
             'ccustname','company_group_name','day','cstattype',
             'officename','Category','Sub- Category',
             'gender','brnd_catg',
             'rto_flag')


train = subset(train, select=Features)
test = subset(test, select=Features)



train <- rename(train,sub_category='Sub- Category')
#data_balanced_over <- ovun.sample(rto_flag ~ ., data = train,  method = "over", p=0.5, seed = 1)$data

ActualLabels = test$rto_flag
test = subset(test, select=-c(rto_flag))


trans = as(train, "transactions")
trans.test = as(test, "transactions")

rules <- apriori(trans,  
                 parameter=list(support =0.001, confidence =0.5,target='rules',maxlen =100), 
                 appearance = list(rhs=c("rto_flag=0", "rto_flag=1"), 
                                   default="lhs"))


#top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support")
#inspect(head(top.support, 10))
# 
# PredictionDataframe=data.frame(Actual=numeric(0),Prediction=numeric(0))
# 
# for(i in 1:nrow(test)){
#   basket <- trans.test[i]
#   rulesMatchLHS <- is.subset(rules@lhs,basket)
#   suitableRules <-  rulesMatchLHS & !(is.subset(rules@rhs,basket))
# 
#   recommendations <- strsplit(LIST(rules[as.logical(suitableRules)]@rhs)[[1]],split=" ")
#   recommendations <- lapply(recommendations,function(x){paste(x,collapse=" ")})
#   recommendations <- as.character(recommendations)
# 
#   recommendations <- recommendations[!sapply(recommendations,function(x){basket %in% x})]
# 
#   print(recommendations)
# 
#   Actual = ActualLabels[i]
#   Prediction = recommendations
#   print(i)
# 
#   PredictionDataframe=rbind(PredictionDataframe,data.frame(Actual,Prediction))
# }


reco <- function(rules, newTrans){
  rules.sorted <- sort(rules, by="confidence")
  rhs_labels <- unlist(as(rhs(rules.sorted), "list"))
  
  matches <- is.subset(lhs(rules.sorted), newTrans) &
    !(is.subset(rhs(rules.sorted), newTrans))
  
  #print(matches@p)
  result = apply(matches, MARGIN = 2, FUN = function(x) unique(rhs_labels[x]))
  
  if(identical(result, character(0))){
    finalResult = NA
  }else{
    
    if(length(result)>1){
      finalResult= result[1]
    }else{
      finalResult = result
    }
  }
  return(finalResult)
}

#Pred= reco(rules, trans.test)

PredictionDataframe=data.frame(Actual=numeric(0),Prediction=numeric(0))
for(i in 1:nrow(test)){
  Pred= reco(rules, trans.test[i])
  Actual = ActualLabels[i]
  Prediction = Pred
  print(paste0('The Test rows is ', i))
  print(paste0('Actual Label is ',Actual))
  print(paste0('Prediction  is ',Prediction))
  print('__________________________________________')
  PredictionDataframe=rbind(PredictionDataframe,data.frame(Actual,Prediction))
}

write.csv(PredictionDataframe,"PredictionDataframe_apr_new_dates_one_week_without_balancing.csv",row.names=F)

pred = fread('/home/pkoti/PredictionDataframe_apr_new_dates_one_week.csv',stringsAsFactors = T)

tbl <- table(pred$Predicted,pred$Actual,dnn=list('predicted','actual'))
confusionMatrix(tbl,positive='1')

# a = lapply(Pred, function(x) if(identical(x, character(0))) NA_character_ else x)
# df <- data.frame(matrix(unlist(a), byrow=T))
# df$Actual = ActualLabels



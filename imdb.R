##Pre-Processing##
#reading in IMDB Dataset#
imdb.df<-read.csv("IMDB Movie Dataset.csv")
#removing NAs from numeric variables replacing with median# 
imdb.df$num_critic_for_reviews[is.na(imdb.df$num_critic_for_reviews)]<-median(imdb.df$num_critic_for_reviews,na.rm=TRUE)
imdb.df$duration[is.na(imdb.df$duration)]<-median(imdb.df$duration,na.rm=TRUE)
imdb.df$director_facebook_likes[is.na(imdb.df$director_facebook_likes)]<-median(imdb.df$director_facebook_likes,na.rm=TRUE)
imdb.df$actor_3_facebook_likes[is.na(imdb.df$actor_3_facebook_likes)]<-median(imdb.df$actor_3_facebook_likes,na.rm=TRUE)
imdb.df$actor_1_facebook_likes[is.na(imdb.df$actor_1_facebook_likes)]<-median(imdb.df$actor_1_facebook_likes,na.rm=TRUE)
imdb.df$gross[is.na(imdb.df$gross)]<-median(imdb.df$gross,na.rm=TRUE)
imdb.df$facenumber_in_poster[is.na(imdb.df$facenumber_in_poster)]<-median(imdb.df$facenumber_in_poster,na.rm=TRUE)
imdb.df$num_user_for_reviews[is.na(imdb.df$num_user_for_reviews)]<-median(imdb.df$num_user_for_reviews,na.rm=TRUE)
imdb.df$budget[is.na(imdb.df$budget)]<-median(imdb.df$budget,na.rm=TRUE)
imdb.df$title_year[is.na(imdb.df$title_year)]<-median(imdb.df$title_year,na.rm=TRUE)
imdb.df$actor_2_facebook_likes[is.na(imdb.df$actor_2_facebook_likes)]<-median(imdb.df$actor_2_facebook_likes,na.rm=TRUE)
imdb.df$aspect_ratio[is.na(imdb.df$aspect_ratio)]<-median(imdb.df$aspect_ratio,na.rm=TRUE)
#removing not needed categorical variables# 
imdb.df<-imdb.df[-c(2,7,10:12,15,17,18,23)]
imdb.df
#Dummy for 4 remaining categorical variables# (my version doing all 4 in one line)
dummies <- model.matrix(~ 0 + ï..color + language + country+ content_rating, data=imdb.df)
dummies <- as.data.frame(dummies)
dummies <- dummies[, -1]
t(t(names(dummies)))
imdb.df<-cbind(imdb.df[,-c(1,12:14)],dummies)
t(t(names(imdb.df)))

#train and valid#
set.seed(1)
train.rows<-sample(nrow(imdb.df),nrow(imdb.df)*0.6)
imdb.train<-imdb.df[train.rows,]
imdb.valid<-imdb.df[-train.rows,]

##Simple Linear Models Run## 

lm_mv_fblike_score <- lm(imdb.df$movie_facebook_likes ~ imdb.df$imdb_score)
sum.mvfblike.lm<-summary(lm_mv_fblike_score)
sum.mvfblike.lm$adj.r.squared
AIC(lm_mv_fblike_score)

plot(imdb.df$movie_facebook_likes ~ imdb.df$imdb_score, xlab = "FB_Likes", ylab = "IMDB Score")
abline(lm_mv_fblike_score, col = "red")

lm_dir_fblike_score <- lm(imdb.df$director_facebook_likes ~ imdb.df$imdb_score)
sum.dirfblike.lm<-summary(lm_dir_fblike_score)
sum.dirfblike.lm$adj.r.squared
AIC(lm_dir_fblike_score)

plot(imdb.df$director_facebook_likes ~ imdb.df$imdb_score, xlab = "Director FB_Likes", ylab = "IMDB Score")
abline(lm_dir_fblike_score, col = "red")

lm_critic_Score <- lm(imdb.df$num_critic_for_reviews ~ imdb.df$imdb_score)
sum.critic.lm<-summary(lm_critic_Score)
sum.critic.lm$adj.r.squared
AIC(lm_critic_Score)

plot(imdb.df$num_critic_for_reviews ~ imdb.df$imdb_score, xlab = "Face_in_poster", ylab = "IMDB Score")
abline(lm_critic_Score, col = "red")


##Multiple Linear Regression with all variables##
library(forecast)
imdb.mlm <- lm(imdb_score ~ ., data = imdb.train)
options(scipen = 999)
summary(imdb.mlm)
valid.mlm.pred <- predict(imdb.mlm, newdata=imdb.valid)
options(scipen = 999, digits = 1)
valid.mlm.resid<- imdb.valid$imdb_score - valid.mlm.pred
accuracy(valid.mlm.pred, imdb.valid$imdb_score)
AIC(imdb.mlm)
#New MLM with removed insignificant predictors#
imdb.mlm.new<-lm(imdb_score ~ . -languageGreek-languageHungarian-languageMongolian-languagePanjabi-languageRomanian-languageSwahili-languageTamil-languageVietnamese-countryBahamas-countryCambodia-countryColumbia-countryGeorgia-countryGreece-countryHungary-countryIceland-countryIndonesia-countryIsrael-countryKenya-countryNetherlands-countryPakistan-countryPanama-countryPhilippines-countryPoland-countrySlovenia-countrySweden-countryThailand-countryTurkey-'countryWest Germany'-'imdb.df$countent_ratingTV-Y7',data=imdb.train)
summary(imdb.mlm.new)
valid.mlm.new.pred <- predict(imdb.mlm, imdb.valid)
options(scipen = 999, digits = 1)
valid.mlm.new.resid<- imdb.valid$imdb_score - valid.mlm.new.pred
accuracy(valid.mlm.new.pred, imdb.valid$imdb_score)

##Exhaustive Search Code## 
library(leaps)
imdb.search<-regsubsets(imdb_score~.,data=imdb.train,nbest=1,nvmax=ncol(imdb.train))
sum<-summary(imdb.search)
sum$which
par(mfrow=c(1,1))
plot(imdb.search,scale="Cp")
t(t(sum$cp))

##Forward/Backward/Step## (havent ran need to pick 1)
imdb.null<-lm(imdb_score~1,data=imdb.train)
imdb.full<-lm(imdb_score~.,data=imdb.train)

#Forward#
imdb.fwd<-step(imdb.null,scope=list(lower=imdb.null,upper=imdb.full),direction="forward")
summary(imdb.fwd)
AIC(imdb.fwd)
pred.imdb.fwd<-predict(imdb.fwd,newdata=imdb.valid)
accuracy(pred.imdb.fwd,imdb.valid$imdb_score)

#Backward#
imdb.back<-step(imdb.full,direction="backward")
summary(imdb.back)
AIC(imdb.back)
pred.imdb.back<-predict(imdb.back,newdata=imdb.valid)
accuracy(pred.imdb.back,imdb.valid$imdb_score)

#StepWise#
imdb.step<-step(imdb.null,scope=list(lower=imdb.null,upper=imdb.full),direction="both")
summary(imdb.step)

#Performance on Validation Data of Simple Linear Regression
library(PerformanceAnalytics)
imdb.perf <- data.frame(imdb.df$imdb_score, imdb.df$num_critic_for_reviews, imdb.df$director_facebook_likes, imdb.df$movie_facebook_likes)
imdb.perf
pairs(imdb.perf, pch = 19)
round(cor(imdb.perf), digits = 3)
chart.Correlation(R = imdb.perf, histogram = TRUE, pch = 19)


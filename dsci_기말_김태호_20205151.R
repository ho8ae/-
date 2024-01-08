library("tidyverse")
library("dplyr")
library("ggplot2")



#게임산업의 대륙별 수입과 어떤 업종이 대륙별 수입에 영향을 끼쳤는지 알아보기
#대륙별 수입에는 더 좋은 학력일수록 상관관계가 있을까?


#파일 읽기
game <- read_csv("게임산업__학력별_업종별_종사자_현황.csv",locale=locale("ko",encoding="EUC-KR"),col_names=TRUE)

store <- read_csv("대륙별_수입액_현황.csv",locale=locale("ko",encoding="EUC-KR"),col_names=TRUE)

programer <- read_csv("게임산업_직무별_연도별_종사자_현황.csv",locale=locale("ko",encoding="EUC-KR"),col_names=TRUE)


game
store
programer

view(game)
view(store)
view(programer)

#데이터 확인
str(store)
str(game)
str(programer)

#데이터 모든 열을 숫자형으로 변환하기
#char 형태의 열들이 있는데 하나로 통일하는게 데이터를 관리하기 편하다.

game <- game %>% mutate_all(as.numeric)
store <- store %>% mutate_all(as.numeric)
programer <- programer %>% mutate_all(as.numeric)

#이상값,결측값 확인하기
#boxplot으로는 이상값과 결츠값을 판단하기 어려움.
boxplot(game)
boxplot(store)
boxplot(programer)
#연도별로 수치가 눈에 뛰게 달라질 수 있기 때문에 그래프로 확인 



#programer 데이터 가공
programer
df_long_pro <- programer %>%
  gather(key = "직무별", value = "인원", -시점)

#파이그래프
df_long_pro %>%
  filter(직무별 != "합계") %>%
  ggplot(aes(x = "", y = 인원, fill = 직무별)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal()

#막대그래프
df_long_pro %>%
  filter(직무별 != "합계") %>%
  ggplot(aes(x = 직무별, y = 인원, fill = 직무별)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "none")

#그래프를 통해서 개발자들이 
#많은 걸 볼 수 있다.


# store 그래프 그리기
#데이터 가공
df_long_store <- store %>%
  gather(key = "나라", value = "수입액", -시점)
 
 ggplot(df_long_store, aes(x = 시점, y = 수입액, color = 나라, linetype = 나라)) +
   geom_line(size = 1) +
   geom_point(size = 3) +
   labs(x = "시점", y = "수입액") +
   theme_minimal()
 #끊겨있는 그래프를 확인할 수 있음

  #game 그래프 그리기
 #데이터 가공
 
 game
 
 df_long_game <- game %>%
   gather(key = "업종_학력", value = "인원", -시점)
 
 ggplot(df_long_game, aes(x = 시점, y = 인원, color = 업종_학력, linetype = 업종_학력)) +
   geom_line(size = 1) +
   geom_point(size = 3) +
   labs(x = "시점", y = "인원 수") +
   theme_minimal()+
   facet_wrap(~업종_학력)
 
 

 
#그래프를 확인해볼 수 있다 !
 
 
#na값이 있는 이유 알아보기 
 table(is.na(game))
 table(is.na(store))
 table(is.na(programer))

 
 
 #게임산업의 대륙별 수입과 어떤 업종이 대륙별 수입에 영향 또한 인원수가 많을수록 유의미한 관계인지 확인도 할 수 있게 순위를 파악한다.
 
 game%>%
   slice(1:11) %>%
   select(contains("합계")) %>%
   summarise_all(sum, na.rm = TRUE) %>%
   gather(key = "열", value = "합계") %>%
   arrange(desc(합계))
 
 

 #현재 내가 관심있는 pc게임과 모바일게임이 대륙별 매출 수입액과 관계가 있는지 알고싶고 
#종사자인원수에 따라 관계가 있는지 확인하기위해
 #아케이드게임까지 데이터를 확인할 수 있게 따로 만들었다.
 


 First_Data <- data.frame(c(store[,1],store[,2],game[,2], game[,27],game[,12], game[,7]),game[,22]) 
 

#헷갈리지 않게 열 이름 변경
names(First_Data)[2] <- "수입액"
First_Data

#모델 만들고 분석 단순 선형회귀 분석 결과
#첫번째 온라인
First_model_1 = lm(수입액~온라인게임_합계,data=First_Data)
plot(First_model_1,col="blue")
abline(First_model_1)

summary(First_model_1)

#두번째 모바일
First_model_2 = lm(수입액~모바일게임_합계,data=First_Data)
plot(First_model_2,col="blue")
abline(First_model_2)

summary(First_model_2)

#세번째 pc게임
First_model_3 = lm(수입액~PC게임_합계,data=First_Data)
plot(First_model_3,col="blue")
abline(First_model_3)

summary(First_model_3)

#네번째 아케이드게임


First_model_4 = lm(수입액~아케이드게임_합계,data=First_Data)
plot(First_model_4,col="blue")
abline(First_model_4)


summary(First_model_4)





#세번째 pc게임의 결과가 유의미한 결과를 
#R-squared가 높다.
 
# 다중 선형 회귀 분석을 해보기
First_model_multi = lm(수입액 ~ 아케이드게임_합계 + 모바일게임_합계 + PC게임_합계, data = First_Data)

summary(First_model_multi)


#가장 유의미한 결과를 보여준 Pc게임에서 학력수준은 상관관계가 있을까?
Second_Data <- data.frame(c(store[,1],store[,2],game[,7],game[,8],game[,9],game[,10],game[,11]))
names(Second_Data)[2] <- "수입액"

Second_Data

"
Second_Model<- lm(수입액 ~ Second_Data[,3], data = Second_Data)
summary(Second_Model)
"

rapid_Model = function(x){

    Second_Model<- lm(수입액 ~ Second_Data[,x+2], data = Second_Data)
    return(summary(Second_Model))
}
#순서 PC게임_합계	PC게임_고졸이하	PC게임_초대졸	PC게임_대졸	PC게임_대학원졸 이상 순서

i=1
while(i<=5){
 print(paste(i,"번째"))
  print(rapid_Model(i))
  
  i=i+1
}

#p-value가 가장 작은 변수가 가장 유의미한 기여를 한 변수. 따라서, PC게임_대졸 (x = 4)가 가장 유의미한 기여를 한 변수. 이는 PC게임_대졸이 수입액에 가장 강한 영향을 미친다는 것을 의미.

Second_Data


Second_Model<- lm(수입액 ~ PC게임_대졸, data = Second_Data)
summary(Second_Model)
#회귀식:
#수입액 = 172,500 + 12.78 * PC게임_대졸

#예측! 성공!
new_data <- data.frame(PC게임_대졸 =17805)
predicted_income <- predict(Second_Model, newdata = new_data)
predicted_income


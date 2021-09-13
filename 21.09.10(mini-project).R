install.packages("foreign") 

library(foreign) 
library(readxl)
library(dplyr)
library(ggplot2)

welfare <- read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
read_excel("Koweps_Codebook.xlsx")
job_code <- read_excel("Koweps_Codebook.xlsx", sheet = 2)


welfare <- rename(welfare, 
                  sex = h10_g3, # 성별
                  birth = h10_g4, # 태어난 연도
                  marriage = h10_g10, # 혼인 상태
                  religion = h10_g11, # 종교
                  income = p1002_8aq1, # 월급
                  code_job = h10_eco9, # 직종 코드
                  code_region = h10_reg7) # 지역 코드


class(welfare$birth)
summary(welfare$birth)#이상치 확인
table(is.na(welfare$birth))#결측치 확인
qplot(welfare$birth)

welfare$age <- 2015 - welfare$birth + 1 #age열 생성 기준연도 : 2015

summary(welfare$age)#이상치 확인
qplot(welfare$age)


class(welfare$sex)
table(welfare$sex)
table(is.na(welfare$sex))#결측치 확인

welfare$sex <- ifelse(welfare$sex == 1, 'Male','Female')
table(welfare$sex)
qplot(welfare$sex)


class(welfare$income)
summary(welfare$income)#이상치 확인
table(is.na(welfare$income))#결측치 확인
qplot(welfare$income)

welfare$income <- ifelse(welfare$income %in% c(0,9999), NA, welfare$income)
table(is.na(welfare$income))


sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(income_mean=mean(income))
sex_income

ggplot(data = sex_income, aes(x=sex, y=income_mean)) +geom_col()


age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age)%>%
  summarise(income_mean = mean(income))
age_income

ggplot(data = age_income, aes(x=age, y=income_mean)) +geom_line()



welfare <- welfare %>%
  mutate(age_group = ifelse(age < 10, "kid",
         ifelse(age < 20, "10대",
                ifelse(age<30, "20대",
                  ifelse(age < 40, "30대",
                       ifelse(age < 50, "40대",
                              ifelse(age < 60, "50대",
                                     ifelse(age < 70 ,"60대",
                                            ifelse(age < 80, "70대",
                                                   ifelse(age <90, "80대", "old"))))))))))

table(welfare$age_group)

age_group_income <- welfare%>%
  filter(!is.na(income))%>%
  group_by(age_group)%>%
  summarise(income_mean = mean(income))
age_group_income

ggplot(data = age_group_income, aes(x = age_group, y=income_mean)) + geom_col()



class(welfare$marriage)
summary(welfare$marriage)

welfare$marriage <- ifelse(welfare$marriage == 0, '비해당',
                           ifelse(welfare$marriage == 1, '유배우',
                                  ifelse(welfare$marriage == 2,'사별',
                                         ifelse(welfare$marriage == 3,'이혼',
                                                ifelse(welfare$marriage == 4,'별거',
                                                       ifelse(welfare$marriage == 5,'미혼','기타'))))))

str(welfare$marriage)

marriage_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(marriage)%>%
  summarise(income_mean = mean(income))
marriage_income

ggplot(data = marriage_income, aes(x = marriage, y = income_mean)) +geom_col()

class(welfare$religion)
summary(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1, '있음','없음')

str(welfare$religion)
table(welfare$religion)

religion_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(religion)%>%
  summarise(income_mean = mean(income))
religion_income

ggplot(data = religion_income, aes(x = religion, y = income_mean)) +geom_col()

summary(welfare$code_region)
table(welfare$code_region)


welfare$code_region <- ifelse(welfare$code_region == 1, '서울',
                              ifelse(welfare$code_region == 2, '수도권',
                                     ifelse(welfare$code_region == 3, '경남',
                                            ifelse(welfare$code_region == 4, '경북',
                                                   ifelse(welfare$code_region == 5, '충남',
                                                          ifelse(welfare$code_region == 6,'강원/충북',
                                                                 '광주/전남/전북/제주도'))))))

region_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(code_region)%>%
  summarise(age_mean = mean(age))
region_age

ggplot(data = region_age, aes(x=code_region, y=age_mean)) + geom_col()







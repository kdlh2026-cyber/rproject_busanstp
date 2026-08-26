# ------ 데이터 준비 ------
setwd('C:/R/teamproject')
getwd()
Rfile <- read.csv('C:/final project2/부산환경공단_부산_하수처리장_일별_방류수_수질정보_20251231_수정.csv',
                  header = T,
                  na.strings = c("", " ", "NA", "N/A", "NULL"))

STP.file <- Rfile[complete.cases(Rfile), ]  #결측값 제거

# 오타로 문자열 취급받는 컬럼을 강제로 숫자 변환
num_cols <- c("BOD", "COD", "부유물질", "총질소", "총인", "수소이온농도")
STP.file[num_cols] <- lapply(STP.file[num_cols], function(x) as.numeric(as.character(x)))

# 변환 실패(오타) 몇 개인지 확인
sapply(STP.file[num_cols], function(x) sum(is.na(x)))

# 변환 실패로 생긴 NA 행 제거
STP.file <- STP.file[complete.cases(STP.file[num_cols]), ]

head(STP.file)
str(STP.file)
sum(is.na(STP.file))

library(ggplot2)
STP.file$날짜 <- as.Date(STP.file$날짜)

# ------ 원형그래프 ------
tmp.c<-table(STP.file$사업단.소.)

pie(tmp.c,
    main='부산시 내 하수처리에 대한 각 사업단 점유율',
    col=c('#F0F8FF','#E4F5FF','#CEF3FF','#B5F3FF','#A2EFFF',
          '#8BEEFF','#6FEAFF','#56EBFF','#40D6FF','#24DAFF',
          '#19B8FF','#159BFF','#0F99FF','#0C75FF','#064FFF'),
    labels=paste0(names(tmp.s), " (",tmp.s,")"),
    radius = 1
    )

# ------ 막대그래프 ------
# 각 사업단별 월별 하수량 평균
# 중첩 막대그래프
STP.file$월<- format(STP.file$날짜,'%m')

STP.file.month<-aggregate(하수량~월+사업단.소.,data = STP.file,FUN = mean)
names(STP.file.month)[3]<-"평균하수량"

library(scales)

ggplot(STP.file.month,
       aes(x=factor(사업단.소.),y=평균하수량,fill=월))+
  geom_col()+
  labs(title='사업단별 월별 평균 하수량',x='사업단',y='평균 하수량')+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values=c('#7B1113','#C8A2C8','#B0E0E6','#E0E0E0','#7BB661','#FFB6C1',
                             '#F88379','#FFA500','#0047AB','#007BA7','#FFD700','#2A52BE'))

# ------ 선그래프 ------
# 각 사업단별 날짜에 따른 하수량 변화
STP.file$월<- format(STP.file$날짜,'%m')
STP.file.month<-aggregate(하수량~월+사업단.소.,data = STP.file,FUN = mean)
str(STP.file.month)

ggplot(STP.file.month,aes(x=월,y=하수량,color=사업단.소.,group=사업단.소.))+
  geom_line()+
  labs(title='사업단별 월별 하수량 변화',x='날짜',y='평균 하수량')+
  scale_y_continuous(labels = comma)

# 막대그래프와 거의 같은 내용


# ------ 상자그림 ------
# 사업단별 pH 분포 차이
ggplot(STP.file, aes(x = 사업단.소., y = 수소이온농도)) +
  geom_boxplot() +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(5, 10)) +
  labs(title = "사업단별 pH 분포 (붉은선 = pH 7)")


# ------ 산점도 ------
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(STP.file$수소이온농도, STP.file$BOD,
     main = '수소이온농도 - 생물학적 산소 요구량 상관관계',
     type = 'p',
     xlab = 'pH',
     ylab = 'BOD',
     xlim = c(5, 9),
     col='#FFB6C1')

plot(STP.file$수소이온농도, STP.file$COD,
     main = '수소이온농도 - 화학적 산소 요구량 상관관계',
     type = 'p',
     xlab = 'pH',
     ylab = 'COD',
     xlim = c(5, 9),
     col='#7BB661')

plot(STP.file$수소이온농도, STP.file$총질소,
     main = '수소이온농도 - 총질소 상관관계',
     type = 'p',
     xlab = 'pH',
     ylab = '총질소',
     xlim = c(5, 9),
     col='#007BA7')

plot(STP.file$수소이온농도, STP.file$부유물질,
     main = '수소이온농도 - 부유물질 상관관계',
     type = 'p',
     xlab = 'pH',
     ylab = 'SS',
     xlim = c(5, 9),
     col='#C8A2C8')

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# ------ 트리맵 ------
# 사업단 ~ 날짜 * BOD+COD
library(treemap)

STP.file$오염도 <- STP.file$BOD + STP.file$COD
STP.file$월 <- format(STP.file$날짜, '%m')

STP.pollution.month <- aggregate(오염도 ~ 사업단.소. + 월, 
                                 data = STP.file, 
                                 FUN = sum)
colnames(STP.pollution.month) <- c('사업단', '월', '오염도')

treemap(STP.pollution.month,
        index = c("사업단", "월"),
        vSize = "오염도",
        vColor = "오염도",
        type = "value",
        palette = "RdYlGn",      # 초록(낮음)~빨강(높음)
        title = "사업단×월 오염도(BOD+COD) 트리맵",
        border.col = "white")

# ------ 방사형 차트 ------
library(fmsb)

# 사업단별로 5개의 컬럼의 평균을 한 번에 계산
# cbind로 변수를 묶어 사업단을 기준으로 하여 14분류로 그룹화
STP.file.radar <- aggregate(cbind(BOD, COD, 부유물질, 총질소, 총인) ~ 사업단.소.,
                            data = STP.file, FUN = mean)

# [,-1]: 1열(사업단.소.) 제거, 새 데이터프레임 생성
rownames(STP.file.radar) <- STP.file.radar$사업단.소.
STP.file.radar <- STP.file.radar[, -1]

# 각 지표마다 최대/최소값을 구하여 방사형 그래프를 위한 축 범위 지정
max_min <- rbind(apply(STP.file.radar, 2, max), apply(STP.file.radar, 2, min))
rownames(max_min) <- c("Max", "Min")

# 한 페이지에 몇 개의 그래프를 그릴지 정함
n <- length(rownames(STP.file.radar))
per_page <- 8

# 사업단 개수별로 다른 색 지정정
color.list <- rainbow(n)
names(color.list) <- rownames(STP.file.radar)

# 반복문을 통해 방사형 그래프를 각각 페이지에 나눔
for (start in seq(1, n, by = per_page)) {
  end <- min(start + per_page - 1, n)
  real.list <- rownames(STP.file.radar)[start:end]
  
  par(mfrow = c(2, 4), mar = c(1, 1, 2, 1))
  
  for (사업단명 in real.list) {
    data_i <- rbind(max_min, STP.file.radar[사업단명, ])
    now.color <- color.list[사업단명]
    
    radarchart(data_i,
               pcol = now.color,
               pfcol = scales::alpha(now.color, 0.3),
               plwd = 2,
               axistype = 1,cglcol="darkgrey", cglty=1,
               axislabcol="darkgrey",
               caxislabels=seq(0,100,25), cglwd=0.8,
               title = 사업단명,
               cex.main = 0.9)
  }
}

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

max(STP.file$BOD)
min(STP.file$BOD)

max(STP.file$COD)
min(STP.file$COD)

max(STP.file$부유물질)
min(STP.file$부유물질)

max(STP.file$총질소)
min(STP.file$총질소)

max(STP.file$총인)
min(STP.file$총인)

# 월별 지표(BOD/COD/SS/T-N/T-P)


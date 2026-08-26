setwd('C:/R/teamproject')
getwd()

stp <-  read.csv(file = '부산환경공단_부산_하수처리장_일별_방류수_수질정보_20251231_수정.csv')

str(stp)
head(stp)


############ 처리
# str(stp)를 해보면 chr(문자형)임을 확인할 수 있음
# 숫자(num)로 변환하여 결측값 자동 전환하기
stp$하수량 <- as.numeric(as.character(stp$하수량))
stp$생물학적.산소.요구량 <- as.numeric(as.character(stp$생물학적.산소.요구량))
stp$화학적.산소.요구량 <- as.numeric(as.character(stp$화학적.산소.요구량))
stp$부유물질 <- as.numeric(as.character(stp$부유물질))
stp$총질소 <- as.numeric(as.character(stp$총질소))
stp$총인 <- as.numeric(as.character(stp$총인))
stp$수소이온농도 <- as.numeric(as.character(stp$수소이온농도))

sum(is.na(stp$하수량))
sum(is.na(stp$생물학적.산소.요구량))
sum(is.na(stp$화학적.산소.요구량))
sum(is.na(stp$부유물질))
sum(is.na(stp$총질소))
sum(is.na(stp$총인))
sum(is.na(stp$수소이온농도))

str(stp)

# 2. 기장사업소 일, 기장사업소 이리광, 기장사업소 일광 -> 기장사업소 일광으로 통일
stp$loc <- stp$사업단.소.
stp$loc <- ifelse(stp$사업단.소. %in% c("기장사업소","기장사업소 기장"), "기장사업소 기장", stp$사업단.소.)
stp$loc[substr(stp$사업단.소., 7,9) == '이리광' | substr(stp$사업단.소.,7,7) == '일' | substr(stp$사업단.소.,7,8) == '일광' ] <- "기장사업소 일광"
stp$loc[substr(stp$사업단.소.,1,5) == '강변사업단' | substr(stp$사업단.소.,1,5) == '강변사업소'] <- "강변사업단(소)"

table(stp$loc)

# 3. 지도를 뽑기 위해 각 사업단 별 위도, 경도 데이터 넣기
map_latlon <- data.frame(
  loc = c("강변사업단(소)","기장사업소 기장","기장사업소 문오성","기장사업소 일광",
          "남부사업소","녹산사업소 녹산","녹산사업소 신호","동부사업소",
          "서부사업소","수영사업단","영도사업소","정관사업소",
          "중앙사업소","해운대사업소"),
  lat = c(35.082360, 35.2393, 35.2974, 35.2575, 35.1263, 35.0747, 35.0931,
          35.1953, 35.1632, 35.188115, 35.0975, 35.3217, 35.0550163, 35.1739926),
  lon = c(128.951699, 129.2223, 129.2599, 129.2273, 129.1177, 128.8741,
          128.8785, 129.1302, 128.9197, 129.113440, 129.0768, 129.2241,
          129.0129342, 129.1848351)
)

# 위도, 경도데이터와 병합했는데 굳이 그럴 필요는 없을 듯... 데이터가 중복됨됨
#install.packages("dplyr")
library(dplyr)
stp_merged <- left_join(stp, map_latlon, by="loc")

stp_loc <- aggregate(cbind(lat, lon)~loc, data=stp_merged, FUN=mean)


############ 데이터 시각화
#----------------------------------------------------------------------------------------------------------------

### 1. 각 사업단 위치 정보 [지도 (map)]
library(leaflet)
library(htmltools)

# 사업단 전처리 필요(중복 테이더 방지)
# 각 사업단 별 위도경도
popup_text <- paste0('<b>',map_latlon$loc,'</b>')
popup_html <- lapply(popup_text, HTML)

m.stp <- leaflet(data = map_latlon) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(
    lng = 129.0756, 
    lat = 35.1796,
    zoom = 11        
  ) %>%
  addMarkers(
    lng = ~lon,
    lat = ~lat,
    popup = ~popup_html,
    clusterOptions = markerClusterOptions()
  )

print(m.stp)

# -> 대체로 부산에서 큰 강가나 바닷가에 가깝게 위치해 있는 하수처리소의 지리적 특성을 볼 수 있음

#----------------------------------------------------------------------------------------------------------------

### 2. loc 계층별 하수 처리 규모 [트리맵] - 부산시 내 하수 처리 거점의 규모 파악 + [원형그래프프]

library(treemap)

stp_mean_sewage <- aggregate(하수량 ~ loc,
                             data = stp,
                             FUN = mean,
                             na.rm = TRUE)

# 트리맵 생성
treemap(stp_mean_sewage,
        index = c("loc"),           
        vSize = "하수량",     
        vColor = "하수량",          
        type = "value",           
        palette = "Blues",          
        title = "2023~2025년 부산시 사업단별 하수 처리 규모 트리맵",
        fontsize.labels = 12,       
        border.col = "white"       
)


# 파이그래프
sorted_df <- stp_mean_sewage[order(stp_mean_sewage$하수량,
                                   decreasing = TRUE), ]

stp_mean_sewage_sort <- sorted_df$하수량
names(stp_mean_sewage_sort) <- sorted_df$loc

library(plotrix)

pie3D(stp_mean_sewage_sort, 
      labels = names(stp_mean_sewage_sort), 
      main = "2023~2025년 부산시 사업단별 하수 처리 규모",
      col = rainbow(length(stp_mean_sewage_sort)),
      cex = 0.7,
      theta = 1.2)


# -> 강변사업단, 수영사업단, 남부사업소가 압도적으로 높은 각각의 지리적 특징이 있을까?
# -> 예를 들면, 공단이거나 주거밀집지역이거나 바다로 흐르는 낙동강 근처이거나

#----------------------------------------------------------------------------------------------------------------

### 3. 2023~2025년 각 사업단별 하수량 비율 비교 [막대그래프]
# - 각 사업단별 3년간 하수처리량 평균치 구하기
# 부산시 하수처리량 총 평균값
busan_stp <- mean(stp$하수량, na.rm = TRUE)
busan_stp_avg <- data.frame(
  loc = "부산지 전체 평균",
  하수량 = busan_stp
)

# loc 별로 하수량(하수량)의 평균(mean) 구하기
stp_mean_sewage <- aggregate(하수량 ~ loc, data = stp, FUN = mean, na.rm = TRUE)
stp_mean_sewage_sort <- stp_mean_sewage[order(stp_mean_sewage$하수량, decreasing = TRUE), ]
busan_stp_mean <- rbind(stp_mean_sewage_sort, busan_stp_avg)
busan_stp_mean <- busan_stp_mean[c(15, 1:14), ]

library(ggplot2)
busan_stp_mean$loc <- factor(busan_stp_mean$loc, levels = busan_stp_mean$loc)

colorchip <- c("tomato","royalblue","royalblue","royalblue","royalblue","royalblue","royalblue",
               "royalblue","royalblue","royalblue","royalblue","royalblue","royalblue","royalblue","royalblue")

ggplot(busan_stp_mean, aes(x=loc, y=하수량, fill = (loc == "부산시 전체 평균")))+
  geom_bar(stat = 'identity', width=0.9, fill=colorchip)+
  ggtitle('2023~2025년 부산시 각 사업단(소)별 하수처리량 평균')+
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 13))


# 참고) 각 사업단별 하수 처리 빈도수
sewage_freq <- table(stp$loc)
sewage_freq
# -> 다른 사업단은 1096회, but 기장사업소 일광만 768회임.
# -> 데이터 안에 하수처리를 수행하지 않은 날이 있음(데이터 누락인지 실제인지는 확인 불가)
# -> 일광의 지리적 특성, 거주지 특성, 인구특성에 따라 가설을 생각해볼 수 도 있을 듯.

#----------------------------------------------------------------------------------------------------------------

### 4. x축(월별), group(연도) - 2023~2025년 월별 부산 전체 하수량 변화 비교 [다중 선그래프]
# 월별 평균 -> 년도별 라인그래프 3가지

stp$year <- substr(stp$날짜, 1, 4)
stp$month <- substr(stp$날짜, 6, 7)

month_stp <- aggregate(하수량 ~ year + month,
                       data = stp,
                       FUN = mean,
                       na.rm = TRUE)

month_stp <- subset(month_stp,
                    year %in% c("2023", "2024", "2025"))

library(ggplot2)

ggplot(month_stp, aes(x = month, y = 하수량,
                      group = year,
                      color = year)) +
  geom_line() +
  geom_point(size = 3) +          
  labs(
    title = "2023~2025년 월별 부산 전체 하수량 변화 비교",
    x = "월 (Month)",
    y = "평균 하수량",
    color = "연도"
  ) +
  theme(
    axis.text.x = element_text(face = "bold"),
    legend.position = "top"
  )


# -> 7월 여름에 급격하게 증가하고, 겨울(1월)에 가장 낮음을 확인
# -> 이유는? 장마, 집중호우로 인해 유입되는 유수의 양 증가

#----------------------------------------------------------------------------------------------------------------

### 5. 월별 생물학적산소요구량(BOD) 분포를 월(month)별, 연도(year)별로 비교 [박스그림]
ggplot(stp, aes(x = month, y = 생물학적.산소.요구량, fill = year)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "2023~2025년 월별 생물학적산소요구량(BOD) 분포 비교",
    subtitle = "계절별 변동 및 이상치(장마철 희석 등) 분석",
    x = "월 (Month)",
    y = "생물학적 산소 요구량 (BOD)",
    fill = "연도"
  ) +
  theme(
    axis.text.x = element_text(face = "bold"),
    legend.position = "top"
  )

# -> 앞에서 2023~2025년 여름철(7월)장마 및 집중호우로 인해 유입되는 우수(빗물)의 양이 늘어나며 하수처리량의 증가가 급격히 증가함을 확인할 수 있다.
# -> 이를 통해 물의 양이 늘어나면서 BOD의 수치가 일시적으로 낮아지는 희석효과가 발생함을 유추할 수 있음
# -> 하지만, 월별 BOD 변화량을 보면 2023,2024년에는 7월이 아닌 9월도에 BOD 수치가 낮아지는 현상을 관찰할 수 있다.
# -> 이는 2023,2024년의 9월에 하수처리량이 다시 증가함으로써 나타난 희석효과이지 않을까 한다

#----------------------------------------------------------------------------------------------------------------

### 6. 주요 사업소별 영양염류 및 물리적 오염도 비교(부유물질, 총질소, 총인 평균값) [다중막대그래프]
bar_data <- aggregate(cbind(부유물질, 총질소, 총인) ~ loc, data = stp, FUN = mean, na.rm = TRUE)

mat_data <- as.matrix(bar_data[, c("부유물질", "총질소", "총인")])
rownames(mat_data) <- bar_data$loc
mat_data_t <- t(mat_data)

par(mar = c(8, 4, 4, 10)) # c(하, 좌, 상, 우)
barplot(mat_data_t,
        beside = TRUE,                      
        col = c("tomato", "royalblue", "darkseagreen3"), 
        main = "주요 사업소별 영양염류 및 물리적 오염도 비교",
        ylab = "농도 평균 (mg/L 등)",
        las = 2,                            
        cex.names = 1.2)                

abline(h = 10, col = "tomato", lty = 2, lwd = 1.5)
abline(h = 20, col = "royalblue", lty = 2, lwd = 1.5)
abline(h = 2, col = "darkseagreen3", lty = 2, lwd = 1.5)

legend("topright", 
       inset = c(-0.28, 0),                   
       legend = c("부유물질", "총질소", "총인", "관리 기준선"), 
       fill = c("tomato", "royalblue", "darkseagreen3", NA), 
       lty = c(NA, NA, NA, 2),                # 기준선 표시용 선 모양
       col = c(NA, NA, NA, "black"),          # 기준선 색상
       bty = "n",                             
       cex = 0.8,
       xpd = TRUE)

colors()

#----------------------------------------------------------------------------------------------------------------

### 7. BOD vs COD 상관관계 분석 [산점도]
library(ggplot2)

ggplot(data=stp,
       aes(x = 하수량, y = 화학적.산소.요구량)) +
  geom_point(color = "royalblue",
             size = 2) +
  ggtitle("하수량과 화학적 산소요구량(COD)의 관계") +
  theme(plot.title = element_text(size=20,
                                  face = 'bold',
                                  colour = 'tomato'))

#

ggplot(data=stp, aes(x = 생물학적.산소.요구량, y = 화학적.산소.요구량)) +
  geom_point(color = "royalblue", size = 2) +
  ggtitle("생물학적 산소요구량(BOD)와 화학적 산소요구량(COD)의 관계")+
  theme(plot.title = element_text(size=20, face = 'bold', colour = 'tomato'))


stp_bc <- stp[,4:5]

stp$loc <- as.factor(stp$loc)
n_groups <- length(levels(stp$loc))

group <- as.numeric(stp$loc)

plot(stp_bc,
     main = '각 사업단별 BOD vs COD',
     pch = group,
     col = group)

legend("topright", 
       legend = levels(stp$loc), 
       col = 1:n_groups,
       pch = 1:n_groups)

# BOD와 COD 간에는 유의미한 상관관계가 있다고 볼 수 없다.
# -> BOD는 낮은데 COD가 높은 경우 ->
# -> COD는 낮은데 BOD가 높은 경우 -> 

###음....
filtered_data <- stp %>%
  filter(부유물질 < 100 & 총질소 < 100 & 총인 < 50) %>%
  select(부유물질, 총질소, 총인)

# 2. 정제된 데이터로 다시 다중 산점도 그리기
pairs(na.omit(filtered_data),
      main = "부유물질·총질소·총인 다중 산점도 (이상치 제외)",
      pch = 19,
      col = adjustcolor("royalblue", alpha.f = 0.4),
      cex = 0.6)

#----------------------------------------------------------------------------------------------------------------

### 각각의 컬럼의 평균값의 박스그림
col_means <- aggregate(stp[, 4:9], by = list(stp$loc), FUN = mean, na.rm = TRUE)

# 4~9번 컬럼들의 전체 데이터 분포를 보여주는 박스플롯 그리기
boxplot(stp[, 4:9], 
        main = "주요 수질 항목별 데이터 분포",
        las = 2,                
        col = "tomato",  
        ylab = "농도 (mg/L 등)",
        ylim = c(0, 30))

#----------------------------------------------------------------------------------------------------------------

### 7월달 BOD vs 하수처리량 관계도 [산점도]
july_kang <- subset(stp, (month == "07" | month == "7") & loc == "강변사업단(소)")
july_su <- subset(stp, (month == "07" | month == "7") & loc == "수영사업단")
july_nam <- subset(stp, (month == "07" | month == "7") & loc == "남부사업소")

# 2. 기본 산점도 그리기
plot(july_kang$하수량, july_data$화학적.산소.요구량,
     main = "강변사업단(소) 7월 하수량과 BOD 관계",
     xlab = "하수량 (㎥/일)",
     ylab = "BOD (mg/L)",
     pch = 19,
     col = "royalblue")

plot(july_su$하수량, july_data$화학적.산소.요구량,
     main = "수영사업단 7월 하수량과 BOD 관계",
     xlab = "하수량 (㎥/일)",
     ylab = "BOD (mg/L)",
     pch = 19,
     col = "royalblue")

plot(july_nam$하수량, july_data$화학적.산소.요구량,
     main = "남부사업소 7월 하수량과 BOD 관계",
     xlab = "하수량 (㎥/일)",
     ylab = "BOD (mg/L)",
     pch = 19,
     col = "royalblue")

# 4. 상관계수 확인
cor(july_kang$하수량, july_data$화학적.산소.요구량)
cor(july_su$하수량, july_data$화학적.산소.요구량)
cor(july_nam$하수량, july_data$화학적.산소.요구량)


# 각 사업단 별 7월 COD 박스
july_data <- subset(stp, month == "07" | month == "7")
july_data$화학적산소요구량 <- as.numeric(as.character(july_data$화학적.산소.요구량))
july_clean <- na.omit(july_data[, c("loc", "화학적산소요구량")])

boxplot(화학적산소요구량 ~ loc, data = july_clean,
        main = "사업단별 7월 COD 분포 비교",
        xlab = "사업단(소)",
        ylab = "COD (mg/L)",
        col = "tomato",
        las = 2)

# 각 사업단 별 7월 하수처리량 박스
july_data <- subset(stp, month == "07" | month == "7")
july_data$하수량 <- as.numeric(as.character(july_data$하수량))
july_clean <- na.omit(july_data[, c("loc", "하수량")])

boxplot(하수량 ~ loc, data = july_clean,
        main = "사업단별 7월 하수량 분포 비교",
        xlab = "사업단(소)",
        ylab = "COD (mg/L)",
        col = "tomato",
        las = 2)



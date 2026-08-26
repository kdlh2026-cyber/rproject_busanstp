setwd('C:/R/teamproject')
getwd()

water <- read.csv(file = '부산환경공단_부산_하수처리장_일별_방류수_수질정보_20251231_수정.csv')
head(water)

# 해당 컬럼 끌어올때 꼭 mutate 할것


library(ggplot2)
library(leaflet)
library(htmltools)

#  팝업 내용 작성
popup_text <- paste0("<b>", water$사업단.소, "</b><br>")
popup_html <- lapply(popup_text, HTML)


m_water <- leaflet(water) %>%
  #지도배경
  addProviderTiles(providers$CartoDB.Positron) %>%
  # 부산중심 좌표
  setView(
    lat = 35.1796, #위도
    lng = 129.0756, #경도
    zoom = 11
  ) %>% 
  # 마커 추가
  addMarkers(
    lat = ~위도, # lat = whater$위도
    lng = ~경도, # lng = water$경도
    popup = ~popup_html,
    clusterOptions = markerClusterOptions()
  )

# 지도 출력
print(m_water)

# 14군데에 하수처리시설
#install.packages("extrafont")

library(dplyr)
library(extrafont)
library(scales) # 숫자 포맷팅(콤마 표기)에 필요

ds_summary <- ds %>%
  mutate(하수량 = as.numeric(하수량)) %>%
  group_by(`사업단.소`) %>%
  summarise(평균하수량 = mean(하수량, na.rm= TRUE))

ggplot(ds_summary, aes(x = reorder(`사업단.소`, -평균하수량), y = 평균하수량, fill = 평균하수량)) +
  
  geom_col(width = 0.65, show.legend = FALSE) +
  
  # 막대 상단에 실제 평균값(콤마 포맷) 텍스트 표시
  geom_text(
    aes(label = comma(round(평균하수량))), 
    vjust = -0.5, 
    size = 3.2, 
    family = "Malgun Gothic",
    fontface = "bold"
  ) +
  
  # Y축 지수 표기(4e+05) 제거 및 콤마 표기 적용,
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  scale_fill_gradient(low = "#9ecae1", high = "#08519c") +
  
  # 폰트, 축 라벨 각도 정리
  theme_minimal(base_family = "Malgun Gothic") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 9, angle = 35, hjust = 1, vjust = 1), # 긴 이름 대각선 회전
    axis.text.y = element_text(size = 9),
    panel.grid.major.x = element_blank(), # x축 세로선 제거로 깔끔하게 정리
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "사업소별 일평균 하수처리량",
    x = "사업소",
    y = "평균 하수처리량 (㎥/일)"
  )


font_family <- ifelse(.Platform$OS.type == "windows", "Malgun Gothic", "AppleGothic")
theme_set(theme_minimal(base_family = font_family))

ds_pie <- ds %>%
  mutate(하수량 = as.numeric(하수량)) %>%
  group_by(`사업단.소`) %>%
  summarise(총하수량 = sum(하수량, na.rm = TRUE)) %>%
  mutate(비율 = 총하수량 / sum(총하수량))

# 도넛 차트 시각화 (fill = `사업단.소.`로 매핑)
# x는 크기
ggplot(ds_pie, aes(x = 2, y = 비율, fill = `사업단.소`)) +
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) + # 도넛 형태 구현
  geom_text(
    aes(label = ifelse(비율 >= 0.03, percent(비율, accuracy = 0.1), "")), # 너무 작은 비율은 텍스트 겹침 방지
    position = position_stack(vjust = 0.5), 
    family = font_family, 
    size = 3.5,
    fontface = "bold"
  ) +
  theme_void(base_family = font_family) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, vjust = -1,margin = margin(b = 10)),
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = "부산시 총 하수처리 분담률",
    fill = "사업소"
  )

library(lubridate) # 분기 추출 함수 quarter() 사용

# 분기별 평균 COD 계산
ds_quarterly_cod <- ds %>%
  mutate(
    날짜_변환 = as.Date(as.character(날짜)),
    COD_수치 = suppressWarnings(as.numeric(gsub("[^0-9.]", "", `화학적.산소.요구량`))),
    # null값 suppressWarnings 경고 무시
    # javascript에도 있다. 어노테이션 @
    # 분기 라벨 생성 ("2023 1분기")
    연도 = year(날짜_변환),
    분기 = quarter(날짜_변환),
    quarter_label = paste0(연도, " ", 분기, "분기")
  ) %>%
  filter(!is.na(날짜_변환) & !is.na(COD_수치)) %>%
  group_by(연도, 분기, quarter_label) %>%
  summarise(평균_COD = mean(COD_수치, na.rm = TRUE), .groups = "drop") %>%
  arrange(연도, 분기) %>% # 연도, 분기로 정렬
  mutate(quarter_label = factor(quarter_label, levels = quarter_label)) # 시간 순서대로 정렬

# 분기별 선 그래프 시각화
ggplot(ds_quarterly_cod, aes(x = quarter_label, y = 평균_COD, group = 1)) +
  geom_area(fill = "#3182bd", alpha = 0.20) +
  # 선
  geom_line(color = "#08519c", linewidth = 1.2) +
  # 점 포인트
  geom_point(color = "#08519c", fill = "white", size = 3.5, shape = 21, stroke = 1.5) +
  # 점 위의 텍스트
  geom_text(
    aes(label = round(평균_COD, 2)), 
    vjust = -2,
    size = 3.5, 
    family = font_family, 
    fontface = "bold"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  theme_minimal(base_family = font_family) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "분기별 화학적 산소 요구량(COD) 변동",
    x = "분기",
    y = "평균 COD (mg/L)"
  )

# 사업소별 평균 BOD/COD 농도 계산 및 순위 부여
ds_ranking <- ds %>%
  mutate(
    BOD_수치 = suppressWarnings(as.numeric(gsub("[^0-9.]", "", `생물학적.산소.요구량`)))
  ) %>%
  filter(!is.na(`사업단.소`) & !is.na(BOD_수치)) %>%
  filter(`사업단.소` != "기장사업소") %>% 
  group_by(`사업단.소`) %>%
  summarise(평균_BOD = mean(BOD_수치, na.rm = TRUE), .groups = "drop") %>%
  arrange(평균_BOD) %>%                                # 농도가 낮은 순(깨끗한 순) 정렬
  mutate(
    순위 = row_number(),
    라벨_표시 = paste0(round(평균_BOD, 2), " mg/L (", 순위, "위)")
  )

ggplot(ds_ranking, aes(x = reorder(`사업단.소`, -평균_BOD), y = 평균_BOD, fill = 평균_BOD)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  # 막대 끝에 수치 및 순위 라벨 표기
  geom_text(
    aes(label = 라벨_표시), 
    hjust = -0.15, 
    size = 3.5, 
    family = font_family, 
    fontface = "bold"
  ) +
  # 깨끗할수록(농도 낮음) 청록색, 높을수록(오염도 높음) 주황/붉은색
  scale_fill_gradient(low = "#2ca25f", high = "#de2d26") +
  # y축 여백
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() + 
  theme_minimal(base_family = font_family) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0, margin = margin(b = 15)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "BOD 수질 랭킹",
    subtitle = "농도가 낮을수록 정화 수준 우수",
    x = "처리장(사업소)",
    y = "평균 방류 BOD 농도 (mg/L)"
  )


# 단독 "기장사업소" 제외 및 랭킹 재계산
ds_ranking2 <- ds %>%
  mutate(
    COD_수치 = suppressWarnings(as.numeric(gsub("[^0-9.]", "", `화학적.산소.요구량`)))
  ) %>%
  # 결측치 제거 및 단독 '기장사업소' 행 필터링 제외
  filter(!is.na(`사업단.소`) & !is.na(COD_수치)) %>%
  filter(`사업단.소` != "기장사업소") %>% 
  group_by(`사업단.소`) %>%
  summarise(평균_COD = mean(COD_수치, na.rm = TRUE), .groups = "drop") %>%
  arrange(평균_COD) %>%                      # 농도가 낮은 순(깨끗한 순) 정렬
  mutate(
    순위 = row_number(),
    라벨_표시 = paste0(round(평균_COD, 2), " mg/L (", 순위, "위)")
  )

# 시각화
ggplot(ds_ranking2, aes(x = reorder(`사업단.소`, -평균_COD), y = 평균_COD, fill = 평균_COD)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = 라벨_표시), 
    hjust = -0.15, 
    size = 3.5, 
    family = font_family, 
    fontface = "bold"
  ) +
  scale_fill_gradient(low = "#2ca25f", high = "#de2d26") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() + 
  theme_minimal(base_family = font_family) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0, margin = margin(b = 15)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 9.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "COD 수질 랭킹",
    subtitle = "농도가 낮을수록 정화 수준 우수",
    x = "처리장(사업소)",
    y = "평균 방류 COD 농도 (mg/L)"
  )

-------------------------x------------
# BOD, COD 데이터 전처리
ds_na <- ds %>%
  mutate( 
    BOD = suppressWarnings(as.numeric(gsub("[^0-9.]", "", `생물학적.산소.요구량`))),
    COD = suppressWarnings(as.numeric(gsub("[^0-9.]", "", `화학적.산소.요구량`)))
  ) %>%
  filter(!is.na(BOD) & !is.na(COD))

# 상관계수(r) 계산
cor_val <- cor(ds_na$BOD, ds_na$COD, use = "complete.obs")
# 0.214 3자리 수
cor_label <- paste0("상관계수 (r) = ", round(cor_val, 3))

# 산점도 및 선형 회귀선 시각화
ggplot(ds_na, aes(x = BOD, y = COD)) +
  # 산점도 포인트 (반투명 처리로 겹침 밀도 표현)
  geom_point(color = "#1f78b4", alpha = 0.5, size = 2) +
  # 선형 회귀선 및 95% 신뢰구간
  geom_smooth(method = "lm", color = "#e31a1c", fill = "#fb9a99", linewidth = 1.2, se = TRUE) +
  # 상관계수 주석(텍스트) 추가
  annotate(
    "text", 
    x = min(ds_na$BOD, na.rm = TRUE), 
    y = max(ds_na$COD, na.rm = TRUE), 
    label = cor_label, 
    hjust = 0, vjust = 2, 
    family = font_family, 
    fontface = "bold", 
    size = 4, 
    color = "darkred"
  ) +
  theme_minimal(base_family = font_family) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "생물학적(BOD) vs 화학적(COD) 산소 요구량 상관관계",
    x = "생물학적 산소 요구량 BOD (mg/L)",
    y = "화학적 산소 요구량 COD (mg/L)"
  )

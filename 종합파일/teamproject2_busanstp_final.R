setwd('C:/R/teamproject')
getwd()

stp <- read.csv(file = '부산환경공단_부산_하수처리장_일별_방류수_수질정보_20251231_수정.csv', stringsAsFactors = FALSE)

# 데이터 구조 확인용
str(stp)
head(stp)

# [핵심] 팀원들이 제각각 쓰던 변수명들을 일괄적으로 통일해 줍니다.
# 실제 CSV 파일의 원본 컬럼 순서에 맞추어 이름을 매핑해주세요. (예시 명칭)
names(stp) <- c("site", "date", "flow", "BOD", "COD", "SS", "TN", "TP", "pH", "lat", "lon")

# 이후 코드에서 자주 쓰이는 변수들을 복사해서 호환성 확보
stp$loc <- stp$site              # 사업단 명칭 호환
stp$화학적.산소.요구량 <- stp$COD # 한글 컬럼 호환
stp$수소이온농도 <- stp$pH        # pH 호환
stp$부유물질 <- stp$SS          # SS 호환
stp$총질소 <- stp$TN            # TN 호환
stp$총인 <- stp$TP              # TP 호환

# 날짜 포맷 및 월/연도 컬럼 미리 생성
stp$날짜 <- as.Date(stp$date)
stp$year <- format(stp$날짜, "%Y")
stp$month <- format(stp$날짜, "%m")

#install.packages("lubridate")
#install.packages("plotly")

library(ggplot2)
library(leaflet)
library(treemap)
library(plotrix)
library(lubridate)
library(fmsb)
library(plotly)

# ---------------------------------------------------------
# 1. 지도 - 사업소 위치 (위도/경도) 산점 + 라벨
# ---------------------------------------------------------
site_loc <- stp %>%
  mutate(lon = ifelse(site == "기장사업소 일광", NA, lon)) %>%  
  group_by(site, lat, lon) %>%
  summarise(BOD_mean = mean(BOD, na.rm = TRUE),
            flow_mean = mean(flow, na.rm = TRUE),    
            .groups = "drop") %>%
  filter(!is.na(lat), !is.na(lon))

plot(site_loc$lon, site_loc$lat,
     main = "사업소 위치 (부산, 좌표만 표시)", xlab = "경도", ylab = "위도",
     pch = 19, col = "darkblue", cex = site_loc$BOD_mean / max(site_loc$BOD_mean) * 3)
text(site_loc$lon, site_loc$lat, labels = site_loc$site, pos = 3, cex = 0.6)

water_icons <- awesomeIcons(icon = "tint", library = "fa", markerColor = "lightblue")

leaflet(site_loc, width = "100%", height = "750px") %>%
  addTiles() %>%   
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = ~ BOD_mean / max(BOD_mean) * 15,    
    color = "darkblue", fillOpacity = 0.6,
    group = "원(BOD 크기)",    
    popup = ~ paste0("<b>", site, "</b><br>평균 하수량: ", format(round(flow_mean), big.mark = ","), "<br>평균 BOD: ", round(BOD_mean, 2))
  ) %>%
  addAwesomeMarkers(
    lng = ~lon, lat = ~lat, icon = water_icons, group = "핀 마커",        
    popup = ~ paste0("<b>", site, "</b><br>평균 하수량: ", format(round(flow_mean), big.mark = ","), "<br>평균 BOD: ", round(BOD_mean, 2))
  ) %>%
  addLayersControl(overlayGroups = c("원(BOD 크기)", "핀 마커"), options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = 129.0756, lat = 35.1796, zoom = 11)


# ---------------------------------------------------------
# 2. 2023~2025년 사업소별 일평균 하수처리량 [막대그래프]
# ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(scales)

# stp 데이터 기준 요약 (컬럼명이 site 또는 사업단.소 인지 확인 후 사용)
ds_summary <- stp %>%
  mutate(하수량 = as.numeric(flow)) %>%
  group_by(site) %>%
  summarise(평균하수량 = mean(하수량, na.rm = TRUE))

ggplot(ds_summary, aes(x = reorder(site, -평균하수량), y = 평균하수량, fill = 평균하수량)) +
  
  geom_col(width = 0.65, show.legend = FALSE) +
  
  # 막대 상단에 실제 평균값(콤마 포맷) 텍스트 표시
  geom_text(
    aes(label = comma(round(평균하수량))), 
    vjust = -0.5, 
    size = 3.2, 
    family = "Malgun Gothic",
    fontface = "bold"
  ) +
  
  # Y축 지수 표기 제거 및 콤마 표기 적용
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  scale_fill_gradient(low = "#9ecae1", high = "#08519c") +
  theme_minimal(base_family = "Malgun Gothic") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(size = 9, angle = 35, hjust = 1, vjust = 1), # 긴 이름 대각선 회전
    axis.text.y = element_text(size = 9),
    panel.grid.major.x = element_blank(), # x축 세로선 제거
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "사업소별 일평균 하수처리량",
    x = "사업소",
    y = "평균 하수처리량 (㎥/일)"
  )


# ---------------------------------------------------------
# 3. 2023~2025년 사업소별 일평균 하수처리량 [트리맵]
# ---------------------------------------------------------

stp_mean_sewage <- aggregate(flow ~ site, data = stp, FUN = mean, na.rm = TRUE)
names(stp_mean_sewage)[2] <- "하수량"

treemap(stp_mean_sewage,
        index = c("site"),            
        vSize = "하수량",     
        vColor = "하수량",          
        type = "value",            
        palette = "Blues",          
        title = "2023~2025년 부산시 사업단별 하수 처리 규모 트리맵",
        fontsize.labels = 12,        
        border.col = "white"        
)


# ---------------------------------------------------------
# 4. 2023~2025년 사업소별 일평균 하수처리량 [파이그래프]
# ---------------------------------------------------------
sorted_df <- stp_mean_sewage[order(stp_mean_sewage$하수량, decreasing = TRUE), ]
stp_mean_sewage_sort <- sorted_df$하수량
names(stp_mean_sewage_sort) <- sorted_df$site

pie3D(stp_mean_sewage_sort, 
      labels = names(stp_mean_sewage_sort), 
      main = "2023~2025년 부산시 사업단별 하수 처리 규모",
      col = rainbow(length(stp_mean_sewage_sort)),
      cex = 0.7,
      theta = 1.2)


# ---------------------------------------------------------
# 5. 사업단 x 수질 지표 히트맵 (min-max 정규화)평균값 [히트맵]
# ---------------------------------------------------------
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# 지표 컬럼 정의
metric_cols <- c("BOD", "COD", "SS", "TN", "TP", "pH")

# ---------------------------------------------------------
# 1. UI
# ---------------------------------------------------------
ui <- fluidPage(
  titlePanel("사업소 × 수질 지표 히트맵 (min-max 정규화)"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "기간 선택",
                     start = "2025-01-01", end = "2025-12-31",
                     min = "2023-01-01", max = "2025-12-31"),
      selectInput("agg_fun", "집계 방식",
                  choices = c("평균" = "mean", "중앙값" = "median")),
      width = 3
    ),
    mainPanel(
      plotlyOutput("heatmap", height = "750px"),
      width = 9
    )
  )
)

# ---------------------------------------------------------
# 2. Server
# ---------------------------------------------------------
server <- function(input, output, session) {
  
  # 전체 통합 스크립트의 stp 데이터를 반응형으로 가져옴
  raw_data <- reactive({
    stp
  })
  
  agg_data <- reactive({
    req(raw_data())
    fun <- if (input$agg_fun == "mean") mean else median
    
    raw_data() %>%
      filter(date >= input$date_range[1], date <= input$date_range[2]) %>%
      group_by(site) %>%
      summarise(across(all_of(metric_cols), ~ fun(.x, na.rm = TRUE)), .groups = "drop")
  })
  
  # 지표(열)별로 min-max 정규화 -> 색상용 값
  norm_data <- reactive({
    agg_data() %>%
      pivot_longer(-site, names_to = "metric", values_to = "value") %>%
      group_by(metric) %>%
      mutate(
        norm_value = if (all(is.na(value))) NA_real_
        else (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
      ) %>%
      ungroup() %>%
      mutate(metric = factor(metric, levels = metric_cols))
  })
  
  output$heatmap <- renderPlotly({
    d <- norm_data()
    
    p <- ggplot(d, aes(x = metric, y = site, fill = norm_value,
                       text = paste0(site, " / ", metric, "<br>값: ", round(value, 2)))) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = round(value, 2)), size = 3, na.rm = TRUE) +
      scale_fill_gradient2(
        low = "darkgreen", mid = "khaki1", high = "red",
        midpoint = 0.5, na.value = "grey60",
        limits = c(0, 1), name = "min-max"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(l = 120))
  })
}

library(shiny)

# 샤이니 앱 실행 (5번 위치에 배치)
shinyApp(ui, server)



# ---------------------------------------------------------
# 6. 각 연도별 부산 전체 하수량의 월별 변화 추이 [다중 선그래프]
# ---------------------------------------------------------
month_stp <- aggregate(flow ~ year + month, data = stp, FUN = mean, na.rm = TRUE)
month_stp <- subset(month_stp, year %in% c("2023", "2024", "2025"))
names(month_stp)[3] <- "하수량"

ggplot(month_stp, aes(x = month, y = 하수량, group = year, color = year)) +
  geom_line() +
  geom_point(size = 3) +          
  labs(title = "2023~2025년 월별 부산 전체 하수량 변화 비교", x = "월 (Month)", y = "평균 하수량", color = "연도") +
  theme(axis.text.x = element_text(face = "bold"), legend.position = "top")


# ---------------------------------------------------------
# 7. 분기별 화학적 산소 요구량(COD) 변동 [선그래프]
# ---------------------------------------------------------
ds_quarterly_cod <- stp %>%
  mutate(
    날짜_변환 = as.Date(date),
    COD_수치 = suppressWarnings(as.numeric(COD)),
    연도 = year(날짜_변환),
    분기 = quarter(날짜_변환),
    quarter_label = paste0(연도, " ", 분기, "분기")
  ) %>%
  filter(!is.na(날짜_변환) & !is.na(COD_수치)) %>%
  group_by(연도, 분기, quarter_label) %>%
  summarise(평균_COD = mean(COD_수치, na.rm = TRUE), .groups = "drop") %>%
  arrange(연도, 분기) %>%
  mutate(quarter_label = factor(quarter_label, levels = quarter_label))

ggplot(ds_quarterly_cod, aes(x = quarter_label, y = 평균_COD, group = 1)) +
  geom_area(fill = "#3182bd", alpha = 0.20) +
  geom_line(color = "#08519c", linewidth = 1.2) +
  geom_point(color = "#08519c", fill = "white", size = 3.5, shape = 21, stroke = 1.5) +
  geom_text(aes(label = round(평균_COD, 2)), vjust = -2, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  theme_minimal() +
  labs(title = "분기별 화학적 산소 요구량(COD) 변동", x = "분기", y = "평균 COD (mg/L)")


# ---------------------------------------------------------
# 8. 사업단별 pH 농도 [박스그림]
# ---------------------------------------------------------
ggplot(stp, aes(x = site, y = pH)) +
  geom_boxplot() +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(5, 10)) +
  labs(title = "사업단별 pH 분포 (붉은선 = pH 7)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ---------------------------------------------------------
# 9. 각 사업단별 방류수 수질 기준 지표 [방사형 차트]
# ---------------------------------------------------------
library(fmsb)

# 1. 수질 지표 평균 계산
STP.file.radar <- aggregate(cbind(BOD, COD, SS, TN, TP) ~ site, 
                            data = stp, 
                            FUN = mean, 
                            na.rm = TRUE)

rownames(STP.file.radar) <- STP.file.radar$site
STP.file.radar <- STP.file.radar[, -1]

# 2. 축 범위 설정 (Max, Min)
max_val <- apply(STP.file.radar, 2, max, na.rm = TRUE)
min_val <- apply(STP.file.radar, 2, min, na.rm = TRUE)
max_min <- rbind(max_val, min_val)
rownames(max_min) <- c("Max", "Min")

# 3. 전체 사업단 개수 확인 및 색상 지정
n <- nrow(STP.file.radar) # 전체 사업단 수 (예: 14개)
color.list <- rainbow(n)
names(color.list) <- rownames(STP.file.radar)

# 4. [핵심] 14개가 한 화면에 다 들어가도록 4행 4열(총 16칸)로 판을 키움
par(mfrow = c(4, 4), mar = c(1, 1, 2, 1))

# 5. 전체 사업단을 순회하며 한 번에 그리기
for (사업단명 in rownames(STP.file.radar)) {
  data_i <- rbind(max_min, STP.file.radar[사업단명, ])
  now.color <- color.list[사업단명]
  
  radarchart(data_i,
             pcol = now.color,
             pfcol = scales::alpha(now.color, 0.3),
             plwd = 2,
             axistype = 1, 
             cglcol = "darkgrey", 
             cglty = 1,
             axislabcol = "darkgrey",
             caxislabels = round(seq(0, max(max_val), length.out = 5), 1), 
             cglwd = 0.8,
             title = 사업단명,
             cex.main = 0.9)
}

# 레이아웃 원상복구
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

## 부산환경공단_부산_하수처리장_일별_방류수_수질정보 팀프로젝트2 ##

# 막대 그래프, 선그래프, 원형그래프,트리맵, 산점도, 상자그림,방사형차트, 지도, 워드클라우드 
# install.packages("readr")
# install.packages("dplyr")
# install.packages("hexbin")
# install.packages("leaflet")
# install.packages("tidyr")
# install.packages("plotly")

library(dplyr)
library(treemap)
library(fmsb)
library(wordcloud)
library(leaflet)
library(hexbin)
library(tm)                            
library(RColorBrewer)
library(shiny)
library(ggplot2)
library(tidyr)
library(plotly)
library(readr)
library(stringr)

# ---------------------------------------------------------
# 0. 데이터 불러오기 + 정제
# ---------------------------------------------------------
setwd('C:/R/teamproject')
getwd()

sewer <- read.csv(
  '부산환경공단_부산_하수처리장_일별_방류수_수질정보_20251231_수정.csv',
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

names(sewer) <- c("site", "date", "flow", "BOD", "COD", "SS", "TN", "TP", "pH", "lat", "lon")

clean_numeric <- function(x) {
  x <- trimws(as.character(x))
  fixed <- vapply(x, function(v) {
    if (is.na(v) || v == "") return(NA_character_)
    if (!grepl("\\.", v)) return(v)
    head_part <- sub("^([^.]*\\.).*$", "\\1", v)
    tail_part <- sub("^[^.]*\\.", "", v)
    tail_part <- gsub("\\.", "", tail_part)
    paste0(head_part, tail_part)
  }, character(1), USE.NAMES = FALSE)
  suppressWarnings(as.numeric(fixed))
}

num_cols <- c("BOD", "COD", "SS", "TN", "TP", "pH")
sewer[num_cols] <- lapply(sewer[num_cols], clean_numeric)
sewer$date <- as.Date(sewer$date)

#########################################################
# 1. 막대 그래프 :사업소별 평균 BOD
#########################################################
bod_mean <- tapply(sewer$BOD,sewer$site,mean,na.rm = T)
bod_mean <- sort(bod_mean,decreasing = T)
colors <- rainbow(20) 

barplot(bod_mean,
        main = "사업소별 평균 BOD",
        col = colors[1:20],
        las = 1,
        cex.names =0.7,
        ylab = "BOD (mg/L)"
        )
# ===========================================================
# 2. 선그래프: 월별 평균 BOD 추이
# ===========================================================
sewer$yearmonth <- format(sewer$date, "%Y-%m")
monthly_bod <- tapply(sewer$BOD, sewer$yearmonth, mean, na.rm = TRUE)

plot(monthly_bod, type = "l", col = "#BAE1FF", lwd = 2,
     main = "월별 평균 BOD 추이", xlab = "월(순번)", ylab = "BOD (mg/L)", xaxt = "n")

# ===========================================================
# 3. 원형그래프: 사업소별 하수량 비중
# ===========================================================
sewage_sum <- tapply(sewer$flow, sewer$site, sum, na.rm = TRUE)
sewage_sum <- sort(sewage_sum, decreasing = TRUE)

pie(sewage_sum, main = "사업소별 하수량 비중",
    col = rainbow(length(sewage_sum)), cex = 0.7)

# ===========================================================
# 4. 트리맵: 사업소별 하수량 크기
# ===========================================================
treemap_data <- data.frame(
  site = names(sewage_sum),
  flow = as.numeric(sewage_sum)
)

treemap(treemap_data,
        index = "site",
        vSize = "flow",
        title = "사업소별 하수량 트리맵",
        palette = "Pastel1")

# ===========================================================
# 5. 산점도: BOD vs COD
# ===========================================================
plot(sewer$flow, sewer$COD,
     main = "하수량 vs COD 산점도",
     xlab = "하수량", 
     ylab = "COD (mg/L)",
     pch = 19, 
     col = c("orange","red"))


plot(hexbin(sewer$BOD, sewer$COD, xbins = 40),
     main = "BOD vs COD 밀도",
     colramp = colorRampPalette(c("skyblue","darkblue"))
     )


# ===========================================================
# 6. 상자그림: 사업소별 BOD 분포
# ===========================================================
boxplot(BOD ~ site, data = sewer,
        main = "사업소별 BOD 분포", ylab = "BOD (mg/L)",
        col = colors[1:20], las = 1, cex.axis = 0.7)

# ===========================================================
# 7. 방사형차트: 사업소 5곳 수질 프로파일 비교 (0~1 정규화 필요)
# ===========================================================
metric_cols <- c("BOD", "COD", "SS", "TN", "TP", "pH")

site_avg <- sewer %>%
  group_by(site) %>%
  summarise(across(all_of(metric_cols), ~ mean(.x, na.rm = TRUE)))

# 지표별 min-max 정규화 (0~1)
site_norm <- site_avg
site_norm[metric_cols] <- lapply(site_avg[metric_cols], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

# radarchart는 맨 위 두 행에 max/min(1,0) 값이 필요함
# 사업소 5개 -> 3개로 줄이고, 선 두껍게 + 반투명 채우기 + 연한 그리드로
# 눈에 덜 피곤하게 조정
pick_sites <- c("강변사업단", "정관사업소", "수영사업단")
radar_df <- site_norm %>% filter(site %in% pick_sites)
radar_input <- as.data.frame(radar_df[metric_cols])
rownames(radar_input) <- radar_df$site
radar_input <- rbind(rep(1, length(metric_cols)), rep(0, length(metric_cols)), radar_input)

radar_colors <- c("red", "skyblue", "pink")

par(mar = c(2, 2, 3, 6))   # 오른쪽 여백만 살짝
radarchart(radar_input,
           pcol = radar_colors,
           pfcol = adjustcolor(radar_colors, alpha.f = 0.25),  # 반투명 채우기
           plwd = 2, plty = 1,
           cglcol = "grey85", cglty = 1, cglwd = 0.8,           # 그리드 연하게
           axislabcol = "grey40", vlcex = 0.9,
           title = "사업소별 수질 지표 방사형 비교 (정규화값)")
legend(x = 1.2, y = 1, legend = pick_sites, col = radar_colors,
       lty = 1, lwd = 2, cex = 0.8, bty = "n", xpd = TRUE)

par(mar = c(5, 4, 4, 2) + 0.1)   # 다음 그래프들 위해 기본 여백으로 복귀 (중요)

# ===========================================================
# 8. 지도: 사업소 위치 (위도/경도) 산점 + 라벨
# ===========================================================
site_loc <- sewer %>%
  mutate(lon = ifelse(site == "기장사업소 일광", NA, lon)) %>%   # 오염된 경도 제거
  group_by(site, lat, lon) %>%
  summarise(BOD_mean = mean(BOD, na.rm = TRUE),
            flow_mean = mean(flow, na.rm = TRUE),   # 하수량 평균도 같이 집계
            .groups = "drop") %>%
  filter(!is.na(lat), !is.na(lon))   # 좌표 없는 행(기장사업소 등) 제외

plot(site_loc$lon, site_loc$lat,
     main = "사업소 위치 (부산, 좌표만 표시)", xlab = "경도", ylab = "위도",
     pch = 19, col = "darkblue", cex = site_loc$BOD_mean / max(site_loc$BOD_mean) * 3)
text(site_loc$lon, site_loc$lat, labels = site_loc$site,
     pos = 3, cex = 0.6)

# 물방울 모양 아이콘 정의 (Font Awesome "tint")
water_icons <- awesomeIcons(
  icon = "tint",
  library = "fa",
  markerColor = "lightblue"
)

map_view <- leaflet(site_loc, width = "100%", height = "750px") %>%
  addTiles() %>%   # 기본 OSM 지도 배경
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = ~ BOD_mean / max(BOD_mean) * 15,   # BOD 클수록 원 크게
    color = "darkblue", fillOpacity = 0.6,
    group = "원(BOD 크기)",   # 레이어 토글용 그룹명
    popup = ~ paste0(
      "<b>", site, "</b><br>",
      "평균 하수량: ", format(round(flow_mean), big.mark = ","), "<br>",
      "평균 BOD: ", round(BOD_mean, 2)
    )
  ) %>%
  addAwesomeMarkers(
    lng = ~lon, lat = ~lat,
    icon = water_icons,       # 기본 핀 대신 물방울 모양
    group = "핀 마커",         # 레이어 토글용 그룹명
    popup = ~ paste0(
      "<b>", site, "</b><br>",
      "평균 하수량: ", format(round(flow_mean), big.mark = ","), "<br>",
      "평균 BOD: ", round(BOD_mean, 2)
    )
  ) %>%
  addLayersControl(
    overlayGroups = c("원(BOD 크기)", "핀 마커"),
    options = layersControlOptions(collapsed = FALSE)   # 오른쪽 위에 체크박스
  ) %>%
  setView(lng = 129.0756, lat = 35.1796, zoom = 11)

map_view


# ===========================================================
# 9. 워드클라우드: 사업소별 평균 BOD 기준 (관측일수는 사업소마다
#    거의 똑같아서 글자 크기 차이가 안 나므로, 의미 있는 값으로 변경)
# ===========================================================
site_bod <- tapply(sewer$BOD, sewer$site, mean, na.rm = TRUE)
site_bod <- site_bod[!is.na(site_bod)]

wordcloud(words = names(site_bod), freq = round(site_bod * 10),  # 소수값 확대해서 크기차 부각
          min.freq = 1, scale = c(3.5, 0.8),
          random.order = FALSE,   
          rot.per = 0,            
          colors = brewer.pal(8, "Dark2"))

# ===========================================================
# 10. 히트맵 : 사업단 x 수질 지표 히트맵(min-max 정규화)
# ===========================================================
clean_numeric <- function(x) {
  x <- trimws(as.character(x))
  fixed <- vapply(x, function(v) {
    if (is.na(v) || v == "") return(NA_character_)
    if (!grepl("\\.", v)) return(v)
    head_part <- sub("^([^.]*\\.).*$", "\\1", v)
    tail_part <- sub("^[^.]*\\.", "", v)
    tail_part <- gsub("\\.", "", tail_part)
    paste0(head_part, tail_part)
  }, character(1), USE.NAMES = FALSE)
  suppressWarnings(as.numeric(fixed))
}

load_data <- function(path) {
  sewer <- read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  
  names(sewer) <- c("site", "date", "flow", "BOD", "COD", "SS", "TN", "TP", "pH", "lat", "lon")
  
  num_cols <- c("BOD", "COD", "SS", "TN", "TP", "pH")
  sewer[num_cols] <- lapply(sewer[num_cols], clean_numeric)
  sewer$date <- as.Date(sewer$date)
  sewer
}

# 기본 파일 경로 (setwd로 c:/R/sewer 잡아놓고 그 폴더에 sewer.csv 두는 걸 전제로 함)
DEFAULT_PATH <- "sewer.csv"
metric_cols <- c("BOD", "COD", "SS", "TN", "TP", "pH")

# ---------------------------------------------------------
# 2. UI
# ---------------------------------------------------------
ui <- fluidPage(
  titlePanel("사업소 × 수질 지표 히트맵 (min-max 정규화)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "CSV 업로드 (안 하면 기본 파일 사용)",
                accept = ".csv"),
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
# 3. Server
# ---------------------------------------------------------
server <- function(input, output, session) {
  
  raw_data <- reactive({
    path <- if (is.null(input$file)) DEFAULT_PATH else input$file$datapath
    load_data(path)
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

shinyApp(ui, server)


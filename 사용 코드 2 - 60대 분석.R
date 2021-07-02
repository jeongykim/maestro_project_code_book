library(raster)
library(rgdal)
library(leaflet)
library(dplyr)
library(plotly)
# shape 파일로 부터 단계구분도 그리기
# 대전 동구 동별 노인인구수 맵
map <- shapefile('map/LSMD_ADM_SECT_UMD_대전/LSMD_ADM_SECT_UMD_30.shp')
map <- spTransform(map, CRSobj = CRS(
    '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
))
slotNames(map)        # SpatialPolygonsDataFrame

data <- choose.files() # 대전동구동별 노인인구수.csv
dong <- read.csv(data) 
head(dong)

names(dong) <- c('EMD_NM','pop') 
df <- left_join(map@data, dong, by='EMD_NM',sort=F)

View(df)    # 대전 동구 동별 노인인구수를 보여줌
df$pop <- ifelse(is.na(df$pop),0,df$pop)      # 결측치를 0으로 처리
head(df)
map@data <- df

labels <- paste(df$EMD_NM, df$pop, sep=', ')  
pal <- colorNumeric('RdPu', NULL)

leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons(
        fillColor = ~pal(pop), weight = 2, opacity = 1,
        color = 'white', dashArray = '3', fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 5, color = '#777', dashArray = '',
            fillColor = 0.7, bringToFront = T),
        label = labels) %>% 
    addLegend(pal=pal, values = ~pop, opacity = 0.7,
              title='노인인구', position='bottomright')
# 대전 동구의 65세이상 인구를 단계구분도지도에 나타냄
## 결론 가양동, 판암동에 노인인구수가 많다.

# 대전 동구 노인복지시설
old_dong <- choose.files() # 대전동구노인시설.csv
old_dong <- read.csv(old_dong)
data.file <- file.choose()
data.add <- read.csv(data.file, header = T)
data.add %>% head()
View(data.add)

#동구 모든 주택
data.1 <- data.add %>% subset(관할.자치구=="대전광역시 동구")
data.1 %>% head()
data.1 %>% str()
data.1 %>% dim()#146행

#동구 아파트제외 주택
data.2 <- data.add %>% filter(공동주택.구분 != "아파트")%>% subset(관할.자치구=="대전광역시 동구")
data.2 %>% head()
data.2 %>% dim()  #아파트부근은 집값이 비쌀것이라 예상하여 제외해보앗지만 26행이어ㅓ 모든주택을 기준으로함

#install.packages("leaflet")

#동구 전체 주택
fig <- plot_ly(data.1, lat=~lat, lon=~lon, 
               type='densitymapbox', coloraxis='coloraxis',
               radius=10)

fig <- fig %>%
    layout(mapbox=list(style='open-street-map',
                       center=list(lat=mean(data.1$lat), lon= mean(data.1$lon)),
                       zoom=12),
           coloraxis=list(coloraxis='Viridis'))
fig <- plot_ly(data.1, lat=~lat, lon=~lon,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10)
fig <- fig %>% layout(mapbox=list(style="open-street-map",
                                  center=list(lat=36.36, lon=127.38),
                                  zoom=10),
                      coloraxis=list(colorscale="Viridis"))
fig

#아파트만 뽑은 동구
fig <- plot_ly(data.2, lat=~lat, lon=~lon, 
               type='densitymapbox', coloraxis='coloraxis',
               radius=10)

fig <- fig %>%
    layout(mapbox=list(style='open-street-map',
                       center=list(lat=mean(data.2$lat), lon= mean(data.2$lon)),
                       zoom=12),
           coloraxis=list(coloraxis='Viridis'))
fig <- plot_ly(data.2, lat=~lat, lon=~lon,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10)
fig <- fig %>% layout(mapbox=list(style="open-street-map",
                                  center=list(lat=36.36, lon=127.38),
                                  zoom=10),
                      coloraxis=list(colorscale="Viridis"))
fig

#동구 모든주택

fig <- plot_ly(data.a3, lat=~lat, lon=~lon, 
               type='densitymapbox', coloraxis='coloraxis',
               radius=10) 

fig <- fig %>%
    layout(mapbox=list(style='open-street-map',
                       center=list(lat=mean(data.a3$lat), lon= mean(data.a3$lon)),
                       zoom=12),
           coloraxis=list(coloraxis='Viridis'))
fig <- plot_ly(data.a3, lat=~lat, lon=~lon,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10)
fig <- fig %>% layout(mapbox=list(style="open-street-map",
                                  center=list(lat=36.36, lon=127.38),
                                  zoom=10),
                      coloraxis=list(colorscale="Viridis"))
fig

#노인복지시설 시각화
leaflet(data=old_dong) %>%
    setView(lng = mean(old_dong$경도), lat= mean(old_dong$위도), zoom=12) %>%
    addTiles() %>%
    addMarkers(lng = ~경도, lat=~위도,label=~대상시설명)

# 가양동 상권 밀집 지역
data <- choose.files() # 상권정보 데이터
dajeon_market <- read.csv(data)
gayang_market <- dajeon_market %>%
    filter(법정동명=='가양동')
head(gayang_market) # 가양동 상권의 정보
#write.csv(gayang_market,'OpenApi/가양동상권.csv')

# 가양동 상권업종대분류별 변수
df_1 <- gayang_market %>%
    filter(상권업종대분류명=='소매')
df_2 <- gayang_market %>%
    filter(상권업종대분류명=='생활서비스')
df_3 <- gayang_market %>%
    filter(상권업종대분류명=='음식')
df_4 <- gayang_market %>%
    filter(상권업종대분류명=='학문/교육')
df_5 <- gayang_market %>%
    filter(상권업종대분류명=='스포츠')
df_6 <- gayang_market %>%
    filter(상권업종대분류명=='관광/여가/오락')
df_7 <- gayang_market %>%
    filter(상권업종대분류명=='부동산')
df_8 <- gayang_market %>%
    filter(상권업종대분류명=='숙박')

# 가양동 상권업종대분류별 히트맵
fig_1 <- plot_ly(df_1, lat=~위도, lon=~경도,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))    # 소매 히트맵

fig_2 <- plot_ly(df_2, lat=~위도, lon=~경도,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))    # 생활서비스 히트맵

fig_3 <- plot_ly(df_3, lat=~위도, lon=~경도,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))    # 음식 히트맵

fig_4 <- plot_ly(df_4, lat=~위도, lon=~경도,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))    # 학문/교육 히트맵

fig_5 <- plot_ly(df_5, lat=~위도, lon=~경도,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))    # 스포츠 히트맵

fig_6 <- plot_ly(df_6, lat=~위도, lon=~경도,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))    # 관광/여가/오락 히트맵

fig_7 <- plot_ly(df_7, lat=~위도, lon=~경도,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))    # 부동산 히트맵

fig_8 <- plot_ly(df_8, lat=~위도, lon=~경도,
               type="densitymapbox", coloraxis="coloraxis",
               radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))    # 숙박 히트맵

# 가양동 소매 상권 밀집지역
df_1_a <- df_1 %>% 
    filter(위도>36.3483) %>%
    filter(위도<36.35) %>%
    filter(경도>=127.4461) %>%
    filter(경도<127.4478)

    plot_ly(df_1_a, lat=~위도, lon=~경도,
            type="densitymapbox", coloraxis="coloraxis",
            radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))
set.seed(0518)
dj1_samp <- sample_n(df_1_a, size = 10)
dj1_samp$mark <- 'A'

# 가양동 생활서비스 상권 밀집지역
df_2_a <- df_2 %>%
    filter(위도>36.3479) %>%
    filter(위도<36.3502) %>%
    filter(경도>127.4455) %>%
    filter(경도<127.4488)
    
    plot_ly(df_2_a, lat=~위도, lon=~경도,
             type="densitymapbox", coloraxis="coloraxis",
             radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))
set.seed(0518)
dj2_samp <- sample_n(df_2_a, size = 10)
dj2_samp$mark <- 'B'

# 가양동 음식 상권 밀집지역 포인트1
df_3_a <- df_3 %>%
    filter(경도>127.4461) %>%
    filter(경도<127.4482) %>%
    filter(위도>36.3483) %>%
    filter(위도<36.3519) 
    
    plot_ly(df_3_a, lat=~위도, lon=~경도,
             type="densitymapbox", coloraxis="coloraxis",
             radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))
set.seed(0518)
dj3_samp <- sample_n(df_3_a, size = 5)
dj3_samp$mark <- 'C'

# 가양동 음식 상권 밀집지역 포인트2
df_3_a <- df_3 %>%
    filter(경도>127.4494) %>%
    filter(경도<127.4548) %>%
    filter(위도>36.3477) %>%
    filter(위도<36.3495) 
plot_ly(df_3_a, lat=~위도, lon=~경도,
        type="densitymapbox", coloraxis="coloraxis",
        radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))
set.seed(0518)
dj3_samp <- sample_n(df_3_a, size = 5)
dj3_samp$mark <- 'C'

# 가양동 학문/교육상권 밀집지역
df_4_a <- df_4 %>%
    filter(경도>127.4474) %>%
    filter(경도<127.4522) %>%
    filter(위도>36.3483) %>%
    filter(위도<36.3539) 

plot_ly(df_4_a, lat=~위도, lon=~경도,
        type="densitymapbox", coloraxis="coloraxis",
        radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))
set.seed(0518)
dj4_samp <- sample_n(df_4_a, size = 10)
dj4_samp$mark <- 'D'

# 가양동 관광/여가/오락상권 밀집지역
df_6_a <- df_6 %>%
    filter(경도>127.4411) %>%
    filter(경도<127.4533) %>%
    filter(위도>36.3429) %>%
    filter(위도<36.3511) 

plot_ly(df_6_a, lat=~위도, lon=~경도,
        type="densitymapbox", coloraxis="coloraxis",
        radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))
set.seed(0518)
dj6_samp <- sample_n(df_6_a, size = 10)
dj6_samp$mark <- 'E'

# 가양동 부동산상권 밀집지역
df_7_a <- df_7 %>%
    filter(경도>127.4464) %>%
    filter(경도<127.4530) %>%
    filter(위도>36.3460) %>%
    filter(위도<36.3505) 

plot_ly(df_7_a, lat=~위도, lon=~경도,
        type="densitymapbox", coloraxis="coloraxis",
        radius=10) %>%
    layout(mapbox=list(style="open-street-map",
                       center=list(lat=36.36, lon=127.38),
                       zoom=10),
           coloraxis=list(colorscale="Viridis"))
set.seed(0518)
dj7_samp <- sample_n(df_7_a, size = 10)
dj7_samp$mark <- 'F'

dj <- rbind(dj1_samp,dj2_samp,dj3_samp,dj4_samp,dj6_samp,dj7_samp) # 각 상권 합
# marker
# 포인트 내에 대분류명 기준으로 10개씩 랜덤 샘플링으로 추출한 것을 마커
getwd()     # 파일 경로 탐색
# 라벨링 작업
leafIcons <- icons(
    iconUrl = ifelse(dj$mark == 'A',
                     "data/소매.png",
                     ifelse(dj$mark == 'B',
                            "data/생활.png",
                            ifelse(dj$mark == 'C', 
                                   "data/음식.png" ,
                                   ifelse(dj$mark == 'D',
                                          "data/학문.png",
                                          ifelse(dj$mark == 'E',
                                                 "data/관광.png",
                                                 "data/부동산.png"))))),
    iconWidth = 150, iconHeight = 150
)
leaflet(dj) %>% addTiles() %>%
    addMarkers(~경도, ~위도, icon = leafIcons, popup = ~상호명) 

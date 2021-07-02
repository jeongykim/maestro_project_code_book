#상권정보가공데이터
#install.packages('ggmap')
library(ggmap)
library(dplyr)
#install.packages('tidyverse')
#install.packages('plotly')
library(tidyverse)
library(plotly)
library(gapminder)
library(RColorBrewer)
library(leaflet)

# df4----데이터 전처리과정------
# df2 <- df[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]
# names(df2)
# df2$zero_9 <- df2$X0.4세+df2$X5.9세
# df2$ten_19 <- df2$X10.14세+df2$X15.19세
# df2$twenty_29 <- df2$X20.24세+df2$X25.29세
# df2$thirty_39 <- df2$X30.34세+df2$X35.39세
# df2$forty_49 <- df2$X40.44세+df2$X45.49세
# df2$fifty_59 <- df2$X50.54세+df2$X55.59세
# df2$sixty_100 <- df2$X60.64세+df2$X65.69세+df2$X70.74세+df2$X75.79세+df2$X80.84세+df2$X85.89세+df2$X90.94세+df2$X95.99세+df2$X100세.이상
# df3 <- df2[,c(23,24,25,26,27,28,29)]
# df3
# df4<- cbind(df$X,df3)
# df4 <- df4[-1,]
# names(df4)[1] ="지역"
# write.csv(df4)
# write.csv(df4, 'OpenAPI/인구비율데이터.csv', row.names = F)

#데이터 정제 후 인구비율데이터를 가져옴
data <- choose.files()
df4<- read.csv(data)

df4_1 <- df4[c(1,18,36,60,72),] #구 전체의 인구수
ggplot(df4_1, aes(지역, ten_19))+geom_bar(stat='identity')
df4_2 <- df4[c(37:59),] #서구 내의 10대 인원수 # 둔산 월평 관저에 10대가 가장 많은 것을 파악
ggplot(df4_2, aes(지역, ten_19))+geom_bar(stat='identity') +coord_flip()



#상권정보가공데이터 불러오기
data2 <- choose.files()
dj<- read.csv(data2)
##### 상권 현황 파악
head(dj)
dj2 <- dj[grep("대전광역시 서구 관저", dj$도로명),] # 관저동
dj2_2 <- dj[grep("대전광역시 서구 둔산", dj$도로명),]
dj2_3 <- dj[grep("대전광역시 서구 월평", dj$도로명),]
dj2_4 <- rbind(dj2,dj2_2,dj2_3) # 관저, 둔산, 월평 

#히트맵으로 상권 형성 알아보기
unique(dj2_4$상권업종대분류명)
dj3_life <- dj2_4 %>% subset(상권업종대분류명=='생활서비스')
fig1 <- plot_ly(dj3_life, lat=~위도, lon=~경도,
        type='densitymapbox',coloraxis='coloraxis',radius=10)
fig1 <- fig1 %>% layout(mapbox=list(style='open-street-map',
                                   center=list(lat=36.33,lon=127.36),
                                   zoom=10),
                                   coloraxis=list(colorscale="Viridis"))
unique(dj2_4$상권업종대분류명)
dj3_food <- dj2_4 %>% subset(상권업종대분류명=='음식')
fig2 <- plot_ly(dj3_food, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig2 <- fig2 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))

unique(dj2_4$상권업종대분류명)
dj3_fancy <- dj2_4 %>% subset(상권업종대분류명=='소매')
fig3 <- plot_ly(dj3_fancy, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig3 <- fig3 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))
unique(dj2_4$상권업종대분류명)
dj3_enjoy <- dj2_4 %>% subset(상권업종대분류명=='관광/여가/오락')
fig4 <- plot_ly(dj3_enjoy, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig4 <- fig4 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))
unique(dj2_4$상권업종대분류명)
dj3_sport <- dj2_4 %>% subset(상권업종대분류명=='스포츠')
fig5 <- plot_ly(dj3_sport, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig5 <- fig5 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))
dj3_hotel <- dj2_4 %>% subset(상권업종대분류명=='숙박')
fig6 <- plot_ly(dj3_hotel, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig6 <- fig6 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))
dj3_house <- dj2_4 %>% subset(상권업종대분류명=='부동산')
fig7 <- plot_ly(dj3_house, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig7 <- fig7 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))
dj3_study <- dj2_4 %>% subset(상권업종대분류명=='학문/교육')
fig8 <- plot_ly(dj3_study, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig8 <- fig8 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))

#관저 포인트 1 색출 완료
dj4 <- dj2_4 %>% filter((경도 >=127.3314 & 경도 <=127.33427) & (위도 >=36.3003 & 위도 <=36.3012))
fig9 <- plot_ly(dj4, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig9 <- fig9 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))

#관저 포인트 2 색출 완료
dj4_1 <- dj2_4 %>% filter((경도 >=127.3351 & 경도 <=127.3425) & (위도 >=36.2965 & 위도 <=36.301))
fig10 <- plot_ly(dj4_1, lat=~위도, lon=~경도,
                type='densitymapbox',coloraxis='coloraxis',radius=10)
fig10 <- fig10 %>% layout(mapbox=list(style='open-street-map',
                                    center=list(lat=36.33,lon=127.36),
                                    zoom=10),
                        coloraxis=list(colorscale="Viridis"))

#관저 포인트 3 색출
dj4_2 <- dj2_4 %>% filter((경도 >=127.3354 & 경도 <=127.3383) & (위도 >=36.302 & 위도 <=36.304))
fig11 <- plot_ly(dj4_2, lat=~위도, lon=~경도,
                 type='densitymapbox',coloraxis='coloraxis',radius=10)
fig11 <- fig11 %>% layout(mapbox=list(style='open-street-map',
                                      center=list(lat=36.33,lon=127.36),
                                      zoom=10),
                          coloraxis=list(colorscale="Viridis"))

#둔산&월평 포인트 1 색출
dj4_3 <- dj2_4 %>% filter((경도 >=127.3528 & 경도 <=127.3705) & (위도 >=36.351 & 위도 <=36.362))
fig12 <- plot_ly(dj4_3, lat=~위도, lon=~경도,
                 type='densitymapbox',coloraxis='coloraxis',radius=10)
fig12 <- fig12 %>% layout(mapbox=list(style='open-street-map',
                                      center=list(lat=36.33,lon=127.36),
                                      zoom=10),
                          coloraxis=list(colorscale="Viridis"))

#둔산&월평 포인트 2 색출
dj4_4 <- dj2_4 %>% filter((경도 >=127.3745 & 경도 <=127.393) & (위도 >=36.349 & 위도 <=36.356))
fig13 <- plot_ly(dj4_4, lat=~위도, lon=~경도,
                 type='densitymapbox',coloraxis='coloraxis',radius=10)
fig13 <- fig13 %>% layout(mapbox=list(style='open-street-map',
                                      center=list(lat=36.33,lon=127.36),
                                      zoom=10),
                          coloraxis=list(colorscale="Viridis"))

#둔산&월평 포인트 3 색출
dj4_5 <- dj2_4 %>% filter((경도 >=127.3958 & 경도 <=127.400) & (위도 >=36.3487 & 위도 <=36.3529))
fig14 <- plot_ly(dj4_5, lat=~위도, lon=~경도,
                 type='densitymapbox',coloraxis='coloraxis',radius=10)
fig14 <- fig14 %>% layout(mapbox=list(style='open-street-map',
                                      center=list(lat=36.33,lon=127.36),
                                      zoom=10),
                          coloraxis=list(colorscale="Viridis"))

# 관저 1~3포인트 , 둔산&월평 1~3포인트 결합
dj5 <- rbind(dj4,dj4_1,dj4_2,dj4_3,dj4_4,dj4_5)
dim(dj2_4) #기존  7058 행
dim(dj5)   #포인트 도출 후 4942 행으로 좁혀짐

##############################################################
####관저동 상권업소대분류명 기준 50개 뽑기

dj_guan <- rbind(dj4,dj4_1,dj4_2) #관저동 1719개

# 서두에서 스포츠에 대한 입지시설은 별로 없으며, 부동산, 숙박은 10대가 크게 사용을 못하기 때문에 분석 목록에서 제외
dj_guan_1 <- dj_guan %>% subset(상권업종대분류명 != "스포츠")
dj_guan_2 <- dj_guan_1 %>% subset(상권업종대분류명 != "부동산")
dj_guan_3 <- dj_guan_2 %>% subset(상권업종대분류명 != "숙박")
unique(dj_guan_3$상권업종대분류명)
dim(dj_guan_3)   #스포츠, 부동산, 숙박 제거 후 관저동 1679개 행으로 좁혀짐

# 관저동 각 대분류명으로 랜덤 10개 추출
dj_guan_fancy <- dj_guan_3 %>% subset(상권업종대분류명 == '소매')
set.seed(0518)
dj_guan_fancy_samp <- sample_n(dj_guan_fancy, size = 10)
dj_guan_fancy_samp$mark <- 'A'

dj_guan_study <- dj_guan_3 %>% subset(상권업종대분류명 == '학문/교육')
set.seed(0518)
dj_guan_study_samp <- sample_n(dj_guan_study, size = 10)
dj_guan_study_samp$mark <- 'B'

dj_guan_food <- dj_guan_3 %>% subset(상권업종대분류명 == '음식')
set.seed(0518)
dj_guan_food_samp <- sample_n(dj_guan_food, size = 10)
dj_guan_food_samp$mark <- 'C'

dj_guan_service <- dj_guan_3 %>% subset(상권업종대분류명 == '생활서비스')
set.seed(0518)
dj_guan_service_samp <- sample_n(dj_guan_service, size = 10)
dj_guan_service_samp$mark <- 'D'

dj_guan_tour <- dj_guan_3 %>% subset(상권업종대분류명 == '관광/여가/오락')
set.seed(0518)
dj_guan_tour_samp <- sample_n(dj_guan_tour, size = 10)
dj_guan_tour_samp$mark <- 'E'

dj7 <- rbind(dj_guan_fancy_samp,dj_guan_study_samp,dj_guan_food_samp,dj_guan_service_samp,dj_guan_tour_samp)
dj7
# 각 뽑힌 것들 마커로 뽑기 
getwd()
# 라벨링 작업
leafIcons <- icons(
  iconUrl = ifelse(dj7$mark == 'A',
                   "C:/Workspace/R/data/소매.png",
                   ifelse(dj7$mark == 'B',
                          "C:/Workspace/R/data/학문.png",
                          ifelse(dj7$mark == 'C', 
                                 "C:/Workspace/R/data/음식.png" ,
                                 ifelse(dj7$mark == 'D',
                                        "C:/Workspace/R/data/생활.png",
                                        "C:/Workspace/R/data/관광.png")))),
  iconWidth = 100, iconHeight = 100
)

leaflet(dj7) %>% addTiles() %>%
  addMarkers(~경도, ~위도, icon = ~leafIcons, popup = ~상호명) 


##############################################################
# 관저와 같은 방법으로 둔산지구 TOP50지역 뽑기

dj_dunsan <- rbind(dj4_3,dj4_4,dj4_5) #둔산월평 3223개

dj_dunsan_1 <- dj_dunsan %>% subset(상권업종대분류명 != "스포츠")
dj_dunsan_2 <- dj_dunsan_1 %>% subset(상권업종대분류명 != "부동산")
dj_dunsan_3 <- dj_dunsan_2 %>% subset(상권업종대분류명 != "숙박")
unique(dj_dunsan_3$상권업종대분류명)
dim(dj_dunsan_3)   #스포츠, 부동산, 숙박 제거 후 둔산&월평동 3095개 행으로 좁혀짐

# 관저동 각 대분류명으로 랜덤 10개 추출
dj_dunsan_fancy <- dj_dunsan_3 %>% subset(상권업종대분류명 == '소매')
set.seed(0518)
dj_dunsan_fancy_samp <- sample_n(dj_dunsan_fancy, size = 10)
dj_dunsan_fancy_samp$mark <- 'A'

dj_dunsan_study <- dj_dunsan_3 %>% subset(상권업종대분류명 == '학문/교육')
set.seed(0518)
dj_dunsan_study_samp <- sample_n(dj_dunsan_study, size = 10)
dj_dunsan_study_samp$mark <- 'B'

dj_dunsan_food <- dj_dunsan_3 %>% subset(상권업종대분류명 == '음식')
set.seed(0518)
dj_dunsan_food_samp <- sample_n(dj_dunsan_food, size = 10)
dj_dunsan_food_samp$mark <- 'C'

dj_dunsan_service <- dj_dunsan_3 %>% subset(상권업종대분류명 == '생활서비스')
set.seed(0518)
dj_dunsan_service_samp <- sample_n(dj_dunsan_service, size = 10)
dj_dunsan_service_samp$mark <- 'D'

dj_dunsan_tour <- dj_dunsan_3 %>% subset(상권업종대분류명 == '관광/여가/오락')
set.seed(0518)
dj_dunsan_tour_samp <- sample_n(dj_dunsan_tour, size = 10)
dj_dunsan_tour_samp$mark <- 'E'

dj8 <- rbind(dj_dunsan_fancy_samp,dj_dunsan_study_samp,dj_dunsan_food_samp,dj_dunsan_service_samp,dj_dunsan_tour_samp)
dj8
# 각 뽑힌 것들 마커로 뽑기 
getwd()
# 라벨링 작업
leafIcons2 <- icons(
  iconUrl = ifelse(dj8$mark == 'A',
                   "C:/Workspace/R/data/소매.png",
                   ifelse(dj8$mark == 'B',
                          "C:/Workspace/R/data/학문.png",
                          ifelse(dj8$mark == 'C', 
                                 "C:/Workspace/R/data/음식.png" ,
                                 ifelse(dj8$mark == 'D',
                                        "C:/Workspace/R/data/생활.png",
                                        "C:/Workspace/R/data/관광.png")))),
  iconWidth = 100, iconHeight = 100
)

leaflet(dj8) %>% addTiles() %>%
  addMarkers(~경도, ~위도, icon = ~leafIcons2, popup = ~상호명) 



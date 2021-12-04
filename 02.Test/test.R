test_data <- read.csv("/Users/ito_hiroki/01.Research/Coloring-Map/03.RawData/FEH_00450021_211202103143.csv", header = T, fileEncoding = "cp932")


jpsf <- sf::read_sf("/Users/ito_hiroki/01.Research/Coloring-Map/03.RawData/japan_ver83/japan_ver83.shp")

length(jpsf$geometry)


jpsf$KEN <- as.character(jpsf$KEN)
prefecture_list <- unique(jpsf$KEN)

hokka <- jpsf %>% filter(KEN == "北海道")
hokka_list = list() 

append(hokka_list,hokka$geometry[1])

jpsf_geometry <- jpsf$geometry

print(jpsf_geometry[1])

hokka$geometry[1]

for (i in 1:194){
  hokka$geometry[i]
}

data("World")
tm_shape(World) + tm_polygons("HPI")

spplot(jpsf,"SHAPE_area")

# boston <- readOGR("/Users/ito_hiroki/01.Research/Coloring-Map/03.RawData/boston_police_districts_f55.shp")
boston <- st_read(dsn ="/Users/ito_hiroki/01.Research/Coloring-Map/03.RawData/11-00091_Shapefiles")

boston_f <- fortify(boston)

ggplot(boston_f, aes(x = long, y = lat, group = group, color = group)) +
  geom_polygon() +
  geom_point() + 
  coord_equal()  + 
  scale_fill_gradient(low = "white", high = "black") +
  theme(legend.position="none")

tm_shape(boston) + tm_polygons("SHAPE_area")

tm_shape(boston) + tm_polygons("SHAPE_area")



sfdata <- left_join(jpsf, temp_m, by = "JCODE" )

jpsf <- fortify(jpsf)

tm_shape(jpsf) + 
  tm_polygons("P_NUM") +
  tmap_options(check.and.fix = TRUE)

hospital_per_person <- test_data %>% select(都道府県_005,令和元年.2019年..人口10万対.)

hospital_per_person <- hospital_per_person %>% filter(都道府県_005 != "全　国")


world_boundry <- st_read("/Users/ito_hiroki/01.Research/Color-Map/03.RawData/ne_10m_admin_1_states_provinces")

japan_boundry <- world_boundry %>% subset(world_shape$geonunit == "Japan")

tm_shape(japan_boundry) + 
  tm_polygons("longitude") +
  tmap_options(check.and.fix = TRUE)

local_capital <- read.csv("/Users/ito_hiroki/01.Research/Color-Map/04.TidyData/kenchou-shozaichi.csv", header = F, fileEncoding = "cp932")

colnames(local_capital) <- c("name_local","city")

japan_boundry <- japan_boundry %>% select(name_local,latitude,longitude)

join_data <- japan_boundry %>% left_join(local_capital,by = "name_local")

prefecture_name <- local_capital$name_local

japan_boundry_order <- japan_boundry %>% mutate(prefecture_name)

japan_boundry_order <- japan_boundry_order %>% relocate(prefecture_name)

japan_boundry_order <- japan_boundry %>% arrange(prefecture_name)

dentist_data <- read.csv("/Users/ito_hiroki/01.Research/Color-Map/04.TidyData/FEH_00450021_211203135022.csv", header = T, fileEncoding = "cp932")

dentist_data <- dentist_data %>% select(都道府県,令和元年.2019年..人口10万対.)

colnames(dentist_data) <- c("name_local","number")


colnames(dentist_data)

correct_name <- local_capital$name_local

dentist_data$name_local <- c(correct_name)

innerdata <- inner_join(japan_boundry,dentist_data,by = "name_local")

innerdata <- fortify(innerdata)

tm_shape(innerdata) + 
  tm_polygons("number") +
  tmap_options(check.and.fix = TRUE)











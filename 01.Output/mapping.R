####################################################################################
####################################################################################

#パッケージ
library(ggplot2)
library(tmap)
library(tmaptools)
library(tidyverse)
library(shinyjs)
library(sf)



#日本地図データ
world_boundary <- st_read("/Users/ito_hiroki/01.Research/Color-Map/03.RawData/ne_10m_admin_1_states_provinces")

japan_boundary <- world_boundary %>%
  dplyr::filter(geonunit == "Japan") %>%
  dplyr::select(name_local,latitude,longitude) %>% 
  dplyr::mutate(complete_name = name_local,
                complete_name = replace(
                  complete_name,
                  is.na(complete_name) == TRUE,
                  "静岡県"))

####################################################################################
####################################################################################
#上をコピー&ペーストして54行目"defalt_map"を実行すれば日本地図の外枠が完成します。



#人口密度データ
pop_density <- read.csv("/Users/ito_hiroki/01.Research/Color-Map/04.TidyData/FEH_00200521_211209120122.csv",
                       header = T,fileEncoding = "cp932")

pop_density <- pop_density %>% 
  select(全国.都道府県.市区町村.2000年市区町村含む.,
           人口密度.1km2当たり.)
colnames(pop_density) <- c("complete_name","pop_density")

#データ内の","を""に変換する　(","が含まれていると数値として扱えないため) 
pop_density$pop_density <- str_replace(pop_density$pop_density, pattern=",", replacement="")
num_density <-as.data.frame(apply(pop_density,2,as.numeric))
num_density$complete_name <- pop_density$complete_name


#2つのデータをくっつける
joint_data_density <- inner_join(japan_boundary,pop_density,by = "complete_name")


#地図の塗り分け

#外枠
defalt_map <-  tm_shape(joint_data_density) +
  tm_borders()

defalt_map

#数値のみ代入
map <-  defalt_map + 
  tm_fill(col = "pop_density") +
  tmap_options_reset()

map

#いくつか追加
mod_map <- defalt_map +
  tm_fill(
    col =  "pop_density",
    style = "fixed",
    title = "density",
    breaks = c(0,50,100,200,300,400,500,600,700,800,900,1000,1500,2000),
    palette = "PuBu") 

mod_map


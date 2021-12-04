preamble <- function(){

setwd("~/Rdirectory/")  
  
library(readr)

library(heatmaply)

library(tidyverse)

library(dplyr)

library(readr)

library(ggplot2)

library(moments) 

library(sf)

library(tmap)

library(tmaptools)

library(ggpubr)
  
library(xtable)

}

initial_year <- 13

last_year <- 20

dataloading_native <- function(){
  
  for (i in initial_year:last_year) {
  
#native
  
# data loading
  
  temp <- read_csv( paste("~/Rdirectory/Data/native/",i,"07nsjin.csv",sep = ""), locale = locale( encoding =  "Shift-JIS") )
  
  # data cleaning
  
  temp$"団体コード" <- as.numeric(temp$"団体コード")
  temp$"人口"[temp$"人口" == 0] <- NA 
  temp <- na.omit(temp) 
  temp$"団体コード" <- as.numeric(gsub(".{1}$", "", temp$"団体コード")) 
  temp <- dplyr::rename( temp, JCODE = 団体コード ) 
  temp <- rename(temp, !!paste("pop_20",i,sep = "") := 人口)
  
  assign( paste("native",i,sep = ""), temp )
  
  rm(temp)
  
  }
}
  
dataloading_overseas <- function(){
  
  for (i in initial_year:last_year) {
  
#overseas
  
  #data loading
  
  temp <- read_csv( paste("~/Rdirectory/Data/overseas/",i,"11gsjin.csv",sep = ""), locale = locale(encoding = "Shift-JIS")  )
  
  #data cleaning
  
  temp$"団体コード" <- as.numeric(temp$"団体コード")
  temp$"人口"[temp$"人口" == 0] <- NA 
  temp <- na.omit(temp) 
  temp$"団体コード" <- as.numeric(gsub(".{1}$", "", temp$"団体コード")) 
  temp <- dplyr::rename( temp, JCODE = 団体コード ) 
  temp <- rename(temp, !!paste("pop_20",i,sep = "") := 人口)
  
  assign( paste("overseas",i,sep = ""), temp )
  
  rm(temp)
  
  }
  
  # overseas13 has no (social) rate of change
  
  for (i in (initial_year+1):last_year) {
    
    temp <- get(paste("overseas",i,sep = ""))
    
    temp$"増減率" <- as.numeric(temp$"増減率")
    temp$"社会増減率" <- as.numeric(temp$"社会増減率")
    
    assign(paste("overseas",i,sep = ""), temp)
    
    rm(temp)
    
  }
  
}

#overseas population 

for (i in initial_year:last_year) {
  
  temp <- get(paste("overseas",i,sep = ""))
  
  temp <- mutate(temp, transfer_moveout_ratio = ( temp$"転出者数（計）" / temp$"転入者数（計）" ) )
  
  #all
  plot_transfer_all <- ggplot(data = temp, mapping = aes(`転出者数（計）`,`転入者数（計）`)) +
    labs(x = paste0("move-in_overseas",i), y = paste0("move-out_overseas",i)) +
    geom_smooth(method = "lm") +
    geom_point()
  
  #inside
  plot_transfer_inside <- ggplot(data = temp, mapping = aes(`転出者数（国内）`,`転入者数（国内）`)) +
    labs(x = paste0("move-in_overseas_inside",i), y = paste0("move-out_overseas_inside",i)) +
    geom_smooth(method = "lm") +
    geom_point()
  
  #outside
  plot_transfer_outside <- ggplot(data = temp, mapping = aes(`転出者数（国外）`,`転入者数（国外）`)) +
    labs(x = paste0("move-in_overseas_outside",i), y = paste0("move-out_overseas_outside",i)) +
    geom_smooth(method = "lm") +
    geom_point()
  
  #distribution of ratio
  plot_hist_ratio <- ggplot(data = temp, mapping = aes(transfer_moveout_ratio)) +
    geom_histogram(binwidth=0.01,fill="sky blue",bins = 500,aes(y= after_stat(density))) +
    geom_density()
  
  #save
  
  ggsave(paste0("figures/overseasplots/movein_moveout_all/overseas",i,"_transfer.pdf"), plot = plot_transfer_all, dpi = 1000, height = 9, width = 16)
  
  ggsave(paste0("figures/overseasplots/movein_moveout_inside/overseas",i,"_transfer_inside.pdf"), plot = plot_transfer_inside, dpi = 1000, height = 9, width = 16)
  
  ggsave(paste0("figures/overseasplots/movein_moveout_outside/overseas",i,"_transfer_outside.pdf"), plot = plot_transfer_outside, dpi = 1000, height = 9, width = 16)
  
  ggsave(paste0("figures/overseasplots/distribution_of_transfer_moveout_ratio/overseas",i,"_transfer_moveout_ratio.pdf"),plot = plot_hist_ratio, dpi = 1000, height = 9, width = 16)
  
  assign(paste0("transfer_overseas",i),plot_transfer_all )
  
  assign(paste0("plot_hist_ratio_overseas",i),plot_hist_ratio )
  
}

p_summary <- ggpubr::ggarrange()

#native

for (i in initial_year:last_year) {
  
  temp <- get(paste("native",i,sep = ""))
  temp <- mutate(temp, transfer_moveout_ratio = ( temp$"転出者数（計）" / temp$"転入者数（計）" ) )
  
  plot_transfer <- ggplot(data = temp, mapping = aes(`転出者数（計）`,`転入者数（計）`)) +
    labs(x = paste0("transfer_native",i), y = paste0("move-out_native",i)) +
    geom_point()
  
  plot_hist_ratio <- ggplot(data = temp, mapping = aes(transfer_moveout_ratio)) +
    geom_histogram(binwidth=0.01)
  
  ggsave(paste0("figures/nativeplots/native",i,"_transfer.pdf"), plot = plot_transfer, dpi = 1000, height = 9, width = 16)
  
  ggsave(paste0("figures/nativeplots/native",i,"_transfer_moveout_ratio.pdf"),plot = plot_hist_ratio, dpi = 1000, height = 9, width = 16)
  
}

#population ratio

for (i in initial_year:last_year) {
  
  temp1 <- get(paste0("overseas",i))
  temp1 <- rename(temp1, pop_overseas = paste0("pop_20",i))
  
  temp2 <- get(paste0("native",i))
  temp2 <- rename(temp2, pop_native = paste0("pop_20",i))
  
  temp_m <- merge(temp1,temp2, by = "JCODE")
  
  temp_m <- mutate(temp_m, population_ratio = (temp_m$pop_overseas / temp_m$pop_native) )
  temp_m <- mutate(temp_m, population_ratio_overseas = (temp_m$pop_overseas / (temp_m$pop_overseas + temp_m$pop_native)))
  temp_m <- mutate(temp_m, population_ratio_native = (temp_m$pop_native / (temp_m$pop_overseas + temp_m$pop_native)))
  temp_m <- mutate(temp_m, log_population_ratio = (log10(temp_m$pop_overseas + 1) - log10(temp_m$pop_native + 1)))
  temp_m <- mutate(temp_m, log_population_ratio_overseas = (log10(temp_m$pop_overseas + 1) - log10(temp_m$pop_native + temp_m$pop_overseas + 1)))
  temp_m <- mutate(temp_m, log_population_ratio_native = (log10(temp_m$pop_native + 1) - log10(temp_m$pop_native + temp_m$pop_overseas + 1)))
  
  
  dist_of_pop_ratio <- ggplot(data = temp_m, aes( population_ratio )) + 
    geom_histogram(binwidth=0.001,fill="sky blue",bins = 500,aes(y= after_stat(density))) +
    geom_density() +
    labs(x = paste0("distribution_of_population_ratio",i))
  
  dist_of_pop_ratio_log <- ggplot(data = temp_m, aes( log_population_ratio )) + 
    geom_histogram(binwidth=0.01,fill="sky blue",bins = 500,aes(y= after_stat(density))) +
    geom_density() +
    labs(x = paste0("distribution_of_population_ratio_log",i))
  
  ggsave(paste0("distribution_of_population_ratio",i,".pdf"),plot = dist_of_pop_ratio, dpi = 1000, height = 9, width = 16)
  
  ggsave(paste0("distribution_of_log10_population_ratio",i,".pdf"),plot = dist_of_pop_ratio_log, dpi = 1000, height = 9, width = 16)
  
    temp_m <- rename(temp_m, !!paste0("pop_overseas",i) := pop_overseas)
    temp_m <- rename(temp_m, !!paste0("pop_native",i) := pop_native)
    temp_m <- rename(temp_m, !!paste0("population_ratio",i) := population_ratio)
    temp_m <- rename(temp_m, !!paste0("population_ratio_native",i) := population_ratio_native)
    temp_m <- rename(temp_m, !!paste0("population_ratio_overseas",i) := population_ratio_overseas)
    temp_m <- rename(temp_m, !!paste0("log_population_ratio",i) := log_population_ratio)
    temp_m <- rename(temp_m, !!paste0("log_population_ratio_native",i) := log_population_ratio_native)
    temp_m <- rename(temp_m, !!paste0("log_population_ratio_overseas",i) := log_population_ratio_overseas)
    
  assign(paste0("merged_data",i), temp_m)
  
}

for (i in initial_year:(last_year-1)) {
  
  temp1 <- get(paste0("merged_data",i))
  
  temp2 <- get(paste0("merged_data",i+1))
  
  temp_m <- merge(temp1,temp2, by = "JCODE")
  
  temp_m <- mutate(temp_m, difference_in_log_population_ratio = get(paste0("log_population_ratio",i+1)) - get(paste0("log_population_ratio",i)))
  
  diff_plot <- ggplot(temp_m, aes(x = get(paste0("log_population_ratio",i)), y = difference_in_log_population_ratio)) +
    geom_smooth(method = "lm") +
    geom_point() +
    labs(x = paste0("log_population_ratio",i),y = paste0("difference_in_log_population_ratio",i+1,"-",i))
    
  ggsave(paste0("difference_in_log_population_ratio_",i+1,"-",i,".pdf"), plot = diff_plot, dpi = 1000, height = 9, width = 16)
 
  #----- mapping
   
  sfTemp <- sf::read_sf("Data/japan_ver83/japan_ver83.shp")
  
  sfTemp$JCODE <- as.numeric(sfTemp$JCODE)
  
  sfdata <- left_join( sfTemp, temp_m, by = "JCODE" )
  
  tmap_mode("view")
  
  #plot map
  
  # plot map
  
  tm1 <- tm_shape(sfdata) +
    tm_grid(labels.inside.frame = FALSE) +
    tm_fill(
      col =  "difference_in_log_population_ratio",
      style = "fixed",
      breaks = c(-1,-0.05,-0.03,-0.02,-0.01,-0.005,0,0.005,0.01,0.02,0.03,0.05,1),
      palette = "OrRd",
      midpoint = 0,
      title = paste("difference in log population ratio ",i+1," - ",i, sep = ""),
      popup.vars = c( "Prefecture:" = "都道府県名.x.x",
                      "City: " = "市区町村名.x.x",
                      "difference in log population ratio: " = "difference_in_log_population_ratio" )  ,
      n = 6,
      legend.format = list(format = "f", digits = 4)
    ) + 
    tmap_options(check.and.fix = TRUE) +
    tm_borders(col = "black", lwd = 0.001) +
    tm_layout(legend.outside = TRUE, 
              legend.outside.position = "right", 
              fontfamily = "sans")
  
  # save
  
  tmap_save(tm1, paste("~/Rdirectory//plot_logpopratio(20",i+1,"-20",i,").pdf",sep = ""))
  
}

# plot map

for (i in initial_year:last_year) {
  
  temp1 <- get(paste0("overseas",i))
  temp1 <- rename(temp1, pop_overseas = paste0("pop_20",i))
  
  temp2 <- get(paste0("native",i))
  temp2 <- rename(temp2, pop_native = paste0("pop_20",i))
  
  temp_m <- merge(temp1,temp2, by = "JCODE")
  
  # variables definition
  
  temp_m <- mutate(temp_m, population_ratio = (temp_m$pop_overseas / temp_m$pop_native) )
  temp_m <- mutate(temp_m, population_ratio_overseas = (temp_m$pop_overseas / (temp_m$pop_overseas + temp_m$pop_native)))
  temp_m <- mutate(temp_m, population_ratio_native = (temp_m$pop_native / (temp_m$pop_overseas + temp_m$pop_native)))
  temp_m <- mutate(temp_m, log_population_ratio = (log10(temp_m$pop_overseas + 1) - log10(temp_m$pop_native + 1)))
  temp_m <- mutate(temp_m, log_population_ratio_overseas = (log10(temp_m$pop_overseas + 1) - log10(temp_m$pop_native + temp_m$pop_overseas + 1)))
  temp_m <- mutate(temp_m, log_population_ratio_native = (log10(temp_m$pop_native + 1) - log10(temp_m$pop_native + temp_m$pop_overseas + 1)))
  
  sfTemp <- sf::read_sf("Data/japan_ver83/japan_ver83.shp")
  
  sfTemp$JCODE <- as.numeric(sfTemp$JCODE)
  
  sfdata <- left_join( sfTemp, temp_m, by = "JCODE" )
  
  tmap_mode("view")
  
  #plot map
  
  tm1 <- tm_shape(sfdata) +
    tm_grid(labels.inside.frame = FALSE) +
    tm_fill(
      col =  "log_population_ratio_overseas",
      style = "fixed",
      breaks = c(-4,-3,-2.9,-2.8,-2.7,-2.6,-2.5,-2.4,-2.3,-2.2,-2.1,-2,-1.9,-1.8,-1.7,-1.6,-1.5,-1.4,-1.3,-1.2,-1.1,-1,-0.5,0),
      palette = "OrRd",
      midpoint = -2,
      title = paste("log population ratio overseas",i, sep = ""),
      popup.vars = c( "Prefecture:" = "都道府県名.x",
                      "City: " = "市区町村名.x",
                      "log pop ratio overseas: " = "log_population_ratio_overseas" )  ,
      n = 6,
      legend.format = list(format = "f", digits = 4)
    ) + 
    tmap_options(check.and.fix = TRUE) +
    tm_borders(col = "black", lwd = 0.001) +
    tm_layout(legend.outside = TRUE, 
              legend.outside.position = "right", 
              fontfamily = "sans")
  
  # save
  
  tmap_save(tm1, paste("~/Rdirectory/plot_logpopratio_overseas",i,".pdf",sep = ""))
  
  # plot map (population ratio overseas) # if you want to produce html, remove lwd and change .pdf to .gtml
  
  tm2 <- tm_shape(sfdata) +
    tm_grid(labels.inside.frame = FALSE) +
    tm_fill(
      col =  "population_ratio_overseas",
      style = "fixed",
      breaks = c(0,0.005,0.01,0.02,0.03,0.05,1),
      palette = "OrRd",
      midpoint = 0,
      title = paste("population ratio overseas",i, sep = ""),
      popup.vars = c( "Prefecture:" = "都道府県名.x",
                      "City: " = "市区町村名.x",
                      "pop ratio overseas: " = "population_ratio_overseas" )  ,
      n = 6,
      legend.format = list(format = "f", digits = 4)
    ) + 
    tmap_options(check.and.fix = TRUE) +
    tm_borders(col = "black", lwd = 0.001) +
    tm_layout(legend.outside = TRUE, 
              legend.outside.position = "right", 
              fontfamily = "sans")
  
  # save
  
  tmap_save(tm2, paste("~/Rdirectory/plot_popratio_overseas",i,".pdf",sep = ""))
  
  
  
}

# extract top 20 authorities (ratio of overseas)

for (i in initial_year:last_year) {
  
  temp1 <- get(paste0("overseas",i))
  temp1 <- rename(temp1, pop_overseas = paste0("pop_20",i))
  
  temp2 <- get(paste0("native",i))
  temp2 <- rename(temp2, pop_native = paste0("pop_20",i))
  
  temp_m <- merge(temp1,temp2, by = "JCODE")
  
  temp_m <- mutate(temp_m, population_ratio = (temp_m$pop_overseas / temp_m$pop_native) )
  temp_m <- mutate(temp_m, population_ratio_overseas = (temp_m$pop_overseas / (temp_m$pop_overseas + temp_m$pop_native)))
  temp_m <- mutate(temp_m, population_ratio_native = (temp_m$pop_native / (temp_m$pop_overseas + temp_m$pop_native)))
  temp_m <- mutate(temp_m, log_population_ratio = (log10(temp_m$pop_overseas + 1) - log10(temp_m$pop_native + 1)))
  temp_m <- mutate(temp_m, log_population_ratio_overseas = (log10(temp_m$pop_overseas + 1) - log10(temp_m$pop_native + temp_m$pop_overseas + 1)))
  temp_m <- mutate(temp_m, log_population_ratio_native = (log10(temp_m$pop_native + 1) - log10(temp_m$pop_native + temp_m$pop_overseas + 1)))
  
  assign(paste0("merged_data",i), temp_m)
  
}

  for (i in initial_year:last_year) {
  
  temp <- get(paste0("merged_data",i))
  
  top_20_list <- top_n(temp,20,population_ratio_overseas)
  
  top_20_list <- subset(top_20_list, select = c("都道府県名.x","市区町村名.x", "population_ratio_overseas"))
  
  top_20_list_n <- order(top_20_list$population_ratio_overseas,decreasing=T)
  
  top_20_list <- top_20_list[top_20_list_n,]
  
  rownames(top_20_list) <- 1:nrow(top_20_list)
  
  top_20_list <- rename(top_20_list, !!paste0("Prefecture") := 都道府県名.x)
  
  top_20_list <- rename(top_20_list, !!paste0("Municipality") := 市区町村名.x)
  
  top_20_list <- rename(top_20_list, !!paste0("population_ratio_overseas",i) := population_ratio_overseas)
  
  assign(paste0("set_of_top20_authorities_overseas",i),top_20_list)
  
}
  
top_20_list_all <- cbind(set_of_top20_authorities_overseas13,set_of_top20_authorities_overseas14,set_of_top20_authorities_overseas15,set_of_top20_authorities_overseas16,set_of_top20_authorities_overseas17,set_of_top20_authorities_overseas18,set_of_top20_authorities_overseas19,set_of_top20_authorities_overseas20)

tab <- xtable(top_20_list_all, caption = "top20 high overseas population ratio authorities ")

print(tab,file="assignment.tex",append=T,table.placement = "h",
      caption.placement="bottom", hline.after=seq(from=-1,to=nrow(tab),by=1), )

write.csv(top_20_list_all, file="~/Rdirectory/top_20_authorities.csv", 
            na="",        #NAを空白にしたい場合
            row.names=F, #列名（ヘッダ行）のみ残す場合。行名・列名いずれも残したい場合は row.names=T, col.names=NA
            quote=F  #ダブルクォーテーションを出力しない
)

# kable

table <- kbl(top_20_list_all, booktabs = T,caption = "Top20 high overseas population ratio authorities") %>%
  add_header_above(c( "2013" = 3, "2014" = 3,"2015" = 3,"2016" = 3,"2017" = 3,"2018" = 3,"2019" = 3,"2020" = 3), border_left = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  kable_classic(full_width = T, html_font = "Cambria")

save_kable(table, "table.pdf")

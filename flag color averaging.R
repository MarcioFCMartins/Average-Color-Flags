###########################About############################
#This script: scrapes sovereign nation flags from wikipedia#
#Converts flags from svg to bitmap, gets sRGB information  #
#Converts sRGB to Lab* color space and averages the colors #
#Plots resulting average color in a world map              #
#Author: M?rcio Martins (marciomartinsred@gmail.com)       #
#----------------------------------------------------------#

####Set up####
#Libraries
library(dplyr)     #Data wrangling
library(ggplot2)   #Plots
library(rsvg)      #Convert svg to bitmap
library(rvest)     #Scrape websites
library(stringr)

#Function to get average colors from an svg file location
get_average_color <- function(svg.link){
  bitmap <- rsvg(svg.link, width = 200)       #Read into a bitmap with 200px width
  bitmap <- bitmap[,,-4]                      #Exclude the alpha layer
  
  colors <- cbind(as.vector(bitmap[,,1]),     #Extract colors into a 3 row matrix
                  as.vector(bitmap[,,2]),
                  as.vector(bitmap[,,3]))
  rm(bitmap)
  
  colors <- convertColor(colors, from = "sRGB", to = "Lab") #Convert to Lab*
  
  country <- str_extract(svg.link,"of_(.*)\\.svg") %>%   #Get country name, clean up
             str_replace(".svg$","") %>%        #Delete format 
             str_replace("^of_","") %>%         #Delete "of" before names 
             str_replace_all("the","") %>%      #Remove "the"
             str_replace_all("\\%.*?\\%","") %>%#Remove stuff between % 
             str_replace_all("%","") %>%        #Remove all %
             str_replace_all("\\d","") %>%      #Remove any numbers
             str_replace_all("_"," ") %>%       #Replace underscores with space
             str_trim(side = "both")            #Remove whitespace and end/start
               
  average_color <- data.frame("L" = mean(colors[,1]),
                              "a" = mean(colors[,2]),
                              "b" = mean(colors[,3]),
                              "Country" = country,
                              stringsAsFactors = FALSE)
   

  return(average_color)
}

####Scrapping paths of flag SVG files####
#I will use the "Sovereign nation state flags" page from wikipedia
flag_list_page <- read_html("https://en.wikipedia.org/wiki/Gallery_of_sovereign_state_flags")

flag_list <- flag_list_page %>%     #This gives me the list of all flags in the page
             html_nodes("td") %>% 
             xml_nodes("a") %>%
             xml_attr("href") %>%
             str_subset("/File:")  
#Paste the https://en.wikipedia.org before the links
flag_list <- sapply(flag_list, FUN = function(x){paste0("https://en.wikipedia.org",x)}) %>%
             unname()
             

#The previous list leads to a page with a link to the SVG we need
#Extract the links that lead to the final image:
flag_svg_links <- sapply(flag_list, 
                         FUN = function(x){read_html(x) %>%
                                                     html_node("#file") %>%
                                                     html_node("a") %>%
                                                     xml_attr("href")
                           }
                         )
  
flag_svg_links <- sapply(flag_svg_links, FUN = function(x){paste0("https:",x)}) %>%
                  unname()

rm(list=setdiff(ls(), c("flag_svg_links", "get_average_color"))) #Clear the workspace a bit

#####Reading and processing images####-------------------------------------------------------------
#Get all values of mean colors
average_colors <- lapply(flag_svg_links, FUN = get_average_color) #Returns a list of dataframes
average_colors <- do.call(rbind, average_colors)     #Bind the dataframes together into one

#Convert back to RGB
average_colors[,1:3] <- convertColor(average_colors[,1:3], from = "Lab", to = "sRGB")

#Convert to hex
hex_average_colors <- data.frame("color" = rgb(average_colors[,1:3]),
                                 "region" = average_colors[,4],
                                 stringsAsFactors = FALSE)

hex_average_colors$region <- recode(hex_average_colors$region,
                             "United States" = "USA", 
                             "United Kingdom" = "UK",
                             "Republic of China" = "China")
#Get world map polygons
world <- map_data("world") 

#Join names in world map with names for our colors
world_region_colors <- data.frame("region"= unique(world$region), stringsAsFactors = FALSE) %>%
                       left_join(hex_average_colors, by = "region")

#Make colors a named vector to use in color scale
map_colors <- world_region_colors$color
names(map_colors) <- world_region_colors$region
map_colors[is.na(map_colors)] <- "grey" #Make NAs grey

####Plot and animate map####
#Export the maps with a rotation of 1 degree per frame
for(angle in seq(1,360,by = 1)){
  ggsave(filename = paste0("./plots/globe",formatC(angle, width = 3, flag = "0"),".png"),
         plot = ggplot() +
                 geom_polygon(data = world, aes(x = long, y = lat, group = group, fill = region),color = "black", size = 0.5) +
                 #scale_fill_manual(values = map_colors) +
                 coord_map(projection = "orthographic", 
                           orientation = c(30,angle,0)) +
                 scale_y_continuous(breaks = c(0,33,66,90,123,156,180),
                                    labels = NULL) +
                 scale_x_continuous(breaks = seq(-180,180,45),
                                    labels = NULL) +
                 scale_fill_manual(values = map_colors) +
                 labs(title = "Average flag color per country", 
                      subtitle = "Weighted per area", 
                      x = NULL, 
                      y = NULL,
                      caption = "Source: Gallery of sovereign state flags, Wikipedia") +
                 theme_bw() +
                 theme(legend.position = "none", 
                       panel.grid.major = element_line(linetype = 1, color = "#cccccc", size = 1)),
         type = "cairo-png",
         height = 15,
         width = 15)
  print(paste(angle, "out of 360"))
}

#Create a movie (install ffmpeg beforehand: https://www.ffmpeg.org) 
#See https://en.wikibooks.org/wiki/FFMPEG_An_Intermediate_Guide/image_sequence
#Optimization and palette generating commands were adapted from:
#   http://cassidy.codes/blog/2017/04/25/ffmpeg-frames-to-gif-optimization/
#   http://www.imagemagick.org/Usage/anim_opt/#compress_opt
#   http://www.ffmpeg.org/ffmpeg-filters.html

#Create palette
palette_cmd <- paste0("ffmpeg -y -start_number 001 -i ", getwd(), "/plots/globe%03d.png -filter_complex \"fps=15, scale=700:700, palettegen=stats_mode=diff\" palette.gif")
system(palette_cmd)

#Export gif
movie_cmd <- paste0("ffmpeg -y -start_number 001 -i ", getwd(), "/plots/globe%03d.png -i palette.gif -filter_complex \"fps=15, scale=700:700, paletteuse=dither=bayer:bayer_scale=5:diff_mode=rectangle\" rotation_globe.gif")
system(movie_cmd)


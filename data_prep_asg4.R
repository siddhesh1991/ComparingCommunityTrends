library(readr)
library(dplyr)
library(ggplot2)
library(maptools)
library(RColorBrewer)
path_housing = "F:\\asg4\\Affordable_Rental_Housing_Developments.csv"

#Housing dataset
df_housing = read_csv(path_housing)
df_housing_final <- data.frame("comm_area" = df_housing$'Community Area Number', 
                               "units" = df_housing$'Units') 
df_housing_final <- df_housing_final %>% filter(comm_area != 'NA')
grouped_housing <- df_housing_final %>% group_by(comm_area) %>% summarise(total_units = sum(units))

#Plotting Community area vs Housing units.
line_plot_housing <- ggplot(data=grouped_housing,aes(x=comm_area, y=total_units,color=total_units))
line_plot_housing + geom_line() + scale_color_gradient(low="blue", high="red")

#Reading and creating Lookup for community area and community number.
path_lookup <- "F:\\asg4\\lie_exp.csv"
lookup = read_csv(path_lookup)

lookup <- data.frame("comm_num" = lookup$'Community Area Number', 
                     "comm_name" = lookup$'Community Area') 

lookup_final <- lookup %>% filter('Community Area Number' !='NA')

#Read the electricity usage file

path_ele <- "F:\\asg4\\ele_usage.csv"
ele_df = read_csv(path_ele)
names(ele_df) <- c("comm_name", "energy_cons")
#Filling the Community area using the lookup
ele_df_final <- merge(ele_df, lookup_final, by.x = "comm_name", by.y = 'comm_name')
#Plot the Community area vs the energy consumed 
ele_df_fin <- data.frame("comm_num" = ele_df_final$'comm_num', 
                         "energy" = ele_df_final$energy_cons) 

line_plot_ele <- ggplot(data=ele_df_fin,aes(x=comm_num, y=energy))
line_plot_ele + geom_line() + ylim(0,250000) + scale_color_gradient(low="blue", high="red")


#Read the Grocery dataset.

path_gro <- "F:\\asg4\\Grocery.csv"
gro_df = read_csv(path_gro)
gro_df_fin <- data.frame("s_name" = gro_df$'STORE NAME', 
                         "comm_area" = gro_df$'COMMUNITY AREA') 
grouped_gro_df<- gro_df_fin %>% group_by(comm_area) %>% summarise(count = n())
#Plot the Grocery dataset.
line_plot_gro <- ggplot(data=grouped_gro_df,aes(x=comm_area, y=count))
line_plot_gro + geom_line()  + scale_color_gradient(low="blue", high="red")

#Read the Garbage dataset.
path_garbage <- "F:\\asg4\\garbage.csv"
gar_df = read_csv(path_garbage)
gar_df_fin <- data.frame("dt" = gar_df$'Creation Date', 
                         "comm_area" = gar_df$'Community Area') 

grouped_gar_df<- gar_df_fin %>%  filter(comm_area != 'NA') %>% group_by(comm_area) %>% summarise(count = n())
#Plotting the Comm area vs Garbage graph
line_plot_gar <- ggplot(data=grouped_gar_df,aes(x=comm_area, y=count))
line_plot_gar + geom_line()  + scale_color_gradient(low="blue", high="red")

#Read the School dataset.
path_sch <- "F:\\asg4\\school.csv"
sch_df = read_csv(path_sch)
sch_df_fin <- data.frame("name" = sch_df$'Name of School',  
                         "comm_area" = sch_df$'Community Area Number') 
grouped_sch_df<- sch_df_fin %>%  filter(comm_area != 'NA') %>% group_by(comm_area) %>% summarise(count = n())
#Plotting school data
line_plot_sch <- ggplot(data=grouped_sch_df,aes(x=comm_area, y=count))
line_plot_sch + geom_line()  + scale_color_gradient(low="blue", high="red")


#Reading the Shape file of Chicago for the Graffiti dataset mapping.

area <- readShapePoly(file.choose())
convert.point<-fortify(area)
chi_map<- get_map(location = "Chicago", zoom = 10, maptype="satellite")

path = "F:\\Graffiti.csv"

'Importing the Graffiti CSV file'
graffiti_df = read_csv(path)

#Filtering incidents of '2015'
count_df = graffiti_df %>%  filter(substr(graffiti_df$'Creation Date',7,10) == "2015")


#Creating the required columns in the new dataset
visualisations = as.data.frame(graffiti_df$'Community Area' ,  stringsAsFactors = FALSE)
names(visualisations)[1]<-paste("comm_area") 

#Grouping by the Community Area for visualisations.
grouped = visualisations %>% filter(comm_area != 'NA')%>% group_by(comm_area) %>% summarise(count = n())

line_plot <- ggplot(data=grouped,aes(x=comm_area, y=count,color=count))
line_plot + geom_line() + scale_color_gradient(low="blue", high="red")

val = function(count) {
  if(is.na(count)){
    return("Not Defined")
  }
  if(count >=0 && count < 500){
    return("Very Low")
  }
  if(count >=500 && count < 1500){
    return("Low")
  }
  if(count >=1500 && count < 10000){
    return("Medium")
  }
  if(count >=10000 && count < 20000){
    return("High")
  }
  if(count >=20000 ){
    return("Extremely High")
  }
}

val_num = function(count) {
  if(is.na(count)){
    return(10)
  }
  if(count >=0 && count < 500){
    return(1)
  }
  if(count >=500 && count <1500){
    return(2)
  }
  if(count >=1500 && count < 10000){
    return(3)
  }
  if(count >=10000 && count < 20000){
    return(4)
  }
  if(count >=20000 ){
    return(5)
  }
}

df1 = grouped
df1$category = sapply(df1$count,val)
df1$cat_count = sapply(df1$count,val_num)

fill_comm_area = function(val) {
  return(ceiling(val))
}
convert.point$comm_area = sapply(as.numeric(convert.point$group),fill_comm_area)

#Do an innter join with the scrubbed Graffiti dataset to populate the sentiment category.

merged_graffiti <- merge(convert.point, df1, by.x = "comm_area", by.y ="comm_area")
  
colors <- brewer.pal(9, "BuGn")
ggmap(chi_map) +geom_polygon(aes(x = long,
                                 y = lat,
                                 fill = category,
                                 group = group),
                                 data = merged_graffiti,
                                 color = colors[9],
                                 alpha = 0.5)


# library('ProjectTemplate')
# load.project()

# Selecting video stats data from all the iterations of the course...
# video_df = get(project.info$cache[7])

# People from which region...
colnames(alpha2lat)[2] = "detected_country"
with_coordinates = merge(x=enrolments,y=alpha2lat[,c(2,5,6)],by="detected_country", all.x=TRUE)
colnames(with_coordinates)[14] = "lat"
colnames(with_coordinates)[15] = "long"
world <- map_data("world")
without_na = with_coordinates[!is.na(with_coordinates$long),]
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/3.png', width = 1920, height = 1080)
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = "lightgray")+
  stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = 0.25), bins = 30, data = without_na, geom = "polygon")
dev.off()
#######################################################################################################################################

# Finding which device is being use more often to access the course content...
desktop_percentage = video %>% group_by(title) %>% summarize(desktop = mean(desktop_device_percentage))
mobile_percentage = video %>% group_by(title) %>% summarize(mobile = mean(mobile_device_percentage))
tablet_percentage = video %>% group_by(title) %>% summarize(tablet = mean(tablet_device_percentage))

# Creating a dataframe of topic wise device used...
device_df = cbind(desktop_percentage, mobile_percentage[,2], tablet_percentage[,2])

# Converting the wide data to long data...
device_df = melt(device_df, id.vars = c('title'))
colnames(device_df) <- c('title', 'Device', 'Percentage_Watch')

# saving the plot to the graphs/ directory...
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/1.png', width = 1920, height = 1080)
ggplot(device_df, aes(x = title, y = Percentage_Watch, fill = Device))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90))
dev.off()

########################################################################################################################################

# Calculating mean of watch percentage of each device for each iteration of the course...

mean_vid_df3 = cyber.security.3_video.stats[, c(17, 18, 20)]
iteration_3 = colMeans(mean_vid_df3)

mean_vid_df4 = cyber.security.4_video.stats[, c(17, 18, 20)]
iteration_4 = colMeans(mean_vid_df4)

mean_vid_df5 = cyber.security.5_video.stats[, c(17, 18, 20)]
iteration_5 = colMeans(mean_vid_df5)

mean_vid_df6 = cyber.security.6_video.stats[, c(17, 18, 20)]
iteration_6 = colMeans(mean_vid_df6)
 
mean_vid_df7 = cyber.security.7_video.stats[, c(17, 18, 20)]
iteration_7 = colMeans(mean_vid_df7)

# Dataframe of means of each device in columns and iterations in rows...
mean_df = rbind(iteration_3, iteration_4, iteration_5, iteration_6, iteration_7)
# # mean_df = scale(mean_df)
# mean_df = cbind(mean_df, c(3, 4, 5, 6, 7))
# ggplot(as.data.frame(mean_df), aes(x = V4))+
#   geom_line(aes(y = desktop_device_percentage), col = "red")+
#   geom_line(aes(y = mobile_device_percentage), col = "blue")+
#   geom_line(aes(y = tablet_device_percentage), col = "green")


# Converting the wide data to long data...
long_mean_df = melt(mean_df)
colnames(long_mean_df) = c('Iteration', 'Device', 'Watch_Time')

# saving the plot to the graphs/ directory...
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/2.png', width = 1920, height = 1080)
ggplot()+
  geom_bar(long_mean_df, mapping = aes(x = Iteration, y = Watch_Time, fill = Device), stat='identity', position = position_dodge())+
  geom_line(as.data.frame(mean_df), mapping = aes(x = as.factor(rownames(mean_df)), y = mobile_device_percentage, group = 1))+
  ylab("Watch Time Percent")
dev.off()

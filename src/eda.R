library('ProjectTemplate')
load.project()

# Selecting video stats data from all the iterations of the course...
# video_df = get(project.info$cache[7])

video_df = video

# Finding which device is being use more often to access the course content...
desktop_percentage = video_df %>% group_by(title) %>% summarize(desktop = mean(desktop_device_percentage))
mobile_percentage = video_df %>% group_by(title) %>% summarize(mobile = mean(mobile_device_percentage))
tablet_percentage = video_df %>% group_by(title) %>% summarize(tablet = mean(tablet_device_percentage))

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

# Extracting all the files with video stats...
#vid_filenames <- list.files("C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/raw_data", pattern="*.csv", full.names=TRUE)
# vid_ldf <- lapply(vid_filenames, read.csv)
# vid_index = grep("video_duration", vid_ldf)



# Calculating mean of watch percentage of each device for each iteration of the course...

# mean_vid_df3 = vid_ldf[vid_index[1]][[1]][, c(17, 18, 20)]  
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

# Converting the wide data to long data...
mean_df = melt(mean_df)
colnames(mean_df) = c('Iteration', 'Device', 'Watch_Time')

# saving the plot to the graphs/ directory...
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/2.png', width = 1920, height = 1080)
ggplot(mean_df, aes(x = Iteration, y = Watch_Time, fill = Device))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90))
dev.off()
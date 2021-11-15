library('ProjectTemplate')
load.project()

# Selecting video stats data from all the iterations of the course...
video_df = get(project.info$data[7])

# Finding which device is being use more often to access the course content...
desktop_percentage = video_df %>% group_by(title) %>% summarize(desktop = mean(desktop_device_percentage))
mobile_percentage = video_df %>% group_by(title) %>% summarize(mobile = mean(mobile_device_percentage))
tablet_percentage = video_df %>% group_by(title) %>% summarize(tablet = mean(tablet_device_percentage))

# Creatring a dataframe of topic wise device used...
device_df = cbind(desktop_percentage, mobile_percentage[,2], tablet_percentage[,2])

# Converting the wide data to long data...
device_df = melt(device_df, id.vars = c('title'))
colnames(device_df)[2] <- "Device"
colnames(device_df)[3] <- "Percentage_Watch"

# saving the plot to the graphs/ directory...
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/1.png', width = 1920, height = 1080)
ggplot(device_df, aes(x = title, y = Percentage_Watch, fill = Device))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90))
dev.off()

vid_filenames <- list.files("C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/raw_data", pattern="*.csv", full.names=TRUE)
vid_ldf <- lapply(vid_filenames, read.csv)
vid_index = grep("video_duration", vid_ldf)

vid_itr3 = vid_ldf[vid_index[1]][[1]][, c(2, 17, 18, 20)]
vid_itr3 = melt(vid_itr3, id.vars = c('title'))
colnames(vid_itr3)[2] <- "Device"
colnames(vid_itr3)[3] <- "Percentage_Watch"

vid_itr4 = vid_ldf[vid_index[2]][[1]][, c(2, 17, 18, 20)]
vid_itr4 = melt(vid_itr4, id.vars = c('title'))
colnames(vid_itr4)[2] <- "Device"
colnames(vid_itr4)[3] <- "Percentage_Watch"

vid_itr5 = vid_ldf[vid_index[3]][[1]][, c(2, 17, 18, 20)]
vid_itr5 = melt(vid_itr5, id.vars = c('title'))
colnames(vid_itr5)[2] <- "Device"
colnames(vid_itr5)[3] <- "Percentage_Watch"

vid_itr6 = vid_ldf[vid_index[4]][[1]][, c(2, 17, 18, 20)]
vid_itr6 = melt(vid_itr6, id.vars = c('title'))
colnames(vid_itr6)[2] <- "Device"
colnames(vid_itr6)[3] <- "Percentage_Watch"

vid_itr7 = vid_ldf[vid_index[5]][[1]][, c(2, 17, 18, 20)]
vid_itr7 = melt(vid_itr7, id.vars = c('title'))
colnames(vid_itr7)[2] <- "Device"
colnames(vid_itr7)[3] <- "Percentage_Watch"

png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/2.png', width = 1920, height = 1920)
plt1 = ggplot(vid_itr3, aes(x = title, y = Percentage_Watch, fill = Device))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90))

plt2 = ggplot(vid_itr4, aes(x = title, y = Percentage_Watch, fill = Device))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90)) 

plt3 = ggplot(vid_itr5, aes(x = title, y = Percentage_Watch, fill = Device))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90))

plt4 = ggplot(vid_itr6, aes(x = title, y = Percentage_Watch, fill = Device))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90))

plt5 = ggplot(vid_itr7, aes(x = title, y = Percentage_Watch, fill = Device))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90))

grid.arrange(plt1, plt2, plt3, plt4, plt5)
dev.off()
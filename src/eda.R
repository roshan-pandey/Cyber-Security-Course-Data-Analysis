library('ProjectTemplate')
load.project()

# for (dataset in project.info$data)
# {
#   message(paste('Showing top 5 rows of', dataset))
#   print(head(get(dataset)))
# }

video_df = get(project.info$data[7])
print(head(video_df))

desktop_percentage = video_df %>% group_by(title) %>% summarize(total_desktop_views = mean(desktop_device_percentage))
mobile_percentage = video_df %>% group_by(title) %>% summarize(total_mobile_views = mean(mobile_device_percentage))
tablet_percentage = video_df %>% group_by(title) %>% summarize(total_tablet_views = mean(tablet_device_percentage))

device_df = cbind(desktop_percentage, mobile_percentage[,2], tablet_percentage[,2])

device_df = melt(device_df, id.vars = c('title'))

png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/1.png', width = 1920, height = 1080)
ggplot(device_df, aes(x = title, y = value, fill = variable))+
  geom_bar(stat='identity', position = position_dodge())+
  theme(axis.text.x = element_text(angle=90))
dev.off()

  
# setwd('C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/')
# print("-------------------- PERFORMING EDA ----------------------------------")
# library('ProjectTemplate')
# load.project()

# Selecting video stats data from all the iterations of the course...
# video_df = get(project.info$cache[7])

world <- map_data("world")
# views_mean = colMeans(video[,c(22:27)])
# col_names = c('Europe', 'Oceania', 'Asia', 'NA', 'SA', 'Africa')
# long = c(20, 150, -100, 80, -60, 15)
# lat = c(50, -25, 40, 35, -15, 15)
# continent = data.frame(region = col_names, views = views_mean, lat = lat, long = long, stringsAsFactors = F)
# # continent = inner_join(world, continent, by = "region")

######################################################################################################################################
enrolment_i1 = cyber.security.1_enrolments
enrolment_i2 = cyber.security.2_enrolments
enrolment_i3 = cyber.security.3_enrolments
enrolment_i4 = cyber.security.4_enrolments
enrolment_i5 = cyber.security.5_enrolments
enrolment_i6 = cyber.security.6_enrolments
enrolment_i7 = cyber.security.7_enrolments

learners_in_batch_1 = dim(enrolment_i1)
learners_in_batch_2 = dim(enrolment_i2)
learners_in_batch_3 = dim(enrolment_i3)
learners_in_batch_4 = dim(enrolment_i4)
learners_in_batch_5 = dim(enrolment_i5)
learners_in_batch_6 = dim(enrolment_i6)
learners_in_batch_7 = dim(enrolment_i7)

dim_df = as.data.frame(rbind(learners_in_batch_1, learners_in_batch_2, learners_in_batch_3, learners_in_batch_4, learners_in_batch_5, learners_in_batch_6, learners_in_batch_7))
colnames(dim_df) = c("Number_of_learners_enrolled", "Number_of_features")
dim_df$batch = rownames(dim_df)
row.names(dim_df) <- NULL

png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/1.png', width = 1920, height = 1080)
ggplot()+
  geom_line(data = dim_df, mapping = aes(x = batch, y = Number_of_learners_enrolled, group = 1), col = "blue")+
  geom_point(data = dim_df, mapping = aes(x = batch, y = Number_of_learners_enrolled, group = 1), col = "red", size = 2)+
  theme(axis.text.x = element_text(angle=90))
dev.off()
#######################################################################################################################################

# Reference: https://stackoverflow.com/questions/28078431/how-to-create-a-world-heat-map-in-r-on-continent-level

url = "https://gist.githubusercontent.com/hrbrmstr/91ea5cc9474286c72838/raw/f3fde312c9b816dff3994f39f2bcda03209eff8f/continents.json"
stop_for_status(GET(url, write_disk("C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/continents.json", overwrite = TRUE)))
continents = readOGR("C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/continents.json")
continents_map = fortify(continents, region="CONTINENT")

cont_data = read.table(text="id,  value
Europe,  59.185692
Australia, 4.355385
Asia,  15.287538
North America, 10.400154
South America, 2.140462
Africa,  7.450000
Antarctica, 0", header=TRUE, stringsAsFactors=FALSE, sep = ",")

png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/2.png', width = 1920, height = 1080)
ggplot()+
  geom_map(data = continents_map, map = continents_map, aes(x=long, y = lat, map_id = id), color = "gray")+
  geom_map(data = cont_data, map = continents_map, aes(map_id = id, fill = value), color = "gray")+
  scale_fill_distiller("Percentage Views")+
  coord_equal()+
  labs(x=NULL, y=NULL)
dev.off()
#######################################################################################################################################
# People from which region...

colnames(alpha2lat)[2] = "detected_country"
alpha2lat$detected_country = trimws(alpha2lat$detected_country)
with_coordinates = merge(x = enrolments, y = alpha2lat[,c(2,5,6)], by = "detected_country", all.x = TRUE)
colnames(with_coordinates)[14] = "lat"
colnames(with_coordinates)[15] = "long"
without_na = with_coordinates[!is.na(with_coordinates$long),]
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/3.png', width = 1920, height = 1080)
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = "lightgray")+
  stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = 0.25), bins = 30, data = without_na, geom = "polygon")
dev.off()

#######################################################################################################################################

# Which type of people are joining the course most?
gender = data.frame(gender = enrolments$gender[enrolments$gender != 'Unknown'])
gender = as.data.frame(table(gender$gender))

edu_lvl = data.frame(edu_lvl = enrolments$highest_education_level[enrolments$highest_education_level != 'Unknown'])
edu_lvl = as.data.frame(table(edu_lvl$edu_lvl))

emp_status = data.frame(emp_status = enrolments$employment_status[enrolments$employment_status != 'Unknown'])
emp_status = as.data.frame(table(emp_status$emp_status))

emp_area = data.frame(emp_area = enrolments$employment_area[enrolments$employment_area != 'Unknown'])
emp_area = as.data.frame(table(emp_area$emp_area))

age_range = data.frame(age_range = enrolments$age_range[enrolments$age_range != 'Unknown'])
age_range = as.data.frame(table(age_range$age_range))

gg_gender = ggplot(data = gender, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Gender")+
  xlab("Gender")

gg_edu_lvl = ggplot(data = edu_lvl, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Education Level")+
  xlab("Education Level")

gg_emp_status = ggplot(data = emp_status, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Employment Status")+
  xlab("Employment Status")

gg_emp_area = ggplot(data = emp_area, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Employment Area")+
  xlab("Employment Area")+
  theme(axis.text.x = element_text(angle=90))

gg_age_range = ggplot(data = age_range, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Age Range")+
  xlab("Age Range")
lay = rbind(c(1,2),
            c(3,4),
            c(5,5))
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/6.png', width = 1920, height = 1080)
grid.arrange(gg_gender, gg_age_range, gg_emp_status, gg_edu_lvl, gg_emp_area, layout_matrix = lay)
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
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/4.png', width = 1920, height = 1080)
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
png(file = 'C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/graphs/5.png', width = 1920, height = 1080)
ggplot()+
  geom_bar(long_mean_df, mapping = aes(x = Iteration, y = Watch_Time, fill = Device), stat = 'identity', position = position_dodge())+
  geom_line(as.data.frame(mean_df), mapping = aes(x = as.factor(rownames(mean_df)), y = mobile_device_percentage, group = 1))+
  ylab("Watch Time Percent")
dev.off()

world <- map_data("world")
options(repr.plot.width = 1920, repr.plot.height = 1080) 
######################################################################################################################################
enrolment_i1 = cyber.security.1_enrolments
enrolment_i2 = cyber.security.2_enrolments
enrolment_i3 = cyber.security.3_enrolments
enrolment_i4 = cyber.security.4_enrolments
enrolment_i5 = cyber.security.5_enrolments
enrolment_i6 = cyber.security.6_enrolments
enrolment_i7 = cyber.security.7_enrolments

batch_1 = dim(enrolment_i1)
batch_2 = dim(enrolment_i2)
batch_3 = dim(enrolment_i3)
batch_4 = dim(enrolment_i4)
batch_5 = dim(enrolment_i5)
batch_6 = dim(enrolment_i6)
batch_7 = dim(enrolment_i7)

dim_df = as.data.frame(rbind(batch_1, batch_2, batch_3, batch_4, batch_5, batch_6, batch_7))
colnames(dim_df) = c("Number_of_learners_enrolled", "Number_of_features")
dim_df$batch = rownames(dim_df)
row.names(dim_df) <- NULL

performance = ggplot()+
  geom_line(data = dim_df, mapping = aes(x = batch, y = Number_of_learners_enrolled, group = 1), col = "blue")+
  geom_point(data = dim_df, mapping = aes(x = batch, y = Number_of_learners_enrolled, group = 1), col = "red", size = 2)+
  xlab("Batches")+
  ylab("Number of Learners")+
  theme(axis.text.x = element_text(angle=90))

png(file = './graphs/1.png', width = 1920, height = 1080)
grid.arrange(performance)
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

continents_plot = ggplot()+
  geom_map(data = continents_map, map = continents_map, aes(x=long, y = lat, map_id = id), color = "gray")+
  geom_map(data = cont_data, map = continents_map, aes(map_id = id, fill = value), color = "gray")+
  scale_fill_distiller("Percentage Views")+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal()+
  labs(x=NULL, y=NULL)

png(file = './graphs/2.png', width = 1920, height = 1080)
grid.arrange(continents_plot)
dev.off()
#######################################################################################################################################
# People from which region...

colnames(alpha2lat)[2] = "detected_country"
alpha2lat$detected_country = trimws(alpha2lat$detected_country)
with_coordinates = merge(x = enrolments, y = alpha2lat[,c(2,5,6)], by = "detected_country", all.x = TRUE)
colnames(with_coordinates)[14] = "lat"
colnames(with_coordinates)[15] = "long"
without_na = with_coordinates[!is.na(with_coordinates$long),]

country_plot = ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = "lightgray")+
  stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = 0.5), bins = 30, data = without_na, geom = "polygon")+
  xlab("Longitude")+
  ylab("Latitude")

png(file = './graphs/3.png', width = 1920, height = 1080)
grid.arrange(country_plot)
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
  xlab("Gender")+
  scale_fill_brewer(palette = "Set3")


gg_edu_lvl = ggplot(data = edu_lvl, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Education Level")+
  xlab("Education Level")+
  scale_fill_brewer(palette = "Set3")


gg_emp_status = ggplot(data = emp_status, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Employment Status")+
  xlab("Employment Status")+
  scale_fill_brewer(palette = "Set3")


gg_emp_area = ggplot(data = emp_area, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Employment Area")+
  xlab("Employment Area")+
  theme(axis.text.x = element_text(angle=15))

gg_age_range = ggplot(data = age_range, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Age Range")+
  xlab("Age Range")+
  scale_fill_brewer(palette = "Set3")

lay = rbind(c(1,2),
            c(3,4),
            c(5,5))
arrange_plot = grid.arrange(gg_gender, gg_age_range, gg_emp_status, gg_edu_lvl, gg_emp_area, layout_matrix = lay)


png(file = './graphs/4.png', width = 1920, height = 1080)
grid.arrange(gg_gender, gg_age_range, gg_emp_status, gg_edu_lvl, gg_emp_area, layout_matrix = lay)
dev.off()
#######################################################################################################################################

# Finding which device is being use more often to access the course content...
desktop_percentage = video %>% group_by(title) %>% summarize(desktop = mean(desktop_device_percentage))
mobile_percentage = video %>% group_by(title) %>% summarize(mobile = mean(mobile_device_percentage))
tablet_percentage = video %>% group_by(title) %>% summarize(tablet = mean(tablet_device_percentage))

# Creating a dataframe of topic wise device used...
device_df = cbind(desktop_percentage, mobile_percentage[,2], tablet_percentage[,2])
device_df = as.data.frame(colMeans(device_df[2:4]))
colnames(device_df) = c("Usage")

device_plot = ggplot(device_df, aes(x = row.names(device_df), y = Usage, fill = row.names(device_df)))+ 
  geom_bar(stat = "identity")+
  xlab("Device")+
  labs(fill = "Devices")+
  theme(text = element_text(size = 25))+
  scale_fill_brewer(palette = "Set3")


# saving the plot to the graphs/ directory...
png(file = './graphs/5.png', width = 1920, height = 1080)
grid.arrange(device_plot)
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
colnames(mean_df) = c("Desktop", "Mobile", "Tablet")
# Converting the wide data to long data...
long_mean_df = melt(mean_df)
colnames(long_mean_df) = c('Iteration', 'Device', 'Watch_Time')

device_ittr_plot = ggplot()+
  geom_bar(long_mean_df, mapping = aes(x = Iteration, y = Watch_Time, fill = Device), stat = 'identity', position = position_dodge())+
  geom_line(as.data.frame(mean_df), mapping = aes(x = as.factor(rownames(mean_df)), y = Mobile, group = 1), size = 1.5, color = "DarkBlue")+
  geom_point(as.data.frame(mean_df), mapping = aes(x = as.factor(rownames(mean_df)), y = Mobile, group = 1), size = 4, color = "DarkBlue")+
  ylab("Usage")+
  xlab("Batch Iteration")+
  scale_fill_brewer(palette = "Set3")


# saving the plot to the graphs/ directory...
png(file = './graphs/6.png', width = 1920, height = 1080)
grid.arrange(device_ittr_plot)
dev.off()

#######################################################################################################################################

cleaned_string = string_cleaner(weekly$reason)
clean_txt_df = data.frame(txt = strsplit(cleaned_string, split = " "))
clean_txt_df = data.frame(table(clean_txt_df))
colnames(clean_txt_df) = c("txt", "freq")

sent <- get_nrc_sentiment(as.character(clean_txt_df$txt))
sent_score = colSums(sent)
sent_score_df = rbind(colnames(sent), sent_score)
sent_score_df = as.data.frame(t(sent_score_df))
sent_score_df = subset(sent_score_df, select = -c(1))
sent_score_df$sent_score = as.numeric(sent_score_df$sent_score)

sentiments_word_cloud = ggplot(clean_txt_df, aes(label = txt, size = freq, color = factor(sample.int(10, nrow(clean_txt_df), replace = TRUE)),)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 35) +
  theme_minimal()

png(file = './graphs/7.png', width = 1920, height = 1080)
grid.arrange(sentiments_word_cloud)
dev.off()


sent_plot = ggplot(data = sent_score_df, mapping = aes(x = row.names(sent_score_df), y = sent_score, fill = row.names(sent_score_df)))+
  geom_bar(stat = "identity")+
  xlab("Sentiments")+
  ylab("Sentiment Score")+
  labs(fill = "Sentiments")+
  scale_fill_brewer(palette = "Set3")



png(file = './graphs/8.png', width = 1920, height = 1080)
grid.arrange(sent_plot)
dev.off()

#######################################################################################################################################
leaving_reason = data.frame(table(leaving$leaving_reason))
colnames(leaving_reason) = c('reason', 'freq')
leaving_reason$reason = gsub("[^[:alnum:]]", " ",as.character(leaving_reason$reason))
leaving_reason$reason = gsub("â", "",as.character(leaving_reason$reason))
leaving_reason$reason = gsub("n t", "n't",as.character(leaving_reason$reason))



hsize = 1

leaving_reason = leaving_reason %>% 
  mutate(x = hsize)

leaving_reason_plot = ggplot(leaving_reason, aes(x = hsize, y = freq, fill = reason))+
  geom_col()+
  coord_polar(theta = "y")+
  xlim(c(0, hsize + 0.5))+
  xlab("")+
  ylab("")+
  labs(fill = "Reasons")+
  scale_fill_brewer(palette = "Set3")+
  geom_text(aes(label = freq), position = position_stack(vjust = 0.5)) +
  theme(axis.text=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())

png(file = './graphs/9.png', width = 1920, height = 1080)
grid.arrange(leaving_reason_plot)
dev.off()

#######################################################################################################################################

total_views = (video %>% group_by(title) %>% summarize(total_views = mean(total_views)))
total_views$full_watch = (video %>% group_by(title) %>% summarize(watch_100 = mean(viewed_onehundred_percent)))$watch_100
total_views$qua3_watch = (video %>% group_by(title) %>% summarize(watch_75 = mean(viewed_seventyfive_percent)))$watch_75
total_views$half_watch = (video %>% group_by(title) %>% summarize(watch_50 = mean(viewed_fifty_percent)))$watch_50
total_views$qua_watch = (video %>% group_by(title) %>% summarize(watch_25 = mean(viewed_twentyfive_percent)))$watch_25
total_views = subset(total_views, select = -c(title))
total_views = normalize(total_views, method = "range", range = c(0,1))
total_views = as.data.frame(cbind(total_views, video$video_duration[1:13]))
colnames(total_views)[6] = "Video_Length"
colnames(total_views)[1] = "View_Count"

total_views$Video_Length = as.factor(total_views$Video_Length)
views = subset(total_views, select = -c(View_Count))
views = melt(views, id.vars = c('Video_Length'))
colnames(views) = c("Video_Length", "Watch_Status", "Value")

views_comparison_plot = ggplot(views, aes(x = Video_Length, y = Value, group = Watch_Status, color = Watch_Status))+
  geom_bar(data = total_views, mapping = aes(x = Video_Length, y = View_Count), 
           fill = "steelblue", stat = "identity", inherit.aes = FALSE)+
  geom_point(size = 2.5)+
  geom_line(size = 1.5)+
  xlab("Video Length")+
  ylab("Normalized Watch Percent")+
  labs(fill = "Watch Status")

png(file = './graphs/10.png', width = 1920, height = 1080)
grid.arrange(views_comparison_plot)
dev.off()
########################################################################################################
#                                   F I L E   D E S C R I P T I O N                                    #
########################################################################################################
#                                                                                                      #
# This file contains the code related to Data Munging: transformations like, joining different tables, #
# merging tables row wise and column wise, text processing, melting table from wide format to long,    #
# column and row renaming, column and row deletion, calculating different statistics like column mean, #
# normalizing the data, etc.                                                                           #
########################################################################################################

# Merging enrollment data from all iterations... (row bind)
enrolment_i1 = cyber.security.1_enrolments
enrolment_i2 = cyber.security.2_enrolments
enrolment_i3 = cyber.security.3_enrolments
enrolment_i4 = cyber.security.4_enrolments
enrolment_i5 = cyber.security.5_enrolments
enrolment_i6 = cyber.security.6_enrolments
enrolment_i7 = cyber.security.7_enrolments
enrolments = rbind(enrolment_i1, enrolment_i2, enrolment_i3, enrolment_i4, enrolment_i5, enrolment_i6, enrolment_i7)

# Merging video stats data from all iterations... (row bind)
video_i3 = cyber.security.3_video.stats
video_i4 = cyber.security.4_video.stats
video_i5 = cyber.security.5_video.stats
video_i6 = cyber.security.6_video.stats
video_i7 = cyber.security.7_video.stats
video = rbind(video_i3, video_i4, video_i5, video_i6, video_i7)

# Merging leaving survey response data from all iterations... (row bind)
leaving_i4 = cyber.security.4_leaving.survey.responses
leaving_i5 = cyber.security.5_leaving.survey.responses
leaving_i6 = cyber.security.6_leaving.survey.responses
leaving_i7 = cyber.security.7_leaving.survey.responses
leaving = rbind(leaving_i4, leaving_i5, leaving_i6, leaving_i7)

# Merging weekly sentiment data from all iterations... (row bind)
weekly_i5 = cyber.security.5_weekly.sentiment.survey.responses
weekly_i6 = cyber.security.6_weekly.sentiment.survey.responses
weekly_i7 = cyber.security.7_weekly.sentiment.survey.responses
weekly = rbind(weekly_i5, weekly_i6, weekly_i7)


# Calculating number of enrollments each year...
# dim() will give number of rows and cols. Number of rows represent number of learners enrolled...
batch_1 = dim(enrolment_i1)
batch_2 = dim(enrolment_i2)
batch_3 = dim(enrolment_i3)
batch_4 = dim(enrolment_i4)
batch_5 = dim(enrolment_i5)
batch_6 = dim(enrolment_i6)
batch_7 = dim(enrolment_i7)

# Creating Data frame of number of rows and cols of enrollment dataset from each iterations...
dim_df = as.data.frame(rbind(batch_1, batch_2, batch_3, batch_4, batch_5, batch_6, batch_7)) 
colnames(dim_df) = c("Number_of_learners_enrolled", "Number_of_features") # changing col names...
dim_df$batch = rownames(dim_df) # Creating new column to hold row names...
row.names(dim_df) <- NULL # making rows as NULL...

# colnames(alpha2lat)[2] = "detected_country"
# alpha2lat$detected_country = trimws(alpha2lat$detected_country)
# with_coordinates = merge(x = enrolments, y = alpha2lat[,c(2,5,6)], by = "detected_country", all.x = TRUE)
# colnames(with_coordinates)[14] = "lat"
# colnames(with_coordinates)[15] = "long"
# without_na = with_coordinates[!is.na(with_coordinates$long),]



# Creating heatmap of continents on world map...
# Reference: https://stackoverflow.com/questions/28078431/how-to-create-a-world-heat-map-in-r-on-continent-level
world = map_data("world")
url = "https://gist.githubusercontent.com/hrbrmstr/91ea5cc9474286c72838/raw/f3fde312c9b816dff3994f39f2bcda03209eff8f/continents.json"
stop_for_status(GET(url, write_disk("./data/continents.json", overwrite = TRUE)))
continents = readOGR("./data/continents.json")
continents_map = fortify(continents, region="CONTINENT")
cont_data = read.table(text="id,  value
Europe,  59.185692
Australia, 4.355385
Asia,  15.287538
North America, 10.400154
South America, 2.140462
Africa,  7.450000
Antarctica, 0", header=TRUE, stringsAsFactors=FALSE, sep = ",")


# Left join on enrollment table with alpha 2 code table using detected_country column...
colnames(alpha2lat)[2] = "detected_country"
alpha2lat$detected_country = trimws(alpha2lat$detected_country)
with_coordinates = merge(x = enrolments, y = alpha2lat[,c(2,5,6)], by = "detected_country", all.x = TRUE)
colnames(with_coordinates)[14] = "lat"
colnames(with_coordinates)[15] = "long"
without_na = with_coordinates[!is.na(with_coordinates$long),]


# Removing unknown from each column(gender, education lvl, employment status, employment area, age range)...
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



# Finding which device is being use more often to access the course content, calculating column wise mean by grouping on title...
desktop_percentage = video %>% group_by(title) %>% summarize(desktop = mean(desktop_device_percentage))
mobile_percentage = video %>% group_by(title) %>% summarize(mobile = mean(mobile_device_percentage))
tablet_percentage = video %>% group_by(title) %>% summarize(tablet = mean(tablet_device_percentage))

# Creating a dataframe of topic wise device used...
device_df = cbind(desktop_percentage, mobile_percentage[,2], tablet_percentage[,2])
device_df = as.data.frame(colMeans(device_df[2:4]))
colnames(device_df) = c("Usage")


# Selecting columns associated with desktop, mobile, and tablet...
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


# Dataframe of means of each device as columns and iterations as rows...
mean_df = rbind(iteration_3, iteration_4, iteration_5, iteration_6, iteration_7)
colnames(mean_df) = c("Desktop", "Mobile", "Tablet")

# Converting the wide data to long data...
long_mean_df = melt(mean_df)
colnames(long_mean_df) = c('Iteration', 'Device', 'Watch_Time')


# Text processing, string_cleaner() is a helper function residing in ./lib/helpers.r used to clean the text...
cleaned_string = string_cleaner(weekly$reason)
clean_txt_df = data.frame(txt = strsplit(cleaned_string, split = " "))
clean_txt_df = data.frame(table(clean_txt_df))
colnames(clean_txt_df) = c("txt", "freq")

# Calculating sentiment scores...
# get_nrc_sentiment function which accepts a character vector and returns the sentiment score for each words...
sent <- get_nrc_sentiment(as.character(clean_txt_df$txt)) #
sent_score = colSums(sent)
sent_score_df = rbind(colnames(sent), sent_score)
sent_score_df = as.data.frame(t(sent_score_df))
sent_score_df = subset(sent_score_df, select = -c(1))
sent_score_df$sent_score = as.numeric(sent_score_df$sent_score)


# Counting the number of time each reason was used in leaving survey...
leaving_reason = data.frame(table(leaving$leaving_reason))
colnames(leaving_reason) = c('reason', 'freq')

# Cleaning the special chars from leaving reasons...
leaving_reason$reason = gsub("[^[:alnum:]]", " ",as.character(leaving_reason$reason))
leaving_reason$reason = gsub("â", "",as.character(leaving_reason$reason))
leaving_reason$reason = gsub("n t", "n't",as.character(leaving_reason$reason))
hsize = 1
leaving_reason = leaving_reason %>% mutate(x = hsize)


# Calculating mean of percent of fully watched, half watched, 75% watched, 25% watched columns by grouping over title...
total_views = (video %>% group_by(title) %>% summarize(total_views = mean(total_views)))
total_views$full_watch = (video %>% group_by(title) %>% summarize(watch_100 = mean(viewed_onehundred_percent)))$watch_100
total_views$qua3_watch = (video %>% group_by(title) %>% summarize(watch_75 = mean(viewed_seventyfive_percent)))$watch_75
total_views$half_watch = (video %>% group_by(title) %>% summarize(watch_50 = mean(viewed_fifty_percent)))$watch_50
total_views$qua_watch = (video %>% group_by(title) %>% summarize(watch_25 = mean(viewed_twentyfive_percent)))$watch_25
total_views = subset(total_views, select = -c(title)) # removing title column...
total_views = normalize(total_views, method = "range", range = c(0,1)) # Normalizing the data in the range of 0,1...
total_views = as.data.frame(cbind(total_views, video$video_duration[1:13]))
colnames(total_views)[6] = "Video_Length"
colnames(total_views)[1] = "View_Count"

total_views$Video_Length = as.factor(total_views$Video_Length) # Converting video_length to factor...
views = subset(total_views, select = -c(View_Count)) # removing view_count column...
views = melt(views, id.vars = c('Video_Length')) # wide to long data...
colnames(views) = c("Video_Length", "Watch_Status", "Value")
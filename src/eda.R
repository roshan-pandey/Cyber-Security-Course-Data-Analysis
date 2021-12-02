########################################################################################################
#                                   F I L E   D E S C R I P T I O N                                    #
########################################################################################################
#                                                                                                      #
# This file contains the code related to EDA: general data analysis by plotting various graphs and     #
# plots on different data to get most out the analysis. This file is using variables created in        #
# ./munge/01-A.R file. So, to check the variable declaration please refer 01-A.R file.                 #
########################################################################################################

# Plot for Number of learners enrolled over different iteration...
performance = ggplot()+
  geom_line(data = dim_df, mapping = aes(x = batch, y = Number_of_learners_enrolled, group = 1), col = "blue")+
  geom_point(data = dim_df, mapping = aes(x = batch, y = Number_of_learners_enrolled, group = 1), col = "red", size = 2)+
  xlab("Batches")+
  ylab("Number of Learners")

#######################################################################################################################################

# Plot for Continent from where most learners are enrolling...
continents_plot = ggplot()+
  geom_map(data = continents_map, map = continents_map, aes(x=long, y = lat, map_id = id), color = "gray")+
  geom_map(data = cont_data, map = continents_map, aes(map_id = id, fill = value), color = "gray")+
  scale_fill_distiller("Percentage Views")+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_equal()+
  labs(x=NULL, y=NULL)+
  theme(text = element_text(size = 20))

#######################################################################################################################################

# Plot for Country from where most learners are enrolling...
country_plot = ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region), color = "black", fill = "lightgray")+
  stat_density2d(aes(x = long, y = lat, fill = ..level.., alpha = 0.5), bins = 30, data = without_na, geom = "polygon")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(text = element_text(size = 20))

#######################################################################################################################################

# Plot for Which type of people are joining the course most..?
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
  theme(axis.text.x = element_text(angle=15, hjust = 1))

gg_age_range = ggplot(data = age_range, aes(x = Var1, y = Freq, fill = Var1))+
  geom_bar(stat = "identity")+
  labs(fill = "Age Range")+
  xlab("Age Range")+
  scale_fill_brewer(palette = "Set3")

lay = rbind(c(1,2),
            c(3,4),
            c(5,5),
            c(5,5))
arrange_plot = grid.arrange(gg_gender, gg_age_range, gg_emp_status, gg_edu_lvl, gg_emp_area, layout_matrix = lay)

#########################################################################################################################################

# Plot for Most widely used device to access the content...
device_plot = ggplot(device_df, aes(x = row.names(device_df), y = Usage, fill = row.names(device_df)))+ 
  geom_bar(stat = "identity")+
  xlab("Device")+
  labs(fill = "Devices")+
  scale_fill_brewer(palette = "Set3")

########################################################################################################################################

# Most widely used device to access the content in each iteration..?
device_ittr_plot = ggplot()+
  geom_bar(long_mean_df, mapping = aes(x = Iteration, y = Watch_Time, fill = Device), stat = 'identity', position = position_dodge())+
  geom_line(as.data.frame(mean_df), mapping = aes(x = as.factor(rownames(mean_df)), y = Mobile, group = 1, color = "Mobile Usage"), size = 1.5)+
  geom_point(as.data.frame(mean_df), mapping = aes(x = as.factor(rownames(mean_df)), y = Mobile, group = 1, color = "Mobile Usage"), size = 4)+
  ylab("Usage")+
  xlab("Batch Iteration")+
  scale_color_manual(" ", values = c("Mobile Usage" = "DarkBlue"))+
  scale_fill_brewer(palette = "Set3")

#######################################################################################################################################

# Word Cloud for Overall sentiments of learners...
sentiments_word_cloud = ggplot(clean_txt_df, aes(label = txt, size = freq, color = factor(sample.int(10, nrow(clean_txt_df), replace = TRUE)),)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 35) +
  theme_minimal()

# Plot for Overall sentiment scores of learners...
sent_plot = ggplot(data = sent_score_df, mapping = aes(x = row.names(sent_score_df), y = sent_score, fill = row.names(sent_score_df)))+
  geom_bar(stat = "identity")+
  xlab("Sentiments")+
  ylab("Sentiment Score")+
  labs(fill = "Sentiments")+
  scale_fill_brewer(palette = "Set3")

#######################################################################################################################################

# Plot for Reason for leaving the course...
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

#######################################################################################################################################

# Plot for Optimal Video Length...
views_comparison_plot = ggplot(views, aes(x = Video_Length, y = Value, group = Watch_Status, color = Watch_Status))+
  geom_bar(data = total_views, mapping = aes(x = Video_Length, y = View_Count), 
           fill = "steelblue", stat = "identity", inherit.aes = FALSE)+
  geom_point(size = 2.5)+
  geom_line(size = 1.5)+
  xlab("Video Length")+
  ylab("Normalized Watch Percent")+
  labs(fill = "Watch Status")

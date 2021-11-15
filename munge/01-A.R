# library(sjmisc)


# # Reading all the files and merging similar files...
# filenames <- list.files("C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/raw_data", pattern="*.csv", full.names=TRUE)
# 
# # Reading all the csv files...
# ldf <- lapply(filenames, read.csv)
# 
# archetype_index = grep("archetype", ldf)
# archetype_df = rbind(data.frame(ldf[1]), data.frame(ldf[7]), data.frame(ldf[14]), data.frame(ldf[22]), data.frame(ldf[30]), data.frame(ldf[38]), data.frame(ldf[46]))
# 
# enrolments_index = grep("enrol", ldf)
# enrolments_df = rbind(data.frame(ldf[2]), data.frame(ldf[8]), data.frame(ldf[15]), data.frame(ldf[23]), data.frame(ldf[31]), data.frame(ldf[39]), data.frame(ldf[47]))
# 
# leaving_index = grep("leaving", ldf)
# leaving_df = rbind(data.frame(ldf[3]), data.frame(ldf[9]), data.frame(ldf[16]), data.frame(ldf[24]), data.frame(ldf[32]), data.frame(ldf[40]), data.frame(ldf[48]))
# 
# question_index = grep("question", ldf)
# question_df = rbind(data.frame(ldf[4]), data.frame(ldf[10]), data.frame(ldf[17]), data.frame(ldf[25]), data.frame(ldf[33]), data.frame(ldf[41]), data.frame(ldf[49]))
# 
# step_index = grep("first_visited_at", ldf)
# step_df = rbind(data.frame(ldf[5]), data.frame(ldf[11]), data.frame(ldf[18]), data.frame(ldf[26]), data.frame(ldf[34]), data.frame(ldf[42]), data.frame(ldf[50]))
# 
# video_index = grep("video_duration", ldf)
# video_df = rbind(data.frame(ldf[20]), data.frame(ldf[28]), data.frame(ldf[36]), data.frame(ldf[44]), data.frame(ldf[52]))
# 
# weekly_index = grep("experience_rating", ldf)
# weekly_df = rbind(data.frame(ldf[6]), data.frame(ldf[13]), data.frame(ldf[21]), data.frame(ldf[29]), data.frame(ldf[37]), data.frame(ldf[45]), data.frame(ldf[53]))
# 
# team_index = grep("team", ldf)
# team_df = rbind(data.frame(ldf[12]), data.frame(ldf[19]), data.frame(ldf[27]), data.frame(ldf[35]), data.frame(ldf[43]), data.frame(ldf[51]))


# writing the data frame to csv files...
# write.csv(archetype_df,"C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/data_v1/archetype.csv", row.names = FALSE)
# write.csv(enrolments_df,"C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/data_v1/enrolments.csv", row.names = FALSE)
# write.csv(leaving_df,"C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/data_v1/leaving.csv", row.names = FALSE)
# write.csv(question_df,"C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/data_v1/question.csv", row.names = FALSE)
# write.csv(step_df,"C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/data_v1/step.csv", row.names = FALSE)
# write.csv(weekly_df,"C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/data_v1/weekly.csv", row.names = FALSE)
# write.csv(video_df,"C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/data_v1/video.csv", row.names = FALSE)
# write.csv(team_df,"C:/DataScience/R/CSC8631_Data_Management/Cyber-Security-Data-Analysis/data/data_v1/team.csv", row.names = FALSE)
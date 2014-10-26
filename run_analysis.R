#
# cproject.R
#
# Course Project for course getdata-008, Getting and Ceaning Data
# Coursera.org
#
# 26 October 2014
#

load_data<-function(path) {
  #
  # Load and combine data sets residing in path
  #
  # path: string specifying path to data root folder
  # returns: Dataframe that contain means and standard deviations for all
  #          measurements.  Data include subject, activity and activity
  #          description.
  #
   
  # Load test data
  test_data<-read.table(paste(path,"/test/X_test.txt", sep=''))
  test_activity<-read.table(paste(path,"/test/y_test.txt", sep=''),col.names=c('Activity_id'))
  test_subject<-read.table(paste(path,"/test/subject_test.txt", sep=''), col.names=c('Subject_id'))

  # Load training data
  train_data<-read.table(paste(path,"/train/X_train.txt", sep=''))
  train_activity<-read.table(paste(path,"/train/y_train.txt", sep=''),col.names=c('Activity_id'))
  train_subject<-read.table(paste(path,"/train/subject_train.txt", sep=''), col.names=c('Subject_id'))
  
  d_test<-combine_data(path, test_data, test_activity, test_subject)
  d_train<-combine_data(path, train_data, train_activity, train_subject)
  rbind(d_test, d_train)
}


combine_data<-function(path, the_data, activity, subject){
  #
  # Enrich data by combining test data, activity and subject.  
  # Description provided for activity.
  #
  # path: string specifying path to data root folder
  # the_data: data_frame with measurements
  # activity: data_frame with activity corresponding to measurements
  # subject:  data_frame with subject corresponding to measurements
  #
  # returns:  Enriched data set
  col_names<-read.table(paste(path,"/features.txt", sep=''))[[2]]
  activity_labels<-read.table(paste(path,"/activity_labels.txt", sep=''), col.names=c('Activity_id', 'Activity'))
  
  #set column names
  names(the_data)<-col_names
  
  # Merge with Subject data
  master_data<-activity
  master_data<-cbind(master_data, subject)
  
  # Merge with Descriptive activity names
  master_data<-merge(master_data, activity_labels, by.x="Activity_id", by.y="Activity_id" )
  
  # Extract only mean and std columns
  for(col_name in col_names) {
    if(grepl("mean()", col_name, fixed=TRUE) | grepl("std()", col_name, fixed=TRUE)) {
      master_data<-cbind(master_data, the_data[col_name])
    }    
  }

  master_data
}

calc_average<-function(data_set) {
  #
  # Calculate average for each variable in data_Set.  Ignores standard variation.
  #
    master_data<-data_set[1:3]
    for(col_name in names(data_set)) {
      if(grepl("mean()", col_name, fixed=TRUE)) {
        master_data<-cbind(master_data, data_set[col_name])
      } 
    }
    
    avg_values = ddply(master_data, .(Activity, Subject_id), numcolwise(mean))
    avg_values
}

master_data<-load_data("UCI HAR Dataset")
averages<-calc_average(master_data)
write.table(averages,file="coursera_output.txt")
write.table(master_data, file="master.txt")
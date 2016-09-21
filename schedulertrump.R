taskscheduler_create(taskname = "tasktrump", rscript = trumptweets, 
  schedule = "DAILY", starttime = "15:30", startdate = format(Sys.Date(), "%d/%m/%Y"))
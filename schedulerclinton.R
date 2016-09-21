taskscheduler_create(taskname = "taskclinton", rscript = clintontweets, 
  schedule = "DAILY", starttime = "11:30", startdate = format(Sys.Date(), "%d/%m/%Y"))
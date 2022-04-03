pacman::p_load(taskscheduleR)
tasks <- taskscheduler_ls()

# Tasks -------------------------------------------------------------------
taskscheduler_create(
  taskname  = "copy_and_move_odx",
  rscript   = "S:/_V_PAHRL/FLAC/R/scripts/TASK_copy_and_move_odx.R",
  schedule  = "WEEKLY",
  starttime = "09:00",
  startdate = "04/08/2022", # %m/%d/%Y format
  days      = "FRI"
)

taskscheduler_delete("copy_and_move_odx")


# HELP --------------------------------------------------------------------


# myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
# 
# ## run script once within 62 seconds
# taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
#                      schedule = "ONCE", starttime = format(Sys.time() + 62, "%H:%M"))
# 
# ## Run every day at the same time on 09:10, starting from tomorrow on
# ## Mark: change the format of startdate to your locale if needed (e.g. US: %m/%d/%Y)
# taskscheduler_create(taskname = "myfancyscriptdaily", rscript = myscript, 
#                      schedule = "DAILY", starttime = "09:10", startdate = format(Sys.Date()+1, "%d/%m/%Y"))
# 
# ## Run every week on Saturday and Sunday at 09:10
# taskscheduler_create(taskname = "myfancyscript_sunsat", rscript = myscript, 
#                      schedule = "WEEKLY", starttime = "09:10", days = c('SUN', 'SAT'))
# 
# ## Run every 5 minutes, starting from 10:40
# taskscheduler_create(taskname = "myfancyscript_5min", rscript = myscript,
#                      schedule = "MINUTE", starttime = "10:40", modifier = 5)
# 
# ## Run every minute, giving some command line arguments
# taskscheduler_create(taskname = "myfancyscript_withargs_a", rscript = myscript,
#                      schedule = "MINUTE", rscript_args = "productxyz 20160101")
# taskscheduler_create(taskname = "myfancyscript_withargs_b", rscript = myscript,
#                      schedule = "MINUTE", rscript_args = c("productabc", "20150101"))
# 
# 
# ## get a data.frame of all tasks
# tasks <- taskscheduler_ls()
# str(tasks)
# 
# ## delete the tasks
# taskscheduler_delete(taskname = "myfancyscript")
# taskscheduler_delete(taskname = "myfancyscriptdaily")
# taskscheduler_delete(taskname = "myfancyscript_sunsat")
# taskscheduler_delete(taskname = "myfancyscript_5min")
# taskscheduler_delete(taskname = "myfancyscript_withargs_a")
# taskscheduler_delete(taskname = "myfancyscript_withargs_b")
# 
# ## Have a look at the log
# mylog <- system.file("extdata", "helloworld.log", package = "taskscheduleR")
# cat(readLines(mylog), sep = "\n")




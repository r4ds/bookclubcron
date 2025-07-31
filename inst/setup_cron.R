library(taskscheduleR)

taskscheduler_delete(taskname = "dslc_clubs")
taskscheduler_delete(taskname = "dslc_clear_reminders")

clubs_script <- system.file("runners", "clubs.R", package = "bookclubcron")
start_time <- lubridate::now()
lubridate::minute(start_time) <- 30
if (start_time <= lubridate::now()) {
  start_time <- start_time + lubridate::hours(1)
}
start_time <- paste0(
  stringr::str_pad(lubridate::hour(start_time), 2, pad = "0"),
  ":",
  30
)
taskscheduler_create(
  taskname = "dslc_clubs",
  rscript = clubs_script,
  schedule = "HOURLY",
  # schedule = "ONCE",
  starttime = start_time,
  startdate = format(Sys.Date(), "%m/%d/%Y")
)


tasks <- taskscheduler_ls() |>
  tibble::as_tibble()

tasks |>
  dplyr::filter(TaskName == "dslc_clubs") |>
  dplyr::glimpse() |>
  dplyr::pull("Task To Run")


reminder_script <- system.file(
  "runners",
  "clear_reminders.R",
  package = "bookclubcron"
)

start_time <- lubridate::now()
lubridate::minute(start_time) <- 0
if (start_time <= lubridate::now()) {
  start_time <- start_time + lubridate::hours(1)
}
start_time <- paste0(
  stringr::str_pad(lubridate::hour(start_time), 2, pad = "0"),
  ":",
  "00"
)
taskscheduler_create(
  taskname = "dslc_clear_reminders",
  rscript = reminder_script,
  schedule = "HOURLY",
  # schedule = "ONCE",
  starttime = start_time,
  startdate = format(Sys.Date(), "%m/%d/%Y")
)

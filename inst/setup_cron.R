library(taskscheduleR)
clubs_script <- system.file("runners", "clubs.R", package = "bookclubcron")

taskscheduler_create(
  taskname = "dslc_clubs",
  rscript = clubs_script,
  schedule = "HOURLY",
  # schedule = "ONCE",
  starttime = "15:30",
  startdate = format(Sys.Date(), "%m/%d/%Y")
)
# taskscheduler_delete(taskname = "dslc_clubs")


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

taskscheduler_create(
  taskname = "dslc_clear_reminders",
  rscript = reminder_script,
  schedule = "HOURLY",
  # schedule = "ONCE",
  starttime = "15:00",
  startdate = format(Sys.Date(), "%m/%d/%Y")
)
# taskscheduler_delete(taskname = "dslc_clubs")

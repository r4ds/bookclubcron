library(taskscheduleR)
clubs_script <- system.file("runners", "clubs.R", package = "bookclubcron")

taskscheduler_create(
  taskname = "r4ds_clubs",
  rscript = clubs_script,
  schedule = "HOURLY",
  # schedule = "ONCE",
  starttime = "15:30",
  startdate = format(Sys.Date(), "%m/%d/%Y")
)
# taskscheduler_delete(taskname = "r4ds_clubs")


tasks <- taskscheduler_ls() |>
  tibble::as_tibble()

tasks |>
  dplyr::filter(TaskName == "r4ds_clubs") |>
  dplyr::glimpse() |>
  dplyr::pull("Task To Run")


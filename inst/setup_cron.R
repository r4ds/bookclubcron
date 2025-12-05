pkgload::load_all()

create_task <- purrr::partial(
  taskscheduleR::taskscheduler_create, 
  Rexe = utils::shortPathName(
    file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe")
  ),
  schedule = "HOURLY",
  startdate = format(Sys.Date(), "%m/%d/%Y")
)

tasks <- tibble::tibble(
  task = c("dslc_clubs", "dslc_clear_reminders"),
  script = c("clubs.R", "clear_reminders.R"),
  start_time_minutes = c(30, 0)
)

purrr::pwalk(
  tasks,
  function(task, script, start_time_minutes) {
    script_path <- system.file("runners", script, package = "bookclubcron")
    start_time <- lubridate::now()
    lubridate::minute(start_time) <- start_time_minutes
    if (start_time <= lubridate::now()) {
      start_time <- start_time + lubridate::hours(1)
    }
    start_time_formatted <- paste0(
      stringr::str_pad(lubridate::hour(start_time), 2, pad = "0"),
      ":",
      stringr::str_pad(start_time_minutes, 2, pad = "0")
    )
    taskscheduleR::taskscheduler_delete(task)
    create_task(
      taskname = task,
      rscript = script_path,
      starttime = start_time_formatted
    )
    configure_task_properties(task)
  }
)

configure_task_properties <- function(task_name) {
  ps_script_content <- glue::glue(
    "
    $taskName = '{task_name}';
    try {{
      $task = Get-ScheduledTask -TaskName $taskName -ErrorAction Stop;
      $newPrincipal = New-ScheduledTaskPrincipal -UserId $task.Principal.UserId -LogonType S4U;
      $settings = $task.Settings;
      $settings.Hidden = $true;
      $settings.WakeToRun = $true;
      $settings.RunOnlyIfNetworkAvailable = $true;
      $settings.StartWhenAvailable = $true;
      $settings.ExecutionTimeLimit = 'PT1H'; # ISO 8601 format
      Set-ScheduledTask -TaskName $taskName -Principal $newPrincipal -Settings $settings -ErrorAction Stop;
    }} catch {{
      Write-Error $_.Exception.Message;
      Read-Host 'Press Enter to exit'; # Keep window open if it errors
    }}
  "
  )

  ps_file <- tempfile(fileext = ".ps1")
  writeLines(ps_script_content, ps_file)

  run_command <- glue::glue(
    "Start-Process powershell -Verb RunAs -ArgumentList '-NoProfile -ExecutionPolicy Bypass -File \"{ps_file}\"'"
  )
  system2("powershell", args = c("-Command", run_command))
  cli::cli_inform("Task {.str {task_name}} properties have been configured.")
}

#repo="https://raw.githubusercontent.com/AdamWilsonLabEDU/geo511_tasks_tests"

questionr::qscan(list.files(pattern="*.R",recursive=T), load = TRUE, detail = FALSE)

setwd(devtools::package_file())

testthat::test_dir(testthat::test_path(),stop_on_failure = TRUE,reporter = "progress")
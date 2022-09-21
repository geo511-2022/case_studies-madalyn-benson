base_url="https://raw.githubusercontent.com/AdamWilsonLabEDU/GEO511_tests/master/"
test_url=paste0(base_url,"test-CS06.R")
setwd(devtools::package_file())

if(RCurl::url.exists(test_url)) source(test_url)

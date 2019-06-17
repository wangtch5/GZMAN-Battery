# guzm utils

standard.date <- function(dat, form=0){
  dat <- as.character(dat) %>% strsplit(" ")
  date <- sapply(dat, function(x) x[1])
  time <- sapply(dat, function(x) x[2])
  # if we need to provide form to manipulate date
  if(form != 0){
    date <- as.Date(date, format = form)
  }

  return(data.frame(DATE=date, TIME=time))
}
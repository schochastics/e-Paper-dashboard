# this is where the libraries are installed to
.libPaths("/home/pi/R/4.0.2/library/")
#libraries ----
library(ggplot2)
library(png)
library(grid)
library(showtext)
library(gcalendr)
library(reticulate)
source("sprite_helper.R")

use_python("/usr/bin/python3")

#custom functions ----
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# fonts ----
showtext_auto()
font_add_google("Acme","Acme")
font_add_google("Montserrat","Montserrat")

# read helper data ----
weather_icons <- read.csv("weather_symbols.csv")
frame <- data.frame(xmin=0,xmax=8,ymin=0,ymax=4.8)

#initialize calendar
cal <- gcalendr::calendar_events("CALENDARID",token =,days_in_past = 0,days_in_future = 5)
update_cal <- 5
calendar_symbol <- readPNG("other_icons/calendar-symbol.png")
calendar_symbol <- 1-calendar_symbol
calendar_grob <- rasterGrob(calendar_symbol, interpolate=TRUE)

while(TRUE){
  # run at
  now <- Sys.time()
  h <- as.numeric(format(now,format="%H"))
  cat(paste0("running at ",now,"\n"))
  #------------------------------------------------------------------------------#
  # get weather data ----
  #------------------------------------------------------------------------------#
  cat("getting weather data\n")
  base_url <- "http://api.openweathermap.org/data/2.5/weather?q="
  city <- "Manchester,uk"
  api_key <- "OPENWEATHERAPIKEY"
  weather_data <- jsonlite::fromJSON(paste0(base_url,city,"&appid=",api_key,"&units=metric"))
  
  sunrise <- paste0("up: ",as.character(format(as.POSIXct(weather_data$sys$sunrise,origin="1970-01-01"),format="%H:%M")))
  sunset <- paste0("down: ",as.character(format(as.POSIXct(weather_data$sys$sunset,origin="1970-01-01"),format="%H:%M")))
  sundf <- data.frame(x=c(2.6,3.25),y=c(3.93,3.93),time=c(sunrise,sunset),label=c("sunrise","sunset"))
  
  id <- which(weather_icons$description==weather_data$weather$description)
  
  weather_symbol <- readPNG(paste0("weather_icons/",weather_icons$icon[id],".png"))
  weather_grob <- rasterGrob(weather_symbol, interpolate=TRUE)
  cur_date <- substr(date(),1,10)
  datedf <- data.frame(x=1.1,y=4.7,label=cur_date,timestamp=now)
  tempdf <- data.frame(x=1.1,y=4.2,label=paste0(round(weather_data$main$temp,1),"°C"),celcius = weather_data$main$temp,timestamp=now)
  conddf <- data.frame(x=c(2.05,2.05),y=c(4.2,4.1),label=c("Manchester, UK",tolower(weather_data$weather$main)))
  write.table(tempdf,file = "weather_data.csv",append=file.exists("weather_data.csv"),quote = FALSE,sep=",",
              col.names = !file.exists("weather_data.csv"))
  #------------------------------------------------------------------------------#
  # get calendar ----
  # code to prevent random crashes of the API
  #------------------------------------------------------------------------------#
  cat("getting calendar data\n")
  if(update_cal==5){
    cal <- tryCatch(gcalendr::calendar_events("CALENDARID",token =,days_in_past = 0,days_in_future = 5),
                    error=function(e){
                      print("error! retrying")  
                      return(tibble::tibble())
                    })
    rt <- 0
    while(nrow(cal)==0 & rt<=5){
      Sys.sleep(5)
      cal <- tryCatch(gcalendr::calendar_events("CALENDARID",token =,days_in_past = 0,days_in_future = 5),
                      error=function(e){
                        print("error! retrying")  
                        return(tibble::tibble())
                      })
    }
    cal$start_datetime <- as.POSIXct(cal$start_datetime+3600) #BST
    cal$end_datetime <- as.POSIXct(cal$end_datetime+3600) #BST
    cal <- cal[order(cal$start_datetime),c(1,2,3,5)]
    cal <- cal[cal$start_datetime>=now,]
    cal <- cal[1:min(c(5,nrow(cal))),]
    cal$summary <- capwords(cal$summary)
    cal$x <- 0.1
    cal$y <- seq(3.1,0.5,length.out = 5)[1:nrow(cal)]
    cal$date <- format(as.Date(cal$start_datetime),format="%b %d %Y")
    cal$time <- paste0(format(cal$start_datetime,format="%H:%M")," - ",format(cal$end_datetime,format="%H:%M"))
    cal$loc <- ifelse(is.na(cal$location),"", paste0("(",cal$location,")"))
    cal$date_time <- paste(cal$date,cal$time,cal$loc)
    update_cal <- 0
  } else{
    update_cal <- update_cal + 1
  }
  #------------------------------------------------------------------------------#
  # sprites ----
  #------------------------------------------------------------------------------#
  cat("rendering sprites\n")
  sprite_set <- lapply(1:64,function(x)
    generate_sprite(
      space,
      color_variations=0.2, 
      brightness_noise=0.3,
      edge_brightness=0.3, 
      saturation=0.8, 
      mirror = TRUE,
      colored = FALSE))
  
  sprites_df <- do.call("rbind",lapply(1:length(sprite_set),function(i){
    A <- sprite_set[[i]]
    data.frame(col=c(A),x=rep(1:ncol(A),each=nrow(A)),y=rep(nrow(A):1,ncol(A)),run=i)
  }))
  
  psprites <- ggplot(sprites_df)+
    geom_raster(aes(x,y,fill=I(col)))+
    facet_wrap(~run,scales = "free",ncol=8)+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    theme_void()+
    theme(strip.text = element_blank())
  
  sprites_annot <- ggplotGrob(psprites)
  
  #------------------------------------------------------------------------------#
  # draw all ----
  # only draw during the day
  #------------------------------------------------------------------------------#
  if(h>=7 && h<=20){
    cat("build plot\n")
    ggplot() +
      # weather
      geom_text(data=datedf,aes(x,y,label=label),size=16,hjust=0,vjust=1,fontface="bold",family="Acme")+
      geom_text(data=tempdf,aes(x,y,label=label),size=8 ,hjust=0,vjust=1,family="Montserrat")+
      geom_text(data=conddf,aes(x,y,label=label),size=4 ,hjust=0,vjust=1,family="Montserrat")+
      geom_text(data=sundf,aes(x,y,label=time),size=3.5 ,hjust=0,vjust=1,family="Montserrat")+
      annotation_custom(weather_grob, xmin=0, xmax=1, ymin=3.8, ymax=4.8) +
      #calendar
      geom_text(data = cal,aes(x,y,label=summary),size = 7,
                fontface="bold",family="Acme",hjust = 0,vjust = 1)+
      geom_text(data = cal,aes(x,y-0.25,label=date_time),size = 5,
                family="Montserrat",hjust = 0,vjust = 1)+
      geom_segment(data=cal,aes(x=x,y=y-0.45,xend=4,yend=y-0.45),size=1.1)+
      annotate("rect",xmin=0,xmax=4,ymin=3.2,ymax=3.8,fill="black")+
      annotate("text",x=0.9,y=3.5,label="Tasks&Meetings",hjust=0,vjust=.5,col="white",size=10,fontface="bold",family="Acme")+
      annotation_custom(calendar_grob, xmin=-0.4, xmax=1, ymin=3.25, ymax=3.75) +
      #sprites
      annotation_custom(sprites_annot,xmin=4.05,xmax=7.95,ymin=0.05,ymax=3.75)+
      # frames
      geom_rect(data=frame,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=NA)+
      annotate("segment",x=0,xend=8,y=3.8,yend=3.8,size=1.2)+
      annotate("segment",x=4,xend=4,y=0,yend=4.8,size=1.2)+
      # scales/themes
      scale_x_continuous(limits=c(0,8),expand = c(0,0))+
      scale_y_continuous(limits=c(0,4.8),expand = c(0,0))+
      theme_void() -> p
    # dev.off()
    
    ggsave("raw-output.bmp",p,width=5,height=3,dpi = 160)
    
    system("convert -colors 2 +dither -type Bilevel -monochrome raw-output.bmp screen-output.bmp")
    # rm("Rplots.pdf")
    cat("sending to display\n")
    py_run_file("display.py")
  }
  cat("sleeping\n")
  Sys.sleep(10*60)
  cat("##########\n")
}

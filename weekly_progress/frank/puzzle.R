# randomly select 4 heritage
library(OpenImageR)
library(purrr)
library(showimage)
library(lubridate)
puzzle_guess <- function(){
       setwd("/Users/frankchao/Desktop/109-2-app101/weekly_progress/frank")
       lv  <- rstudioapi::showPrompt(title = "請選擇難度" ,
                                     message = paste0("1. easy ",
                                                      "2. medium ",
                                                      "3. hard "),
                                     default = "")
       heritages_index <- sample(1:982,4,replace = F)
       pic <- generate_puzzle(as.numeric(lv),heritages_index)
       imageShow(pic)
       guess_order <- {
                         order <- sample(1:4,4,replace = F)
                         c (  heritages_index[order[1]],
                              heritages_index[order[2]],
                              heritages_index[order[3]],
                              heritages_index[order[4]]) 
       }
       time_start <- format_ISO8601(now(),usetz = T)
       answer <- rstudioapi::showPrompt(title = "請輸入選項編號" ,
                                        message =paste( "1. ",heritageData$caseName[guess_order[1]],
                                                        "2. ",heritageData$caseName[guess_order[2]],
                                                        "3. ",heritageData$caseName[guess_order[3]],
                                                        "4. ",heritageData$caseName[guess_order[4]]
                                        ),  
                                        default ="")
       return(
               list(
                     timestamp = as.character(time_start) ,
                     answer  = heritageData$caseId[heritages_index[1]] ,
                     options = c(heritageData$caseId[guess_order]) ,
                     user_choice = heritageData$caseId[guess_order[as.numeric(answer)]]    
               )
             ) 
}

load_image <- function(heritages_index){
          imgX <- {
            # 取得古蹟的圖片網址
            targetUrl <- heritageData$representImage[[heritages_index[1]]] 
            targetUrlencoded <- URLencode(targetUrl)
            download.file(targetUrlencoded,paste0(heritageData$caseName[[heritages_index[1]]],".jpg"))
            readImage(paste0(heritageData$caseName[[heritages_index[1]]],".jpg"))
          } 
          return(imgX)
}

generate_puzzle <- function(level,heritages_index){
          imgX <- load_image(heritages_index)
          if(nrow(imgX) %% level != 0) imgX = imgX[1:(nrow(imgX) - nrow(imgX) %% level),,]
          if(ncol(imgX) %% level != 0) imgX = imgX[,1:(ncol(imgX) - ncol(imgX) %% level),]
          rows  <-  map(
            1:level,    
            ~{
              (((.x-1)*nrow(imgX)/level)+1) : (.x*nrow(imgX)/level)
            }
          )
          cols  <-  map(
            1:level,    
            ~{
              (((.x-1)*ncol(imgX)/level)+1) : (.x*ncol(imgX)/level)
            }
          )
          rows_order <- sample(1:level,level,replace = F)
          cols_order <- sample(1:level,level,replace = F)
          newPicture <- array(vector("numeric",length(imgX)),dim=c(nrow(imgX),ncol(imgX),3))
          for(ii in 1:level){
            for(jj in 1:level){
                newPicture[rows[[ii]],cols[[jj]],] <-         
                   imgX[rows[[rows_order[ii]]],cols[[cols_order[jj]]],]
            }
          }
          return(newPicture)
}




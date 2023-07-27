library(stringr)
library(dplyr)
library(tidyr)

combine_files = function( head, filelist, type = "demo"){
  n=35 # get the laste 32 rows
  if(!type %in% c("demo", "questions", "all")){
    stop("type is not one of 'demo', 'questions', or 'all'")
  }
  
  if(length(filelist) > 0){
    file_path = paste(head, filelist[1], sep="")
    dataset = read.csv(file_path)
    
    if(type == "demo"){
      dataset = dataset[,(ncol(dataset)-n+1):ncol(dataset)]
      dataset = dataset %>% mutate(Year.of.Birth = 2023-Year.of.Birth)
      
      # age recode
      dataset = dataset %>% 
        mutate(Year.of.Birth = case_when( 
          Year.of.Birth >= 18 & Year.of.Birth <= 24 ~ "18_to_24",
          Year.of.Birth >= 25 & Year.of.Birth <=34 ~ "25_to_34",
          Year.of.Birth >= 35 & Year.of.Birth <=44 ~ "35_to_44",
          Year.of.Birth >= 45 & Year.of.Birth <= 54 ~ "45_to_54",
          Year.of.Birth >= 55 & Year.of.Birth <= 64 ~ "55_to_64",
          Year.of.Birth >= 65 ~ "65_over"
          ) )
      # edu recode
      dataset = dataset %>%
        mutate(edu_recode = case_when(
          edu_recode == "Less than High School Diploma" ~ "LESS_THAN_HS",
          edu_recode == "High School Graduate (Includes Equivalency)" ~ "HS",
          edu_recode == "Some College or Associates Degree" ~ "SOME_COLLEGE",
          edu_recode == "Bachelors Degree or Higher" ~ "COLLEGE",
          .default = edu_recode
        ))
    }else if(type == "questions"){
      dataset = dataset[,1:(ncol(dataset)-n)]
    }
    
    if(length(filelist) > 1){
      for(i in 2:(length(filelist))){
        file_path = paste(head, filelist[i], sep="")
        temp = read.csv(file_path)
        
        if(type=="demo"){
          temp = temp[,(ncol(temp)-n+1):ncol(temp)] 
          dataset = rbind(dataset, temp)
        }else if(type == "questions"){
          temp = temp[,1:(ncol(dataset)-n)]
          dataset = full_join(dataset, temp)
        }else if(type == "all"){
          dataset = full_join(dataset, temp)
        }
      }
    }
    return(dataset) 
  }
}

get_files = function(head){
  files = list.files(path = head, pattern = "*.csv")
  filtered_files = grep("^(?!.*(?=ID))", files, value = T, perl = T)
  to_return = list(filtered_files)
  return(to_return)
}

get_demographic_info = function(head, surveys, save_loc, save_dir, save_name, survey = T){
  for(i in 1:length(surveys)){
    survey_dir = paste(head, surveys[i],"/",sep = "")
    survey_files = get_files(survey_dir)[[1]] # get all surveys from the survey directory;
    
    # combine the demographic data
    for(file in survey_files){
      data = combine_files(survey_dir, file, type = "demo")
      
      # create save directory 
      save_dir_head = paste(save_loc, save_dir, "/",sep = "") #/Volumes/cbjackson2/ccs-knowledge/ccs-data/ccs-data-demographic_unprocessed/
      if(!save_dir %in% list.files(save_loc)){
        dir.create(save_dir_head)
      }
      
      survey_dir_save_name = NA
      save_dir_survey_dir = NA
      if( survey ){
        if( grepl("map", file, fixed = T) ){
          survey_dir_save_name = paste(surveys[i],"-map/", sep="")
          save_dir_survey_dir = paste(save_dir_head,survey_dir_save_name,sep="")
        }else if( grepl("survey", file, fixed=T) ){
          survey_dir_save_name = paste(surveys[i],"-survey/",sep="")
          save_dir_survey_dir = paste(save_dir_head,survey_dir_save_name,sep="")
        }
      }else{
        survey_dir_save_name= paste(surveys[i], "/", sep="")
        save_dir_survey_dir = paste(save_dir_head,survey_dir_save_name,sep="")
      }
      
      if(!str_sub(survey_dir_save_name,1,-2) %in% list.files(save_dir_head)){
        dir.create(save_dir_survey_dir)
      }
      save_path = paste(save_dir_survey_dir,save_name,sep="")
      print(save_path)
      write.csv(data, save_path)
    }
  }
}

save_loc = "/Volumes/cbjackson2/ccs-knowledge/"

# combine and save demographic survey for air, tree, urban surveys
head = "/Volumes/cbjackson2/ccs-knowledge/ccs-data/"
surveys = c("air-quality", "tree-canopy", "urban-heat")
save_dir = "ccs-data-demographic_unprocessed"
survey_save_name = "demographic_data.csv"
get_demographic_info(head, surveys, save_loc, save_dir, survey_save_name, survey = T)

# save demographic information for ej type surveys
head = "/Volumes/cbjackson2/ccs-knowledge/ccs-data/"
surveys = c("ej-report", "ej-storytile", "ej-survey")
save_dir = "ccs-data-demographic_unprocessed"
survey_save_name = "demographic_data.csv"
get_demographic_info(head, surveys, save_loc, save_dir, survey_save_name, survey = F)




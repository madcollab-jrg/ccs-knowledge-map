combine_files = function( head, filelist, type = "demo"){
  if(!type %in% c("demo", "questions", "all")){
    stop("type is not one of 'demo', 'questions', or 'all'")
  }
  
  if(length(filelist) > 0){
    file_path = paste(head, filelist[1], sep="")
    dataset = read.csv(file_path, skip = 9)
    
    if(type == "demo"){
      dataset = dataset[,(ncol(dataset)-12+1):ncol(dataset)]
    }else if(type == "questions"){
      dataset = dataset[,1:(ncol(dataset)-12)]
    }
    
    if(length(filelist) > 1){
      for(i in 2:(length(filelist))){
        file_path = paste(head, filelist[i], sep="")
        temp = read.csv(file_path, skip = 9)
        
        if(type=="demo"){
          temp = temp[,(ncol(temp)-12+1):ncol(temp)] 
          dataset = rbind(dataset, temp)
        }else if(type == "questions"){
          temp = temp[,1:(ncol(dataset)-12)]
          dataset = full_join(dataset, temp)
        }else if(type == "all"){
          dataset = full_join(dataset, temp)
        }
      }
    }
    return(dataset) 
  }
}

get_files = function(head, dataset, ej = F){
  files = list.files(path = head, pattern = ".csv")
  if(!ej){
    forms = c()
    socialmap = c()
    for(i in 1:length(files)){
      file = files[i]
      if(grepl("Form-", file)){
        forms = append(forms, file)
      }else if(grepl("SocialMap-", file)){
        socialmap = append(socialmap, file)
      }
    }
    to_return = list(forms, socialmap)
    return(to_return)
  }else{
    return(list(files))
  }
}

get_demographic_info = function(head, surveys, save_dir, save_names, ej = F){
  for(i in 1:length(surveys)){
    survey_dir = paste(head, surveys[i], sep = "")
    survey_files = get_files(survey_dir, ej = ej) # get all surveys from the survey directory
    for(j in 1:length(save_names)){
      combined_data = combine_files(survey_dir, survey_files[[j]], type = "demo")
      
      save_dir_path = paste(survey_dir, save_dir, "/",sep = "")
      if(!save_dir %in% list.files(survey_dir)){
        dir.create(save_dir_path)
      }
      
      save_path = paste(save_dir_path, save_names[j],sep="")
      write.csv(combined_data, save_path)
    }
  }
}

synthesize_demographic_info = function(){
  return()
}

# combine and save demographic survey for air, tree, urban surveys
head = "/Volumes/cbjackson2/ccs-knowledge/ccs-data/"
surveys = c("air-quality/", "tree-canopy/", "urban-heat/")
save_dir = "demographic_unprocessed"
survey_save_names = c("Forms_demographic_combined.csv", "SocialMap_demographic_combined.csv")
get_demographic_info(head, surveys, save_dir, survey_save_names, ej = F)

# save demographic information for ej type surveys
head = "/Volumes/cbjackson2/ccs-knowledge/ccs-data/"
surveys = c("ej-report/", "ej-storytile/", "ej-survey/")
save_dir = "demographic_unprocessed"
survey_save_names = c("/demographic_data.csv")
get_demographic_info(head, surveys, save_dir, survey_save_names, ej = T)




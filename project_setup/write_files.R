### Write standard files to working directory

write_files = function(project_shortname = "proj", datasets = list(dataset1 = list(filename="dataset1", location="dataset1_location"), dataset2=list(filename="dataset2", location="dataset_location2")), join=FALSE){
  wd = getwd()
  write_import(project_shortname, wd, "./import.txt", datasets)
  
  write_process(project_shortname, wd, "./process_all.txt", datasets)
  
}

write_import = function(project_shortname, wd, template_file = "./import.txt", datasets){
  import_template = readChar(template_file, file.info(template_file)$size)
  import_files = ""
  return_string = ""
  for(i in 1:length(datasets)){
    dataset_name = names(datasets)[i]
    dataset = datasets[[i]]
    import_files = paste(import_files, str_interp("${dataset$filename} = csv.read('${dataset$location}', stringsAsFactors=FALSE)\n  "), sep="")
    return_string = paste(return_string, str_interp("${dataset_name} = "), str_interp("${dataset_name}, "), sep="")
  }
  return_list = str_interp("list(${gsub(', $', '', return_list)})")
  file_string = import_files
  
  ### Write out file
  writeLines(str_interp(eval(import_template)), "./import.R")
}

write_process = function(project_shortname, wd, template_file = "./process_all.txt", datasets){
  
  process_all_template = readChar(template_file, file.info(template_file)$size)
  ### process all needs variables - project_shortname, process_strings, return_string, process_individual_strings
  process_strings = ""
  return_string = ""
  process_individual_strings = ""
  
  for(i in 1:length(datasets)){
    dataset_name = names(datasets)[i]
    dataset = datasets[[i]]
    process_strings = paste(process_strings, str_interp("${dataset_name}_processed = process_${dataset_name}(${dataset_name})\n  "), sep="")
    return_string = paste(return_string, str_interp("${dataset_name}="), str_interp("${dataset_name}, "), sep="")
    ### import individual template and write to string
    process_individual_template = readChar("./process_individual.txt", file.info("./process_individual.txt")$size)
    process_individual_strings = paste(process_individual_strings, str_interp(eval(process_individual_template)), sep="\n\n")
  }
  
  ### wrap return string
  return_string = str_interp("return(list(${return_string})")
  
  writeLines(str_interp(eval(process_all_template)), "./process.R")
}
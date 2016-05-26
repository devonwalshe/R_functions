## trims whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim_internal = function(x) gsub("\\s+", " ", x)
trim_all = function(x){
  trimmed = trim(x)
  trimmed_internal = trim_internal(trimmed)
  return(trimmed_internal)
}

extract_titles = function(name_strings){
  patterns = "ms|miss|mrs|mr|master|ref|fr|dr|atty|prof|hon|pres|gov|coach|ofc|unknown|ms\\.|miss\\.|mrs\\.|mr\\.|master\\.|ref\\.|fr\\.|dr\\.|atty\\.|prof\\.|hon\\.|pres\\.|gov\\.|coach\\.|ofc\\."
  titles = str_match(name_strings, patterns)
  return(titles)
}

strip_titles = function(name_strings){
  patterns = "ms|miss|mrs|mr|master|ref|fr|dr|atty|prof|hon|pres|gov|coach|ofc|ms\\.|miss\\.|mrs\\.|mr\\.|master\\.|ref\\.|fr\\.|dr\\.|atty\\.|prof\\.|hon\\.|pres\\.|gov\\.|coach\\.|ofc\\."
  scrubbed_names = trim_all(gsub(patterns, "", name_strings, ignore.case=TRUE))
  return(scrubbed_names)
}

top_tail_names = function(name_strings){
  matches = str_match(name_strings, "(^['-[\\w]]+)(.+?)(['-[\\w]]+$)")
  matches[,3] = trim_all(matches[,3])
  return(matches[,2:4])
}

## Rounds up (badly -fix this)
roundUp <- function(x) 10^ceiling(log10(x))


## Function that populates a new column with a standard score
## based the frequency or sum of a certain attribute against a category.
category_to_value = function(df, cat_col, var_col=NULL, new_col_name, scale_data=TRUE, type="sum"){
  
  if(type == "sum") {
    
    cat_table = aggregate(df[,var_col], by=list(cat_col = df[,cat_col]), FUN=eval(type))
    
  } else if (type == "freq") {
    
    cat_table = data.frame(table(df[,cat_col]))
    colnames(cat_table) = c("cat_col", "x")
  }
  if(scale_data == TRUE)   {
    cat_table$x = scale(cat_table$x)
  }
  
  df[,new_col_name] = cat_table$x[match(df[,cat_col], cat_table$cat_col)]
  
  return(df)
}

## create a matrix of aggregated columns by category, choose your columns, and what function to aggregate with

create_aggregate_matrix = function(dataframe, comparison_col, aggregate_columns=c(), func="sum") {
  
  aggregate_matrix = data.frame(table(dataframe[,comparison_col]))
  
  for (i in aggregate_columns) {
    aggregate_matrix = merge(aggregate_matrix, setNames(aggregate(dataframe[,i], by=list(Var1 = dataframe[,comparison_col]), FUN=eval(func)), c("Var1", i)), by="Var1")
  }
  
  #Set column names
  colnames(aggregate_matrix) = c(c(comparison_col,"count"), aggregate_columns )
  return(aggregate_matrix)
}

## Compare two factors in a df
table_comparison = function(df, col1, col2) {
  tab = data.frame(prop.table(table(df[,col1], df[, col2])))
  colnames(tab) = c(col1, col2, "Freq")
  
  return(tab)
}

## Generic function to match an existing table of values and fill a data frame with its contents - NOTE - both df's have to have the same match colname"

match_and_populate= function(df, match_df, match_col, fill_col, new_col_name){
  df[,new_col_name] = match_df[,fill_col][match(df[,match_col], match_df[,match_col])]
  return(df)
}

## Add custom metadata to a new table

custom_table = function(df, table_col, new_col_name, new_col_dat, scale_dat=FALSE){
  df_table = data.frame(table(df[,table_col]))
  if(scale_dat==TRUE){
    new_col_dat = scale(new_col_dat)
  }
  df_table[,new_col_name] = new_col_dat
  colnames(df_table) = c(table_col, "count", new_col_name)
  
  return(df_table)
}


## Partition dataset into train and test
partition_df = function(df, set="train", seed = 123, train_size = 0.75){
  
  ## Set train size
  smp_size = floor(train_size * nrow(df))
  
  ## Set seed to make it reproduceable
  set.seed(seed)
  train_ind = sample(seq_len(nrow(df)), size = smp_size)
  
  train=df[train_ind,]
  test =df[-train_ind,]
  
  if (set == "train"){
    return(train)
  } else if (set == "test") {
    return(test)
  }
  
}

### Data integrity check
data_integrity_check = function(dataframe, by_col=FALSE){
  if(by_col==FALSE){
    di_check = data.frame(colname = colnames(dataframe), records = NA, percent=NA)
    for (col in colnames(dataframe)){
      di_check[di_check$colname == col, ]$records = nrow(dataframe[!is.na(dataframe[,col]),])
      di_check[di_check$colname == col, ]$percent = round(nrow(dataframe[!is.na(dataframe[,col]),])/nrow(dataframe)*100, 2)
      
    }
    return(di_check[rev(order(di_check$percent)),])
  } else {
    ## Split factor records into columns with one to list the original column names
    ## and place the column names in the df
    di_cols = unique(dataframe[,by_col])
    di_cols = c("column_name", di_cols)
    di_check = data.frame(matrix(ncol=length(di_cols), nrow=length(colnames(dataframe))))
    colnames(di_check) = as.list(di_cols)
    df_cols = colnames(dataframe)
    di_check$column_name = df_cols
    
    ## Start this mad iteration
    for (di_col in colnames(di_check[,2:length(colnames(di_check))])) {
      ## di_check iteration
      ## slice by current col and then iterate over every column in original df
      slice = dataframe[dataframe[,by_col] == di_col,]
      
      ## Dream within a dream - count NA in each DF col and place it in the DI place
      for (df_col in df_cols) {
        
        has_data = length(na.omit(slice[,df_col]))
        if(has_data != 0){
          no_data = has_data / nrow(slice) 
        } else {
          no_data = 0
        }
        di_check[di_check[,"column_name"] == df_col,di_col] = round(no_data, 2)
      }
    }
    return(di_check)
  }
  
}

### store normalization scale values
store_scale_values = function(df, cols = c()){
  
  df_attrs = data.frame(col_scaled = cols, scale=NA, center=NA)
  for(col in cols){
    scale.center = c(attr(scale(df[,col]), "scaled:scale"), attr(scale(df[,col]), "scaled:center"))
    df_attrs[df_attrs$col_scaled == col,]$scale = scale.center[1]
    df_attrs[df_attrs$col_scaled == col,]$center = scale.center[2]
  }
  return(df_attrs)
}

### Cast many rows into single row with group members
cast_long_df = function(df, uid, cast_col, cast_prefix){
  
  ## Trim to only include needed columns  
  df_trimmed = df[, c(uid, cast_col)]
  ## Remove empty rows
  df_trimmed = na.omit(df_trimmed)
  ## Convert to data table
  setDT(df_trimmed)
  ## New column with unique group count
  df_trimmed[, N:= 1:.N, by = uid]
  ## Cast the data to rows
  df_trimmed = dcast(df_trimmed, get(uid)~paste(cast_prefix, N, sep=""), value.var=cast_col, sep="")
  ## Rename uid to original name
  colnames(df_trimmed)[1] = uid
  ## Return these to original df
  df = merge(df, df_trimmed, by=uid)
  ## Remove original single column
  df[,c(cast_col)] = NULL
  ## Remove duplicate entries
  df = df[!duplicated(df[,uid]),]
  ## Return df
  return(df)
}

melt_wide_df = function(df, uid, melt_cols = c(), variable_name = "variable", value_name="values", remove_na = "TRUE"){
  df_melted = melt(dwp_melted, uid, melt_cols, variable.name = variable_name, value.name=value_name, na.rm=remove_na)
}
### matching functions

### start with simple - try the different methods, pick row they converge on
fuzzy_match = function(df1, df2, id1, id2, match_cols1, match_cols2) {
  
  
  ## Add match_strings to both dfs and store the column information
  df1 = add_match_strings_to_df(df1, match_cols1)
  df1_col_names = store_match_col_names(match_cols1)
  
  df2 = add_match_strings_to_df(df2, match_cols2)
  df2_col_names = store_match_col_names(match_cols2)
  
  print(colnames(df2))
  print(colnames(df1))
  
  ## rip this into a function
  ## Try the different match methods and add the distance nextdoor
  ## For each match variant column
  ## for each match method
  ##add two columns - one containing the foreign row id, 
  ##the other containing the lowest distance score
  
  ## For each method converge on the common match row and add a the foreign id for the , 
  ## the sum of the distances, and the foreign match_string for that one
  
  ## Store in a new variant df of matches, and variant df of non-matches
  
  
  
  ## For each variant df
  
  ## Sum the sum of method distances
  
  ## Pick the one with the lowest sum
  
  ## Save lowest sum DF (with correct meta) and its non-matches to disk, with additional file including match success
}

### 
fuzzy_get_match_info = function(local_match_string, foreign_df, foreign_col, fuzzy_method="lv"){
  row_dists = stringdist(local_match_string, foreign_df[,foreign_col], method=fuzzy_method)
  min_dist = min(row_dists)
  num_matches = length(which(row_dists == min(row_dists)))
  ## Returns the minimum distance and the number of matches
  return(matrix(nrow=1, ncol=2, c(min_dist, num_matches)))
}

fuzzy_get_multiple = function(local_match_string, foreign_df, foreign_col) {
  distances = stringdist(local_match_string, foreign_df[, foreign_col])
  min_rows = which(distances == min(distances))
  match_strings = foreign_df[min_rows, foreign_col]
  return(match_strings)
}

### Add fuzzy matching columns to df1 and df2
fuzzy_add_cols = function(local_df, master_df, match_col = "match_string", master_id_col, max_dist=60, fuzzy_method="lv"){
  
  ### Get all match info between the dfs
  info_cols = sapply(local_df[,match_col], function(x) {
    best_match = amatch(x, master_df[,match_col], maxDist=max_dist, method=fuzzy_method)
    match_info = fuzzy_get_match_info(x, master_df, match_col, fuzzy_method)
    min_dist = match_info[1]
    num_matches = match_info[2]
    c(best_match,min_dist,num_matches)
  })
  ## info cols needs to be transposed
  info_cols = t(info_cols)
  
  ### Place match cols in local_df
  local_df[,"master_match_string"] = master_df[info_cols[,1], match_col]
  local_df[,"master_match_id"] = master_df[info_cols[,1], master_id_col]
  local_df[,"master_min_dist"] = info_cols[,2]
  local_df[,"master_num_matches"] = info_cols[,3]
  
  return(local_df)
}

### Add match strings to df based on match_cols
### EDIT - changed to reflect new approach with no variants
add_match_string_to_df = function(df, match_cols){
  ## Kept in case of a return to variant approach
  #   match_col_variants = get_match_col_variants(match_cols)
  #   for(match_col_variant in match_col_variants){
  #     match_col_title = paste("match_string", paste(match_col_variant, collapse="-"), sep="-")
  #     df[, match_col_title] = generate_match_string(df, match_col_variant)
  #   }
  
  ## Simples = return match string from chosen cols
  df$match_string = generate_match_string(df, match_cols)
  return(df)
}


### Store match_col names for accessing the new columns
store_match_col_names = function(match_cols){
  match_col_variants = get_match_col_variants(match_cols)
  for(match_col_variant in match_col_variants){
    match_col_name = paste("match_string", paste(match_col_variant, collapse="-"), sep="-")
    match_col_names = c(match_col_names, match_col_name)
  }
  return(match_col_names)
}


### Get match string for column names and return to datasets
generate_match_string = function(df, match_cols){
  if(length(match_cols) == 1)
    match_string = tolower(df[,match_cols])
  else {
    match_string = tolower(do.call(paste, c(df[,match_cols], sep=" ")))
  }
  match_string = trim_all(match_string)
  return(match_string)
}

### Get possible variants for matching columns and return list of variants 
get_match_col_variants = function(match_cols) {
  variants = list()
  ### for each item in match_cols
  for(i in 1:length(match_cols)){
    ## Set the combinations to the size we are currently on
    combination = combn(match_cols, i)
    ## Loop through each of the combinations
    for(comb_col in 1:ncol(combination)){
      ## Store them in this list
      variants = c(variants, list(combination[,comb_col]))
    }
  }
  return(variants)
}
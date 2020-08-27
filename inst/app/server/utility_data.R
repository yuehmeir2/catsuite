

get_cate_col_names = function(data, excludedCols = EXCLUDED_CATE_COLS, excludeListCol = T, includedCols = INCLUDED_CATE_COLS, threshold = 12) {
  
  cate_fields = which(map(data, ~is.categorical(vec = .x, threshold = threshold)) %>% unlist())
  if (!is.null(excludedCols)) {
    excludedAt <- map(excludedCols, ~grep(paste0("^", .x), names(data))) %>% unlist()
    if (length(excludedAt) > 0)
      cate_fields = setdiff(cate_fields, excludedAt)
  }
  
  if (excludeListCol && any(map_lgl(data, is.list))) {
    cate_fields = setdiff(cate_fields, which(map_lgl(data, is.list)))
  }
  
  if (!is.null(includedCols)) {
    includedAt <- map(includedCols, ~grep(paste0("^", .x), names(data))) %>% unlist()
    if (length(includedAt) > 0)
      cate_fields = union(cate_fields, includedAt)
  }
  return (names(data)[cate_fields])
}

is.categorical = function(vec, threshold = 12) {
  
  if (length(vec) < 1) return ()
  
  uniqueVec = unique(vec)
  if (!is.numeric(uniqueVec)) return(TRUE)
  
  return (if (length(uniqueVec) <= threshold) TRUE else FALSE)
}



get_nume_col_names = function(data, excludedCols = EXCLUDED_NUME_COLS) {
  
  if (!is.null(excludedCols)) data <- data %>% select(-c(intersect(excludedCols, names(.))))
  
  num_fields = which(map(data, is.numeric) %>% unlist())
  if (length(num_fields)>0) names(data)[num_fields] else NULL
  
}


get_cate_field_from_constraint = function (items, fields) {
  
  print(fields)
  attributes = unique(unlist(strsplit(fields, ":")))
  attributes = attributes[!grepl('^_', attributes)]   # remove any attributes starting with "_" such as "_FORM"
  
  attributes = attributes[unlist(
    sapply(attributes, function (i) {
      temp = unlist(items[, i])
      return ( ((is.categorical(temp) || length(unique(temp)) < 3) & length(unique(temp)) > 1)  )}
    ))]
  
  return (attributes)
}


get_numeric_field_from_constraint = function (items, fields) {
  
  attributes = unique(unlist(strsplit(fields, ":")))
  attributes = attributes[!grepl('^_', attributes)]   # remove any attributes starting with "_" such as "_FORM"
  
  attributes = attributes[unlist(
    sapply(attributes, function (i) is.numeric(unlist(items[, i])))
  )]
  
  return (attributes)
}

convertListColToStr = function (data, listColNames = NULL) {
  
  if (is.null(listColNames))
    listColNames <- data %>% map(~class(.x)) %>% 
      keep(function(x) x=="list") %>% imap(~.y) %>% unlist()
  
  temp<- map(listColNames, .f = function (x) {
    data[[x]] <<- map(data[[x]], ~ paste0(.x, collapse = ",")) 
  })
  
  return(mutate_at(data, listColNames, as.character))
  
}

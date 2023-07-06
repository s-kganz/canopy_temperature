# Misc. functions for working with flux data files

get_all_basenames <- function(table) {
  # Get all the Ameriflux basenames from a BASE file. Regex matches a one digit
  # number following an underscore.
  stringr::str_split_i(names(table), "_\\d(?=[^\\d])", 1)
}

get_all_qualifiers <- function(table) {
  # Get all qualifiers following the variable basenames in a BASE file. Regex
  # is the same as in get_all_basenames() except it matches the entire string
  # after the first qualifier.
  stringr::str_extract(names(table), "_\\d(?=[^\\d]).*")
}

get_highest_measurement <- function(table, base) {
  # Determine which measurement of variable basename is highest along a vertical
  # profile. Higher numbers are assumed to be higher in real life. Returns the
  # indices of columns of table that contain the highest vertical 
  # measurement of base.
  
  # First, filter to only that basename
  basenames <- get_all_basenames(table)
  quals     <- get_all_qualifiers(table)
  
  matched_indices <- which(basenames == base)
  basenames <- basenames[matched_indices]
  quals     <- quals[matched_indices]

  # Return all indices if there are no qualifiers for this measurement
  if (all(is.na(quals))) { return(matched_indices) }
  
  #print(base)
  # Find which quals have the highest vertical component. Vertical qualifier
  # is in the second position.
  qual_vertical <- str_extract_all(quals, "\\d", simplify=TRUE)[, 2] %>%
    parse_number()
  
  # Highest vertical measurement has the LOWEST qualifier
  # See https://ameriflux.lbl.gov/data/data-variable-qualifier-examples/
  qual_vertical_highest <- min(qual_vertical, na.rm=TRUE)
  target_indices <- matched_indices[which(qual_vertical == qual_vertical_highest)]
  
  target_indices
}

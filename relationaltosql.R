#Want to pass in full tree
full.table <- function(data) {
  shapes <- data$shapes
  cons <- data$connectors
  
  #get vector of all object ids
  attrs <- bind_rows(shapes$details$attributes)
  attrs.ids <- attrs[c('names', 'id')]
  tab.ids <- shapes$details[c('name', 'id')]
  names(attrs.ids) <- c('name', 'id')
  attrs.ids$name <- sapply(attrs.ids$name, paste, collapse = '-')
  ids <- bind_rows(attrs.ids, tab.ids)
  
  script <- list(nrow(shapes$details) + 1)
  
  for (i in 1:nrow(shapes$details)) {
    script[[i]] <- init.row(shapes$details[i,], ids)
  }
  
  fks <- attrs %>% filter(fk == TRUE)
  fk.con <- inner_join(fks, flatten(cons), by = c('id' = 'details.fkAttributeId'))
  dests <- unique(fk.con$destination)
  
  out <- ''
  
  for (dest in dests) {
    cur <- filter(fk.con, destination == dest)
    dest.tab <- ids[ids$id == dest, 'name']
    tmp <- paste0('ALTER TABLE ', dest.tab, '\n')
    if (nrow(cur) > 1) {
      #Need a way to account for composite primary keys
      #Currently the key works, but hard to reference for REFRENCES APARTMENT(AptNo, BuildingID) e.g.
      for (i in 1:nrow(cur)) { #tables with multiple foreign keys, e.g. relation tables
        refs <- cur[i,]$references[[1]]
        if (nrow(refs) > 1) {
          atts <- refs$attributeId
          src.att <- ids[ids$id %in% atts, 'name']
        } else {
          src.att <- ids[ids$id == refs[[1,2]],'name']
        }
        src.tab <- ids[ids$id == refs[[1,1]],'name']
        tmp <- paste0(tmp, 'ADD FOREIGN KEY (')
        tmp <- paste0(tmp, key.name(cur[i,]$names), ') REFERENCES ')
        tmp <- paste0(tmp, src.tab, '(', key.name(src.att), ');\n')
      }
      tmp <- gsub(';(\n[^$])', ',\\1', tmp)
    } else {
      refs <- cur$references[[1]]
      src.tab <- ids[ids$id == refs[[1,1]],'name']
      src.att <- ids[ids$id == refs[[1,2]],'name']
      tmp <- paste0(tmp, 'ADD FOREIGN KEY (')
      tmp <- paste0(tmp, key.name(cur$names), ') REFERENCES ')
      tmp <- paste0(tmp, src.tab, '(', src.att, ');\n')
    }
    out <- c(out, tmp)
  }
  script[[length(script) + 1]] <- out
  return(script)
}

#pass in each row of the $shapes$details table
#should always be called as part of full.table
init.row <- function(row, ids) {
  out <- character()
  
  tab.name <- row[['name']]
  attr.tab <- row[['attributes']][[1]]
  
  pks <- (filter(attr.tab, pkMember == TRUE))
  fks <- (filter(attr.tab, fk == TRUE))
  uniq <- (filter(attr.tab, soloUnique == TRUE))
  
  out <- c(out, paste('CREATE TABLE', tab.name, '\n(\n'))
  for (i in 1:nrow(attr.tab)) {
    dt <- attr.tab[[i,'dataType']]
    if (dt == 'varcharn' & !is.na(attr.tab[[i,'dataTypeSize']])) {
      dt <- paste0('varchar(', attr.tab[[i,'dataTypeSize']], ')')
    }
    out <- c(out, paste(attr.tab[[i,'names']], toupper(dt), ifelse(attr.tab[[i,'optional']],',\n','NOT NULL,\n')))
  }
  
  #Will always have at least 1
  if (nrow(pks) > 0) {
    pk.vec <- paste(unlist(pks[[1]]), collapse = ', ')
    out <- c(out, paste('PRIMARY KEY (', pk.vec, '),\n', sep = ''))
  }
  
  if (nrow(uniq) > 0) {
    for (i in 1:nrow(uniq)) {
      out <- c(out, paste0('UNIQUE (', uniq[1,]$names[[1]],')\n'))
    }
  }
  out <- paste(out, collapse = ' ')
  out <- gsub(',\n$', '\n', out)
  out <- paste(out, ');\n')
  return(out)
}

key.name <- function(v) {
  if (class(v) == 'list') v <- unlist(v)
  if (length(v) > 1) {
    return(paste(v, collapse = ', '))
  } else {
    return(v)
  }
}
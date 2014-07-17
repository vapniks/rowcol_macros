.packageName <- "rowcol_macros"

### Useful macros

##' Remove from workspace all objects with names matching regexp
##'
##' @param regexp a regular expression (as a character object)
##' @return none - side effects only
##' @seealso \code{\link{rmexgrep}}
##' @author Ben Veal
##' @export
rmgrep <- defmacro(regexp,expr={ 
  rm(list=ls()[grep(regexp,ls())])
})

##' Remove from workspace all objects with names not matching regexp
##'
##' @param regexp a regular expression (as a character object)
##' 
##' @return none - side effects only
##' @seealso \code{\link{rmgrep}}
##' @author Ben Veal
##' @export
rmexgrep <- defmacro(regexp,expr={ 
  rm(list=keep(list=ls()[grep(regexp,ls())]))
})

##' Select columns from data.frame matching regexp
##'
##' @param df a data.frame object
##' @param regex a regular expression
##'
##' @return A subset of the columns of 'df'
##' @seealso \code{\link{selectcolsT}}, \code{\link{aselectcols}}, \code{\link{aselectcolsT}}
##' @author Ben Veal
##' @export
selectcols <- defmacro(df,regex,expr={ 
  df[,grep(regex,colnames(df))]       
})

##' Select columns from data.table matching regexp
##' 
##' @param dt a data.table object
##' @param regex a regular expression
##'
##' @return A subset of the columns of 'dt'
##' @seealso \code{\link{selectcols}}, \code{\link{aselectcols}}, \code{\link{aselectcolsT}}
##' @author Ben Veal
##' @export
selectcolsT <- defmacro(dt,regex,expr={
  dt[,grep(regex,colnames(dt)),with=F]       
})

##' Select columns from data.frame fuzzy matching string
##'
##' Uses fuzzy matching to select columns from 'df' that match 'str'
##' @param df a data.frame object
##' @param str a string
##'
##' @return A subset of the columns of 'df'
##' @seealso \code{\link{selectcols}}, \code{\link{selectcolsT}}, \code{\link{aselectcolsT}}
##' @author Ben Veal
##' @export
aselectcols <- defmacro(df,str,expr={ 
  df[,agrep(str,colnames(df))]       
})

##' Select columns from data.table fuzzy matching string
##'
##' Uses fuzzy matching to select columns from 'dt' that match 'str'
##' @param dt a data.table object
##' @param str a string
##'
##' @return A subset of the columns of 'dt'
##' @seealso \code{\link{selectcols}}, \code{\link{selectcolsT}}, \code{\link{aselectcols}}
##' @author Ben Veal
##' @export
aselectcolsT <- defmacro(dt,str,expr={
  dt[,agrep(str,colnames(dt)),with=F]
})

##' Show names and numbers of columns from df with names matching regexp (ignoring case)
##'
##' @param df a data.frame object
##' @param regex a regular expression (as a character object)
##'
##' @return A data.frame object with one column containing names of columns of 'df', and whose row names are the
##' corresponding row numbers of 'df'
##' @export
findcols <- defmacro(df,regex,expr={ 
  data.frame(grep(regex,colnames(df),ignore.case=T,value=T),row.names=grep(regex,colnames(df),ignore.case=T,value=F))
})

##' Show names and numbers of columns from df with names fuzzy matching string (ignoring case)
##'
##' Uses fuzzy matching to find columns of 'df' that match string 'str'
##' @param df a data.frame object
##' @param str  a character object
##'
##' @return A data.frame object with one column containing names of columns of 'df', and whose row names are the
##' corresponding row numbers of 'df'
##' @author Ben Veal
##' @export
afindcols <- defmacro(df,str,expr={ 
  data.frame(agrep(str,colnames(df),ignore.case=T,value=T),row.names=agrep(str,colnames(df),ignore.case=T,value=F))
})

##' Remove columns from data.frame that match regexp
##'
##' @param df a data.frame object
##' @param regex a regular expression (as a character object)
##'
##' @return A subset of the columns of 'df'
##' @author Ben Veal
##' @export
removecols <- defmacro(df,regex,expr={ 
  if(length(grep(regex,colnames(df))))
    df[,-grep(regex,colnames(df))]
  else
    df
})

##' Remove columns from data.table that match regexp
##'
##' @param dt a data.table object
##' @param regex a regular expression (as a character object)
##'
##' @return A subset of the columns of 'dt'
##' @author Ben Veal
##' @export
removecolsT <- defmacro(dt,regex,expr={ # as above but for datatables
  if(length(grep(regex,colnames(dt))))
    dt[,-grep(regex,colnames(dt)),with=F]
  else
    dt
})

##' Remove columns from data.frame that fuzzy match string
##'
##' Uses fuzzy string matching to select which columns of 'df' to remove.
##' @param df a data.frame object
##' @param str a string with which to fuzzy match the column names of 'df'
##'
##' @return A subset of the columns of 'df'
##' @author Ben Veal
##' @export
aremovecols <- defmacro(df,str,expr={ 
  if(length(agrep(str,colnames(df))))
    df[,-agrep(str,colnames(df))]
  else
    df
})

##' Remove columns from data.table that fuzzy match string
##'
##' Uses fuzzy string matching to select which columns of 'dt' to remove.
##' @param dt a data.table object
##' @param str a string with which to fuzzy match the column names of 'dt'
##'
##' @return A subset of the columns of 'dt'
##' @author Ben Veal
##' @export
aremovecolsT <- defmacro(dt,str,expr={ 
  if(length(agrep(str,colnames(dt))))
    dt[,-agrep(str,colnames(dt)),with=F]
  else
    dt
})

##' @title Rename matches to regexp in column names of data.frame object.
##' @details Replace first match to regexp 'regex' in columns of 'df' with 'repl'.
##' @param df a data.frame object
##' @param regex a string/character object
##' @param repl a string/character object
##'
##' @return none - side effects only
##' @author Ben Veal
##' @export
renamecols <- defmacro(df,regex,repl,expr={ 
  sub(regex,repl,colnames(df))
  colnames(df) <- sub(regex,repl,colnames(df))
})

##' @title Rename matches to regexp in column names of data.frame object.
##' @details Replace all matches to regexp 'regex' in columns of 'df' with 'repl'.
##' @param df a data.frame object
##' @param regex a string/character object
##' @param repl a string/character object
##' @return none - side effects only
##' @author Ben Veal
##' @export
grenamecols <- defmacro(df,regex,repl,expr={ 
  gsub(regex,repl,colnames(df))
  colnames(df) <- gsub(regex,repl,colnames(df))
})


##' @title Select rows from data.frame matching regexp
##' @param df a data.frame object
##' @param regex a regular expression
##' @return A subset of the rows of 'df'
##' @author Ben Veal
##' @export
selectrows <- defmacro(df,regex,expr={
  df[grep(regex,rownames(df)),]       
})

##' @title Select rows from data.frame fuzzy matching string
##' @details Uses fuzzy matching to select rows from 'df' that match 'str'
##' @param df a data.frame object
##' @param str a string
##' @return A subset of the rows of 'df'
##' @author Ben Veal
##' @export
aselectrows <- defmacro(df,str,expr={
  df[agrep(str,rownames(df)),]       
})

##' @title Show names and numbers of rows from data.frame with names matching regexp (ignoring case)
##' @param df a data.frame object
##' @param regex a regular expression (as a character object)
##' @return A data.frame object with one row containing names of rows of 'df', and whose row names are the
##' corresponding row numbers of 'df'
##' @author Ben Veal
##' @export
findrows <- defmacro(df,regex,expr={ 
  data.frame(grep(regex,rownames(df),ignore.case=T,value=T),row.names=grep(regex,rownames(df),ignore.case=T,value=F))
})

##' @title Select rows from data.frame fuzzy matching string (ignoring case)
##' @details Uses fuzzy matching to select rows from 'df' that match 'str'
##' @param df a data.frame object
##' @param str a string
##' @return A subset of the rows of 'df'
##' @author Ben Veal
##' @export
afindrows <- defmacro(df,str,expr={ # as above but using fuzzy string matching (doesn't parse regexps!) 
  data.frame(agrep(str,rownames(df),ignore.case=T,value=T),row.names=agrep(str,rownames(df),ignore.case=T,value=F))
})

##' @title Remove rows from data.frame that match regexp
##' @param df a data.frame object
##' @param regex a regular expression (as a character object)
##' @return A subset of the rows of 'df'
##' @author Ben Veal
##' @export
removerows <- defmacro(df,regex,expr={
  if(length(grep(regex,rownames(df)))>0)
    df[-grep(regex,rownames(df)),]
  else
    df
})

##' @title Remove rows from data.frame that fuzzy match string
##' @details Uses fuzzy string matching to select which rows of 'df' to remove.
##' @param df a data.frame object
##' @param str a string with which to fuzzy match the row names of 'df'
##' @return A subset of the rows of 'df'
##' @author Ben Veal
##' @export
aremoverows <- defmacro(df,str,expr={
  if(length(agrep(str,rownames(df))>0))
    df[-agrep(str,rownames(df)),]
  else
    df
})

##' @title Rename matches to regexp in row names of data.frame object.
##' @details Replace first match to regexp 'regex' in rows of 'df' with 'repl'.
##' @param df a data.frame object
##' @param regex a string/character object
##' @param repl a string/character object
##' @return none - side effects only
##' @author Ben Veal
##' @export
renamerows <- defmacro(df,regex,repl,expr={
  sub(regex,repl,rownames(df))
  rownames(df)<-sub(regex,repl,rownames(df))
})

##' @title Rename matches to regexp in row names of data.frame object.
##' @details Replace all matches to regexp 'regex' in rows of 'df' with 'repl'.
##' @param df a data.frame object
##' @param regex a string/character object
##' @param repl a string/character object
##' @return none - side effects only
##' @author Ben Veal
##' @export
grenamerows <- defmacro(df,regex,repl,expr={
  gsub(regex,repl,rownames(df))
  rownames(df) <- gsub(regex,repl,rownames(df))
})


##' @title Macro to remove empty strings from character vector.
##' @param strvec a character vector
##' @return A character vector with empty strings removed
##' @author Ben Veal
##' @export
strRmEmpty <- defmacro(strvec,expr={Filter(function(str) nchar(str) > 0, strvec)})

##' @title Macro to remove whitespace from strings (inside string aswell as at ends).
##' @param strvec a character vector
##' @return A character vector with empty strings removed
##' @author Ben Veal
##' @export
strRmSpace <- defmacro(str,expr={gsub("[[:space:]]*","",str)})



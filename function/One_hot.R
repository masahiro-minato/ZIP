One_hot <- function(df, subject_col){
  # dfと対象列subject_colsを引数として渡すとワンホット化したdfを戻り値として返す
  # subject_colsは文字列で渡す　例 subject_col="機種略", one_hot(df, "機種略")
  col_names <- 
    df %>% 
    distinct(eval(parse(text = subject_col)))
  one_hot_df <-
    df
  for (col_name in col_names$`eval(parse(text = subject_col))`){
    print(col_name)
    one_hot_df <- 
      one_hot_df %>% 
      mutate(
        !!col_name := if_else(eval(parse(text = subject_col)) == col_name , 1, 0)
      )
  }
  return(one_hot_df)
}

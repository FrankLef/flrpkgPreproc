df_tdict <- function(nm) {
  lst <- list()
  lst <- within(lst, {
    df1 <- data.frame(
      path = rep(r"(C:\Users\Public\MyJob\DesjCap_cies\NSE\OlapNse_V03)", 3),
      file = c("db_NSE_V03.accdb", "db_NSE_V03.accdb", "test_data.xlsx"),
      table = c("qryx_dim_clients", "qryx_projects", "tbl_data"),
      name = c("clients", "projects", "data"),
      raw_name = c("raw_clients", "raw_projects", "raw_data"),
      label = c("Clients", "Projects", "Test Data"),
      type = c("accdb", "accdb", "xlsx"),
      role = c("dim", "trx", "test"),
      process = c("load", NA_character_, "load"),
      rule = c("rule_set1", NA_character_, NA_character_),
      desc = NA_character_,
      note = NA_character_
    )
    tdict1 <- df1
  })
  lst[[nm]]
}

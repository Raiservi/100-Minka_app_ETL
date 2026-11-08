library(DBI)
library(sf)
library(tidyverse)
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("PGDATABASE"),
  host = Sys.getenv("PGHOST"),
  port = as.integer(Sys.getenv("PGPORT")),
  user = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

# 1. Para que se cierre sola aunque haya un error
on.exit(DBI::dbDisconnect(con))

# 2. Listado de tablas
tablas <- DBI::dbListTables(con)
print(tablas)

# 3. Campos de una tabla
utm10_litoral <- DBI::dbListFields(con, "utm10_litoral" )
print(utm10_litoral)

substrat_fons_utm10 <- DBI::dbListFields(con, "substrat_fons_utm10")
print(substrat_fons_utm10)

# Si quieres también el tipo de cada campo
tipos_mun <- DBI::dbGetQuery(con, "
  SELECT column_name, data_type
  FROM information_schema.columns
  WHERE table_name = 'mun_lit'
")
View(tipos_mun)

# 4. Consulta de ejemplo
prueba <- DBI::dbGetQuery(con, "
  SELECT nommuni, comarques_
  FROM mun_lit
  LIMIT 10
")
View(prueba)

# 5. Cerrar a mano al final, si no usas on.exit
DBI::dbDisconnect(con)
#############################################################################

#           PIPELINE DE DESCARREGA DEL ETL

# Pipeline de descarrega de dades de la API de Minka  a la BBDD en Postgre
# La consulta de l API es de les dades que han variat desde la ultima consulta

# Data 12/10/2026

# Versio: 1

#################################################################################

library(sf)
library(DBI)
library(RPostgres)
library(tidyverse)

#############################################################################

run_pipeline <- function() {
  # Dades de conexio

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PGDATABASE"),
    host = Sys.getenv("PGHOST"),
    port = as.integer(Sys.getenv("PGPORT")),
    user = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )

  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Carreguem la funcio del update de dades de la web atraves de la API de Minka

  source("mnk_obs_updatedsince.R")

  # Obetnim el timestamp de l ultima sincro
  last_sync <- dbGetQuery(
    con,
    "SELECT MAX(NULLIF(updated_at, '')::timestamptz) AS last FROM public.obs_cat"
  )$last

  # Apliquem aquet timestamp a la darrera update de les dades amb conexio a l API

  # Limits de catalunya

  lim_catalunya <- c(42.4562621, 3.3719196, 40.4341866, 0.3975516)

  # actualitzacio <- rminka::mnk_obs(updated_since = last_sync,
  #                                  quality="research",
  #                                  bounds=lim_catalunya,
  #                                  limit_download=FALSE)

  actualitzacio <- mnk_obs_updatedsince(
    updated_since = last_sync,
    quality = "research",
    bounds = lim_catalunya
  )

  # Ho pasem a capa geografica

  capa_new <- actualitzacio %>%
    rename(id_minka = id) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(25831) %>%
    rename(geom = geometry) %>%
    st_set_geometry("geom") %>%
    filter(!st_is_empty(geom)) %>%
    st_make_valid()

  # reparacio del link de les imatges.......

  capa_new$url_picture <- gsub("/square\\.", "/original.", capa_new$url_picture)
  capa_new$url_picture <- gsub("https://minka-sdg.org", "https://observe.minka-sdg.org", capa_new$url_picture)

  # Estadistiques previes

  cat("Total descargados:", nrow(capa_new), "\n")
  cat("No válidos:", sum(!st_is_valid(capa_new)), "\n")
  cat("Vacíos:", sum(st_is_empty(capa_new)), "\n")
  cat("Duplicados id_minka en el df:", sum(duplicated(capa_new$id_minka)), "\n")

  # repara automáticamente

  capa_new <- capa_new %>%
    filter(!st_is_empty(geom)) %>%
    st_make_valid()

  # --- CHECK 2: conteig avans en BBDD ---
  before <- dbGetQuery(con, "SELECT COUNT(*) as total, COUNT(*) FILTER (WHERE year=2026) as y2026 FROM public.obs_cat")
  print(before)

  # 3. staging - sobreescribim sempre

  st_write(capa_new, con, Id(schema = "public", table = "obs_cat_staging"), delete_layer = TRUE)

  # 4. UPSERT automatic

  dbExecute(con, "
INSERT INTO public.obs_cat (id_minka, observed_on, year, month, week, day, hour, created_at, updated_at, positional_accuracy, geoprivacy, obscured, uri, url_picture, quality_grade, taxon_id, taxon_name, taxon_rank, taxon_min_ancestry, taxon_endemic, taxon_threatened, taxon_introduced, taxon_native, user_id, user_login, geom, utm10_code, utm1_code, codi_hic, cod_corine, habitat_id, profunditat, substrat_gid)
SELECT id_minka, observed_on, year, month, week, day, hour, created_at, updated_at, positional_accuracy, geoprivacy, obscured, uri, url_picture, quality_grade, taxon_id, taxon_name, taxon_rank, taxon_min_ancestry, taxon_endemic, taxon_threatened, taxon_introduced, taxon_native, user_id, user_login, geom, NULL,NULL,NULL,NULL,NULL,NULL,NULL
FROM public.obs_cat_staging
ON CONFLICT (id_minka) DO UPDATE SET
  observed_on = EXCLUDED.observed_on,
  year = EXCLUDED.year, month = EXCLUDED.month, week = EXCLUDED.week, day = EXCLUDED.day, hour = EXCLUDED.hour,
  created_at = EXCLUDED.created_at, updated_at = EXCLUDED.updated_at,
  positional_accuracy = EXCLUDED.positional_accuracy, geoprivacy = EXCLUDED.geoprivacy, obscured = EXCLUDED.obscured,
  uri = EXCLUDED.uri, url_picture = EXCLUDED.url_picture, quality_grade = EXCLUDED.quality_grade,
  taxon_id = EXCLUDED.taxon_id, taxon_name = EXCLUDED.taxon_name, taxon_rank = EXCLUDED.taxon_rank, taxon_min_ancestry = EXCLUDED.taxon_min_ancestry,
  taxon_endemic = EXCLUDED.taxon_endemic, taxon_threatened = EXCLUDED.taxon_threatened, taxon_introduced = EXCLUDED.taxon_introduced, taxon_native = EXCLUDED.taxon_native,
  user_id = EXCLUDED.user_id, user_login = EXCLUDED.user_login,
  geom = EXCLUDED.geom,
  utm10_code = NULL, utm1_code = NULL, codi_hic = NULL, cod_corine = NULL, habitat_id = NULL, profunditat = NULL, substrat_gid = NULL
")

  # Comprobacio que hem pujat observacions

  dbGetQuery(con, "
SELECT
  COUNT(*) as staging,
  COUNT(*) FILTER (WHERE geom IS NULL) as geom_null,
  COUNT(*) FILTER (WHERE NOT ST_IsValid(geom)) as invalid
FROM public.obs_cat_staging
")

  # 5. rRecaluem els creuaments per les noves dades o les actualitzades amb les capes mediambientals

  dbExecute(con, "UPDATE public.obs_cat o SET utm10_code = u.coord_10k FROM public.utm10_litoral u WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND ST_Intersects(o.geom, u.geom)")
  dbExecute(con, "UPDATE public.obs_cat o SET utm1_code = u.cod1x1 FROM public.utm1_litoral u WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND ST_Intersects(o.geom, u.geom)")
  dbExecute(con, "UPDATE public.obs_cat o SET habitat_id = h.id, cod_corine = h.cod_corine, codi_hic = h.codi_hic FROM public.habitats_simple h WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND ST_Intersects(o.geom, h.geom)")
  dbExecute(con, "UPDATE public.obs_cat o SET substrat_gid = s.gid FROM public.substrat_fons_2m s WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND ST_Intersects(o.geom, s.geom)")
  dbExecute(con, "UPDATE public.obs_cat o SET profunditat = (SELECT ST_Value(b.rast, o.geom) FROM public.batimetria_5m_mar b WHERE ST_Intersects(b.rast, o.geom) LIMIT 1) WHERE o.id_minka IN (SELECT id_minka FROM obs_cat_staging) AND o.profunditat IS NULL")
  dbExecute(con, "DROP TABLE public.obs_cat_staging")

  # Comprobacio despres del creuamnet de les capes

  after <- dbGetQuery(con, "
SELECT
  COUNT(*) as total,
  COUNT(*) FILTER (WHERE year=2026) as y2026,
  COUNT(*) FILTER (WHERE year=2026 AND utm10_code IS NULL) as sin_utm10,
  COUNT(*) FILTER (WHERE year=2026 AND profunditat IS NULL) as sin_bati,
  COUNT(*) FILTER (WHERE year=2026 AND substrat_gid IS NULL) as sin_substrat
FROM public.obs_cat;
")
  print(after)

  # geometries trencades despres del creuament

  dbGetQuery(con, "SELECT id_minka FROM public.obs_cat WHERE NOT ST_IsValid(geom) LIMIT 10")

  DBI::dbGetQuery(con, "SELECT year, COUNT(*) FROM public.obs_cat GROUP BY year ORDER BY year DESC")
}

run_pipeline()

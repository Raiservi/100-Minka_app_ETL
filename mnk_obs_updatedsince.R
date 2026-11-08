# Download Minka observations updated since a timestamp, continuing in
# successive timestamp windows when the API's 10,000-result cap is reached.
mnk_obs_updatedsince <- function(updated_since, query = NULL,
                                 taxon_name = NULL, taxon_id = NULL,
                                 user_id = NULL, project_id = NULL,
                                 place_id = NULL, endemic = NULL,
                                 introduced = NULL, threatened = NULL,
                                 quality = NULL, geo = NULL,
                                 annotation = NULL, bounds = NULL,
                                 quiet = FALSE) {
  api_url <- "https://api.minka-sdg.org/v1/observations"
  api_max_results <- 10000L
  api_page_size <- 200L

  if (length(updated_since) != 1L || is.na(updated_since) ||
      !inherits(updated_since, c("Date", "POSIXt")) &&
        !is.character(updated_since)) {
    stop("'updated_since' must be one non-missing date, timestamp, or string.",
         call. = FALSE)
  }
  if (is.character(updated_since) && !nzchar(updated_since)) {
    stop("'updated_since' must not be empty.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("'quiet' must be TRUE or FALSE.", call. = FALSE)
  }

  cursor <- if (inherits(updated_since, "POSIXt")) {
    format(as.POSIXct(updated_since, tz = "UTC"),
           "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  } else if (inherits(updated_since, "Date")) {
    format(updated_since, "%Y-%m-%d")
  } else {
    updated_since
  }

  params <- list(updated_since = cursor)
  if (!is.null(query)) params$q <- query
  if (!is.null(taxon_name)) params$taxon_name <- taxon_name
  if (!is.null(taxon_id)) params$taxon_id <- taxon_id
  if (!is.null(user_id)) params$user_id <- user_id
  if (!is.null(project_id)) params$project_id <- project_id
  if (!is.null(place_id)) params$place_id <- place_id
  if (!is.null(quality)) {
    if (length(quality) != 1L || is.na(quality) ||
        !quality %in% c("casual", "research")) {
      stop("'quality' must be 'casual' or 'research'.", call. = FALSE)
    }
    params$quality_grade <- quality
  }
  for (flag in c("endemic", "introduced", "threatened")) {
    value <- get(flag)
    if (!is.null(value)) {
      if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        stop(sprintf("'%s' must be TRUE or FALSE.", flag), call. = FALSE)
      }
      params[[flag]] <- tolower(as.character(value))
    }
  }
  if (!is.null(geo)) {
    if (!is.logical(geo) || length(geo) != 1L || is.na(geo)) {
      stop("'geo' must be TRUE or FALSE.", call. = FALSE)
    }
    if (geo) params[["has[]"]] <- "geo"
  }
  if (!is.null(annotation)) {
    if (!is.numeric(annotation) || length(annotation) != 2L ||
        anyNA(annotation)) {
      stop("'annotation' must contain two numeric values: term ID and term value ID.",
           call. = FALSE)
    }
    params$term_id <- annotation[1]
    params$term_value_id <- annotation[2]
  }
  if (!is.null(bounds)) {
    if (inherits(bounds, c("sf", "sfc"))) {
      crs <- sf::st_crs(bounds)
      if (is.na(crs) || crs$epsg != 4326) {
        stop("'bounds' must have CRS EPSG:4326 (WGS84).", call. = FALSE)
      }
      bbox <- sf::st_bbox(bounds)
      params <- c(params, list(
        swlng = as.numeric(bbox[["xmin"]]),
        swlat = as.numeric(bbox[["ymin"]]),
        nelng = as.numeric(bbox[["xmax"]]),
        nelat = as.numeric(bbox[["ymax"]])
      ))
    } else {
      if (!is.numeric(bounds) || length(bounds) != 4L || anyNA(bounds)) {
        stop("'bounds' must be an sf object or c(nelat, nelng, swlat, swlng).",
             call. = FALSE)
      }
      params <- c(params, list(
        nelat = bounds[1],
        nelng = bounds[2],
        swlat = bounds[3],
        swlng = bounds[4]
      ))
    }
  }

  process_results <- function(results) {
    if (!length(results)) return(tibble::tibble())

    value_or <- function(value, default) {
      if (is.null(value)) default else value
    }

    purrr::map_dfr(results, function(obs) {
      photo <- purrr::pluck(obs, "photos", 1, "url", .default = NA_character_)
      if (is.null(photo) || !length(photo) || is.na(photo)) {
        photo <- purrr::pluck(
          obs, "observation_photos", 1, "photo", "url",
          .default = NA_character_
        )
      }
      if (is.null(photo) || !length(photo)) photo <- NA_character_

      tibble::tibble(
        id = value_or(obs$id, NA_integer_),
        observed_on = value_or(obs$observed_on, NA_character_),
        year = purrr::pluck(obs, "observed_on_details", "year",
                            .default = NA_integer_),
        month = purrr::pluck(obs, "observed_on_details", "month",
                             .default = NA_integer_),
        week = purrr::pluck(obs, "observed_on_details", "week",
                            .default = NA_integer_),
        day = purrr::pluck(obs, "observed_on_details", "day",
                           .default = NA_integer_),
        hour = purrr::pluck(obs, "observed_on_details", "hour",
                            .default = NA_integer_),
        created_at = value_or(obs$created_at, NA_character_),
        updated_at = value_or(obs$updated_at, NA_character_),
        latitude = purrr::pluck(obs, "geojson", "coordinates", 2,
                                .default = NA_real_),
        longitude = purrr::pluck(obs, "geojson", "coordinates", 1,
                                 .default = NA_real_),
        positional_accuracy = value_or(obs$positional_accuracy, NA_integer_),
        geoprivacy = value_or(obs$taxon_geoprivacy, NA_character_),
        obscured = value_or(obs$obscured, NA),
        uri = value_or(obs$uri, NA_character_),
        url_picture = photo,
        quality_grade = value_or(obs$quality_grade, NA_character_),
        taxon_id = purrr::pluck(obs, "taxon", "id", .default = NA_integer_),
        taxon_name = purrr::pluck(obs, "taxon", "name",
                                  .default = NA_character_),
        taxon_rank = purrr::pluck(obs, "taxon", "rank",
                                  .default = NA_character_),
        taxon_min_ancestry = purrr::pluck(
          obs, "taxon", "min_species_ancestry", .default = NA_character_
        ),
        taxon_endemic = purrr::pluck(obs, "taxon", "endemic", .default = NA),
        taxon_threatened = purrr::pluck(
          obs, "taxon", "threatened", .default = NA
        ),
        taxon_introduced = purrr::pluck(
          obs, "taxon", "introduced", .default = NA
        ),
        taxon_native = purrr::pluck(obs, "taxon", "native", .default = NA),
        user_id = purrr::pluck(obs, "user", "id", .default = NA_integer_),
        user_login = purrr::pluck(obs, "user", "login",
                                  .default = NA_character_)
      )
    })
  }

  download_batch <- function(batch_cursor) {
    ordered_params <- params
    ordered_params$updated_since <- batch_cursor
    ordered_params$order_by <- "updated_at"
    ordered_params$order <- "asc"
    ping <- httr::GET(api_url, query = c(ordered_params, list(per_page = 1)))
    if (httr::http_error(ping)) {
      stop("Minka API count request failed with HTTP ",
           httr::status_code(ping), ".", call. = FALSE)
    }
    total <- httr::content(ping, as = "parsed")$total_results
    if (is.null(total) || length(total) != 1L || is.na(total)) {
      stop("Minka API did not return a valid total_results count.",
           call. = FALSE)
    }
    if (total == 0L) return(list(data = tibble::tibble(), total = 0L))

    expected <- min(as.double(total), api_max_results)
    pages <- ceiling(expected / api_page_size)
    results <- list()
    for (page in seq_len(pages)) {
      response <- httr::GET(
        api_url,
        query = c(ordered_params, list(per_page = api_page_size, page = page))
      )
      if (httr::http_error(response)) {
        stop("Minka API download failed on page ", page, " with HTTP ",
             httr::status_code(response), ".", call. = FALSE)
      }
      page_results <- httr::content(response, as = "parsed")$results
      if (!length(page_results)) break
      results <- c(results, page_results)
    }

    if (length(results) < expected) {
      stop("Minka API returned fewer observations than reported; ",
           "the download may be incomplete.", call. = FALSE)
    }
    list(data = process_results(results), total = total)
  }

  all_data <- tibble::tibble()
  batch_number <- 0L

  repeat {
    batch_number <- batch_number + 1L
    if (!quiet) {
      message("Downloading updated observations, batch ", batch_number,
              " (updated_since: ", cursor, ")...")
    }

    batch <- download_batch(cursor)
    if (!nrow(batch$data)) break
    if (anyNA(batch$data$id)) {
      stop("The API returned an observation without an ID; stopping to avoid duplicates.",
           call. = FALSE)
    }

    batch$data <- batch$data[!duplicated(batch$data$id), , drop = FALSE]
    new_data <- batch$data[!batch$data$id %in% all_data$id, , drop = FALSE]
    all_data <- dplyr::bind_rows(all_data, new_data)

    if (batch$total <= api_max_results) break
    if (!nrow(new_data)) {
      stop(
        "The API result limit was reached, but updated_since did not yield new IDs. ",
        "The observations at the cursor may share the same updated_at timestamp; ",
        "stopping to avoid an incomplete result or an infinite loop.",
        call. = FALSE
      )
    }

    latest_updated_at <- suppressWarnings(
      as.POSIXct(batch$data$updated_at, tz = "UTC")
    )
    if (all(is.na(latest_updated_at))) {
      stop("Could not advance the download cursor: updated_at is missing or invalid.",
           call. = FALSE)
    }
    next_cursor_time <- max(latest_updated_at, na.rm = TRUE)
    previous_cursor_time <- suppressWarnings(as.POSIXct(cursor, tz = "UTC"))
    if (!is.na(previous_cursor_time) &&
        next_cursor_time <= previous_cursor_time) {
      stop(
        "Could not advance updated_since beyond the current cursor. ",
        "The API may have more than 10,000 observations with the same timestamp.",
        call. = FALSE
      )
    }
    cursor <- format(next_cursor_time, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  }

  if (!quiet) {
    message("Download complete: ", format(nrow(all_data), big.mark = ","),
            " observations.")
  }
  all_data
}

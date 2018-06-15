#' Attempts to install a package directly from Bitbucket Dev Server.
#'
#' This function is vectorised on repo so you can install multiple packages in a single command.
#'
#' @usage install_bitbucket_server(projects, repos,
#' auth_token = rstudioapi::askForPassword("Enter your personal token:"),
#' ref = "master", subdir = NULL, quiet = FALSE,
#' host = NULL, ...)
#'
#' @param projects Project Name
#' @param repos Repository Name
#' @param auth_token Personal token from Bitbucket Dev Server
#' @param ref default: master
#' @param subdir default: NULL
#' @param quiet default: FALSE
#' @param host default: NULL (e.g. https://hostserver.com/bitbucket)
#' @param ... Other arguments passed on to install.
#'
#' @export
#'
#' @seealso \code{\link[devtools]{install_github}}, \code{\link[devtools]{install_bitbucket}}
#'
#' @examples
#' \dontrun{
#' install_bitbucket_server(
#' projects = "project_name",
#' repos = "repository_name",
#' host = "host_server_name"
#' )
#' }
install_bitbucket_server <- function(projects, repos,
                                     auth_token = rstudioapi::askForPassword("Enter your personal token:"),
                                     ref = "master", subdir = NULL, quiet = FALSE,
                                     host = NULL, ...) {
  remotes <- lapply(repos, bitbucket_server_remote, projects = projects,
                    ref = ref, subdir = subdir, auth_token = auth_token, host = host)
  my_install_remotes(remotes, ..., quiet = quiet)
}


bitbucket_server_remote <- function(projects, repos, auth_token, ref = NULL, subdir = NULL,
                                    host = NULL) {
  my_remote("bitbucket_server",
            host = host,
            repos = repos,
            subdir = subdir,
            projects = projects,
            ref = ref,
            auth_token = auth_token)
}


my_remote <- function(type, ...) {
  structure(list(...), class = c(paste0(type, "_remote"), "remote"))
}


my_install_remotes <- function(remotes, ...) {
  invisible(vapply(remotes, my_install_remote, ..., FUN.VALUE = logical(1)))
}

remote_package_name <- function(remote, ...) UseMethod("remote_package_name")

remote_package_name.bitbucket_server_remote <- function(remote, ...) {
  url <- remote$host
  temp <- tempfile()
  path <- paste(c("bitbucket",
                  "projects",
                  remote$projects,
                  "repos",
                  remote$repos,
                  "browse",
                  "DESCRIPTION"), collapse = "/")
  if (!is.null(remote$auth_token)) {
    auth <- httr::add_headers(Authorization = paste("Bearer", remote$auth_token, sep = " "))
  } else {
    auth <- NULL
  }
  req <- httr::GET(url, path = path, httr::write_disk(path = temp), auth)
  if (httr::status_code(req) >= 400) {
    return(NA_character_)
  }
  read_dcf(temp)$Package
}

read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  as.list(read.dcf(path, keep.white = fields)[1, ])
}

is.remote <- function(x) inherits(x, "remote")

is_windows <- isTRUE(.Platform$OS.type == "windows")

my_install_remote <- function(remote, ..., quiet = FALSE,
                              out_dir = NULL, skip_if_log_exists = FALSE,
                              repos = getOption("repos"), type = getOption("pkgType")) {
  stopifnot(is.remote(remote))
  package_name <- remote_package_name(remote)
  if (!is.null(out_dir)) {
    out_file <- file.path(out_dir, paste0(package_name, ".out"))
    if (skip_if_log_exists && file.exists(out_file)) {
      message("Skipping ", package_name, ", installation failed before, see log in ", out_file)
      return(invisible(FALSE))
    }
  }
  if (is_windows && inherits(remote, "cran_remote")) {
    install_packages(
      package_name, repos = remote$repos, type = remote$pkg_type, dependencies = NA, ..., quiet = quiet, out_dir = out_dir,
      skip_if_log_exists = skip_if_log_exists)
    return(invisible(TRUE))
  }
  bundle <- remote_download(remote, quiet = quiet)
  on.exit(unlink(bundle), add = TRUE)
  source <- source_pkg(bundle, subdir = remote$subdir)
  on.exit(unlink(source, recursive = TRUE), add = TRUE)
  metadata <- remote_metadata(remote, bundle, source)
  devtools::install(source, ..., quiet = quiet, metadata = metadata,
                    out_dir = out_dir, skip_if_log_exists = skip_if_log_exists,
                    repos = repos, type = type)
}

remote_download <- function(x, quiet = FALSE) UseMethod("remote_download")

remote_download.bitbucket_server_remote <- function(x, quiet = FALSE) {
  dest <- tempfile(fileext = paste0(".zip"))
  if (missing_protocol <- !grepl("^[^:]+?://", x$host)) {
    x$host <- paste0("https://", x$host)
  }
  src_root <- paste0(x$host, "/rest/api/latest/projects/", x$projects, "/repos/", x$repos)
  src <- paste0(src_root, "/archive?format=zip")
  if (!quiet) {
    message("Downloading Bitbucket repo ", x$projects, "/", x$repos, "@", x$ref,
            "\nfrom URL ", src)
  }
  if (!is.null(x$auth_token)) {
    auth <- httr::add_headers(Authorization = paste("Bearer", x$auth_token, sep = " "))
  } else {
    auth <- NULL
  }
  download_bitbucket_server(src, httr::write_disk(path = dest), auth, path = dest)
}

download_bitbucket_server <- function(url, path, ...) {
  request <- httr::GET(url, ...)
  if (httr::status_code(request) >= 400) {
    stop(github_error(request))
  }
  writeBin(httr::content(request, "raw"), path)
  path
}

remote_metadata <- function(x, bundle = NULL, source = NULL) UseMethod("remote_metadata")

remote_metadata.bitbucket_server_remote <- function(x, bundle = NULL, source = NULL) {
  # Determine sha as efficiently as possible
  # if (!is.null(bundle)) {
  #   # Might be able to get from zip archive
  #   sha <- git_extract_sha1(bundle)
  # } else {
  #   # Otherwise can lookup with remote_ls
  #   sha <- remote_sha(x)
  # }
  list(
    RemoteType = "bitbucket_server",
    RemoteHost = x$host,
    RemoteRepo = x$repos,
    RemoteUsername = x$projects,
    RemoteRef = x$ref,
    # RemoteSha = sha,
    RemoteSubdir = x$subdir,
    # Backward compatibility for packrat etc.
    BitBucketServerRepo = x$repos,
    BitBucketServerUsername = x$projects,
    BitBucketServerRef = x$ref,
    # BitBucketServerSHA1 = sha,
    BitBucketServerSubdir = x$subdir
  )
}

git_extract_sha1 <- function(bundle) {
  # open the bundle for reading
  conn <- file(bundle, open = "rb", raw = TRUE)
  on.exit(close(conn))
  # seek to where the comment length field should be recorded
  seek(conn, where = -0x2a, origin = "end")
  # verify the comment is length 0x28
  len <- readBin(conn, "raw", n = 2)
  if (len[1] == 0x28 && len[2] == 0x00) {
    # read and return the SHA1
    rawToChar(readBin(conn, "raw", n = 0x28))
  } else {
    NULL
  }
}

source_pkg <- function(path, subdir = NULL, before_install = NULL) {
  info <- source_pkg_info(path = path, subdir = subdir)
  # Check configure is executable if present
  config_path <- file.path(info$pkg_path, "configure")
  if (file.exists(config_path)) {
    Sys.chmod(config_path, "777")
  }
  # Call before_install for bundles (if provided)
  if (!is.null(info$bundle) && !is.null(before_install))
    before_install(info$bundle, info$pkg_path)
  info$pkg_path
}

source_pkg_info <- function(path, subdir = NULL) {
  if (!file.info(path)$isdir) {
    bundle <- path
    outdir <- tempfile(pattern = "devtools")
    dir.create(outdir)
    path <- decompress(path, outdir)
  } else {
    bundle <- NULL
  }
  pkg_path <- if (is.null(subdir)) path else file.path(path, subdir)
  # Check it's an R package
  if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
    stop("Does not appear to be an R package (no DESCRIPTION)", call. = FALSE)
  }
  list(pkg_path = pkg_path, bundle = bundle)
}

decompress <- function(src, target) {
  stopifnot(file.exists(src))
  if (grepl("\\.zip$", src)) {
    my_unzip(src, target)
    outdir <- getrootdir(as.vector(utils::unzip(src, list = TRUE)$Name))
  } else if (grepl("\\.tar$", src)) {
    utils::untar(src, exdir = target)
    outdir <- getrootdir(utils::untar(src, list = TRUE))
  } else if (grepl("\\.(tar\\.gz|tgz)$", src)) {
    utils::untar(src, exdir = target, compressed = "gzip")
    outdir <- getrootdir(utils::untar(src, compressed = "gzip", list = TRUE))
  } else if (grepl("\\.(tar\\.bz2|tbz)$", src)) {
    utils::untar(src, exdir = target, compressed = "bzip2")
    outdir <- getrootdir(utils::untar(src, compressed = "bzip2", list = TRUE))
  } else {
    ext <- gsub("^[^.]*\\.", "", src)
    stop("Don't know how to decompress files with extension ", ext,
         call. = FALSE)
  }
  file.path(target, outdir)
}

my_unzip <- function(src, target, unzip = getOption("unzip")) {
  if (unzip == "internal" || unzip == "") {
    return(utils::unzip(src, exdir = target))
  }
  args <- paste(
    "-oq", shQuote(src),
    "-d", shQuote(target)
  )
  devtools::system_check(unzip, args, quiet = TRUE)
}

getdir <- function(path)  sub("/[^/]*$", "", path)

getrootdir <- function(file_list) {
  slashes <- nchar(gsub("[^/]", "", file_list))
  if (min(slashes) == 0) return("")
  getdir(file_list[which.min(slashes)])
}

install_packages <- function(pkgs, repos = getOption("repos"),
                             type = getOption("pkgType"), ...,
                             dependencies = FALSE, quiet = NULL) {
  if (is.null(quiet))
    quiet <- !identical(type, "source")

  message(sprintf(ngettext(length(pkgs),
                           "Installing %d package: %s",
                           "Installing %d packages: %s"
  ), length(pkgs), paste(pkgs, collapse = ", ")))

  pkgbuild::with_build_tools(
    withr::with_options(list("install.packages.compile.from.source" = "never"),
                        utils::install.packages(pkgs, repos = repos, type = type,
                                                dependencies = dependencies, quiet = quiet
                        )
    ),
    required = FALSE
  )
}

github_error <- function(req) {
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                     error = function(e) {
                       list(message = text)
                     })
  errors <- vapply(parsed$errors, `[[`, "message", FUN.VALUE = character(1))

  structure(
    list(
      call = sys.call(-1),
      message = paste0(parsed$message, " (", httr::status_code(req), ")\n",
                       if (length(errors) > 0) {
                         paste("* ", errors, collapse = "\n")
                       })
    ), class = c("condition", "error", "github_error"))
}

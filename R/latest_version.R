## This code borrowed almost entirely from devtools. Thanks, Hadley.

#' Retrieve Github personal access token.
#' Looks in env var \code{GITHUB_PAT}.
#' @keywords internal
github_pat <- function() {
  pat <- Sys.getenv('GITHUB_PAT')
  if (identical(pat, "")) return(NULL)

  message("Using github PAT from envvar GITHUB_PAT")
  pat
}

##
github_GET <- function(path, ..., pat = github_pat()) {
  if (!is.null(pat)) {
    auth <- httr::authenticate(pat, "x-oauth-basic", "basic")
  } else {
    auth <- NULL
  }

  req <- httr::GET("https://api.github.com/", path = path, auth, ...)

  text <- httr::content(req, as = "text")
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  if (httr::status_code(req) >= 400) {
    stop("Request failed (", httr::status_code(req), ")\n", parsed$message,
      call. = FALSE)
  }

  parsed
}

##
github_commit <- function(username, repo, ref = "master") {
  github_GET(file.path("repos", username, repo, "commits", ref))
}

##
remote_metadata.github_remote <- function(x, bundle = NULL, source = NULL) {
  # Determine sha as efficiently as possible
  if (!is.null(x$sha)) {
    # Might be cached already (because re-installing)
    sha <- x$sha
  } else if (!is.null(bundle)) {
    # Might be able to get from zip archive
    sha <- git_extract_sha1(bundle)
  } else {
    # Otherwise can use github api
    sha <- github_commit(x$username, x$repo, x$ref)$sha
  }

  list(
    RemoteType = "github",
    RemoteHost = x$host,
    RemoteRepo = x$repo,
    RemoteUsername = x$username,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteSubdir = x$subdir,
    # Backward compatibility for packrat etc.
    GithubRepo = x$repo,
    GithubUsername = x$username,
    GithubRef = x$ref,
    GithubSHA1 = sha,
    GithubSubdir = x$subdir
  )
}

##
remote_metadata <- function(x, bundle = NULL, source = NULL) UseMethod("remote_metadata")


checkMasterSha <- function() {
  ## Static remote variable for rwrfhydro repo master.
  remote <-
    structure(list(host = "api.github.com", repo = "rwrfhydro", subdir = NULL, 
                   username = "mccreigh", ref = "master", sha = NULL,
                   auth_token = NULL),
              .Names = c("host", "repo", "subdir",
                         "username", "ref", "sha", "auth_token"),
              class = c("github_remote", "remote"))

  remoteSha <- remote_metadata(remote, NULL, source)$RemoteSha
  localSha  <- packageDescription('rwrfhydro')$RemoteSha
  if(is.null(localSha)) return(invisible('devtools'))
  
  print(remoteSha)
  print(localSha)
  if( remoteSha != localSha )
    cat("",
        "*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*",
        "^*                                                                     *^",
        "*^   A  **NEW** update of rwrfhydro has been made (to master branch).  ^*",
        "^*                                                                     *^",
        "*^   devtools::install_github('mccreigh/rwrfhydro')                    ^*",
        "^*   (for developers: 'git pull origin master', then merg              *^",
        "*^                                                                     ^*",
        "^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^" ,
        sep='\n      ')
  devtools::install_github('mccreigh/rwrfhydro')
  invisible( remoteSha == localSha )
}
  
.onLoad <- function(libname, pkgname) {
  checkMasterSha()
}

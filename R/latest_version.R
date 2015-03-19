#install_github('mccreigh/rwrfhydro')
#Error in install_github("mccreigh/rwrfhydro") : 
#Enter a frame number, or 0 to exit   
#1: install_github("mccreigh/rwrfhydro")
#Selection: 1
#Called from: top level 
#Browse[1]> remote <- remotes[[1]]
#Browse[1]> quiet=FALSE
#Browse[1]> remote_metadata(remote, bundle, source)
#Error during wrapup: object 'bundle' not found
#Browse[1]>  remote_metadata(remote, NULL, source)


remote <- structure(list(host = "api.github.com", repo = "rwrfhydro", subdir = NULL, 
    username = "mccreigh", ref = "master", sha = NULL, auth_token = NULL), .Names = c("host", 
"repo", "subdir", "username", "ref", "sha", "auth_token"), class = c("github_remote", 
"remote"))

remote_metadata(remote, NULL, source)




#' Retrieve Github personal access token.
#'
#' Looks in env var \code{GITHUB_PAT}.
#'
#' @keywords internal
#' @export
github_pat <- function() {
  pat <- Sys.getenv('GITHUB_PAT')
  if (identical(pat, "")) return(NULL)

  message("Using github PAT from envvar GITHUB_PAT")
  pat
}


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

github_commit <- function(username, repo, ref = "master") {
  github_GET(file.path("repos", username, repo, "commits", ref))
}


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


remote_metadata <- function(x, bundle = NULL, source = NULL) UseMethod("remote_metadata")

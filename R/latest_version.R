## This code borrowed almost entirely from devtools. Thanks, Hadley.

#' Retrieve Github personal access token.
#' Looks in env var \code{GetGithubPat}.
#' @keywords internal
GetGithubPat <- function() {
  pat <- Sys.getenv('GetGithubPat')
  if (identical(pat, "")) return(NULL)
  message("Using github PAT from envvar GetGithubPat")
  pat
}


#' @keywords internal
#' @export
github_GET <- function(path, ..., pat = GetGithubPat()) {
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
#' @keywords internal
#' @export
CommitGithub <- function(username, repo, ref = "master") {
  github_GET(file.path("repos", username, repo, "commits", ref))
}

##
#' @keywords internal
#' @export
GetRemoteMetadata.github_remote <- function(x, bundle = NULL, source = NULL) {
  # Determine sha as efficiently as possible
  if (!is.null(x$sha)) {
    # Might be cached already (because re-installing)
    sha <- x$sha
  } else if (!is.null(bundle)) {
    # Might be able to get from zip archive
    sha <- git_extract_sha1(bundle)
  } else {
    # Otherwise can use github api
    sha <- CommitGithub(x$username, x$repo, x$ref)$sha
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
#' @keywords internal
#' @export
GetRemoteMetadata <- function(x, bundle = NULL, source = NULL) UseMethod("GetRemoteMetadata")

#' @keywords internal
#' @export
CheckMasterSha <- function() {
  ## Static remote variable for rwrfhydro repo master.
  localRef <- packageDescription('rwrfhydro')$RemoteRef
  if(!length(localRef)) {
    cat('\n*** NOTE: You loaded your own, local development branch. \n    Switch to a different install to check for updates.')
    return(invisible(FALSE))
  }
  if(localRef != 'master')
    cat('\n*** NOTE: You are using branch "',localRef,'" ***\n\n', sep='')
  remote <-
    structure(list(host = "api.github.com", repo = "rwrfhydro", subdir = NULL, 
                   username = "mccreigh", ref = localRef, sha = NULL,
                   auth_token = NULL),
              .Names = c("host", "repo", "subdir",
                         "username", "ref", "sha", "auth_token"),
              class = c("github_remote", "remote"))

  remoteSha <- GetRemoteMetadata(remote, NULL, source)$RemoteSha
  localSha  <- packageDescription('rwrfhydro')$RemoteSha
  
  if(is.null(localSha)) return(invisible('devtools'))
  
  if( remoteSha != localSha ) {
    
    if(localRef == 'master') {
      cat("",
        "*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*",
        "*^                                                                     ^*",
        "*^   A  **NEW** update of rwrfhydro has been made (to master branch).  ^*",
        "*^                                                                     ^*",
        "*^   To update: devtools::install_github('mccreigh/rwrfhydro')         ^*",        
        "*^   Developers take note to update:                                   ^*",
        "*^             'git pull origin master', then merge devBranch)         ^*",
        "*^                                                                     ^*",
        "*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*",
        "                                                                         ",
        sep='\n      ')
    } else {
      cat("",
          "*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*",
          "",
          "   A  **NEW** update of rwrfhydro has been made to this branch.",
          "",
          "   To update:",
   paste0("    devtools::install_github('mccreigh/rwrfhydro', ref='",localRef,"')"),        
          "",
          "*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*^*",
          "",
          sep='\n      ')
    }
  }
   
  if( remoteSha == localSha ) cat("You are using the latest version.",sep='\n')                                  
  
  invisible( remoteSha == localSha )
}
 

#' @export
CheckForUpdates <- function() { CheckMasterSha() }

.onAttach <- function(libname, pkgname) {
  msg <- "To check rwrfhydro updates run: CheckForUpdates()"
  packageStartupMessage(msg)
}
 

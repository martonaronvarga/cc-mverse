# R/hpc/00_ssh_bridge.R
# SSH connection management and HPC communication bridge

#' SSH connection manager for cluster access
#'
#' Handles authentication, key management, and remote execution
#'
#' @param cluster_host Hostname or IP of cluster login node
#' @param cluster_user Username on cluster
#' @param ssh_key Path to SSH private key (default: ~/.ssh/id_rsa)
#' @param port SSH port (default: 22)
#' @param timeout Connection timeout in seconds
#'
#' @return List with connection metadata and methods
#'
new_ssh_connection <- function(
    cluster_host,
    cluster_user,
    ssh_key = "~/.ssh/id_ed25519",
    port = 22,
    timeout = 10) {
  logger::log_info("Setting up SSH connection to {cluster_user}@{cluster_host}:{port}")

  # Expand path
  ssh_key <- path.expand(ssh_key)

  # Validate key exists
  if (!file.exists(ssh_key)) {
    logger::log_error("SSH key not found: {ssh_key}")
    stop(glue::glue("SSH key not found: {ssh_key}"))
  }

  # Validate permissions (should be 600)
  key_perms <- substr(as.character(file.info(ssh_key)$mode), 4, 6)
  if (key_perms != "600") {
    logger::log_warn("SSH key permissions not 600: {key_perms}")
    logger::log_warn("Attempting to fix with: chmod 600 {ssh_key}")
    system(glue::glue("chmod 600 {ssh_key}"))
  }

  connection <- list(
    host = cluster_host,
    user = cluster_user,
    key = ssh_key,
    port = port,
    timeout = timeout,

    # Connection status
    connected = FALSE,
    last_check = NULL
  )

  class(connection) <- c("ssh_connection", "list")

  # Test connection
  if (!test_ssh_connection(connection)) {
    stop("SSH connection failed")
  }

  connection$connected <- TRUE
  connection$last_check <- Sys.time()

  logger::log_info("SSH connection established")

  connection
}

#' Test SSH connection with timeout
#'
#' @param conn SSH connection object
#'
#' @return Logical TRUE if successful
#'
test_ssh_connection <- function(conn) {
  logger::log_debug("Testing SSH connection...")

  cmd <- glue::glue(
    "ssh -i {conn$key} -p {conn$port} -o ConnectTimeout={conn$timeout} ",
    "{conn$user}@{conn$host} 'echo OK'"
  )

  result <- tryCatch(
    {
      output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
      "OK" %in% output
    },
    error = function(e) {
      logger::log_error("SSH connection test failed: {e$message}")
      FALSE
    }
  )

  if (result) {
    logger::log_debug("SSH connection test successful")
  }

  result
}

#' Execute command on remote cluster
#'
#' @param conn SSH connection object
#' @param remote_cmd Command to execute on cluster
#' @param ignore_error If TRUE, don't stop on non-zero exit
#'
#' @return List with status, stdout, stderr
#'
ssh_execute <- function(conn, remote_cmd, ignore_error = FALSE) {
  logger::log_debug("Executing on {conn$host}: {substr(remote_cmd, 1, 50)}...")

  # Prepare SSH command
  ssh_cmd <- glue::glue(
    "ssh -i {conn$key} -p {conn$port} {conn$user}@{conn$host} '{remote_cmd}'"
  )

  result <- tryCatch(
    {
      output <- system(ssh_cmd, intern = TRUE, ignore.stderr = FALSE)
      list(
        status = 0,
        stdout = paste(output, collapse = "\n"),
        stderr = ""
      )
    },
    error = function(e) {
      list(
        status = 1,
        stdout = "",
        stderr = e$message
      )
    }
  )

  if (result$status != 0 && !ignore_error) {
    logger::log_error("Remote command failed: {result$stderr}")
    stop(glue::glue("SSH command failed: {result$stderr}"))
  }

  result
}

#' Copy file to remote cluster
#'
#' @param local_path Local file path
#' @param remote_path Remote path (user@host:path format handled automatically)
#' @param conn SSH connection object
#' @param recursive If TRUE, copy directories recursively
#'
#' @return Invisibly TRUE
#'
scp_to_cluster <- function(local_path, remote_path, conn, recursive = FALSE) {
  logger::log_info("Copying to cluster: {local_path} → {remote_path}")

  # Construct remote path
  full_remote <- glue::glue("{conn$user}@{conn$host}:{remote_path}")

  # SCP command
  recursive_flag <- if (recursive) "-r" else ""
  scp_cmd <- glue::glue(
    "scp {recursive_flag} -i {conn$key} -P {conn$port} {local_path} {full_remote}"
  )

  status <- system(scp_cmd, ignore.stderr = FALSE)

  if (status != 0) {
    logger::log_error("SCP upload failed")
    stop("File copy to cluster failed")
  }

  logger::log_info("File copied successfully")

  invisible(TRUE)
}

#' Copy file from remote cluster
#'
#' @param remote_path Remote file path
#' @param local_path Local path to save to
#' @param conn SSH connection object
#' @param recursive If TRUE, copy directories recursively
#'
#' @return Invisibly TRUE
#'
scp_from_cluster <- function(remote_path, local_path, conn, recursive = FALSE) {
  logger::log_info("Copying from cluster: {remote_path} → {local_path}")

  # Construct remote path
  full_remote <- glue::glue("{conn$user}@{conn$host}:{remote_path}")

  # SCP command
  recursive_flag <- if (recursive) "-r" else ""
  scp_cmd <- glue::glue(
    "scp {recursive_flag} -i {conn$key} -P {conn$port} {full_remote} {local_path}"
  )

  status <- system(scp_cmd, ignore.stderr = FALSE)

  if (status != 0) {
    logger::log_error("SCP download failed")
    stop("File copy from cluster failed")
  }

  logger::log_info("File copied successfully")

  invisible(TRUE)
}

#' Print connection info
#'
#' @param x SSH connection object
#'
#' @export
#'
print.ssh_connection <- function(x, ...) {
  cat("SSH Connection:\n")
  cat("  Host:", x$host, "\n")
  cat("  User:", x$user, "\n")
  cat("  Port:", x$port, "\n")
  cat("  Key:", x$key, "\n")
  cat("  Status:", if (x$connected) "Connected" else "Disconnected", "\n")
  if (!is.null(x$last_check)) {
    cat("  Last check:", format(x$last_check), "\n")
  }
  invisible(x)
}

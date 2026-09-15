#' Get currently available system memory
#'
#' Dispatches on `.Platform$OS.type`: on Windows, queries free
#' physical memory via PowerShell's
#' `Get-CimInstance Win32_OperatingSystem` (`wmic` is
#' deprecated/unreliable on this project's machine, hence PowerShell).
#' On Linux, reads `/proc/meminfo`'s `MemAvailable` field — the
#' reclaimable-cache-aware "true free" estimate (kernel >= 3.14),
#' rather than `MemFree`, which is needlessly pessimistic. Under
#' SLURM (`SLURM_JOB_ID` set), the Linux result is additionally capped
#' at the job's allocated memory ceiling, since a shared node's free
#' memory can otherwise overstate what the job is actually entitled
#' to. Real-time cgroup usage/limit accounting would be more precise
#' still but its cgroup path varies by SLURM version and site
#' configuration, so it's left out for now.
#'
#' @return Available memory in MB, as a numeric value.
#'
#' @examples
#' \dontrun{
#' get_available_memory_mb()
#' }
get_available_memory_mb <- function() {
   if (.Platform$OS.type == "windows") {
      return(get_available_memory_mb_win())
   }
   get_available_memory_mb_nix()
}

# Windows: free physical memory via PowerShell/CIM.
get_available_memory_mb_win <- function() {
   out_kb <- system2(
      "powershell",
      c("-NoProfile", "-Command",
        "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"),
      stdout = TRUE
   )
   as.numeric(trimws(out_kb)) / 1024
}

# Linux: /proc/meminfo's MemAvailable, capped by the SLURM allocation
# ceiling (if running under SLURM).
get_available_memory_mb_nix <- function() {
   meminfo <- readLines("/proc/meminfo")
   line    <- grep("^MemAvailable:", meminfo, value = TRUE)
   if (length(line) != 1) {
      stop("Could not find MemAvailable in /proc/meminfo.")
   }
   available_mb <- as.numeric(gsub("[^0-9]", "", line)) / 1024

   slurm_ceiling_mb <- get_slurm_memory_ceiling_mb()
   if (!is.na(slurm_ceiling_mb)) {
      available_mb <- min(available_mb, slurm_ceiling_mb)
   }
   available_mb
}

# The job's allocated memory ceiling (MB) under SLURM, or NA if not
# running under SLURM or the allocation can't be determined.
get_slurm_memory_ceiling_mb <- function() {
   if (!nzchar(Sys.getenv("SLURM_JOB_ID"))) {
      return(NA_real_)
   }

   mem_per_node <- suppressWarnings(
      as.numeric(Sys.getenv("SLURM_MEM_PER_NODE"))
   )
   if (!is.na(mem_per_node)) {
      return(mem_per_node)
   }

   mem_per_cpu <- suppressWarnings(
      as.numeric(Sys.getenv("SLURM_MEM_PER_CPU"))
   )
   cpus_per_task <- suppressWarnings(
      as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK", unset = "1"))
   )
   if (!is.na(mem_per_cpu)) {
      return(mem_per_cpu * cpus_per_task)
   }

   NA_real_
}

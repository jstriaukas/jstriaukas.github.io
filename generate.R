# build_wrappers.R
# Creates LaTeX wrappers with shared "common info", then \input{page}, then closing.

# ---- 1) Define your shared content ----
common_top <- c(
  "% ====== AUTO-GENERATED FILE ======",
  "% Edit the common sections here to update all pages at once.",
  "\\documentclass[11pt]{article}",
  "\\usepackage[a4paper,margin=1in]{geometry}",
  "\\usepackage{hyperref}",
  "\\usepackage{graphicx}",
  "\\usepackage{amsmath, amssymb}",
  "",
  "\\begin{document}",
  "% ---- common info (BEGIN) ----",
  "\\thispagestyle{empty}",
  "\\begin{center}",
  "{\\Large Jonas Striaukas}\\\\[6pt]",
  "{\\small Copenhagen Business School}\\\\[4pt]",
  "\\href{mailto:jonas.striaukas@gmail.com}{jonas.striaukas@gmail.com} \\quad",
  "\\href{mailto:jost.fi@cbs.dk}{jost.fi@cbs.dk}",
  "\\end{center}",
  "\\vspace{1em}",
  "% ----------------------------------"
)

common_bottom <- c(
  "% ----------------------------------",
  "% ---- end of common info (END) ----",
  "\\end{document}"
)

# ---- 2) Helper that writes ONE wrapper around a page-specific snippet ----
make_wrapper <- function(page_tex,
                         out_dir = "build",
                         top = common_top,
                         bottom = common_bottom,
                         input_dir = NULL) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Normalize paths
  page_tex <- normalizePath(page_tex, mustWork = TRUE)
  
  # TeX \input expects path WITHOUT extension
  page_no_ext <- sub("\\.tex$", "", page_tex, ignore.case = TRUE)
  
  # Optionally shorten the input path if snippets live in a known folder
  if (!is.null(input_dir)) {
    page_no_ext <- sub(paste0("^", normalizePath(input_dir, mustWork = FALSE), "/?"), "", page_no_ext)
  }
  
  # Output file has same base name but lives in out_dir
  out_file <- file.path(out_dir, paste0(basename(sub("\\.tex$", "", page_tex)), "_wrapper.tex"))
  
  body <- c(
    top,
    sprintf("\\input{%s}", page_no_ext),
    bottom
  )
  
  writeLines(body, out_file)
  message("Wrote: ", out_file)
  invisible(out_file)
}

# ---- 3) Batch mode: build wrappers for all page snippets in a folder ----
make_wrappers_from_dir <- function(snippets_dir = "pages",
                                   pattern = "\\.tex$",
                                   out_dir = "build",
                                   top = common_top,
                                   bottom = common_bottom) {
  if (!dir.exists(snippets_dir)) {
    stop("Directory not found: ", snippets_dir)
  }
  pages <- list.files(snippets_dir, pattern = pattern, full.names = TRUE)
  if (length(pages) == 0) {
    stop("No matching *.tex files found in: ", snippets_dir)
  }
  vapply(pages, make_wrapper, character(1),
         out_dir = out_dir,
         top = top,
         bottom = bottom,
         input_dir = normalizePath(snippets_dir, mustWork = FALSE))
}

# ---- 4) (Optional) Create a master file that compiles every page in one PDF ----
make_master <- function(wrappers_dir = "build",
                        pattern = "_wrapper\\.tex$",
                        master_file = file.path(wrappers_dir, "MASTER_all_pages.tex")) {
  files <- list.files(wrappers_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No wrapper files found in: ", wrappers_dir)
  
  # A minimal master that \input{}s each wrapper (which are full documents).
  # If you prefer a single document with sections, change wrappers to snippets
  # and \input the snippets instead.
  master <- c(
    "% A master index that inputs each wrapper as-is.",
    "% Note: This produces separate documents concatenated; most LaTeX engines",
    "% will not compile multiple \\documentclass in one run. If you want a single",
    "% PDF, instead input the PAGE-SPECIFIC snippets directly into one document.",
    "% (See alternate master below.)",
    "% --- Alternate SINGLE-DOC MASTER EXAMPLE ---",
    "% \\documentclass[11pt]{article}",
    "% \\usepackage[a4paper,margin=1in]{geometry}",
    "% \\begin{document}",
    "% % Common info here once...",
    "% \\section*{All pages}",
    paste0("% \\input{", sub("_wrapper\\.tex$", "", files), "}"),
    "% \\end{document}"
  )
  writeLines(master, master_file)
  message("Wrote master template to: ", master_file)
  invisible(master_file)
}

# -------------------------------
# EXAMPLES
# -------------------------------
# 1) Single file:
# make_wrapper("pages/publications.tex")
#
# 2) All *.tex snippets inside 'pages/' -> wrappers in 'build/':
# make_wrappers_from_dir(snippets_dir = "pages", out_dir = "build")
#
# 3) (Optional) Create a master template (commented guidance inside):
# make_master(wrappers_dir = "build")
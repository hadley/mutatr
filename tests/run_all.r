frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
FILE <- frame_files[[length(frame_files)]]

PATH <- dirname(FILE)

tests <- setdiff(dir(PATH, full.names = TRUE), FILE)

lapply(tests, source, chdir = TRUE)
frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
FILE <- frame_files[[length(frame_files)]]

PATH <- dirname(FILE)

source(file.path(PATH, "assert.r"))
tests <- setdiff(dir(PATH, "test-", full.names = TRUE), FILE)

lapply(tests, source, chdir = TRUE)
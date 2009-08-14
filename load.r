frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
PATH <- dirname(frame_files[[length(frame_files)]])

source(file.path(PATH, "R", "object-inheritance.r"), chdir = T)

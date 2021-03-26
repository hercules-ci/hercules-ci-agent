
#define compatPath(store, path) (path)
#define compatOutputPathSet(store, paths) (paths)
#define compatComputeFSClosure(src, pathSet, closurePaths) (src->computeFSClosure(pathSet, closurePaths))

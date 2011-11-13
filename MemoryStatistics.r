#following function combines memory.limit, memory.size, and gc into a single function
#with friendly formatting; it can be used only on Windows platform

reportMem <- function(){
# Perform garbage collection
gc(verbose=FALSE)
#How much memory is being used by malloc
intvar <- round(memory.size())
cat("Memory currently allocated is", intvar, "Mb\n")
#Maximum amount of memory that has been obtained from the OS
intvar <- round(memory.size(max=TRUE))
cat("Maximum amount of memory allocated during this session is",
intvar, "Mb\n")
#Memory limit
intvar <- round(memory.limit())
cat("Current limit for total allocation is", intvar, "Mb\n")
}
#reportObjSize function defined below is a simple wrapper for object.size that
#prints the size of an object in MB
reportObjSize <- function(x=x){
bm_conv <- 1024*1024
objsize <- round(object.size(x)/bm_conv)
cat("Object Size =", objsize, "Mb", "\n")
}
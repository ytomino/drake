#include <unwind.h> /* exception mechanism of gcc */
#include <stdint.h> /* included by unwind-pe.h */
#if defined(__APPLE__)
#include <sys/types.h> /* avoiding circular dependency */
#include <crt_externs.h> /* environment variable */
#endif
#if defined(__FreeBSD__)
#include <sys/types.h>
#include <sys/param.h> /* PAGE_SIZE */
#endif
#if defined(__unix__) || defined(__APPLE__)
#include <errno.h>
#include <signal.h> /* signal handler */
#include <time.h> /* time and sleep */
#include <sys/time.h> /* get current time */
#include <sys/resource.h> /* get CPU time */
#include <sys/mman.h> /* memory mapped I/O */
#include <sys/wait.h> /* waitpid */
#include <unistd.h> /* low-level I/O */
#include <pwd.h> /* user info */
#include <grp.h> /* group info */
#include <sys/fcntl.h> /* low-level file op */
#include <sys/stat.h> /* low-level file info */
#include <dirent.h> /* directory searching */
#include <fnmatch.h> /* wildcard */
#include <termios.h> /* terminal control */
#include <stdlib.h> /* memory op, abort */
#include <pthread.h> /* tasking */
#endif
#include <string.h> /* string op */

#if defined(__unix__) || defined(__APPLE__)
#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"
#pragma for Ada overload int open(const char *, int, mode_t)
#pragma for Ada overload int fcntl(int, int, int)
#endif
#if defined(__APPLE__)
#pragma for Ada "errno.h" include "sys/errno.h"
#pragma for Ada "sys/signal.h" include "sys/_structs.h" /* stack_t */
#pragma for Ada "sys/time.h" include "sys/_structs.h"
#pragma for Ada overload int gettimeofday(struct timeval *, struct timezone *)
#pragma for Ada "termios.h" include "sys/termios.h"
#pragma for Ada "sys/stat.h" include "sys/fcntl.h" /* S_IF* */
#pragma for Ada "pthread.h" include "sys/types.h"
#elif defined(__FreeBSD__)
#pragma for Ada "sys/time.h" include "sys/_timeval.h"
#pragma for Ada "sys/time.h" include "sys/timespec.h"
#pragma for Ada "sys/mman.h" include "sys/types.h" /* mmap */
#pragma for Ada "unistd.h" include "sys/types.h" /* lseek */
#pragma for Ada "pthread.h" include "sys/_pthreadtypes.h"
#endif

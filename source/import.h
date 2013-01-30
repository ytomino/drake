#if defined(__linux__)
#define _GNU_SOURCE /* use GNU extension */
#endif

#include <unwind.h> /* exception mechanism of gcc */
#include <stdint.h> /* included by unwind-pe.h */

#if defined(__linux__) || defined (__WINNT__)
#include <limits.h> /* before bits/posix1_lim.h / winuser.h */
#endif

#if defined(__unix__) || defined(__APPLE__)
#include <stddef.h>
#include <errno.h>
#include <sys/types.h> /* before other system headers */
#include <time.h> /* time and sleep */
#include <sys/time.h> /* get current time */
#include <signal.h> /* before unistd.h */
#include <string.h> /* strsignal */
#include <sys/syscall.h> /* sigreturn */
#include <sys/ucontext.h>
#include <sys/mman.h> /* low-level memory op */
#include <unistd.h> /* low-level I/O */
#include <stdlib.h> /* abort, atexit, lldiv, getenv/setenv and memory op */
#include <sys/resource.h> /* get CPU time */
#include <sys/wait.h> /* waitpid */
#include <pwd.h> /* user info */
#include <grp.h> /* group info */
#include <sys/fcntl.h> /* low-level file op */
#include <sys/stat.h> /* low-level file info */
#include <sys/socket.h> /* socket, before sys/mount.h */
#include <sys/mount.h> /* filesystem */
#include <dirent.h> /* directory searching */
#include <fnmatch.h> /* wildcard */
#include <termios.h> /* terminal control */
#include <netdb.h> /* getaddrinfo */
#include <netinet/in.h> /* protocols */
#include <pthread.h> /* tasking */
#endif
#if defined(__APPLE__)
#include <crt_externs.h> /* environment variable */
#include <malloc/malloc.h> /* malloc_size */
#include <spawn.h> /* spawn */
#include <copyfile.h> /* copyfile */
#elif defined(__FreeBSD__)
#include <sys/param.h> /* PAGE_SIZE */
#include <malloc_np.h> /* malloc_usable_size */
#include <pthread_np.h> /* pthread_attr_get_np */
#elif defined(__linux__)
#undef _GNU_SOURCE
#undef __USE_GNU /* avoiding circular dependency between libio.h and stdio.h */
#include <malloc.h> /* malloc_usable_size */
#endif

#if defined(__WINNT__)
#define UNICODE
#define WIN32_LEAN_AND_MEAN
#include <basetsd.h>
/* avoiding circular dependency between windef.h and winnt.h */
#define NT_INCLUDED
#define SHORT short
#define LONG long
#define HANDLE void *
#define DECLARE_HANDLE(name) \
	struct name##__ { int unused; }; \
	typedef struct name##__ *name
#include <windef.h>
#undef DECLARE_HANDLE
#undef HANDLE
#undef LONG
#undef SHORT
#undef NT_INCLUDED
#include <winnt.h>
#include <windows.h>
#define RC_INVOKED /* headmaster can not translate some inline functions */
#include <malloc.h>
#undef RC_INVOKED
#include <stdlib.h> /* abort, atexit, lldiv */
#undef UNICODE
#undef WIN32_LEAN_AND_MEAN
#endif

#if defined(__unix__) || defined(__APPLE__)
#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"
#pragma for Ada overload int fcntl(int, int, int)
#if !defined(__linux__)
#pragma for Ada overload int open(const char *, int, mode_t)
#pragma for Ada overload int syscall(int, void *, unsigned int)
#endif
#endif
#if defined(__APPLE__)
#pragma for Ada "errno.h" include "sys/errno.h"
#pragma for Ada "pthread.h" include "signal.h" /* pthread_kill */
#pragma for Ada "pthread.h" include "sys/types.h"
#pragma for Ada "sys/signal.h" include "sys/_structs.h" /* stack_t */
#pragma for Ada "sys/stat.h" include "sys/fcntl.h" /* S_IF* */
#pragma for Ada "sys/time.h" include "sys/_structs.h"
#pragma for Ada overload int gettimeofday(struct timeval *, struct timezone *)
#pragma for Ada "sys/ucontext.h" include "sys/_structs.h" /* ucontext_t */
#pragma for Ada "termios.h" include "sys/termios.h"
#elif defined(__FreeBSD__)
#pragma for Ada "pthread.h" include "sys/_pthreadtypes.h"
#pragma for Ada "sys/mman.h" include "sys/types.h" /* mmap */
#pragma for Ada "sys/signal.h" include "sys/select.h" /* sigset_t */
#pragma for Ada "sys/time.h" include "sys/_timeval.h"
#pragma for Ada "sys/time.h" include "sys/timespec.h"
#pragma for Ada "unistd.h" include "sys/types.h" /* lseek */
#elif defined(__linux__)
#undef si_value /* cannot inline returning unchecked union */
#pragma for Ada overload int open(const char *, int, __mode_t)
#pragma for Ada overload long syscall(long, void *, unsigned int)
#pragma for Ada "bits/time.h" monolithic_include "bits/timex.h"
#pragma for Ada "pthread.h" include "bits/pthreadtypes.h"
#pragma for Ada "signal.h" monolithic_include "bits/sigaction.h"
#pragma for Ada "sys/mman.h" include "bits/mman.h"
#pragma for Ada "sys/signal.h" include "bits/signum.h"
#pragma for Ada "sys/signal.h" include "bits/siginfo.h"
#pragma for Ada "sys/signal.h" include "bits/sigstack.h" /* MINSIGSTKSZ */
#pragma for Ada "sys/signal.h" include "signal.h" /* SA_ */
#pragma for Ada "sys/syscall.h" include "bits/syscall.h"
#endif

#if defined(__WINNT__)
#endif

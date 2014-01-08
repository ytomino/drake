#if defined(__APPLE__)
#define _DONT_USE_CTYPE_INLINE_
#elif defined(__FreeBSD__)
#define _DONT_USE_CTYPE_INLINE_
#define d_fileno d_ino
#elif defined(__linux__)
#define _GNU_SOURCE /* use GNU extension */
#define _FILE_OFFSET_BITS 64
#define st_atim st_atimespec
#define st_mtim st_mtimespec
#define st_ctim st_ctimespec
#endif

#include <stdint.h> /* included by unwind-pe.h */

#if defined(__linux__) || defined(__WINNT__)
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
#if defined(__linux__)
#include <sys/stat.h> /* low-level file info, before fcntl.h */
#include <fcntl.h> /* low-level file op */
#else
#include <fcntl.h> /* low-level file op, before sys/stat.h */
#include <sys/stat.h> /* low-level file info */
#endif
#include <sys/file.h> /* flock */
#include <sys/socket.h> /* socket, before sys/mount.h */
#include <sys/mount.h> /* filesystem */
#include <dirent.h> /* directory searching */
#include <fnmatch.h> /* wildcard */
#include <termios.h> /* terminal control */
#include <netdb.h> /* getaddrinfo */
#include <netinet/in.h> /* protocols */
#include <pthread.h> /* tasking */
#include <dlfcn.h>
#include <iconv.h>
#if !defined(__linux__)
#include <wchar.h> /* after malloc.h in Linux */
#endif
#endif
#if defined(__APPLE__)
#undef _DONT_USE_CTYPE_INLINE_
#include <crt_externs.h> /* environment variable */
#include <malloc/malloc.h> /* malloc_size */
#include <spawn.h> /* spawn */
#include <copyfile.h> /* copyfile */
#elif defined(__FreeBSD__)
#undef _DONT_USE_CTYPE_INLINE_
#undef d_fileno
#include <sys/param.h> /* PAGE_SIZE */
#include <malloc_np.h> /* malloc_usable_size */
#include <pthread_np.h> /* pthread_attr_get_np */
#elif defined(__linux__)
#include <sys/statvfs.h> /* filesystem */
#include <spawn.h> /* spawn */
#undef _GNU_SOURCE
#undef __USE_GNU /* avoiding circular dependency between libio.h and stdio.h */
#undef st_atim
#undef st_mtim
#undef st_ctim
#undef st_atime
#undef st_mtime
#undef st_ctime
#undef _SC_NPROCESSORS_ONLN
#include <malloc.h> /* malloc_usable_size */
#undef __USE_XOPEN2K8 /* avoiding circular dependency between wchar.h and stdio.h */
#include <wchar.h>
#undef _FILE_OFFSET_BITS
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
#define __INTRIN_H_
#include <winnt.h>
#undef __INTRIN_H_
#include <winsock2.h> /* before windows.h */
#undef h_errno /* headmaster can not translate it */
#include <windows.h>
#include <wincrypt.h> /* random */
#include <ws2tcpip.h>
#undef _S6_un /* false positive warning of gcc */
#undef s6_addr /* use _S6_un */
#define RC_INVOKED /* headmaster can not translate some inline functions */
#include <malloc.h>
#undef RC_INVOKED
#include <stdlib.h> /* abort, atexit, lldiv */
#include <signal.h>
#undef UNICODE
#undef WIN32_LEAN_AND_MEAN
#endif

#include <unwind.h> /* exception mechanism of gcc, after windows.h */

#if defined(__unix__) || defined(__APPLE__)
#include "fix-fcntl.h"
#pragma for Ada "fcntl.h" monolithic_include "fix-fcntl.h"
#include "fix-wchar.h"
#pragma for Ada "wchar.h" monolithic_include "fix-wchar.h"
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
#pragma for Ada overload int gettimeofday(struct timeval *, struct timezone *)
#pragma for Ada overload size_t iconv(iconv_t cd, \
	char const **inbuf, size_t *inbytesleft, \
	char **outbuf, size_t *outbytesleft)
#pragma for Ada "dirent.h" include "sys/dirent.h"
#pragma for Ada "errno.h" include "sys/errno.h"
#pragma for Ada "fcntl.h" include "sys/fcntl.h"
#pragma for Ada "pthread.h" include "signal.h" /* pthread_kill */
#pragma for Ada "pthread.h" include "sys/types.h"
#pragma for Ada "signal.h" include "sys/_structs.h" /* stack_t */
#pragma for Ada "signal.h" include "sys/signal.h"
#pragma for Ada "sys/file.h" include "sys/fcntl.h"
#pragma for Ada "sys/stat.h" include "sys/fcntl.h" /* S_IF* */
#pragma for Ada "sys/time.h" include "sys/_structs.h" /* timeval */
#pragma for Ada "sys/ucontext.h" include "sys/_structs.h" /* ucontext_t */
#pragma for Ada "termios.h" include "sys/termios.h"
#pragma for Ada "time.h" include "sys/_structs.h" /* timespec */
#pragma for Ada "unistd.h" include "sys/unistd.h"
#elif defined(__FreeBSD__)
#pragma for Ada "dirent.h" include "sys/dirent.h"
#pragma for Ada "pthread.h" include "sys/_pthreadtypes.h"
#pragma for Ada "signal.h" include "sys/select.h" /* sigset_t */
#pragma for Ada "signal.h" include "sys/signal.h"
#pragma for Ada "sys/file.h" include "fcntl.h"
#pragma for Ada "sys/mman.h" include "sys/types.h" /* mmap */
#pragma for Ada "sys/time.h" include "sys/_timeval.h" /* timeval */
#pragma for Ada "time.h" include "sys/timespec.h" /* timespec */
#pragma for Ada "unistd.h" include "sys/types.h" /* lseek */
#pragma for Ada "unistd.h" include "sys/unistd.h"
#elif defined(__linux__)
#undef si_value /* cannot inline returning unchecked union */
#pragma for Ada overload int open(const char *, int, __mode_t)
#pragma for Ada overload long syscall(long, void *, unsigned int)
#pragma for Ada overload void pthread_yield(void)
#pragma for Ada overload size_t iconv(iconv_t __cd, \
	char const ** restrict __inbuf, size_t * restrict __inbytesleft, \
	char ** restrict __outbuf, size_t * restrict __outbytesleft)
#pragma for Ada "bits/time.h" monolithic_include "bits/timex.h"
#pragma for Ada "dirent.h" include "bits/dirent.h"
#pragma for Ada "dlfcn.h" include "bits/dlfcn.h"
#pragma for Ada "errno.h" include "asm-generic/errno.h"
#pragma for Ada "errno.h" include "asm-generic/errno-base.h"
#pragma for Ada "errno.h" include "bits/errno.h"
#pragma for Ada "fcntl.h" include "bits/fcntl.h"
#pragma for Ada "pthread.h" include "bits/pthreadtypes.h"
#pragma for Ada "pthread.h" include "bits/sigthread.h"
#pragma for Ada "signal.h" include "bits/siginfo.h"
#pragma for Ada "signal.h" include "bits/sigset.h"
#pragma for Ada "signal.h" include "bits/sigstack.h" /* MINSIGSTKSZ */
#pragma for Ada "signal.h" monolithic_include "bits/sigaction.h"
#pragma for Ada "signal.h" monolithic_include "bits/signum.h"
#pragma for Ada "sys/file.h" include "bits/fcntl.h"
#pragma for Ada "sys/mman.h" include "bits/mman.h"
#pragma for Ada "sys/resource.h" include "bits/resource.h"
#pragma for Ada "sys/socket.h" include "bits/socket.h"
#pragma for Ada "sys/socket.h" include "bits/socket_type.h"
#pragma for Ada "sys/stat.h" include "bits/stat.h"
#pragma for Ada "sys/statvfs.h" include "bits/statvfs.h"
#pragma for Ada "sys/syscall.h" include "bits/syscall.h"
#pragma for Ada "sys/time.h" include "bits/time.h" /* timeval */
#pragma for Ada "sys/types.h" include "bits/types.h" /* time_t */
#pragma for Ada "termios.h" include "bits/termios.h"
#pragma for Ada "unistd.h" include "bits/confname.h" /* _SC_NPROCESSORS_ONLN */
#endif

#if defined(__WINNT__)
#pragma instance DWORD "INFINITE" /* winbase.h */
#pragma instance DWORD "CRYPT_VERIFYCONTEXT" /* wincrypt.h */
#pragma instance DWORD "GENERIC_READ" /* winnt.h */
#endif

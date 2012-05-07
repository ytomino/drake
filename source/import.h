#include <unwind.h> /* exception mechanism of gcc */
#include <stdint.h> /* included by unwind-pe.h */

#if defined(__unix__) || defined(__APPLE__)
#include <sys/types.h> /* before other system headers */
#include <signal.h> /* before unistd.h */
#include <errno.h>
#include <string.h> /* strsignal */
#include <sys/syscall.h> /* sigreturn */
#include <sys/mman.h> /* low-level memory op */
#include <unistd.h> /* low-level I/O */
#include <stdlib.h> /* memory op, abort */
#include <time.h> /* time and sleep */
#include <sys/time.h> /* get current time */
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
#endif

#if defined(__WINNT__)
#define UNICODE
#define WIN32_LEAN_AND_MEAN

/* avoiding circular dependency between windef.h and winnt.h */
#define CONST const
#define UCHAR unsigned char
#define BYTE uint8_t
#define WORD uint16_t
#define DWORD uint32_t
#define PDWORD uint32_t *
#define WINAPI __stdcall
#include <winnt.h>
#undef CONST
#undef UCHAR
#undef BYTE
#undef WORD
#undef DWORD
#undef PDWORD
#undef WINAPI

#include <windows.h>
#undef UNICODE
#undef WIN32_LEAN_AND_MEAN
#endif

#if defined(__unix__) || defined(__APPLE__)
#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"
#pragma for Ada overload int open(const char *, int, mode_t)
#pragma for Ada overload int fcntl(int, int, int)
#pragma for Ada overload int syscall(int, void *, unsigned int)
#endif
#if defined(__APPLE__)
#pragma for Ada "errno.h" include "sys/errno.h"
#pragma for Ada "sys/signal.h" include "sys/_structs.h" /* stack_t */
#pragma for Ada "sys/time.h" include "sys/_structs.h"
#pragma for Ada overload int gettimeofday(struct timeval *, struct timezone *)
#pragma for Ada "termios.h" include "sys/termios.h"
#pragma for Ada "sys/stat.h" include "sys/fcntl.h" /* S_IF* */
#pragma for Ada "pthread.h" include "sys/types.h"
#pragma for Ada "pthread.h" include "signal.h" /* pthread_kill */
#elif defined(__FreeBSD__)
#pragma for Ada "sys/time.h" include "sys/_timeval.h"
#pragma for Ada "sys/time.h" include "sys/timespec.h"
#pragma for Ada "sys/mman.h" include "sys/types.h" /* mmap */
#pragma for Ada "sys/signal.h" include "sys/select.h" /* sigset_t */
#pragma for Ada "unistd.h" include "sys/types.h" /* lseek */
#pragma for Ada "pthread.h" include "sys/_pthreadtypes.h"
#endif

#if defined(__WINNT__)
#endif

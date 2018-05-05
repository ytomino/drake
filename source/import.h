#if defined(__APPLE__)
#define _DONT_USE_CTYPE_INLINE_
#define st_atimespec st_atim
#define st_mtimespec st_mtim
#define st_ctimespec st_ctim
#elif defined(__FreeBSD__)
#define _DONT_USE_CTYPE_INLINE_
#define d_fileno d_ino
#if __FreeBSD__ < 9
#define st_atimespec st_atim
#define st_mtimespec st_mtim
#define st_ctimespec st_ctim
#endif
#elif defined(__gnu_linux__)
#define _GNU_SOURCE /* use GNU extension */
#define _FILE_OFFSET_BITS 64
#endif

#include <stdint.h> /* included by unwind-pe.h */

#if defined(__gnu_linux__) || defined(__WINNT__)
#include <limits.h> /* before bits/posix1_lim.h / winuser.h */
#endif

#if defined(__unix__) || defined(__APPLE__)
#include <stddef.h>
#include <errno.h>
#if defined(__gnu_linux__)
#define _SYS_SELECT_H
#endif
#include <sys/types.h> /* before other system headers */
#if defined(__gnu_linux__)
#undef _SYS_SELECT_H
#endif
#include <sys/ucontext.h> /* before signal.h */
#include <signal.h> /* before unistd.h and time.h */
#include <time.h>
#include <sys/resource.h> /* before sys/time.h */
#include <sys/time.h>
#include <sys/uio.h>
#include <string.h>
#include <sys/syscall.h>
#if defined(__APPLE__)
#include <sys/vm.h> /* before sys/sysctl.h */
#include <sys/attr.h> /* before unistd.h */
#endif
#if defined(__APPLE__) || defined(__FreeBSD__)
#include <sys/sysctl.h>
#endif
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#if defined(__gnu_linux__)
#include <sys/stat.h> /* before fcntl.h */
#include <fcntl.h>
#else
#include <fcntl.h> /* before sys/stat.h */
#include <sys/stat.h>
#endif
#include <sys/file.h>
#include <poll.h>
#if defined(__gnu_linux__)
#define _SYS_SOCKET_H
#include <bits/socket.h> /* before netinet/in.h */
#include <netinet/in.h> /* before sys/socket.h */
#undef _SYS_SOCKET_H
#endif
#include <sys/socket.h> /* before sys/mount.h */
#include <netdb.h>
#if !defined(__gnu_linux__)
#include <netinet/in.h> /* after netdb.h in FreeBSD */
#endif
#include <sys/mount.h>
#if defined(__gnu_linux__)
#include <sys/statfs.h>
#endif
#include <dirent.h>
#include <fnmatch.h>
#include <termios.h>
#if defined(__APPLE__) || defined(__FreeBSD__)
#define _SYS_SOCKIO_H_ /* sys/sockio.h has many bad macros */
#endif
#include <sys/ioctl.h>
#if defined(__APPLE__) || defined(__FreeBSD__)
#undef _SYS_SOCKIO_H_
#endif
#include <pthread.h>
#include <dlfcn.h>
#include <spawn.h>
#if !defined(__gnu_linux__)
#if defined(__FreeBSD__)
#include <stdio.h> /* before wchar.h in FreeBSD */
#endif
#include <wchar.h> /* before iconv.h in FreeBSD, after malloc.h in Linux */
#include <pwd.h>
#include <grp.h>
#endif
#include <iconv.h>
#endif
#if defined(__APPLE__)
#undef _DONT_USE_CTYPE_INLINE_
#undef st_atimespec
#undef st_mtimespec
#undef st_ctimespec
#undef st_atime
#undef st_mtime
#undef st_ctime
#include <malloc/malloc.h>
#include <copyfile.h>
#include <mach/mach_time.h>
#define _ARCHITECTURE_BYTE_ORDER_H_ /* headmaster can not translate some inline functions */
#include <mach-o/dyld.h>
#undef _ARCHITECTURE_BYTE_ORDER_H_
#include <crt_externs.h> /* after mach-o/dyld.h */
#elif defined(__FreeBSD__)
#undef _DONT_USE_CTYPE_INLINE_
#undef d_fileno
#if __FreeBSD__ < 9
#undef st_atimespec
#undef st_mtimespec
#undef st_ctimespec
#endif
#undef st_atime
#undef st_mtime
#undef st_ctime
#include <sys/param.h>
#include <malloc_np.h>
#include <pthread_np.h>
#include <sys/link_elf.h>
#elif defined(__gnu_linux__)
#undef st_atime
#undef st_mtime
#undef st_ctime
#include <sys/sendfile.h>
#include <link.h>
#undef _GNU_SOURCE
#undef __USE_GNU /* avoiding circular dependency between libio.h and stdio.h */
#undef __USE_XOPEN2K8 /* avoiding circular dependency between wchar.h and stdio.h */
#if !__GLIBC_PREREQ(2, 27)
#include <libio.h> /* before stdio.h */
#else
#define _LIBIO_H
#include <bits/libio.h> /* before stdio.h */
#undef _LIBIO_H
#endif
#undef _SC_NPROCESSORS_ONLN
#include <malloc.h>
#include <pwd.h> /* after stdio.h in Linux */
#include <grp.h>
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
#include <winternl.h> /* before wincrypt.h */
#include <wincrypt.h>
#include <ws2tcpip.h>
#undef _S6_un /* false positive warning of gcc */
#undef s6_addr /* use _S6_un */
#define RC_INVOKED /* headmaster can not translate some inline functions */
#include <malloc.h>
#undef RC_INVOKED
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#undef UNICODE
#undef WIN32_LEAN_AND_MEAN
#endif

#include <unwind.h> /* exception mechanism of gcc, after windows.h */

#if defined(__unix__) || defined(__APPLE__)
#include "fix-fcntl.h"
#pragma for Ada "fcntl.h" monolithic_include "fix-fcntl.h"
#include "fix-signal.h"
#pragma for Ada "signal.h" monolithic_include "fix-signal.h"
#include "fix-wchar.h"
#pragma for Ada "wchar.h" monolithic_include "fix-wchar.h"
#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"
#pragma for Ada overload int fcntl(int, int, int)
#pragma for Ada overload int ioctl(int, unsigned long, struct winsize *)
#if !defined(__gnu_linux__)
#pragma for Ada overload int open(const char *, int, mode_t)
#pragma for Ada overload int syscall(int, void *, unsigned int)
#endif
#endif
#if defined(__APPLE__)
#pragma for Ada overload int fcntl(int, int, char *)
#pragma for Ada overload int gettimeofday(struct timeval *, struct timezone *)
#pragma for Ada overload size_t iconv(iconv_t cd, \
	char const **inbuf, size_t *inbytesleft, \
	char **outbuf, size_t *outbytesleft)
#pragma for Ada "dirent.h" include "sys/dirent.h"
#pragma for Ada "errno.h" include "sys/errno.h"
#pragma for Ada "fcntl.h" include "sys/fcntl.h"
#pragma for Ada "poll.h" include "sys/poll.h"
#pragma for Ada "pthread.h" include "signal.h" /* pthread_kill */
#pragma for Ada "pthread.h" include "sys/types.h"
#pragma for Ada "signal.h" include "sys/_structs.h" /* stack_t */
#pragma for Ada "signal.h" include "sys/signal.h"
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1090
#pragma for Ada "signal.h" include "sys/ucontext.h" /* sigset_t */
#endif
#pragma for Ada "sys/file.h" include "sys/fcntl.h"
#pragma for Ada "sys/ioctl.h" include "sys/ttycom.h" /* struct winsize */
#pragma for Ada "sys/stat.h" include "sys/fcntl.h" /* S_IF* */
#pragma for Ada "sys/time.h" include "sys/_structs.h" /* timeval */
#pragma for Ada "sys/ucontext.h" include "sys/_structs.h" /* ucontext_t */
#pragma for Ada "termios.h" include "sys/termios.h"
#pragma for Ada "time.h" include "sys/_structs.h" /* timespec */
#pragma for Ada "unistd.h" include "sys/unistd.h"
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1090
#pragma for Ada "pthread.h" include "sys/_types/_pthread_cond_t.h"
#pragma for Ada "pthread.h" include "sys/_types/_pthread_mutex_t.h"
#pragma for Ada "pthread.h" include "sys/_types/_pthread_rwlock_t.h"
#pragma for Ada "pthread.h" include "sys/_types/_pthread_t.h"
#pragma for Ada "signal.h" include "sys/_types/_sigaltstack.h" /* stack_t */
#pragma for Ada "signal.h" include "sys/_types/_sigset_t.h"
#pragma for Ada "stdint.h" include "sys/_types/_int8_t.h" /* int8_t */
#pragma for Ada "stdint.h" include "sys/_types/_int16_t.h" /* int16_t */
#pragma for Ada "stdint.h" include "sys/_types/_int32_t.h" /* int32_t */
#pragma for Ada "stdint.h" include "sys/_types/_int64_t.h" /* int64_t */
#pragma for Ada "stdint.h" include "_types/_uint8_t.h" /* uint8_t */
#pragma for Ada "stdint.h" include "_types/_uint16_t.h" /* uint16_t */
#pragma for Ada "stdint.h" include "_types/_uint32_t.h" /* uint32_t */
#pragma for Ada "stdint.h" include "_types/_uint64_t.h" /* uint64_t */
#pragma for Ada "stdint.h" include "sys/_types/_intptr_t.h" /* intptr_t */
#pragma for Ada "stdint.h" include "sys/_types/_uintptr_t.h" /* uintptr_t */
#pragma for Ada "stdint.h" include "_types/_intmax_t.h" /* intmax_t */
#pragma for Ada "stdint.h" include "_types/_uintmax_t.h" /* uintmax_t */
#pragma for Ada "sys/socket.h" include "sys/_types/_socklen_t.h" /* socklen_t */
#pragma for Ada "sys/stat.h" include "sys/_types/_s_ifmt.h" /* S_IF* */
#pragma for Ada "sys/time.h" include "sys/_types/_timeval.h"
#pragma for Ada "sys/types.h" include "sys/_types/_gid_t.h"
#pragma for Ada "sys/types.h" include "sys/_types/_mode_t.h"
#pragma for Ada "sys/types.h" include "sys/_types/_ssize_t.h"
#pragma for Ada "sys/types.h" include "sys/_types/_suseconds_t.h"
#pragma for Ada "sys/types.h" include "sys/_types/_off_t.h"
#pragma for Ada "sys/types.h" include "sys/_types/_pid_t.h"
#pragma for Ada "sys/types.h" include "sys/_types/_time_t.h"
#pragma for Ada "sys/types.h" include "sys/_types/_uid_t.h"
#pragma for Ada "sys/ucontext.h" include "sys/_types/_ucontext.h" /* ucontext_t */
#pragma for Ada "sys/uio.h" include "sys/_types/_iovec_t.h"
#pragma for Ada "time.h" include "sys/_types/_timespec.h" /* struct timespec */
#pragma for Ada "unistd.h" include "sys/_types/_seek_set.h" /* SEEK_* */
#endif
#elif defined(__FreeBSD__)
#pragma for Ada "dirent.h" include "sys/dirent.h"
#pragma for Ada "pthread.h" include "signal.h" /* pthread_kill */
#pragma for Ada "pthread.h" include "sys/_pthreadtypes.h"
#pragma for Ada "signal.h" include "sys/select.h" /* sigset_t */
#pragma for Ada "signal.h" include "sys/signal.h"
#pragma for Ada "sys/file.h" include "fcntl.h"
#pragma for Ada "sys/ioccom.h" include "sys/ttycom.h" /* struct winsize */
#pragma for Ada "sys/ioctl.h" include "sys/ioccom.h"
#pragma for Ada "sys/ioctl.h" include "sys/ttycom.h" /* struct winsize */
#pragma for Ada "sys/mman.h" include "sys/types.h" /* mmap */
#pragma for Ada "sys/time.h" include "sys/_timeval.h" /* timeval */
#pragma for Ada "sys/uio.h" include "sys/_iovec.h" /* struct iovec */
#pragma for Ada "time.h" include "sys/timespec.h" /* timespec */
#pragma for Ada "time.h" include "sys/_timespec.h" /* struct timespec */
#pragma for Ada "unistd.h" include "sys/types.h" /* lseek */
#pragma for Ada "unistd.h" include "sys/unistd.h"
#if __FreeBSD__ >= 9
#pragma for Ada "stdint.h" include "sys/_stdint.h"
#pragma for Ada "termios.h" include "sys/_termios.h"
#endif
#elif defined(__gnu_linux__)
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
#pragma for Ada "fcntl.h" include "bits/fcntl-linux.h"
#pragma for Ada "poll.h" include "bits/poll.h"
#pragma for Ada "poll.h" include "sys/poll.h"
#pragma for Ada "pthread.h" include "bits/pthreadtypes.h"
#pragma for Ada "pthread.h" include "bits/sigthread.h"
#pragma for Ada "signal.h" include "bits/sigstack.h" /* MINSIGSTKSZ */
#pragma for Ada "signal.h" monolithic_include "bits/sigaction.h"
#pragma for Ada "signal.h" monolithic_include "bits/signum.h"
#if !__GLIBC_PREREQ(2, 26)
#pragma for Ada "signal.h" include "bits/siginfo.h"
#endif
#pragma for Ada "sys/file.h" include "bits/fcntl-linux.h"
#pragma for Ada "sys/ioctl.h" include "asm-generic/ioctls.h"
#pragma for Ada "sys/ioctl.h" include "bits/ioctl-types.h"
#pragma for Ada "sys/mman.h" include "bits/mman-linux.h"
#pragma for Ada "sys/resource.h" include "bits/resource.h"
#pragma for Ada "sys/socket.h" include "asm-generic/socket.h"
#pragma for Ada "sys/socket.h" include "bits/socket.h"
#pragma for Ada "sys/socket.h" include "bits/socket_type.h"
#pragma for Ada "sys/stat.h" include "bits/stat.h"
#pragma for Ada "sys/statfs.h" include "bits/statfs.h"
#pragma for Ada "sys/syscall.h" include "bits/syscall.h"
#if !__GLIBC_PREREQ(2, 26)
#pragma for Ada "sys/time.h" include "bits/time.h" /* timeval */
#pragma for Ada "sys/types.h" include "bits/types.h" /* __time_t */
#pragma for Ada "sys/uio.h" include "bits/uio.h" /* struct iovec */
#endif
#pragma for Ada "sys/wait.h" include "bits/waitflags.h" /* WNOHANG */
#pragma for Ada "termios.h" include "bits/termios.h"
#pragma for Ada "time.h" include "bits/time.h" /* CLOCK_MONOTONIC */
#pragma for Ada "unistd.h" include "bits/confname.h" /* _SC_NPROCESSORS_ONLN */
#if __GLIBC_PREREQ(2, 26)
#pragma for Ada "signal.h" monolithic_include "bits/signum-generic.h"
#pragma for Ada "signal.h" include "bits/types/siginfo_t.h"
#pragma for Ada "signal.h" include "bits/types/sigset_t.h"
#pragma for Ada "signal.h" include "bits/types/stack_t.h"
#pragma for Ada "stdint.h" include "bits/stdint-intn.h"
#pragma for Ada "stdint.h" include "bits/stdint-uintn.h"
#pragma for Ada "sys/resource.h" include "bits/types/struct_rusage.h"
#pragma for Ada "sys/time.h" include "bits/types/struct_timeval.h"
#pragma for Ada "sys/types.h" include "bits/types/time_t.h"
#pragma for Ada "sys/uio.h" include "bits/types/struct_iovec.h"
#pragma for Ada "time.h" include "bits/types/struct_timespec.h"
#pragma for Ada "time.h" include "bits/types/struct_tm.h"
#endif
#endif

#if defined(__WINNT__)
#include "fix-windef.h"
#pragma for Ada "windef.h" monolithic_include "fix-windef.h"
#pragma instance DWORD "INFINITE" /* winbase.h */
#pragma instance DWORD "CRYPT_VERIFYCONTEXT" /* wincrypt.h */
#pragma instance DWORD "GENERIC_READ" /* winnt.h */
#pragma for Ada "errno.h" include "stddef.h"
#pragma for Ada "stdlib.h" include "malloc.h"
#endif

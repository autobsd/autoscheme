/*  This file is part of the 'AutoScheme' project.
 *  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
 *  SPDX-License-Identifier: BSD-2-Clause
 */
#include <errno.h>
#include "error.h"

const char *error_num_to_msg( int num )
{
    const char *error_string;
    switch( num ) 
    {

#ifdef E2BIG
    case E2BIG : error_string = "argument list is too long"; break;
#endif

#ifdef EACCES
    case EACCES : error_string = "permission denied"; break;
#endif

#ifdef EADDRINUSE
    case EADDRINUSE : error_string = "address is already in use"; break;
#endif

#ifdef EADDRNOTAVAIL
    case EADDRNOTAVAIL : error_string = "cannot assign requested address"; break;
#endif

#ifdef EADV
    case EADV : error_string = "advertise error"; break;
#endif

#ifdef EAFNOSUPPORT
    case EAFNOSUPPORT : error_string = "address family is not supported by protocol family"; break;
#endif

#ifdef EAGAIN
    case EAGAIN : error_string = "resource is temporarily unavailable; try again"; break;
#endif

#ifdef EALREADY
    case EALREADY : error_string = "operation is already in progress (see “Changes to EALREADY,” below)"; break;
#endif

#ifdef EBADE
    case EBADE : error_string = "invalid exchange"; break;
#endif

#ifdef EBADF
    case EBADF : error_string = "bad file descriptor"; break;
#endif

#ifdef EBADFD
    case EBADFD : error_string = "FD is invalid for this operation"; break;
#endif

#ifdef EBADFSYS
    case EBADFSYS : error_string = "corrupted filesystem detected"; break;
#endif

#ifdef EBADMSG
    case EBADMSG : error_string = "bad message (1003.1b-1993)"; break;
#endif

#ifdef EBADR
    case EBADR : error_string = "invalid request descriptor"; break;
#endif

#ifdef EBADRPC
    case EBADRPC : error_string = "RPC struct is bad"; break;
#endif

#ifdef EBADRQC
    case EBADRQC : error_string = "invalid request code"; break;
#endif

#ifdef EBADSLT
    case EBADSLT : error_string = "invalid slot"; break;
#endif

#ifdef EBFONT
    case EBFONT : error_string = "bad font-file format"; break;
#endif

#ifdef EBUSY
    case EBUSY : error_string = "device or resource is busy"; break;
#endif

#ifdef ECANCELED
    case ECANCELED : error_string = "operation canceled (1003.1b-1993)"; break;
#endif

#ifdef ECHILD
    case ECHILD : error_string = "no child processes"; break;
#endif

#ifdef ECHRNG
    case ECHRNG : error_string = "channel number is out of range"; break;
#endif

#ifdef ECOMM
    case ECOMM : error_string = "communication error occurred on send"; break;
#endif

#ifdef ECONNABORTED
    case ECONNABORTED : error_string = "software caused connection to abort"; break;
#endif

#ifdef ECONNREFUSED
    case ECONNREFUSED : error_string = "connection refused"; break;
#endif

#ifdef ECONNRESET
    case ECONNRESET : error_string = "connection reset by peer"; break;
#endif

#ifdef ECTRLTERM
    case ECTRLTERM : error_string = "remap to the controlling terminal"; break;
#endif

#ifdef EDEADLK
    case EDEADLK : error_string = "resource deadlock avoided"; break;
#endif

#ifdef EDEADLOCK
    case EDEADLOCK : error_string = "file locking deadlock"; break;
#endif

#ifdef EDESTADDRREQ
    case EDESTADDRREQ : error_string = "destination address is required"; break;
#endif

#ifdef EDOM
    case EDOM : error_string = "math argument is out of domain for the function"; break;
#endif

#ifdef EDQUOT
    case EDQUOT : error_string = "disk quota exceeded"; break;
#endif

#ifdef EENDIAN
    case EENDIAN : error_string = "endian not supported"; break;
#endif

#ifdef EEXIST
    case EEXIST : error_string = "file exists"; break;
#endif

#ifdef EFAULT
    case EFAULT : error_string = "fad address"; break;
#endif

#ifdef EFPOS
    case EFPOS : error_string = "file positioning error"; break;
#endif

#ifdef EFBIG
    case EFBIG : error_string = "file is too large"; break;
#endif

#ifdef EHOSTDOWN
    case EHOSTDOWN : error_string = "host is down"; break;
#endif

#ifdef EHOSTUNREACH
    case EHOSTUNREACH : error_string = "unable to communicate with remote node"; break;
#endif

#ifdef EIDRM
    case EIDRM : error_string = "identifier removed"; break;
#endif

#ifdef EILSEQ
    case EILSEQ : error_string = "illegal byte sequence"; break;
#endif

#ifdef EINPROGRESS
    case EINPROGRESS : error_string = "operation now in progress"; break;
#endif

#ifdef EINTR
    case EINTR : error_string = "interrupted function call"; break;
#endif

#ifdef EINVAL
    case EINVAL : error_string = "invalid argument"; break;
#endif

#ifdef EIO
    case EIO : error_string = "I/O error"; break;
#endif

#ifdef EISCONN
    case EISCONN : error_string = "socket is already connected"; break;
#endif

#ifdef EISDIR
    case EISDIR : error_string = "is a directory"; break;
#endif

#ifdef EL2HLT
    case EL2HLT : error_string = "Level 2 halted"; break;
#endif

#ifdef EL2NSYNC
    case EL2NSYNC : error_string = "Level 2 not synchronized"; break;
#endif

#ifdef EL3HLT
    case EL3HLT : error_string = "Level 3 halted"; break;
#endif

#ifdef EL3RST
    case EL3RST : error_string = "Level 3 reset"; break;
#endif

#ifdef ELIBACC
    case ELIBACC : error_string = "cannot access shared library"; break;
#endif

#ifdef ELIBBAD
    case ELIBBAD : error_string = "accessing a corrupted shared library"; break;
#endif

#ifdef ELIBEXEC
    case ELIBEXEC : error_string = "attempting to exec a shared library"; break;
#endif

#ifdef ELIBMAX
    case ELIBMAX : error_string = "attempting to link in too many libraries"; break;
#endif

#ifdef ELIBSCN
    case ELIBSCN : error_string = "the .lib section in a.out is corrupted"; break;
#endif

#ifdef ELNRNG
    case ELNRNG : error_string = "link number is out of range"; break;
#endif

#ifdef ELOOP
    case ELOOP : error_string = "too many levels of symbolic links or prefixes"; break;
#endif

#ifdef EMFILE
    case EMFILE : error_string = "too many open files"; break;
#endif

#ifdef EMLINK
    case EMLINK : error_string = "too many links"; break;
#endif

#ifdef EMORE
    case EMORE : error_string = "more to do, send message again"; break;
#endif

#ifdef EMSGSIZE
    case EMSGSIZE : error_string = "inappropriate message buffer length"; break;
#endif

#ifdef EMULTIHOP
    case EMULTIHOP : error_string = "multihop attempted"; break;
#endif

#ifdef ENAMETOOLONG
    case ENAMETOOLONG : error_string = "filename is too long"; break;
#endif

#ifdef ENETDOWN
    case ENETDOWN : error_string = "network is down"; break;
#endif

#ifdef ENETRESET
    case ENETRESET : error_string = "network dropped connection on reset"; break;
#endif

#ifdef ENETUNREACH
    case ENETUNREACH : error_string = "network is unreachable"; break;
#endif

#ifdef ENFILE
    case ENFILE : error_string = "too many open files in the system"; break;
#endif

#ifdef ENOANO
    case ENOANO : error_string = "no anode"; break;
#endif

#ifdef ENOBUFS
    case ENOBUFS : error_string = "no buffer space available"; break;
#endif

#ifdef ENOCSI
    case ENOCSI : error_string = "no CSI structure available"; break;
#endif

#ifdef ENODATA
    case ENODATA : error_string = "no data (for no-delay I/O)"; break;
#endif

#ifdef ENODEV
    case ENODEV : error_string = "no such device"; break;
#endif

#ifdef ENOENT
    case ENOENT : error_string = "no such file or directory"; break;
#endif

#ifdef ENOEXEC
    case ENOEXEC : error_string = "exec format error"; break;
#endif

#ifdef ENOLCK
    case ENOLCK : error_string = "no locks available"; break;
#endif

#ifdef ENOLIC
    case ENOLIC : error_string = "no license available"; break;
#endif

#ifdef ENOLINK
    case ENOLINK : error_string = "the link has been severed"; break;
#endif

#ifdef ENOMEM
    case ENOMEM : error_string = "not enough memory"; break;
#endif

#ifdef ENOMSG
    case ENOMSG : error_string = "no message of desired type"; break;
#endif

#ifdef ENONDP
    case ENONDP : error_string = "need an NDP (8087...) to run"; break;
#endif

#ifdef ENONET
    case ENONET : error_string = "machine is not on the network"; break;
#endif

#ifdef ENOPKG
    case ENOPKG : error_string = "package is not installed"; break;
#endif

#ifdef ENOPROTOOPT
    case ENOPROTOOPT : error_string = "protocol is not available"; break;
#endif

#ifdef ENOREMOTE
    case ENOREMOTE : error_string = "must be done on local machine"; break;
#endif

#ifdef ENOSPC
    case ENOSPC : error_string = "no space left on device"; break;
#endif

#ifdef ENOSR
    case ENOSR : error_string = "out of streams resources"; break;
#endif

#ifdef ENOSTR
    case ENOSTR : error_string = "device is not a stream"; break;
#endif

#ifdef ENOSYS
    case ENOSYS : error_string = "function is not implemented"; break;
#endif

#ifdef ENOTBLK
    case ENOTBLK : error_string = "block device is required"; break;
#endif

#ifdef ENOTCONN
    case ENOTCONN : error_string = "socket is not connected"; break;
#endif

#ifdef ENOTDIR
    case ENOTDIR : error_string = "not a directory"; break;
#endif

#ifdef ENOTEMPTY
    case ENOTEMPTY : error_string = "directory is not empty"; break;
#endif

#ifdef ENOTSOCK
    case ENOTSOCK : error_string = "socket operation on nonsocket"; break;
#endif

#ifdef ENOTSUP
    case ENOTSUP : error_string = "not supported (1003.1b-1993)"; break;
#endif

#ifdef ENOTTY
    case ENOTTY : error_string = "inappropriate I/O control operation"; break;
#endif

#ifdef ENOTUNIQ
    case ENOTUNIQ : error_string = "given name is not unique"; break;
#endif

#ifdef ENXIO
    case ENXIO : error_string = "no such device or address"; break;
#endif

#ifdef EOK
    case EOK : error_string = "no error"; break;
#endif

#ifdef EOPNOTSUPP
    case EOPNOTSUPP : error_string = "operation is not supported"; break;
#endif

#ifdef EOVERFLOW
    case EOVERFLOW : error_string = "value too large to be stored in data type"; break;
#endif

#ifdef EOWNERDEAD
    case EOWNERDEAD : error_string = "fhe owner of a lock died while holding it"; break;
#endif

#ifdef EPERM
    case EPERM : error_string = "operation is not permitted"; break;
#endif

#ifdef EPFNOSUPPORT
    case EPFNOSUPPORT : error_string = "protocol family is not supported"; break;
#endif

#ifdef EPIPE
    case EPIPE : error_string = "broken pipe"; break;
#endif

#ifdef EPROCUNAVAIL
    case EPROCUNAVAIL : error_string = "bad procedure for program"; break;
#endif

#ifdef EPROGMISMATCH
    case EPROGMISMATCH : error_string = "program version wrong"; break;
#endif

#ifdef EPROGUNAVAIL
    case EPROGUNAVAIL : error_string = "RPC program is not available"; break;
#endif

#ifdef EPROTO
    case EPROTO : error_string = "protocol error"; break;
#endif

#ifdef EPROTONOSUPPORT
    case EPROTONOSUPPORT : error_string = "protocol is not supported"; break;
#endif

#ifdef EPROTOTYPE
    case EPROTOTYPE : error_string = "protocol is wrong type for socket"; break;
#endif

#ifdef ERANGE
    case ERANGE : error_string = "result is too large"; break;
#endif

#ifdef EREMCHG
    case EREMCHG : error_string = "remote address changed"; break;
#endif

#ifdef EREMOTE
    case EREMOTE : error_string = "the object is remote"; break;
#endif

#ifdef ERESTART
    case ERESTART : error_string = "restartable system call"; break;
#endif

#ifdef EROFS
    case EROFS : error_string = "read-only filesystem"; break;
#endif

#ifdef ERPCMISMATCH
    case ERPCMISMATCH : error_string = "RPC version is wrong"; break;
#endif

#ifdef ESHUTDOWN
    case ESHUTDOWN : error_string = "cannot send after socket shutdown"; break;
#endif

#ifdef ESOCKTNOSUPPORT
    case ESOCKTNOSUPPORT : error_string = "socket type is not supported"; break;
#endif

#ifdef ESPIPE
    case ESPIPE : error_string = "illegal seek"; break;
#endif

#ifdef ESRCH
    case ESRCH : error_string = "no such process"; break;
#endif

#ifdef ESRMNT
    case ESRMNT : error_string = "server mount error"; break;
#endif

#ifdef ESRVRFAULT
    case ESRVRFAULT : error_string = "the receive side of a message transfer encountered a memory fault accessing the receive/reply buffer."; break;
#endif

#ifdef ESTALE
    case ESTALE : error_string = "Potentially recoverable I/O error"; break;
#endif

#ifdef ESTRPIPE
    case ESTRPIPE : error_string = "if pipe/FIFO, do not sleep in stream head"; break;
#endif

#ifdef ETIME
    case ETIME : error_string = "timer expired"; break;
#endif

#ifdef ETIMEDOUT
    case ETIMEDOUT : error_string = "connection timed out"; break;
#endif

#ifdef ETOOMANYREFS
    case ETOOMANYREFS : error_string = "too many references, cannot splice"; break;
#endif

#ifdef ETXTBSY
    case ETXTBSY : error_string = "text file is busy"; break;
#endif

#ifdef EUNATCH
    case EUNATCH : error_string = "protocol driver is not attached"; break;
#endif

#ifdef EUSERS
    case EUSERS : error_string = "too many users (for UFS)"; break;
#endif

#if defined (EWOULDBLOCK) && EWOULDBLOCK != EAGAIN
    case EWOULDBLOCK : error_string = "operation would block"; break;
#endif

#ifdef EXDEV
    case EXDEV : error_string = "cross-device link"; break;
#endif

#ifdef EXFULL
    case EXFULL : error_string = "exchange full"; break;
#endif


    default : 
	error_string = "unable to complete operation for";
    }
    return error_string;
}

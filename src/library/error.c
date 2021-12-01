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
    case E2BIG :
	error_string = "argument list is too long";
	break;

    case EACCES :
	error_string = "permission denied";
	break;

    case EADDRINUSE :
	error_string = "address is already in use";
	break;

    case EADDRNOTAVAIL :
	error_string = "can't assign requested address";
	break;

/* case EADV :
   error_string = "advertise error";
   break;
*/
    case EAFNOSUPPORT :
	error_string = "address family isn't supported by protocol family";
	break;

/* case EAGAIN :
   error_string = "resource is temporarily unavailable; try again";
   break;
*/
    case EALREADY :
	error_string = "operation is already in progress (see “Changes to EALREADY,” below)";
	break;

/* case EBADE :
   error_string = "invalid exchange";
   break;
*/
    case EBADF :
	error_string = "bad file descriptor";
	break;

/* case EBADFD :
   error_string = "FD is invalid for this operation";
   break;
*/
/* case EBADFSYS :
   error_string = "corrupted filesystem detected";
   break;
*/
    case EBADMSG :
	error_string = "bad message (1003.1b-1993)";
	break;

/* case EBADR :
   error_string = "invalid request descriptor";
   break;
*/
    case EBADRPC :
	error_string = "RPC struct is bad";
	break;

/* case EBADRQC :
   error_string = "invalid request code";
   break;
*/
/* case EBADSLT :
   error_string = "invalid slot";
   break;
*/
/* case EBFONT :
   error_string = "bad font-file format";
   break;
*/
    case EBUSY :
	error_string = "device or resource is busy";
	break;

    case ECANCELED :
	error_string = "operation canceled (1003.1b-1993)";
	break;

    case ECHILD :
	error_string = "no child processes";
	break;

/* case ECHRNG :
   error_string = "channel number is out of range";
   break;
*/
/* case ECOMM :
   error_string = "communication error occurred on send";
   break;
*/
    case ECONNABORTED :
	error_string = "software caused connection to abort";
	break;

    case ECONNREFUSED :
	error_string = "connection refused";
	break;

    case ECONNRESET :
	error_string = "connection reset by peer";
	break;

/* case ECTRLTERM :
   error_string = "remap to the controlling terminal";
   break;
*/
    case EDEADLK :
	error_string = "resource deadlock avoided";
	break;

/* case EDEADLOCK :
   error_string = "file locking deadlock";
   break;
*/
    case EDESTADDRREQ :
	error_string = "destination address is required";
	break;

    case EDOM :
	error_string = "math argument is out of domain for the function";
	break;

    case EDQUOT :
	error_string = "disk quota exceeded";
	break;

/* case EENDIAN :
   error_string = "endian not supported";
   break;
*/
    case EEXIST :
	error_string = "file exists";
	break;

    case EFAULT :
	error_string = "fad address";
	break;

/* case EFPOS :
   error_string = "file positioning error";
   break;
*/
    case EFBIG :
	error_string = "file is too large";
	break;

    case EHOSTDOWN :
	error_string = "host is down";
	break;

    case EHOSTUNREACH :
	error_string = "unable to communicate with remote node";
	break;

    case EIDRM :
	error_string = "identifier removed";
	break;

    case EILSEQ :
	error_string = "illegal byte sequence";
	break;

    case EINPROGRESS :
	error_string = "operation now in progress";
	break;

    case EINTR :
	error_string = "interrupted function call";
	break;

    case EINVAL :
	error_string = "invalid argument";
	break;

    case EIO :
	error_string = "I/O error";
	break;

    case EISCONN :
	error_string = "socket is already connected";
	break;

    case EISDIR :
	error_string = "is a directory";
	break;

/* case EL2HLT :
   error_string = "Level 2 halted";
   break;
*/
/* case EL2NSYNC :
   error_string = "Level 2 not synchronized";
   break;
*/
/* case EL3HLT :
   error_string = "Level 3 halted";
   break;
*/
/* case EL3RST :
   error_string = "Level 3 reset";
   break;
*/
/* case ELIBACC :
   error_string = "can't access shared library";
   break;
*/
/* case ELIBBAD :
   error_string = "accessing a corrupted shared library";
   break;
*/
/* case ELIBEXEC :
   error_string = "attempting to exec a shared library";
   break;
*/
/* case ELIBMAX :
   error_string = "attempting to link in too many libraries";
   break;
*/
/* case ELIBSCN :
   error_string = "the .lib section in a.out is corrupted";
   break;
*/
/* case ELNRNG :
   error_string = "link number is out of range";
   break;
*/
    case ELOOP :
	error_string = "too many levels of symbolic links or prefixes";
	break;

    case EMFILE :
	error_string = "too many open files";
	break;

    case EMLINK :
	error_string = "too many links";
	break;

/* case EMORE :
   error_string = "more to do, send message again";
   break;
*/
    case EMSGSIZE :
	error_string = "inappropriate message buffer length";
	break;

    case EMULTIHOP :
	error_string = "multihop attempted";
	break;

    case ENAMETOOLONG :
	error_string = "filename is too long";
	break;

    case ENETDOWN :
	error_string = "network is down";
	break;

    case ENETRESET :
	error_string = "network dropped connection on reset";
	break;

    case ENETUNREACH :
	error_string = "network is unreachable";
	break;

    case ENFILE :
	error_string = "too many open files in the system";
	break;

/* case ENOANO :
   error_string = "no anode";
   break;
*/
    case ENOBUFS :
	error_string = "no buffer space available";
	break;

/* case ENOCSI :
   error_string = "no CSI structure available";
   break;
*/
    case ENODATA :
	error_string = "no data (for no-delay I/O)";
	break;

    case ENODEV :
	error_string = "no such device";
	break;

    case ENOENT :
	error_string = "no such file or directory";
	break;

    case ENOEXEC :
	error_string = "exec format error";
	break;

    case ENOLCK :
	error_string = "no locks available";
	break;

/* case ENOLIC :
   error_string = "no license available";
   break;
*/
    case ENOLINK :
	error_string = "the link has been severed";
	break;

    case ENOMEM :
	error_string = "not enough memory";
	break;

    case ENOMSG :
	error_string = "no message of desired type";
	break;

/* case ENONDP :
   error_string = "need an NDP (8087...) to run";
   break;
*/
/* case ENONET :
   error_string = "machine isn't on the network";
   break;
*/
/* case ENOPKG :
   error_string = "package isn't installed";
   break;
*/
    case ENOPROTOOPT :
	error_string = "protocol isn't available";
	break;

/* case ENOREMOTE :
   error_string = "must be done on local machine";
   break;
*/
    case ENOSPC :
	error_string = "no space left on device";
	break;

    case ENOSR :
	error_string = "out of streams resources";
	break;

    case ENOSTR :
	error_string = "device isn't a stream";
	break;

    case ENOSYS :
	error_string = "function isn't implemented";
	break;

    case ENOTBLK :
	error_string = "block device is required";
	break;

    case ENOTCONN :
	error_string = "socket isn't connected";
	break;

    case ENOTDIR :
	error_string = "not a directory";
	break;

    case ENOTEMPTY :
	error_string = "directory isn't empty";
	break;

    case ENOTSOCK :
	error_string = "socket operation on nonsocket";
	break;

    case ENOTSUP :
	error_string = "not supported (1003.1b-1993)";
	break;

    case ENOTTY :
	error_string = "inappropriate I/O control operation";
	break;

/* case ENOTUNIQ :
   error_string = "given name isn't unique";
   break;
*/
    case ENXIO :
	error_string = "no such device or address";
	break;

/* case EOK :
   error_string = "no error";
   break;
*/
    case EOPNOTSUPP :
	error_string = "operation isn't supported";
	break;

    case EOVERFLOW :
	error_string = "value too large to be stored in data type";
	break;

    case EOWNERDEAD :
	error_string = "fhe owner of a lock died while holding it";
	break;

    case EPERM :
	error_string = "operation isn't permitted";
	break;

    case EPFNOSUPPORT :
	error_string = "protocol family isn't supported";
	break;

    case EPIPE :
	error_string = "broken pipe";
	break;

    case EPROCUNAVAIL :
	error_string = "bad procedure for program";
	break;

    case EPROGMISMATCH :
	error_string = "program version wrong";
	break;

    case EPROGUNAVAIL :
	error_string = "RPC program isn't available";
	break;

    case EPROTO :
	error_string = "protocol error";
	break;

    case EPROTONOSUPPORT :
	error_string = "protocol isn't supported";
	break;

    case EPROTOTYPE :
	error_string = "protocol is wrong type for socket";
	break;

    case ERANGE :
	error_string = "result is too large";
	break;

/* case EREMCHG :
   error_string = "remote address changed";
   break;
*/
    case EREMOTE :
	error_string = "the object is remote";
	break;

/* case ERESTART :
   error_string = "restartable system call";
   break;
*/
    case EROFS :
	error_string = "read-only filesystem";
	break;

    case ERPCMISMATCH :
	error_string = "RPC version is wrong";
	break;

    case ESHUTDOWN :
	error_string = "can't send after socket shutdown";
	break;

    case ESOCKTNOSUPPORT :
	error_string = "socket type isn't supported";
	break;

    case ESPIPE :
	error_string = "illegal seek";
	break;

    case ESRCH :
	error_string = "no such process";
	break;

/* case ESRMNT :
   error_string = "server mount error";
   break;
*/
/* case ESRVRFAULT :
   error_string = "the receive side of a message transfer encountered a memory fault accessing the receive/reply buffer.";
   break;
*/
    case ESTALE :
	error_string = "Potentially recoverable I/O error";
	break;

/* case ESTRPIPE :
   error_string = "if pipe/FIFO, don't sleep in stream head";
   break;
*/
    case ETIME :
	error_string = "timer expired";
	break;

    case ETIMEDOUT :
	error_string = "connection timed out";
	break;

    case ETOOMANYREFS :
	error_string = "too many references: can't splice";
	break;

    case ETXTBSY : error_string = "text file is busy";
	break;

/* case EUNATCH :
   error_string = "protocol driver isn't attached";
   break;
*/
    case EUSERS :
	error_string = "too many users (for UFS)";
	break;

    case EWOULDBLOCK :
	error_string = "operation would block";
	break;

    case EXDEV :
	error_string = "cross-device link";
	break;

/* case EXFULL :
   error_string = "exchange full";
   break;
*/


	/* case EACCES : */
	/* 	error_string = "permission denied to access"; */
	/* 	break;  */
	
	/* case EBUSY : */
	/* 	error_string = "file currently in use"; */
	/* 	break; */

	/* case EEXIST : */
	/* 	error_string = "file already exists"; */
	/* 	break; */

	/* case ENOENT : */
	/* 	error_string = "file does not exist"; */
	/* 	break; */

	/* case EROFS : */
	/* 	error_string = "parent directory resides on read-only file system"; */
	/* 	break; */
	
	/* case EBADF : */
	/* 	error_string = "invalid file descriptor for"; */
	/* 	break;  */

	/* case EMFILE : */
	/* case ENFILE : */
	/* 	error_string = "open file descriptor limit exceeded"; */
	/* 	args = NIL; */
	/* 	break; */

	/* case ENOMEM : */
	/* 	error_string = "insufficient memory to open"; */
	/* 	break; */

	/* case ENOTDIR : */
	/* 	error_string = "file is not a directory"; */
	/* 	break; */



    default : 
	error_string = "unable to complete operation for";
    }
    return error_string;
}

/*
 * MinGW-w64's rpcdce.h, which TinyCC's headers lack, for ld's ldbuildid.c:
 * --build-id=uuid calls rpcrt4.dll's UuidCreate through a pointer whose type
 * ldbuildid.c writes from these names.  It includes this header after
 * windows.h, which in MinGW-w64 has already included rpc.h
 * (mingw-w64-headers/include/windows.h:88), so what it reads of both is
 * here: UUID (rpcdce.h:40-44, declared by TinyCC's guiddef.h), RPC_STATUS,
 * whose __LONG32 is long (rpc.h:49 and crt/_mingw.h.in:24), RPC_ENTRY for a
 * target that is not ARM (rpc.h:53-60), and RPC_S_OK (rpcnterr.h:9).
 */
#ifndef __RPCDCE_H__
#define __RPCDCE_H__

#include <guiddef.h>

typedef long RPC_STATUS;

#define __RPC_API __stdcall
#define RPC_ENTRY __RPC_API

#define RPC_S_OK ERROR_SUCCESS

#endif

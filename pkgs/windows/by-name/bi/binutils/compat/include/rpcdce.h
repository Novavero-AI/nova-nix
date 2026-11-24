/*
 * What MinGW-w64's rpcdce.h declares for ld's build IDs, which TinyCC's
 * headers lack: --build-id=uuid calls rpcrt4.dll's UuidCreate through a
 * pointer whose type ld writes from these names.  ld includes this header
 * after windows.h, which in MinGW-w64's headers has already included rpc.h,
 * so what it reads of both is here: UUID, which TinyCC's guiddef.h
 * declares, RPC_STATUS as a long, RPC_ENTRY for a target that is not ARM,
 * and RPC_S_OK, each as MinGW-w64's headers define it.
 */
#ifndef __RPCDCE_H__
#define __RPCDCE_H__

#include <guiddef.h>

typedef long RPC_STATUS;

#define __RPC_API __stdcall
#define RPC_ENTRY __RPC_API

#define RPC_S_OK ERROR_SUCCESS

#endif

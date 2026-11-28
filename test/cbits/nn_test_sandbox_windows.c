/*
 * The Windows calls the sandbox helper's Windows tests make
 * (test/Test/WindowsSandbox.hs).  As SYSTEM, standing in for the service
 * that holds each build slot's token: the build accounts the tests make
 * and remove, the window station they hold, and the directories they lay
 * out.  In a builder, run as a slot's account: what the builder is, and
 * what it may reach.  Each answers 0 or the Win32 error code of the call
 * that failed.  The suite compiles this file on every OS; off Windows
 * each call answers NOT_ON_WINDOWS, and the tests never make one there.
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

/* The error each call answers off Windows (ERROR_CALL_NOT_IMPLEMENTED's
 * value). */
#define NOT_ON_WINDOWS 120UL

#if defined(_WIN32)

#include <windows.h>
#include <bcrypt.h>
#include <lmaccess.h>
#include <lmerr.h>
#include <ntsecapi.h>
#include <sddl.h>

/* The right a build account's batch logon needs, which ntsecapi.h spells
 * narrow without UNICODE. */
#define BATCH_LOGON_RIGHT L"SeBatchLogonRight"

/* A password: an upper-case letter, a lower-case letter, a digit and a
 * symbol, which the password complexity policy asks one to hold, then
 * random letters and digits. */
#define PASSWORD_PREFIX L"Aa1!"
#define PASSWORD_RANDOM_CHARS 24

/* The room a domain name and a window station's or desktop's name are
 * given. */
#define NAME_CHARS 256

/* A code a builder's crash raises: a customer code (bit 29), with the
 * severity of an error, which no runtime the test suite links handles. */
#define CRASH_EXCEPTION_CODE 0xE04E4E01UL

/* Fill PASSWORD, PASSWORD_PREFIX and PASSWORD_RANDOM_CHARS random
 * letters and digits and a NUL. */
static unsigned long make_password(WCHAR *password)
{
    static const WCHAR alphabet[] = L"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    size_t prefix = wcslen(PASSWORD_PREFIX);
    unsigned char bytes[PASSWORD_RANDOM_CHARS];
    size_t i;
    if (BCryptGenRandom(NULL, bytes, sizeof bytes, BCRYPT_USE_SYSTEM_PREFERRED_RNG) != 0)
        return ERROR_GEN_FAILURE;
    memcpy(password, PASSWORD_PREFIX, prefix * sizeof *password);
    for (i = 0; i < PASSWORD_RANDOM_CHARS; i++)
        password[prefix + i] = alphabet[bytes[i] % (sizeof alphabet / sizeof alphabet[0] - 1)];
    password[prefix + PASSWORD_RANDOM_CHARS] = L'\0';
    return 0;
}

/* The local security policy, opened to look accounts up and grant them
 * rights. */
static unsigned long open_policy(LSA_HANDLE *policy)
{
    LSA_OBJECT_ATTRIBUTES attributes;
    memset(&attributes, 0, sizeof attributes);
    return LsaNtStatusToWinError(LsaOpenPolicy(NULL, &attributes, POLICY_CREATE_ACCOUNT | POLICY_LOOKUP_NAMES, policy));
}

/* NAME's SID, in SID, SECURITY_MAX_SID_SIZE bytes. */
static unsigned long account_sid(const wchar_t *name, PSID sid)
{
    DWORD sid_size = SECURITY_MAX_SID_SIZE;
    WCHAR domain[NAME_CHARS];
    DWORD domain_size = NAME_CHARS;
    SID_NAME_USE use;
    return LookupAccountNameW(NULL, name, sid, &sid_size, domain, &domain_size, &use) ? 0 : GetLastError();
}

/* Make the local account NAME, a member of Users alone, with a password
 * generated for it; grant it the batch logon right; and log it on as a
 * batch job, as the service logs each slot's account on.  *SID gets its
 * SID's string, for LocalFree, and *TOKEN its primary token, inheritable,
 * so a helper this process starts holds it.  A failure names its call in
 * *FAILED; what was made by then, remove_account removes. */
unsigned long nova_test_account_make(const wchar_t *name, wchar_t **sid, void **token, const char **failed)
{
    WCHAR password[sizeof PASSWORD_PREFIX / sizeof(WCHAR) + PASSWORD_RANDOM_CHARS];
    BYTE sid_bytes[SECURITY_MAX_SID_SIZE];
    USER_INFO_1 user;
    LSA_UNICODE_STRING right;
    LSA_HANDLE policy = NULL;
    HANDLE logged = NULL;
    DWORD bad_field = 0;
    unsigned long error;

    *sid = NULL;
    *token = NULL;
    *failed = NULL;
    if ((error = make_password(password)) != 0) {
        *failed = "BCryptGenRandom";
        return error;
    }
    memset(&user, 0, sizeof user);
    user.usri1_name = (LPWSTR)name;
    user.usri1_password = password;
    user.usri1_priv = USER_PRIV_USER;
    user.usri1_flags = UF_SCRIPT | UF_DONT_EXPIRE_PASSWD;
    if ((error = NetUserAdd(NULL, 1, (LPBYTE)&user, &bad_field)) != NERR_Success) {
        *failed = "NetUserAdd";
        goto done;
    }
    if ((error = account_sid(name, sid_bytes)) != 0) {
        *failed = "LookupAccountNameW";
        goto done;
    }
    if ((error = open_policy(&policy)) != 0) {
        *failed = "LsaOpenPolicy";
        policy = NULL;
        goto done;
    }
    right.Buffer = BATCH_LOGON_RIGHT;
    right.Length = (USHORT)(wcslen(BATCH_LOGON_RIGHT) * sizeof(WCHAR));
    right.MaximumLength = (USHORT)(right.Length + sizeof(WCHAR));
    if ((error = LsaNtStatusToWinError(LsaAddAccountRights(policy, sid_bytes, &right, 1))) != 0) {
        *failed = "LsaAddAccountRights";
        goto done;
    }
    if (!LogonUserW(name, L".", password, LOGON32_LOGON_BATCH, LOGON32_PROVIDER_DEFAULT, &logged)) {
        error = GetLastError();
        *failed = "LogonUserW";
        goto done;
    }
    if (!SetHandleInformation(logged, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT)) {
        error = GetLastError();
        *failed = "SetHandleInformation";
        CloseHandle(logged);
        goto done;
    }
    if (!ConvertSidToStringSidW(sid_bytes, sid)) {
        error = GetLastError();
        *failed = "ConvertSidToStringSidW";
        *sid = NULL;
        CloseHandle(logged);
        goto done;
    }
    *token = logged;
done:
    SecureZeroMemory(password, sizeof password);
    if (policy != NULL)
        LsaClose(policy);
    return error;
}

/* Remove the local account NAME, its rights first, while its name still
 * finds its SID; an account or a right already gone is no failure.  A
 * failure names its call in *FAILED. */
unsigned long nova_test_account_remove(const wchar_t *name, const char **failed)
{
    BYTE sid_bytes[SECURITY_MAX_SID_SIZE];
    LSA_HANDLE policy = NULL;
    unsigned long error;
    *failed = NULL;
    error = account_sid(name, sid_bytes);
    if (error == ERROR_NONE_MAPPED)
        return 0;
    if (error != 0) {
        *failed = "LookupAccountNameW";
        return error;
    }
    if ((error = open_policy(&policy)) != 0) {
        *failed = "LsaOpenPolicy";
        return error;
    }
    /* An account holding no right has none to remove. */
    error = LsaNtStatusToWinError(LsaRemoveAccountRights(policy, sid_bytes, TRUE, NULL, 0));
    LsaClose(policy);
    if (error != 0 && error != ERROR_FILE_NOT_FOUND) {
        *failed = "LsaRemoveAccountRights";
        return error;
    }
    error = NetUserDel(NULL, name);
    if (error != NERR_Success && error != NERR_UserNotFound) {
        *failed = "NetUserDel";
        return error;
    }
    return 0;
}

unsigned long nova_test_close_handle(void *handle)
{
    return CloseHandle(handle) ? 0 : GetLastError();
}

/* Release what a call here allocated for LocalFree: a SID's or a
 * descriptor's string. */
void nova_test_local_free(wchar_t *text)
{
    LocalFree(text);
}

/* Make the window station NAME, with the default DACL of this process's
 * token, and hold it in *STATION, for a test that a build's name already
 * taken is refused. */
unsigned long nova_test_station_hold(const wchar_t *name, void **station)
{
    HWINSTA made = CreateWindowStationW(name, CWF_CREATE_ONLY, WINSTA_ALL_ACCESS, NULL);
    *station = made;
    return made != NULL ? 0 : GetLastError();
}

unsigned long nova_test_station_release(void *station)
{
    return CloseWindowStation(station) ? 0 : GetLastError();
}

/* Make the directory PATH with the security descriptor SDDL describes. */
unsigned long nova_test_directory_with_sddl(const wchar_t *path, const wchar_t *sddl)
{
    PSECURITY_DESCRIPTOR descriptor = NULL;
    SECURITY_ATTRIBUTES attributes;
    unsigned long error = 0;
    if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(sddl, SDDL_REVISION_1, &descriptor, NULL))
        return GetLastError();
    attributes.nLength = sizeof attributes;
    attributes.lpSecurityDescriptor = descriptor;
    attributes.bInheritHandle = FALSE;
    if (!CreateDirectoryW(path, &attributes))
        error = GetLastError();
    LocalFree(descriptor);
    return error;
}

/* Whether the process PID is running: 1 when it is, 0 when it has ended
 * or no process has the ID, and -1 when it cannot be told. */
int nova_test_process_running(unsigned long pid)
{
    HANDLE process = OpenProcess(SYNCHRONIZE, FALSE, pid);
    DWORD waited;
    if (process == NULL)
        return GetLastError() == ERROR_INVALID_PARAMETER ? 0 : -1;
    waited = WaitForSingleObject(process, 0);
    CloseHandle(process);
    if (waited == WAIT_TIMEOUT)
        return 1;
    return waited == WAIT_OBJECT_0 ? 0 : -1;
}

/* The SID of the user this process runs as, its token's user, for
 * LocalFree. */
unsigned long nova_test_token_user(wchar_t **sid)
{
    union {
        TOKEN_USER user;
        BYTE bytes[sizeof(TOKEN_USER) + SECURITY_MAX_SID_SIZE];
    } buffer;
    HANDLE token = NULL;
    DWORD size = 0;
    unsigned long error = 0;
    *sid = NULL;
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token))
        return GetLastError();
    if (!GetTokenInformation(token, TokenUser, &buffer, sizeof buffer, &size)
        || !ConvertSidToStringSidW(buffer.user.User.Sid, sid))
        error = GetLastError();
    CloseHandle(token);
    return error;
}

/* The user object WHICH names, this process's window station for 0 and
 * its thread's desktop for anything else.  Neither handle is closed. */
static HANDLE user_object(int which)
{
    return which == 0 ? (HANDLE)GetProcessWindowStation() : (HANDLE)GetThreadDesktop(GetCurrentThreadId());
}

/* The name of this process's window station or desktop, as WHICH says,
 * in NAME, CAPACITY characters. */
unsigned long nova_test_user_object_name(int which, wchar_t *name, unsigned long capacity)
{
    DWORD needed = 0;
    return GetUserObjectInformationW(user_object(which), UOI_NAME, name, capacity * sizeof *name, &needed)
               ? 0
               : GetLastError();
}

/* The DACL of this process's window station or desktop, as WHICH says, in
 * SDDL, for LocalFree. */
unsigned long nova_test_user_object_dacl(int which, wchar_t **sddl)
{
    SECURITY_INFORMATION wanted = DACL_SECURITY_INFORMATION;
    PSECURITY_DESCRIPTOR descriptor;
    DWORD needed = 0;
    unsigned long error = 0;
    *sddl = NULL;
    GetUserObjectSecurity(user_object(which), &wanted, NULL, 0, &needed);
    if (needed == 0)
        return GetLastError();
    descriptor = LocalAlloc(LMEM_FIXED, needed);
    if (descriptor == NULL)
        return GetLastError();
    if (!GetUserObjectSecurity(user_object(which), &wanted, descriptor, needed, &needed)
        || !ConvertSecurityDescriptorToStringSecurityDescriptorW(descriptor, SDDL_REVISION_1, DACL_SECURITY_INFORMATION,
                                                                 sddl, NULL))
        error = GetLastError();
    LocalFree(descriptor);
    return error;
}

/* Whether this process has a console to write: CONOUT$ opens, which
 * libiberty asks of gcc's console before it lets gcc's subprograms share
 * it. */
unsigned long nova_test_console(void)
{
    HANDLE console = CreateFileW(L"CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
    if (console == INVALID_HANDLE_VALUE)
        return GetLastError();
    CloseHandle(console);
    return 0;
}

/* Commit BYTES of memory and release them. */
unsigned long nova_test_commit(unsigned long long bytes)
{
    void *block = VirtualAlloc(NULL, (SIZE_T)bytes, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (block == NULL)
        return GetLastError();
    VirtualFree(block, 0, MEM_RELEASE);
    return 0;
}

/* Start COMMAND with CreateProcessW's FLAGS, inheriting no handle, so it
 * shares this process's console; *PID gets its ID and *PROCESS its handle,
 * for nova_test_wait. */
unsigned long nova_test_start(const wchar_t *command, unsigned long flags, unsigned long *pid, void **process)
{
    STARTUPINFOW startup;
    PROCESS_INFORMATION started;
    size_t length = wcslen(command) + 1;
    WCHAR *line = malloc(length * sizeof *line);
    unsigned long error = 0;
    *pid = 0;
    *process = NULL;
    if (line == NULL)
        return ERROR_NOT_ENOUGH_MEMORY;
    memcpy(line, command, length * sizeof *line);
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    if (CreateProcessW(NULL, line, NULL, NULL, FALSE, flags, NULL, NULL, &startup, &started)) {
        CloseHandle(started.hThread);
        *pid = started.dwProcessId;
        *process = started.hProcess;
    } else {
        error = GetLastError();
    }
    free(line);
    return error;
}

/* Wait for PROCESS, from nova_test_start, to end, put its exit code in
 * *CODE and close it. */
unsigned long nova_test_wait(void *process, unsigned long *code)
{
    DWORD exit_code = 0;
    unsigned long error = 0;
    if (WaitForSingleObject(process, INFINITE) != WAIT_OBJECT_0 || !GetExitCodeProcess(process, &exit_code))
        error = GetLastError();
    *code = exit_code;
    CloseHandle(process);
    return error;
}

/* Raise CRASH_EXCEPTION_CODE with no handler left to take it: the test
 * suite's runtime installs a filter for unhandled exceptions, which this
 * takes out first, so the crash ends the process as a crash ends one with
 * none. */
void nova_test_crash(void)
{
    SetUnhandledExceptionFilter(NULL);
    RaiseException(CRASH_EXCEPTION_CODE, EXCEPTION_NONCONTINUABLE, 0, NULL);
}

/* Open PATH with ACCESS, a file or a directory, and close it. */
unsigned long nova_test_open(const wchar_t *path, unsigned long access)
{
    HANDLE file = CreateFileW(path, access, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                              OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (file == INVALID_HANDLE_VALUE)
        return GetLastError();
    CloseHandle(file);
    return 0;
}

/* Create the file PATH, which must not exist. */
unsigned long nova_test_create(const wchar_t *path)
{
    HANDLE file = CreateFileW(path, GENERIC_WRITE, 0, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE)
        return GetLastError();
    CloseHandle(file);
    return 0;
}

unsigned long nova_test_delete(const wchar_t *path)
{
    return DeleteFileW(path) ? 0 : GetLastError();
}

/* List the first entry PATTERN, such as DIRECTORY\*, finds. */
unsigned long nova_test_list(const wchar_t *pattern)
{
    WIN32_FIND_DATAW found;
    HANDLE search = FindFirstFileW(pattern, &found);
    if (search == INVALID_HANDLE_VALUE)
        return GetLastError();
    FindClose(search);
    return 0;
}

/* Open the key PATH beneath HKEY_USERS to read it. */
unsigned long nova_test_open_key(const wchar_t *path)
{
    HKEY key;
    LSTATUS status = RegOpenKeyExW(HKEY_USERS, path, 0, KEY_READ, &key);
    if (status != ERROR_SUCCESS)
        return (unsigned long)status;
    RegCloseKey(key);
    return 0;
}

/* Open the process PID to read its memory. */
unsigned long nova_test_open_process(unsigned long pid)
{
    HANDLE process = OpenProcess(PROCESS_VM_READ, FALSE, pid);
    if (process == NULL)
        return GetLastError();
    CloseHandle(process);
    return 0;
}

#else

unsigned long nova_test_account_make(const wchar_t *name, wchar_t **sid, void **token, const char **failed)
{
    (void)name;
    *sid = NULL;
    *token = NULL;
    *failed = "nova_test_account_make";
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_account_remove(const wchar_t *name, const char **failed)
{
    (void)name;
    *failed = "nova_test_account_remove";
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_close_handle(void *handle)
{
    (void)handle;
    return NOT_ON_WINDOWS;
}

void nova_test_local_free(wchar_t *text)
{
    (void)text;
}

unsigned long nova_test_station_hold(const wchar_t *name, void **station)
{
    (void)name;
    *station = NULL;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_station_release(void *station)
{
    (void)station;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_directory_with_sddl(const wchar_t *path, const wchar_t *sddl)
{
    (void)path;
    (void)sddl;
    return NOT_ON_WINDOWS;
}

int nova_test_process_running(unsigned long pid)
{
    (void)pid;
    return -1;
}

unsigned long nova_test_token_user(wchar_t **sid)
{
    *sid = NULL;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_user_object_name(int which, wchar_t *name, unsigned long capacity)
{
    (void)which;
    (void)name;
    (void)capacity;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_user_object_dacl(int which, wchar_t **sddl)
{
    (void)which;
    *sddl = NULL;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_console(void)
{
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_commit(unsigned long long bytes)
{
    (void)bytes;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_start(const wchar_t *command, unsigned long flags, unsigned long *pid, void **process)
{
    (void)command;
    (void)flags;
    *pid = 0;
    *process = NULL;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_wait(void *process, unsigned long *code)
{
    (void)process;
    *code = 0;
    return NOT_ON_WINDOWS;
}

void nova_test_crash(void)
{
}

unsigned long nova_test_open(const wchar_t *path, unsigned long access)
{
    (void)path;
    (void)access;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_create(const wchar_t *path)
{
    (void)path;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_delete(const wchar_t *path)
{
    (void)path;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_list(const wchar_t *pattern)
{
    (void)pattern;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_open_key(const wchar_t *path)
{
    (void)path;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_open_process(unsigned long pid)
{
    (void)pid;
    return NOT_ON_WINDOWS;
}

#endif

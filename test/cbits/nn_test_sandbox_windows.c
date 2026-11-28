/*
 * The Windows calls the sandbox helper's Windows tests make
 * (test/Test/WindowsSandbox.hs).  As an administrator: the build accounts
 * and the test service the tests make and remove.  As the test service,
 * which stands in for the service that logs each build slot on: the
 * service's own dispatch, the slots' logons, the directories and files
 * the tests lay out, and a process started as a slot.  In a builder, run
 * as a slot's account: what the builder is, and what it may reach.  Each
 * answers 0 or the Win32 error code of the call that failed.  The suite
 * compiles this file on every OS; off Windows each call answers
 * NOT_ON_WINDOWS, and the tests never make one there.
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
#include <aclapi.h>
#include <bcrypt.h>
#include <tlhelp32.h>
#include <lmaccess.h>
#include <lmerr.h>
#include <ntsecapi.h>
#include <sddl.h>

/* The right a build account's batch logon needs, and the privileges the
 * service that starts builds as another account needs, which ntsecapi.h
 * and winnt.h spell narrow without UNICODE. */
#define BATCH_LOGON_RIGHT L"SeBatchLogonRight"
static const WCHAR *const service_privileges[] = {L"SeAssignPrimaryTokenPrivilege", L"SeIncreaseQuotaPrivilege"};
#define SERVICE_PRIVILEGE_COUNT (sizeof service_privileges / sizeof service_privileges[0])

/* The account a service runs as when it runs as its virtual account:
 * this prefix, then the service's name. */
#define VIRTUAL_ACCOUNT_PREFIX L"NT SERVICE\\"

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

/* The exit code a process the test service's stop ends is given. */
#define STOPPED_EXIT_CODE ERROR_PROCESS_ABORTED

/* A token's user: a TOKEN_USER and the SID it points at, which follows it
 * in the buffer and is at most SECURITY_MAX_SID_SIZE bytes, so one call
 * fills it.  The union aligns the buffer for the structure. */
union token_user {
    TOKEN_USER user;
    BYTE bytes[sizeof(TOKEN_USER) + SECURITY_MAX_SID_SIZE];
};

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

/* An LSA string for TEXT, which it points into. */
static LSA_UNICODE_STRING lsa_string(const WCHAR *text)
{
    LSA_UNICODE_STRING string;
    string.Buffer = (PWSTR)text;
    string.Length = (USHORT)(wcslen(text) * sizeof(WCHAR));
    string.MaximumLength = (USHORT)(string.Length + sizeof(WCHAR));
    return string;
}

/* Make the local account NAME, a member of Users alone, with a password
 * generated for it, and grant it the batch logon right, as the service's
 * install makes each slot's account.  *PASSWORD gets the password, for
 * LocalFree, which the test service logs the account on with.  A failure
 * names its call in *FAILED; what was made by then,
 * nova_test_account_remove removes. */
unsigned long nova_test_account_make(const wchar_t *name, wchar_t **password, const char **failed)
{
    WCHAR generated[sizeof PASSWORD_PREFIX / sizeof(WCHAR) + PASSWORD_RANDOM_CHARS];
    BYTE sid_bytes[SECURITY_MAX_SID_SIZE];
    USER_INFO_1 user;
    LSA_UNICODE_STRING right = lsa_string(BATCH_LOGON_RIGHT);
    LSA_HANDLE policy = NULL;
    DWORD bad_field = 0;
    unsigned long error;

    *password = NULL;
    *failed = NULL;
    if ((error = make_password(generated)) != 0) {
        *failed = "BCryptGenRandom";
        return error;
    }
    memset(&user, 0, sizeof user);
    user.usri1_name = (LPWSTR)name;
    user.usri1_password = generated;
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
    if ((error = LsaNtStatusToWinError(LsaAddAccountRights(policy, sid_bytes, &right, 1))) != 0) {
        *failed = "LsaAddAccountRights";
        goto done;
    }
    *password = LocalAlloc(LMEM_FIXED, sizeof generated);
    if (*password == NULL) {
        error = GetLastError();
        *failed = "LocalAlloc";
        goto done;
    }
    memcpy(*password, generated, sizeof generated);
done:
    SecureZeroMemory(generated, sizeof generated);
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

/* The virtual account the service NAME runs as, NT SERVICE\NAME, for
 * free. */
static WCHAR *virtual_account(const wchar_t *name)
{
    size_t prefix = wcslen(VIRTUAL_ACCOUNT_PREFIX);
    size_t length = wcslen(name) + 1;
    WCHAR *account = malloc((prefix + length) * sizeof *account);
    if (account == NULL)
        return NULL;
    memcpy(account, VIRTUAL_ACCOUNT_PREFIX, prefix * sizeof *account);
    memcpy(account + prefix, name, length * sizeof *account);
    return account;
}

/* Make the service NAME, which COMMAND starts and which runs as its
 * virtual account, and grant that account the privileges that start a
 * process as another account, as the service's install does.  *SID gets
 * the account's SID's string, for LocalFree.  A failure names its call in
 * *FAILED; what was made by then, nova_test_service_remove removes. */
unsigned long nova_test_service_install(const wchar_t *name, const wchar_t *command, wchar_t **sid,
                                        const char **failed)
{
    WCHAR *account = virtual_account(name);
    SC_HANDLE manager = NULL;
    SC_HANDLE service = NULL;
    BYTE sid_bytes[SECURITY_MAX_SID_SIZE];
    LSA_HANDLE policy = NULL;
    LSA_UNICODE_STRING rights[SERVICE_PRIVILEGE_COUNT];
    unsigned long error = 0;
    size_t i;

    *sid = NULL;
    *failed = NULL;
    if (account == NULL) {
        *failed = "malloc";
        return ERROR_NOT_ENOUGH_MEMORY;
    }
    manager = OpenSCManagerW(NULL, NULL, SC_MANAGER_CREATE_SERVICE);
    if (manager == NULL) {
        error = GetLastError();
        *failed = "OpenSCManagerW";
        goto done;
    }
    service = CreateServiceW(manager, name, name, SERVICE_QUERY_STATUS, SERVICE_WIN32_OWN_PROCESS, SERVICE_DEMAND_START,
                             SERVICE_ERROR_NORMAL, command, NULL, NULL, NULL, account, NULL);
    if (service == NULL) {
        error = GetLastError();
        *failed = "CreateServiceW";
        goto done;
    }
    /* The virtual account's name finds its SID once the service exists. */
    if ((error = account_sid(account, sid_bytes)) != 0) {
        *failed = "LookupAccountNameW";
        goto done;
    }
    if ((error = open_policy(&policy)) != 0) {
        *failed = "LsaOpenPolicy";
        policy = NULL;
        goto done;
    }
    for (i = 0; i < SERVICE_PRIVILEGE_COUNT; i++)
        rights[i] = lsa_string(service_privileges[i]);
    if ((error = LsaNtStatusToWinError(LsaAddAccountRights(policy, sid_bytes, rights, SERVICE_PRIVILEGE_COUNT))) != 0) {
        *failed = "LsaAddAccountRights";
        goto done;
    }
    if (!ConvertSidToStringSidW(sid_bytes, sid)) {
        error = GetLastError();
        *failed = "ConvertSidToStringSidW";
        *sid = NULL;
    }
done:
    if (policy != NULL)
        LsaClose(policy);
    if (service != NULL)
        CloseServiceHandle(service);
    if (manager != NULL)
        CloseServiceHandle(manager);
    free(account);
    return error;
}

/* The service NAME, opened with ACCESS, in *SERVICE, and the service
 * manager it was opened through, in *MANAGER, for CloseServiceHandle. */
static unsigned long open_service(const wchar_t *name, DWORD access, SC_HANDLE *manager, SC_HANDLE *service)
{
    *service = NULL;
    *manager = OpenSCManagerW(NULL, NULL, SC_MANAGER_CONNECT);
    if (*manager == NULL)
        return GetLastError();
    *service = OpenServiceW(*manager, name, access);
    return *service != NULL ? 0 : GetLastError();
}

static void close_service(SC_HANDLE manager, SC_HANDLE service)
{
    if (service != NULL)
        CloseServiceHandle(service);
    if (manager != NULL)
        CloseServiceHandle(manager);
}

unsigned long nova_test_service_start(const wchar_t *name)
{
    SC_HANDLE manager = NULL;
    SC_HANDLE service = NULL;
    unsigned long error = open_service(name, SERVICE_START, &manager, &service);
    if (error == 0 && !StartServiceW(service, 0, NULL))
        error = GetLastError();
    close_service(manager, service);
    return error;
}

/* The state of the service NAME, a SERVICE_* state, in *STATE, and the
 * code it reported when it stopped, in *CODE: its own, where it reported
 * one, or else the Win32 error it stopped with. */
unsigned long nova_test_service_state(const wchar_t *name, unsigned long *state, unsigned long *code)
{
    SC_HANDLE manager = NULL;
    SC_HANDLE service = NULL;
    SERVICE_STATUS status;
    unsigned long error = open_service(name, SERVICE_QUERY_STATUS, &manager, &service);
    *state = 0;
    *code = 0;
    if (error == 0 && !QueryServiceStatus(service, &status))
        error = GetLastError();
    if (error == 0) {
        *state = status.dwCurrentState;
        *code = status.dwWin32ExitCode == ERROR_SERVICE_SPECIFIC_ERROR ? status.dwServiceSpecificExitCode
                                                                        : status.dwWin32ExitCode;
    }
    close_service(manager, service);
    return error;
}

/* Remove the account rights of the service NAME's virtual account, while
 * the service exists and so its name finds its SID.  A failure names its
 * call in *FAILED. */
static unsigned long remove_service_rights(const wchar_t *name, const char **failed)
{
    WCHAR *account = virtual_account(name);
    BYTE sid_bytes[SECURITY_MAX_SID_SIZE];
    LSA_HANDLE policy = NULL;
    unsigned long error;
    if (account == NULL) {
        *failed = "malloc";
        return ERROR_NOT_ENOUGH_MEMORY;
    }
    error = account_sid(account, sid_bytes);
    free(account);
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
    return 0;
}

/* Remove the service NAME: its account's rights first, then the service,
 * asked to stop if it runs, which goes once it has stopped.  Each step is
 * tried whatever the one before came to, and a service already gone is no
 * failure.  The first failure names its call in *FAILED. */
unsigned long nova_test_service_remove(const wchar_t *name, const char **failed)
{
    SC_HANDLE manager = NULL;
    SC_HANDLE service = NULL;
    SERVICE_STATUS status;
    unsigned long error;

    *failed = NULL;
    error = open_service(name, SERVICE_STOP | DELETE, &manager, &service);
    if (error != 0) {
        close_service(manager, service);
        if (error == ERROR_SERVICE_DOES_NOT_EXIST)
            return 0;
        *failed = "OpenServiceW";
        return error;
    }
    error = remove_service_rights(name, failed);
    if (!ControlService(service, SERVICE_CONTROL_STOP, &status) && GetLastError() != ERROR_SERVICE_NOT_ACTIVE
        && error == 0) {
        error = GetLastError();
        *failed = "ControlService";
    }
    if (!DeleteService(service) && GetLastError() != ERROR_SERVICE_MARKED_FOR_DELETE && error == 0) {
        error = GetLastError();
        *failed = "DeleteService";
    }
    close_service(manager, service);
    return error;
}

/* The test service as it runs: its name, the command line of the checks
 * it starts, the event its control handler sets when it is asked to
 * stop, and the handle it reports its state through. */
static const WCHAR *service_name;
static WCHAR *service_checks;
static HANDLE service_stop;
static SERVICE_STATUS_HANDLE service_status;

/* Report the test service's STATE, and, once it has stopped, CODE: 0, or
 * the checks' exit code, or the error that kept them from starting. */
static void report_service(DWORD state, DWORD code)
{
    SERVICE_STATUS status;
    memset(&status, 0, sizeof status);
    status.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
    status.dwCurrentState = state;
    status.dwControlsAccepted = state == SERVICE_RUNNING ? SERVICE_ACCEPT_STOP : 0;
    status.dwWin32ExitCode = code == 0 ? NO_ERROR : ERROR_SERVICE_SPECIFIC_ERROR;
    status.dwServiceSpecificExitCode = code;
    SetServiceStatus(service_status, &status);
}

static DWORD WINAPI service_control(DWORD control, DWORD type, LPVOID data, LPVOID context)
{
    (void)type;
    (void)data;
    (void)context;
    switch (control) {
    case SERVICE_CONTROL_STOP:
        SetEvent(service_stop);
        return NO_ERROR;
    case SERVICE_CONTROL_INTERROGATE:
        return NO_ERROR;
    default:
        return ERROR_CALL_NOT_IMPLEMENTED;
    }
}

/* Run the checks as a process of the service's own, which runs as its
 * account with its token, wait until they end or the service is asked to
 * stop, which ends them, and stop with their exit code. */
static VOID WINAPI service_main(DWORD argc, LPWSTR *argv)
{
    STARTUPINFOW startup;
    PROCESS_INFORMATION checks;
    HANDLE waits[2];
    DWORD code = 0;
    (void)argc;
    (void)argv;
    service_status = RegisterServiceCtrlHandlerExW(service_name, service_control, NULL);
    if (service_status == NULL)
        return;
    report_service(SERVICE_RUNNING, 0);
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    if (!CreateProcessW(NULL, service_checks, NULL, NULL, FALSE, 0, NULL, NULL, &startup, &checks)) {
        report_service(SERVICE_STOPPED, GetLastError());
        return;
    }
    CloseHandle(checks.hThread);
    waits[0] = checks.hProcess;
    waits[1] = service_stop;
    if (WaitForMultipleObjects(2, waits, FALSE, INFINITE) != WAIT_OBJECT_0) {
        TerminateProcess(checks.hProcess, STOPPED_EXIT_CODE);
        WaitForSingleObject(checks.hProcess, INFINITE);
    }
    if (!GetExitCodeProcess(checks.hProcess, &code))
        code = GetLastError();
    CloseHandle(checks.hProcess);
    report_service(SERVICE_STOPPED, code);
}

/* Run as the service NAME, which the service manager started, until it
 * stops: it runs the command line CHECKS and stops when that ends.
 * Answers once the service has stopped. */
unsigned long nova_test_service_dispatch(const wchar_t *name, const wchar_t *checks)
{
    SERVICE_TABLE_ENTRYW table[2];
    size_t length = wcslen(checks) + 1;
    unsigned long error = 0;
    service_checks = malloc(length * sizeof *service_checks);
    if (service_checks == NULL)
        return ERROR_NOT_ENOUGH_MEMORY;
    memcpy(service_checks, checks, length * sizeof *service_checks);
    service_name = name;
    service_stop = CreateEventW(NULL, TRUE, FALSE, NULL);
    if (service_stop == NULL) {
        error = GetLastError();
        free(service_checks);
        return error;
    }
    table[0].lpServiceName = (LPWSTR)name;
    table[0].lpServiceProc = service_main;
    table[1].lpServiceName = NULL;
    table[1].lpServiceProc = NULL;
    if (!StartServiceCtrlDispatcherW(table))
        error = GetLastError();
    CloseHandle(service_stop);
    free(service_checks);
    return error;
}

/* An ACL granting all access to each of the COUNT SIDS, in a buffer of
 * its own. */
static PACL grant_all(PSID *sids, size_t count)
{
    DWORD size = (DWORD)sizeof(ACL);
    PACL acl;
    size_t i;
    for (i = 0; i < count; i++)
        size += (DWORD)(sizeof(ACCESS_ALLOWED_ACE) - sizeof(DWORD)) + GetLengthSid(sids[i]);
    acl = malloc(size);
    if (acl == NULL)
        return NULL;
    if (!InitializeAcl(acl, size, ACL_REVISION)) {
        free(acl);
        return NULL;
    }
    for (i = 0; i < count; i++)
        if (!AddAccessAllowedAce(acl, ACL_REVISION, GENERIC_ALL, sids[i])) {
            free(acl);
            return NULL;
        }
    return acl;
}

/* Log the account NAME on with PASSWORD as a batch job, as the service
 * logs a slot on for its builds, and give the token the default DACL the
 * service gives it, which every process it starts takes: all access for
 * SYSTEM, for this process's user, the service, and for the account.  *SID
 * gets the account's SID's string, for LocalFree, and *TOKEN the primary
 * token, inheritable, so a helper this process starts holds it. */
unsigned long nova_test_account_logon(const wchar_t *name, const wchar_t *password, wchar_t **sid, void **token)
{
    union token_user own;
    union token_user slot;
    BYTE system_bytes[SECURITY_MAX_SID_SIZE];
    DWORD system_size = sizeof system_bytes;
    PSID grantees[3];
    TOKEN_DEFAULT_DACL dacl;
    HANDLE logged = NULL;
    HANDLE self = NULL;
    DWORD size = 0;
    unsigned long error = 0;

    *sid = NULL;
    *token = NULL;
    dacl.DefaultDacl = NULL;
    if (!LogonUserW(name, L".", password, LOGON32_LOGON_BATCH, LOGON32_PROVIDER_DEFAULT, &logged))
        return GetLastError();
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &self)
        || !GetTokenInformation(self, TokenUser, &own, sizeof own, &size)
        || !GetTokenInformation(logged, TokenUser, &slot, sizeof slot, &size)
        || !CreateWellKnownSid(WinLocalSystemSid, NULL, system_bytes, &system_size)) {
        error = GetLastError();
        goto done;
    }
    grantees[0] = system_bytes;
    grantees[1] = own.user.User.Sid;
    grantees[2] = slot.user.User.Sid;
    dacl.DefaultDacl = grant_all(grantees, sizeof grantees / sizeof grantees[0]);
    if (dacl.DefaultDacl == NULL) {
        error = ERROR_NOT_ENOUGH_MEMORY;
        goto done;
    }
    if (!SetTokenInformation(logged, TokenDefaultDacl, &dacl, sizeof dacl)
        || !SetHandleInformation(logged, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT)
        || !ConvertSidToStringSidW(slot.user.User.Sid, sid)) {
        error = GetLastError();
        *sid = NULL;
        goto done;
    }
    *token = logged;
    logged = NULL;
done:
    free(dacl.DefaultDacl);
    if (self != NULL)
        CloseHandle(self);
    if (logged != NULL)
        CloseHandle(logged);
    return error;
}

/* Give the file or directory PATH the DACL of the security descriptor
 * SDDL describes, protected from what its directory passes down. */
unsigned long nova_test_set_sddl(const wchar_t *path, const wchar_t *sddl)
{
    PSECURITY_DESCRIPTOR descriptor = NULL;
    PACL dacl = NULL;
    BOOL present = FALSE;
    BOOL defaulted = FALSE;
    unsigned long error;
    if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(sddl, SDDL_REVISION_1, &descriptor, NULL))
        return GetLastError();
    if (!GetSecurityDescriptorDacl(descriptor, &present, &dacl, &defaulted))
        error = GetLastError();
    else
        error = SetNamedSecurityInfoW((LPWSTR)path, SE_FILE_OBJECT,
                                      DACL_SECURITY_INFORMATION | PROTECTED_DACL_SECURITY_INFORMATION, NULL, NULL, dacl,
                                      NULL);
    LocalFree(descriptor);
    return error;
}

/* Start COMMAND as the account whose primary TOKEN this is, with no
 * console, as a process a build left running is, outside the build's
 * job; *PID gets its ID.  The privileges that takes are enabled in this
 * process's token first. */
unsigned long nova_test_start_as(void *token, const wchar_t *command, unsigned long *pid)
{
    static WCHAR logon_desktop[] = L"";
    HANDLE self = NULL;
    STARTUPINFOW startup;
    PROCESS_INFORMATION started;
    size_t length = wcslen(command) + 1;
    WCHAR *line;
    unsigned long error = 0;
    size_t i;

    *pid = 0;
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES, &self))
        return GetLastError();
    for (i = 0; i < SERVICE_PRIVILEGE_COUNT && error == 0; i++) {
        TOKEN_PRIVILEGES wanted;
        wanted.PrivilegeCount = 1;
        wanted.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
        if (!LookupPrivilegeValueW(NULL, service_privileges[i], &wanted.Privileges[0].Luid)
            || !AdjustTokenPrivileges(self, FALSE, &wanted, 0, NULL, NULL))
            error = GetLastError();
        /* The call succeeds for a privilege the token does not hold, and
         * says so in the thread's last error alone. */
        else if (GetLastError() != ERROR_SUCCESS)
            error = GetLastError();
    }
    CloseHandle(self);
    if (error != 0)
        return error;
    line = malloc(length * sizeof *line);
    if (line == NULL)
        return ERROR_NOT_ENOUGH_MEMORY;
    memcpy(line, command, length * sizeof *line);
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    startup.lpDesktop = logon_desktop;
    if (CreateProcessAsUserW(token, NULL, line, NULL, NULL, FALSE, DETACHED_PROCESS, NULL, NULL, &startup, &started)) {
        CloseHandle(started.hThread);
        CloseHandle(started.hProcess);
        *pid = started.dwProcessId;
    } else {
        error = GetLastError();
    }
    free(line);
    return error;
}

/* What this process may do to each process running as the account whose
 * SID's string SID_TEXT is: entries of its program's name and what opening
 * it to end it and to wait on it answer, NAME:TERMINATE:SYNCHRONIZE, each
 * ended by a semicolon, in TEXT, CAPACITY characters; one that does not
 * fit is left out, and *COUNT gets how many processes there are. */
unsigned long nova_test_account_processes(const wchar_t *sid_text, wchar_t *text, unsigned long capacity,
                                          unsigned long *count)
{
    PSID sid = NULL;
    HANDLE snapshot;
    PROCESSENTRY32W entry;
    size_t used = 0;
    BOOL listed;
    unsigned long error = 0;
    *count = 0;
    if (capacity == 0)
        return ERROR_INSUFFICIENT_BUFFER;
    text[0] = L'\0';
    if (!ConvertStringSidToSidW(sid_text, &sid))
        return GetLastError();
    snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE) {
        error = GetLastError();
        LocalFree(sid);
        return error;
    }
    entry.dwSize = sizeof entry;
    for (listed = Process32FirstW(snapshot, &entry); listed; listed = Process32NextW(snapshot, &entry)) {
        HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, entry.th32ProcessID);
        HANDLE token = NULL;
        union token_user user;
        DWORD size = 0;
        if (process == NULL)
            continue;
        if (OpenProcessToken(process, TOKEN_QUERY, &token)
            && GetTokenInformation(token, TokenUser, &user, sizeof user, &size) && EqualSid(user.user.User.Sid, sid)) {
            HANDLE ending = OpenProcess(PROCESS_TERMINATE, FALSE, entry.th32ProcessID);
            unsigned long terminate = ending != NULL ? 0 : GetLastError();
            HANDLE waitable = OpenProcess(SYNCHRONIZE, FALSE, entry.th32ProcessID);
            unsigned long synchronize = waitable != NULL ? 0 : GetLastError();
            WCHAR line[MAX_PATH + 32];
            int length = swprintf(line, sizeof line / sizeof line[0], L"%ls:%lu:%lu;", entry.szExeFile, terminate,
                                  synchronize);
            if (length > 0 && used + (size_t)length < capacity) {
                memcpy(text + used, line, ((size_t)length + 1) * sizeof *text);
                used += (size_t)length;
            }
            (*count)++;
            if (ending != NULL)
                CloseHandle(ending);
            if (waitable != NULL)
                CloseHandle(waitable);
        }
        if (token != NULL)
            CloseHandle(token);
        CloseHandle(process);
    }
    CloseHandle(snapshot);
    LocalFree(sid);
    return 0;
}

unsigned long nova_test_close_handle(void *handle)
{
    return CloseHandle(handle) ? 0 : GetLastError();
}

/* Release what a call here allocated for LocalFree: a SID's string or a
 * password. */
void nova_test_local_free(wchar_t *text)
{
    LocalFree(text);
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
    union token_user buffer;
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

/* The ID of the logon session this process's token belongs to, its high
 * and low parts, which name the window station Windows makes for it. */
unsigned long nova_test_logon_id(unsigned long *high, unsigned long *low)
{
    TOKEN_STATISTICS statistics;
    HANDLE token = NULL;
    DWORD size = 0;
    unsigned long error = 0;
    *high = 0;
    *low = 0;
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token))
        return GetLastError();
    if (GetTokenInformation(token, TokenStatistics, &statistics, sizeof statistics, &size)) {
        *high = (unsigned long)statistics.AuthenticationId.HighPart;
        *low = statistics.AuthenticationId.LowPart;
    } else {
        error = GetLastError();
    }
    CloseHandle(token);
    return error;
}

/* Open the window station NAME with whatever access its DACL grants this
 * process, and close it: a refusal says it grants none. */
unsigned long nova_test_open_station(const wchar_t *name)
{
    HWINSTA station = OpenWindowStationW(name, FALSE, MAXIMUM_ALLOWED);
    if (station == NULL)
        return GetLastError();
    CloseWindowStation(station);
    return 0;
}

/* The DACL of STATION, a window station, in SDDL, for LocalFree. */
static unsigned long station_sddl(HWINSTA station, wchar_t **sddl)
{
    SECURITY_INFORMATION wanted = DACL_SECURITY_INFORMATION;
    PSECURITY_DESCRIPTOR descriptor;
    DWORD needed = 0;
    unsigned long error = 0;
    GetUserObjectSecurity(station, &wanted, NULL, 0, &needed);
    if (needed == 0)
        return GetLastError();
    descriptor = LocalAlloc(LMEM_FIXED, needed);
    if (descriptor == NULL)
        return GetLastError();
    if (!GetUserObjectSecurity(station, &wanted, descriptor, needed, &needed)
        || !ConvertSecurityDescriptorToStringSecurityDescriptorW(descriptor, SDDL_REVISION_1, DACL_SECURITY_INFORMATION,
                                                                 sddl, NULL))
        error = GetLastError();
    LocalFree(descriptor);
    return error;
}

/* The DACL of this process's window station, NAME, in SDDL, for
 * LocalFree: read through the handle the process was given to it, or,
 * where that may not read it, through one opened by name to read it. */
unsigned long nova_test_station_dacl(const wchar_t *name, wchar_t **sddl)
{
    HWINSTA opened;
    unsigned long error;
    *sddl = NULL;
    if (station_sddl(GetProcessWindowStation(), sddl) == 0)
        return 0;
    *sddl = NULL;
    opened = OpenWindowStationW(name, FALSE, READ_CONTROL);
    if (opened == NULL)
        return GetLastError();
    error = station_sddl(opened, sddl);
    CloseWindowStation(opened);
    return error;
}

/* The value of this process's standard output handle, as another process
 * names it to copy it. */
unsigned long nova_test_stdout_value(void)
{
    return (unsigned long)(ULONG_PTR)GetStdHandle(STD_OUTPUT_HANDLE);
}

/* Copy the handle VALUE of the process PID into this one, which keeps the
 * copy until it ends, as a process holding what it copied from a build
 * does. */
unsigned long nova_test_hold_handle(unsigned long pid, unsigned long value)
{
    HANDLE source = OpenProcess(PROCESS_DUP_HANDLE, FALSE, pid);
    HANDLE copy = NULL;
    unsigned long error = 0;
    if (source == NULL)
        return GetLastError();
    if (!DuplicateHandle(source, (HANDLE)(ULONG_PTR)value, GetCurrentProcess(), &copy, 0, FALSE, DUPLICATE_SAME_ACCESS))
        error = GetLastError();
    CloseHandle(source);
    return error;
}

/* Whether this process is in a job: 1 when it is, 0 when it is not, and
 * -1 when it cannot be told. */
int nova_test_in_job(void)
{
    BOOL in_job = FALSE;
    if (!IsProcessInJob(GetCurrentProcess(), NULL, &in_job))
        return -1;
    return in_job ? 1 : 0;
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

unsigned long nova_test_account_make(const wchar_t *name, wchar_t **password, const char **failed)
{
    (void)name;
    *password = NULL;
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

unsigned long nova_test_service_install(const wchar_t *name, const wchar_t *command, wchar_t **sid,
                                        const char **failed)
{
    (void)name;
    (void)command;
    *sid = NULL;
    *failed = "nova_test_service_install";
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_service_start(const wchar_t *name)
{
    (void)name;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_service_state(const wchar_t *name, unsigned long *state, unsigned long *code)
{
    (void)name;
    *state = 0;
    *code = 0;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_service_remove(const wchar_t *name, const char **failed)
{
    (void)name;
    *failed = "nova_test_service_remove";
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_service_dispatch(const wchar_t *name, const wchar_t *checks)
{
    (void)name;
    (void)checks;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_account_logon(const wchar_t *name, const wchar_t *password, wchar_t **sid, void **token)
{
    (void)name;
    (void)password;
    *sid = NULL;
    *token = NULL;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_set_sddl(const wchar_t *path, const wchar_t *sddl)
{
    (void)path;
    (void)sddl;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_start_as(void *token, const wchar_t *command, unsigned long *pid)
{
    (void)token;
    (void)command;
    *pid = 0;
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

unsigned long nova_test_logon_id(unsigned long *high, unsigned long *low)
{
    *high = 0;
    *low = 0;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_open_station(const wchar_t *name)
{
    (void)name;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_station_dacl(const wchar_t *name, wchar_t **sddl)
{
    (void)name;
    *sddl = NULL;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_account_processes(const wchar_t *sid_text, wchar_t *text, unsigned long capacity,
                                          unsigned long *count)
{
    (void)sid_text;
    if (capacity > 0)
        text[0] = L'\0';
    *count = 0;
    return NOT_ON_WINDOWS;
}

unsigned long nova_test_stdout_value(void)
{
    return 0;
}

unsigned long nova_test_hold_handle(unsigned long pid, unsigned long value)
{
    (void)pid;
    (void)value;
    return NOT_ON_WINDOWS;
}

int nova_test_in_job(void)
{
    return -1;
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

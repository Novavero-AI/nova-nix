/*
 * A probe of what the Windows build sandbox's service rests on, and of
 * the network rule a build that is not fixed-output needs, run on a
 * hosted runner.  The design runs each build as a per-slot local account,
 * started by nova-nix-sandbox, which a service running as its virtual
 * account NT SERVICE\<name> starts with the slot's token.  This probe
 * installs such a service, a throwaway one whose binary is this program,
 * grants it SeAssignPrimaryTokenPrivilege and SeIncreaseQuotaPrivilege
 * as an install would, and measures, as that account:
 *
 *   - its token, and whether it can enable the two privileges;
 *   - whether it can log a slot's account on as a batch job;
 *   - whether it can make a named window station and a desktop on it,
 *     and what it can make instead: its unnamed window station, and
 *     named desktops on that;
 *   - whether nova-nix-sandbox starts a builder for it, and the same
 *     start made directly on the unnamed window station's desktops;
 *   - whether CreateProcessAsUserW opens the builder's image as the
 *     caller or as the token, by a builder only the service may read and
 *     one only the slot may read;
 *   - whether a build can start a process outside its job: through Task
 *     Scheduler, a task it makes as itself; through WMI's
 *     Win32_Process.Create; and whether it can register a COM server in
 *     its own hive, as a per-user COM class would need; whether such a
 *     process survives the build, and holds the build log open after it;
 *   - what a slot's build reaches of the network: TCP to an address and
 *     to a name, a name's resolution, UDP to a DNS server, and loopback,
 *     its own and the service's; with no rule, under a Windows Filtering
 *     Platform filter on the slot's SID and on its group's, under a
 *     Windows Firewall rule for the slot, and under a filter the service
 *     adds itself in a dynamic session, with what each costs to add and
 *     remove.
 *
 * Each measurement is a line "MEASURE label: what was seen"; the probe
 * judges nothing, and exits nonzero only when it cannot set its
 * measurements up.  The workflow runs the commands in order and the
 * teardown on every path:
 *
 *   probe setup ROOT HELPER      as an administrator: the slot accounts,
 *                                their group, the service, its rights,
 *                                the layout under ROOT and the service's
 *                                configuration
 *   probe phase ROOT PHASE       start the service for PHASE (main, net,
 *                                wfp-self) and print its log
 *   probe wfp ROOT TARGET        add WFP block filters for TARGET (slot or
 *                                group) in a dynamic session of this
 *                                process's, run the service's net phase
 *                                under them, then close the session
 *   probe wfp-self ROOT          grant the service the WFP rights to add
 *                                filters, run its wfp-self phase, and put
 *                                the rights back
 *   probe escapes ROOT           whether the processes the main phase's
 *                                builds started outside their jobs still
 *                                run, ending them, and removing the tasks
 *   probe teardown ROOT          remove the service, its rights, the
 *                                accounts, their rights and group, and
 *                                the tasks, by name, whatever exists
 *   probe service ROOT           the service's binary, which the service
 *                                manager starts
 *   probe child OP ARG...        one operation of a builder's, or of a
 *                                process a build started
 *
 * Functions of user32, which loads only on a desktop the process may
 * open, are looked up by name where they are called, so a process a build
 * starts on another desktop does not load it.
 */

#define _WIN32_WINNT 0x0A00
#define NTDDI_VERSION 0x0A000006

#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <bcrypt.h>
#include <fwpmu.h>
#include <lmaccess.h>
#include <lmerr.h>
#include <ntsecapi.h>
#include <sddl.h>
#include <aclapi.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

/* ------------------------------------------------------------------------
 * Names and limits
 * ------------------------------------------------------------------------ */

#define SERVICE_NAME L"nnprobesvc"
#define SERVICE_ACCOUNT L"NT SERVICE\\nnprobesvc"
#define SLOT_GROUP L"nnprobegrp"
#define SLOT_PREFIX L"nnprobe"
#define SLOT_COUNT 2
#define TASK_NAME L"nnprobe-escape-task"
#define BATCH_LOGON_RIGHT L"SeBatchLogonRight"
#define PASSWORD_PREFIX L"Aa1!"
#define PASSWORD_RANDOM_CHARS 24
#define NAME_CHARS 256

/* How long the service gets for a phase, a build for its run, a build's
 * log to end once its helper has, and a process a build started to say
 * it runs. */
#define PHASE_TIMEOUT_MS 900000
#define BUILD_TIMEOUT_MS 180000
#define LOG_END_WAIT_MS 15000
#define ESCAPE_WAIT_MS 60000
#define POLL_MS 250

/* How long a process a build started outside its job holds what it
 * holds, so the escapes command finds it running when it did escape. */
#define ESCAPED_HOLD_MS 600000

/* Network checks: the address and name dialled, and how long each waits. */
#define NET_ADDRESS "1.1.1.1"
#define NET_NAME "example.com"
#define NET_TCP_PORT 443
#define NET_DNS_PORT 53
#define NET_TIMEOUT_MS 5000

#define MEMORY_CAP_BYTES "1073741824"
#define PLAN_VERSION_LINE "nova-nix-sandbox-plan 4\n"

/* The privileges a process needs to start another logon's token, spelled
 * here as winnt.h spells them narrow without UNICODE. */
static const WCHAR *const start_privileges[] = {L"SeAssignPrimaryTokenPrivilege", L"SeIncreaseQuotaPrivilege"};

/* WFP's keys and flags, which mingw-w64's fwpmu.h does not define; the
 * values are the Windows SDK's (fwpmu.h, fwpmtypes.h). */
static const GUID layer_connect_v4 = {0xc38d57d1, 0x05a7, 0x4c33, {0x90, 0x4f, 0x7f, 0xbc, 0xee, 0xe6, 0x0e, 0x82}};
static const GUID layer_connect_v6 = {0x4a72393b, 0x319f, 0x44bc, {0x84, 0xc3, 0xba, 0x54, 0xdc, 0xb3, 0xb6, 0xb4}};
static const GUID layer_accept_v4 = {0xe1cd9fe7, 0xf4b5, 0x4273, {0x96, 0xc0, 0x59, 0x2e, 0x48, 0x7b, 0x86, 0x50}};
static const GUID layer_accept_v6 = {0xa3b42c97, 0x9f04, 0x4672, {0xb8, 0x7e, 0xce, 0xe9, 0xc4, 0x83, 0x25, 0x7f}};
static const GUID condition_user_id = {0xaf043a0a, 0xb34d, 0x4f86, {0x97, 0x9c, 0xc9, 0x03, 0x71, 0xaf, 0x6e, 0x66}};
static const GUID condition_flags = {0x632ce23b, 0x5167, 0x435c, {0x86, 0xd7, 0xe9, 0x03, 0x68, 0x4a, 0xa8, 0x0c}};
static const GUID sublayer_universal = {0xeebecc03, 0xced4, 0x4380, {0x81, 0x9a, 0x27, 0x34, 0x39, 0x7b, 0x2b, 0x74}};
#define SESSION_FLAG_DYNAMIC 0x00000001
#define ACTRL_ADD 0x00000001
#define ACTRL_ADD_LINK 0x00000002
#define ACTRL_OPEN 0x00000040
#define ACTRL_READ 0x00000080
#define ACTRL_ENUM 0x00000020
#define ACTRL_BEGIN_READ_TXN 0x00000004
#define ACTRL_BEGIN_WRITE_TXN 0x00000008

static const GUID *const block_layers[] = {&layer_connect_v4, &layer_connect_v6, &layer_accept_v4, &layer_accept_v6};
static const char *const block_layer_names[] = {"ALE connect v4", "ALE connect v6", "ALE accept v4", "ALE accept v6"};
#define BLOCK_LAYER_COUNT 4

/* ------------------------------------------------------------------------
 * Output and text
 * ------------------------------------------------------------------------ */

/* Where lines go: standard output, or the service's log. */
static HANDLE out = INVALID_HANDLE_VALUE;

static void say(const char *format, ...)
{
    char line[8192];
    va_list arguments;
    int length;
    DWORD written;
    va_start(arguments, format);
    length = vsnprintf(line, sizeof line - 2, format, arguments);
    va_end(arguments);
    if (length < 0)
        return;
    if ((size_t)length > sizeof line - 2)
        length = (int)(sizeof line - 2);
    line[length++] = '\n';
    WriteFile(out, line, (DWORD)length, &written, NULL);
}

static void *take(size_t length)
{
    void *bytes = calloc(1, length > 0 ? length : 1);
    if (bytes == NULL) {
        say("FATAL out of memory");
        ExitProcess(3);
    }
    return bytes;
}

/* TEXT as UTF-8, in a buffer of its own. */
static char *utf8(const WCHAR *text)
{
    int length;
    char *bytes;
    if (text == NULL)
        return "(none)";
    length = WideCharToMultiByte(CP_UTF8, 0, text, -1, NULL, 0, NULL, NULL);
    bytes = take((size_t)(length > 0 ? length : 1));
    if (length > 0)
        WideCharToMultiByte(CP_UTF8, 0, text, -1, bytes, length, NULL, NULL);
    return bytes;
}

/* BYTES, UTF-8, as UTF-16. */
static WCHAR *widen(const char *bytes)
{
    int length = MultiByteToWideChar(CP_UTF8, 0, bytes, -1, NULL, 0);
    WCHAR *text = take((size_t)(length > 0 ? length : 1) * sizeof(WCHAR));
    if (length > 0)
        MultiByteToWideChar(CP_UTF8, 0, bytes, -1, text, length);
    return text;
}

#define END ((const WCHAR *)NULL)

/* The strings given, up to END, joined. */
static WCHAR *join(const WCHAR *first, ...)
{
    va_list pieces;
    const WCHAR *piece;
    size_t total = 0;
    WCHAR *joined;
    va_start(pieces, first);
    for (piece = first; piece != NULL; piece = va_arg(pieces, const WCHAR *))
        total += wcslen(piece);
    va_end(pieces);
    joined = take((total + 1) * sizeof(WCHAR));
    va_start(pieces, first);
    for (piece = first; piece != NULL; piece = va_arg(pieces, const WCHAR *))
        wcscat(joined, piece);
    va_end(pieces);
    return joined;
}

static WCHAR *decimal(unsigned long long value)
{
    char digits[32];
    snprintf(digits, sizeof digits, "%llu", value);
    return widen(digits);
}

/* A Win32 error's number and text, for a line. */
static char *error_text(DWORD error)
{
    char *text = take(512);
    char message[400] = "";
    DWORD length = FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, error, 0, message,
                                  sizeof message, NULL);
    while (length > 0 && (message[length - 1] == '\n' || message[length - 1] == '\r' || message[length - 1] == '.'))
        message[--length] = '\0';
    snprintf(text, 512, "%lu (%s)", (unsigned long)error, length > 0 ? message : "no text");
    return text;
}

static double elapsed_ms(LARGE_INTEGER from)
{
    LARGE_INTEGER now;
    LARGE_INTEGER frequency;
    QueryPerformanceCounter(&now);
    QueryPerformanceFrequency(&frequency);
    return (double)(now.QuadPart - from.QuadPart) * 1000.0 / (double)frequency.QuadPart;
}

static LARGE_INTEGER now_counter(void)
{
    LARGE_INTEGER now;
    QueryPerformanceCounter(&now);
    return now;
}

/* A file's whole contents, NUL-terminated, or NULL. */
static char *read_file(const WCHAR *path, DWORD *error)
{
    HANDLE file = CreateFileW(path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                              OPEN_EXISTING, 0, NULL);
    LARGE_INTEGER size;
    char *bytes;
    DWORD got = 0;
    *error = 0;
    if (file == INVALID_HANDLE_VALUE) {
        *error = GetLastError();
        return NULL;
    }
    if (!GetFileSizeEx(file, &size) || size.QuadPart > 64 * 1024 * 1024) {
        *error = GetLastError();
        CloseHandle(file);
        return NULL;
    }
    bytes = take((size_t)size.QuadPart + 1);
    if (!ReadFile(file, bytes, (DWORD)size.QuadPart, &got, NULL))
        *error = GetLastError();
    CloseHandle(file);
    bytes[got] = '\0';
    return bytes;
}

static DWORD write_file(const WCHAR *path, const char *bytes)
{
    HANDLE file = CreateFileW(path, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    DWORD written = 0;
    DWORD error = 0;
    if (file == INVALID_HANDLE_VALUE)
        return GetLastError();
    if (!WriteFile(file, bytes, (DWORD)strlen(bytes), &written, NULL))
        error = GetLastError();
    CloseHandle(file);
    return error;
}

/* Write a file whole: made under another name and renamed into place,
 * so a reader never sees it half written. */
static DWORD write_file_whole(const WCHAR *path, const char *bytes)
{
    WCHAR *partial = join(path, L".partial", END);
    DWORD error = write_file(partial, bytes);
    if (error != 0)
        return error;
    return MoveFileExW(partial, path, MOVEFILE_REPLACE_EXISTING) ? 0 : GetLastError();
}

/* Print a block of text a line at a time, each line labelled. */
static void say_lines(const char *label, const char *text)
{
    const char *at = text;
    if (text == NULL || *text == '\0') {
        say("%s: (empty)", label);
        return;
    }
    while (*at != '\0') {
        const char *end = strchr(at, '\n');
        size_t length = end != NULL ? (size_t)(end - at) : strlen(at);
        while (length > 0 && at[length - 1] == '\r')
            length--;
        say("%s: %.*s", label, (int)length, at);
        if (end == NULL)
            break;
        at = end + 1;
    }
}

/* The value of a "key=value" line of a configuration text, or NULL. */
static char *config_value(const char *text, const char *key)
{
    size_t key_length = strlen(key);
    const char *at = text;
    while (at != NULL && *at != '\0') {
        const char *end = strchr(at, '\n');
        size_t length = end != NULL ? (size_t)(end - at) : strlen(at);
        if (length > key_length && strncmp(at, key, key_length) == 0 && at[key_length] == '=') {
            char *value = take(length - key_length);
            memcpy(value, at + key_length + 1, length - key_length - 1);
            if (length - key_length - 1 > 0 && value[length - key_length - 2] == '\r')
                value[length - key_length - 2] = '\0';
            return value;
        }
        at = end != NULL ? end + 1 : NULL;
    }
    return NULL;
}

/* ------------------------------------------------------------------------
 * SIDs, security descriptors and tokens
 * ------------------------------------------------------------------------ */

static WCHAR *sid_text(PSID sid)
{
    WCHAR *text = NULL;
    WCHAR *copy;
    if (sid == NULL || !ConvertSidToStringSidW(sid, &text))
        return L"?";
    copy = join(text, END);
    LocalFree(text);
    return copy;
}

static PSID sid_of(const WCHAR *text)
{
    PSID sid = NULL;
    return ConvertStringSidToSidW(text, &sid) ? sid : NULL;
}

/* An account's SID, by name, or NULL with its error. */
static PSID account_sid(const WCHAR *name, DWORD *error)
{
    DWORD sid_size = SECURITY_MAX_SID_SIZE;
    WCHAR domain[NAME_CHARS];
    DWORD domain_size = NAME_CHARS;
    SID_NAME_USE use;
    PSID sid = take(SECURITY_MAX_SID_SIZE);
    *error = 0;
    if (!LookupAccountNameW(NULL, name, sid, &sid_size, domain, &domain_size, &use)) {
        *error = GetLastError();
        return NULL;
    }
    return sid;
}

static char *descriptor_text(PSECURITY_DESCRIPTOR descriptor, SECURITY_INFORMATION what)
{
    WCHAR *text = NULL;
    char *copy;
    if (!ConvertSecurityDescriptorToStringSecurityDescriptorW(descriptor, SDDL_REVISION_1, what, &text, NULL))
        return error_text(GetLastError());
    copy = utf8(text);
    LocalFree(text);
    return copy;
}

static char *path_sddl(const WCHAR *path)
{
    PSECURITY_DESCRIPTOR descriptor = NULL;
    char *text;
    DWORD error = GetNamedSecurityInfoW(path, SE_FILE_OBJECT, OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
                                        NULL, NULL, NULL, NULL, &descriptor);
    if (error != ERROR_SUCCESS)
        return error_text(error);
    text = descriptor_text(descriptor, OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION);
    LocalFree(descriptor);
    return text;
}

/* Give PATH the protected DACL SDDL describes. */
static DWORD apply_sddl(const WCHAR *path, const WCHAR *sddl)
{
    PSECURITY_DESCRIPTOR descriptor = NULL;
    BOOL present = FALSE;
    BOOL defaulted = FALSE;
    PACL dacl = NULL;
    DWORD error;
    if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(sddl, SDDL_REVISION_1, &descriptor, NULL))
        return GetLastError();
    if (!GetSecurityDescriptorDacl(descriptor, &present, &dacl, &defaulted)) {
        error = GetLastError();
        LocalFree(descriptor);
        return error;
    }
    error = SetNamedSecurityInfoW((LPWSTR)path, SE_FILE_OBJECT,
                                  DACL_SECURITY_INFORMATION | PROTECTED_DACL_SECURITY_INFORMATION, NULL, NULL, dacl,
                                  NULL);
    LocalFree(descriptor);
    return error;
}

/* Make the directory PATH with the DACL SDDL describes, which an existing
 * one is given too. */
static DWORD make_directory(const WCHAR *path, const WCHAR *sddl)
{
    if (!CreateDirectoryW(path, NULL) && GetLastError() != ERROR_ALREADY_EXISTS)
        return GetLastError();
    return apply_sddl(path, sddl);
}

/* Enable each start privilege in TOKEN, saying how each went. */
static DWORD enable_start_privileges(HANDLE token, const char *label)
{
    size_t i;
    DWORD failed = 0;
    for (i = 0; i < sizeof start_privileges / sizeof start_privileges[0]; i++) {
        TOKEN_PRIVILEGES wanted;
        DWORD outcome = 0;
        wanted.PrivilegeCount = 1;
        wanted.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
        if (!LookupPrivilegeValueW(NULL, start_privileges[i], &wanted.Privileges[0].Luid))
            outcome = GetLastError();
        else if (!AdjustTokenPrivileges(token, FALSE, &wanted, 0, NULL, NULL))
            outcome = GetLastError();
        else
            outcome = GetLastError();
        say("MEASURE %s enable %s: %s", label, utf8(start_privileges[i]),
            outcome == 0 ? "enabled" : error_text(outcome));
        if (outcome != 0)
            failed = outcome;
    }
    return failed;
}

static void describe_token(HANDLE token, const char *label)
{
    union {
        TOKEN_USER user;
        BYTE bytes[sizeof(TOKEN_USER) + SECURITY_MAX_SID_SIZE];
    } user;
    DWORD size = 0;
    DWORD session = 0;
    TOKEN_GROUPS *groups;
    TOKEN_PRIVILEGES *privileges;
    DWORD i;
    if (GetTokenInformation(token, TokenUser, &user, sizeof user, &size))
        say("MEASURE %s user: %s", label, utf8(sid_text(user.user.User.Sid)));
    if (GetTokenInformation(token, TokenSessionId, &session, sizeof session, &size))
        say("MEASURE %s session: %lu", label, (unsigned long)session);
    GetTokenInformation(token, TokenGroups, NULL, 0, &size);
    groups = take(size);
    if (GetTokenInformation(token, TokenGroups, groups, size, &size))
        for (i = 0; i < groups->GroupCount; i++)
            say("MEASURE %s group: %s attributes 0x%lx", label, utf8(sid_text(groups->Groups[i].Sid)),
                (unsigned long)groups->Groups[i].Attributes);
    GetTokenInformation(token, TokenPrivileges, NULL, 0, &size);
    privileges = take(size);
    if (GetTokenInformation(token, TokenPrivileges, privileges, size, &size))
        for (i = 0; i < privileges->PrivilegeCount; i++) {
            WCHAR name[NAME_CHARS];
            DWORD name_size = NAME_CHARS;
            if (!LookupPrivilegeNameW(NULL, &privileges->Privileges[i].Luid, name, &name_size))
                wcscpy(name, L"?");
            say("MEASURE %s privilege: %s attributes 0x%lx", label, utf8(name),
                (unsigned long)privileges->Privileges[i].Attributes);
        }
}

/* ------------------------------------------------------------------------
 * user32, looked up by name
 * ------------------------------------------------------------------------ */

typedef void (*any_function)(void);
typedef HWINSTA(WINAPI *create_station_call)(LPCWSTR, DWORD, ACCESS_MASK, LPSECURITY_ATTRIBUTES);
typedef HWINSTA(WINAPI *get_station_call)(void);
typedef BOOL(WINAPI *set_station_call)(HWINSTA);
typedef HDESK(WINAPI *create_desktop_call)(LPCWSTR, LPCWSTR, void *, DWORD, ACCESS_MASK, LPSECURITY_ATTRIBUTES);
typedef HDESK(WINAPI *thread_desktop_call)(DWORD);
typedef BOOL(WINAPI *object_information_call)(HANDLE, int, PVOID, DWORD, LPDWORD);
typedef BOOL(WINAPI *close_station_call)(HWINSTA);
typedef BOOL(WINAPI *close_desktop_call)(HDESK);
typedef HWINSTA(WINAPI *open_station_call)(LPCWSTR, BOOL, ACCESS_MASK);

struct user32 {
    create_station_call create_station;
    get_station_call get_station;
    set_station_call set_station;
    create_desktop_call create_desktop;
    thread_desktop_call thread_desktop;
    object_information_call object_information;
    close_station_call close_station;
    close_desktop_call close_desktop;
    open_station_call open_station;
};

static int load_user32(struct user32 *u)
{
    HMODULE module = LoadLibraryExW(L"user32.dll", NULL, LOAD_LIBRARY_SEARCH_SYSTEM32);
    memset(u, 0, sizeof *u);
    if (module == NULL)
        return 0;
    u->create_station = (create_station_call)(any_function)GetProcAddress(module, "CreateWindowStationW");
    u->get_station = (get_station_call)(any_function)GetProcAddress(module, "GetProcessWindowStation");
    u->set_station = (set_station_call)(any_function)GetProcAddress(module, "SetProcessWindowStation");
    u->create_desktop = (create_desktop_call)(any_function)GetProcAddress(module, "CreateDesktopW");
    u->thread_desktop = (thread_desktop_call)(any_function)GetProcAddress(module, "GetThreadDesktop");
    u->object_information = (object_information_call)(any_function)GetProcAddress(module, "GetUserObjectInformationW");
    u->close_station = (close_station_call)(any_function)GetProcAddress(module, "CloseWindowStation");
    u->close_desktop = (close_desktop_call)(any_function)GetProcAddress(module, "CloseDesktop");
    u->open_station = (open_station_call)(any_function)GetProcAddress(module, "OpenWindowStationW");
    return u->create_station != NULL && u->get_station != NULL && u->set_station != NULL && u->create_desktop != NULL
           && u->thread_desktop != NULL && u->object_information != NULL && u->close_station != NULL
           && u->close_desktop != NULL && u->open_station != NULL;
}

/* UOI_NAME, winuser.h's. */
#define OBJECT_NAME 2

static WCHAR *object_name(const struct user32 *u, HANDLE object)
{
    WCHAR *name = take(NAME_CHARS * sizeof(WCHAR));
    DWORD needed = 0;
    if (object == NULL || !u->object_information(object, OBJECT_NAME, name, NAME_CHARS * sizeof(WCHAR), &needed))
        wcscpy(name, L"?");
    return name;
}

/* A security descriptor whose DACL grants each SID all access, for a
 * window station or desktop. */
static PSECURITY_DESCRIPTOR grant_all(PSID const *sids, size_t count)
{
    DWORD entry = (DWORD)(sizeof(ACCESS_ALLOWED_ACE) - sizeof(DWORD));
    DWORD size = (DWORD)sizeof(ACL);
    PACL acl;
    PSECURITY_DESCRIPTOR descriptor = take(SECURITY_DESCRIPTOR_MIN_LENGTH);
    size_t i;
    for (i = 0; i < count; i++)
        size += entry + GetLengthSid(sids[i]);
    acl = take(size);
    InitializeAcl(acl, size, ACL_REVISION);
    for (i = 0; i < count; i++)
        AddAccessAllowedAce(acl, ACL_REVISION, GENERIC_ALL, sids[i]);
    InitializeSecurityDescriptor(descriptor, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(descriptor, TRUE, acl, FALSE);
    return descriptor;
}

/* ------------------------------------------------------------------------
 * Accounts and the service, as an administrator
 * ------------------------------------------------------------------------ */

static WCHAR *slot_name(int index)
{
    return join(SLOT_PREFIX, decimal((unsigned long long)index + 1), END);
}

static WCHAR *make_password(void)
{
    static const WCHAR alphabet[] = L"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    size_t prefix = wcslen(PASSWORD_PREFIX);
    unsigned char bytes[PASSWORD_RANDOM_CHARS];
    WCHAR *password = take((prefix + PASSWORD_RANDOM_CHARS + 1) * sizeof(WCHAR));
    size_t i;
    if (BCryptGenRandom(NULL, bytes, sizeof bytes, BCRYPT_USE_SYSTEM_PREFERRED_RNG) != 0)
        return NULL;
    memcpy(password, PASSWORD_PREFIX, prefix * sizeof(WCHAR));
    for (i = 0; i < PASSWORD_RANDOM_CHARS; i++)
        password[prefix + i] = alphabet[bytes[i] % (sizeof alphabet / sizeof alphabet[0] - 1)];
    return password;
}

static LSA_HANDLE open_policy(DWORD *error)
{
    LSA_OBJECT_ATTRIBUTES attributes;
    LSA_HANDLE policy = NULL;
    memset(&attributes, 0, sizeof attributes);
    *error = LsaNtStatusToWinError(LsaOpenPolicy(NULL, &attributes, POLICY_CREATE_ACCOUNT | POLICY_LOOKUP_NAMES, &policy));
    return *error == 0 ? policy : NULL;
}

static LSA_UNICODE_STRING lsa_string(const WCHAR *text)
{
    LSA_UNICODE_STRING string;
    string.Buffer = (PWSTR)text;
    string.Length = (USHORT)(wcslen(text) * sizeof(WCHAR));
    string.MaximumLength = (USHORT)(string.Length + sizeof(WCHAR));
    return string;
}

static DWORD grant_right(PSID sid, const WCHAR *right)
{
    DWORD error;
    LSA_HANDLE policy = open_policy(&error);
    LSA_UNICODE_STRING string = lsa_string(right);
    if (policy == NULL)
        return error;
    error = LsaNtStatusToWinError(LsaAddAccountRights(policy, sid, &string, 1));
    LsaClose(policy);
    return error;
}

/* Remove every right SID holds; one holding none is no failure. */
static DWORD remove_rights(PSID sid)
{
    DWORD error;
    LSA_HANDLE policy = open_policy(&error);
    if (policy == NULL)
        return error;
    error = LsaNtStatusToWinError(LsaRemoveAccountRights(policy, sid, TRUE, NULL, 0));
    LsaClose(policy);
    return error == ERROR_FILE_NOT_FOUND ? 0 : error;
}

/* The layout's paths under ROOT. */
struct layout {
    WCHAR *root;
    WCHAR *tools;
    WCHAR *service_only;
    WCHAR *slot_only;
    WCHAR *service_dir;
    WCHAR *config;
    WCHAR *builds[SLOT_COUNT];
};

static void layout_of(const WCHAR *root, struct layout *l)
{
    int i;
    l->root = join(root, END);
    l->tools = join(root, L"\\tools", END);
    l->service_only = join(root, L"\\service-only", END);
    l->slot_only = join(root, L"\\slot-only", END);
    l->service_dir = join(root, L"\\service", END);
    l->config = join(root, L"\\service\\config", END);
    for (i = 0; i < SLOT_COUNT; i++)
        l->builds[i] = join(root, L"\\build", decimal((unsigned long long)i + 1), END);
}

static DWORD copy_file(const WCHAR *from, const WCHAR *to, const WCHAR *sddl)
{
    if (!CopyFileW(from, to, FALSE))
        return GetLastError();
    return apply_sddl(to, sddl);
}

static int setup_command(const WCHAR *root, const WCHAR *helper)
{
    struct layout l;
    WCHAR self[MAX_PATH];
    WCHAR *passwords[SLOT_COUNT];
    WCHAR *sids[SLOT_COUNT];
    PSID group_sid;
    PSID service_sid;
    WCHAR *service_text;
    WCHAR *group_text;
    LOCALGROUP_INFO_1 group;
    SC_HANDLE manager;
    SC_HANDLE service;
    WCHAR *binary;
    char *config;
    size_t config_size = 16384;
    DWORD error;
    int i;

    layout_of(root, &l);
    GetModuleFileNameW(NULL, self, MAX_PATH);

    memset(&group, 0, sizeof group);
    group.lgrpi1_name = SLOT_GROUP;
    group.lgrpi1_comment = L"nova-nix sandbox service probe";
    error = NetLocalGroupAdd(NULL, 1, (LPBYTE)&group, NULL);
    say("SETUP group %s: %s", utf8(SLOT_GROUP), error == NERR_Success ? "made" : error_text(error));
    if (error != NERR_Success)
        return 1;
    group_sid = account_sid(SLOT_GROUP, &error);
    if (group_sid == NULL)
        return 1;
    group_text = sid_text(group_sid);

    for (i = 0; i < SLOT_COUNT; i++) {
        USER_INFO_1 user;
        LOCALGROUP_MEMBERS_INFO_3 member;
        WCHAR *name = slot_name(i);
        PSID sid;
        DWORD bad = 0;
        passwords[i] = make_password();
        if (passwords[i] == NULL)
            return 1;
        memset(&user, 0, sizeof user);
        user.usri1_name = name;
        user.usri1_password = passwords[i];
        user.usri1_priv = USER_PRIV_USER;
        user.usri1_flags = UF_SCRIPT | UF_DONT_EXPIRE_PASSWD;
        error = NetUserAdd(NULL, 1, (LPBYTE)&user, &bad);
        say("SETUP account %s: %s", utf8(name), error == NERR_Success ? "made" : error_text(error));
        if (error != NERR_Success)
            return 1;
        member.lgrmi3_domainandname = name;
        error = NetLocalGroupAddMembers(NULL, SLOT_GROUP, 3, (LPBYTE)&member, 1);
        say("SETUP %s in %s: %s", utf8(name), utf8(SLOT_GROUP), error == NERR_Success ? "yes" : error_text(error));
        sid = account_sid(name, &error);
        if (sid == NULL)
            return 1;
        sids[i] = sid_text(sid);
        error = grant_right(sid, BATCH_LOGON_RIGHT);
        say("SETUP %s batch logon right: %s", utf8(name), error == 0 ? "granted" : error_text(error));
        if (error != 0)
            return 1;
    }

    error = make_directory(l.tools, L"D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;0x1200a9;;;BU)");
    if (error == 0)
        error = copy_file(self, join(l.tools, L"\\probe.exe", END), L"D:P(A;;FA;;;SY)(A;;FA;;;BA)(A;;0x1200a9;;;BU)");
    if (error == 0)
        error = copy_file(helper, join(l.tools, L"\\nova-nix-sandbox.exe", END),
                          L"D:P(A;;FA;;;SY)(A;;FA;;;BA)(A;;0x1200a9;;;BU)");
    say("SETUP tools: %s", error == 0 ? "laid out" : error_text(error));
    if (error != 0)
        return 1;

    manager = OpenSCManagerW(NULL, NULL, SC_MANAGER_CONNECT | SC_MANAGER_CREATE_SERVICE);
    if (manager == NULL) {
        say("SETUP service manager: %s", error_text(GetLastError()));
        return 1;
    }
    binary = join(L"\"", l.tools, L"\\probe.exe\" service \"", root, L"\"", END);
    service = CreateServiceW(manager, SERVICE_NAME, L"nova-nix sandbox service probe", SERVICE_ALL_ACCESS,
                             SERVICE_WIN32_OWN_PROCESS, SERVICE_DEMAND_START, SERVICE_ERROR_NORMAL, binary, NULL, NULL,
                             NULL, SERVICE_ACCOUNT, NULL);
    say("SETUP service %s as %s: %s", utf8(SERVICE_NAME), utf8(SERVICE_ACCOUNT),
        service != NULL ? "made" : error_text(GetLastError()));
    if (service == NULL) {
        CloseServiceHandle(manager);
        return 1;
    }
    CloseServiceHandle(service);
    CloseServiceHandle(manager);

    service_sid = account_sid(SERVICE_ACCOUNT, &error);
    say("SETUP service SID: %s", service_sid != NULL ? utf8(sid_text(service_sid)) : error_text(error));
    if (service_sid == NULL)
        return 1;
    service_text = sid_text(service_sid);
    /* The service's binary is opened for it whichever way a start opens
     * an image, so the tools grant the service by name too. */
    error = apply_sddl(l.tools, join(L"D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;0x1200a9;;;BU)(A;OICI;0x1200a9;;;",
                                      service_text, L")", END));
    if (error == 0)
        error = apply_sddl(join(l.tools, L"\\probe.exe", END),
                           join(L"D:P(A;;FA;;;SY)(A;;FA;;;BA)(A;;0x1200a9;;;BU)(A;;0x1200a9;;;", service_text, L")", END));
    if (error == 0)
        error = apply_sddl(join(l.tools, L"\\nova-nix-sandbox.exe", END),
                           join(L"D:P(A;;FA;;;SY)(A;;FA;;;BA)(A;;0x1200a9;;;BU)(A;;0x1200a9;;;", service_text, L")", END));
    say("SETUP tools granted to the service: %s", error == 0 ? "granted" : error_text(error));
    if (error != 0)
        return 1;
    for (i = 0; i < (int)(sizeof start_privileges / sizeof start_privileges[0]); i++) {
        error = grant_right(service_sid, start_privileges[i]);
        say("SETUP service right %s: %s", utf8(start_privileges[i]), error == 0 ? "granted" : error_text(error));
        if (error != 0)
            return 1;
    }

    error = make_directory(l.service_dir, join(L"D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;FA;;;", service_text, L")", END));
    if (error == 0)
        error = make_directory(l.service_only, join(L"D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;0x1200a9;;;",
                                                     service_text, L")", END));
    if (error == 0)
        error = copy_file(self, join(l.service_only, L"\\probe.exe", END),
                          join(L"D:P(A;;FA;;;SY)(A;;FA;;;BA)(A;;0x1200a9;;;", service_text, L")", END));
    if (error == 0)
        error = make_directory(l.slot_only, join(L"D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;0x1200a9;;;", sids[0],
                                                  L")", END));
    if (error == 0)
        error = copy_file(self, join(l.slot_only, L"\\probe.exe", END),
                          join(L"D:P(A;;FA;;;SY)(A;;FA;;;BA)(A;;0x1200a9;;;", sids[0], L")", END));
    for (i = 0; i < SLOT_COUNT && error == 0; i++)
        error = make_directory(l.builds[i], join(L"D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;FA;;;", service_text,
                                                  L")(A;OICI;FA;;;", sids[i], L")", END));
    say("SETUP layout: %s", error == 0 ? "laid out" : error_text(error));
    if (error != 0)
        return 1;
    say("MEASURE layout service-only image: %s", path_sddl(join(l.service_only, L"\\probe.exe", END)));
    say("MEASURE layout slot-only image: %s", path_sddl(join(l.slot_only, L"\\probe.exe", END)));
    say("MEASURE layout tools image: %s", path_sddl(join(l.tools, L"\\probe.exe", END)));

    config = take(config_size);
    snprintf(config, config_size,
             "root=%s\nhelper=%s\\nova-nix-sandbox.exe\nprobe=%s\\probe.exe\nservice-only=%s\\probe.exe\n"
             "slot-only=%s\\probe.exe\nservice-sid=%s\ngroup-sid=%s\n",
             utf8(root), utf8(l.tools), utf8(l.tools), utf8(l.service_only), utf8(l.slot_only), utf8(service_text),
             utf8(group_text));
    for (i = 0; i < SLOT_COUNT; i++) {
        size_t used = strlen(config);
        snprintf(config + used, config_size - used, "slot%d-name=%s\nslot%d-password=%s\nslot%d-sid=%s\nslot%d-build=%s\n",
                 i + 1, utf8(slot_name(i)), i + 1, utf8(passwords[i]), i + 1, utf8(sids[i]), i + 1, utf8(l.builds[i]));
    }
    error = write_file(l.config, config);
    if (error == 0)
        error = apply_sddl(l.config, join(L"D:P(A;;FA;;;SY)(A;;FA;;;BA)(A;;FR;;;", service_text, L")", END));
    say("SETUP service configuration: %s", error == 0 ? "written, for the service alone to read" : error_text(error));
    return error == 0 ? 0 : 1;
}

static void delete_task(const char *label);

/* Remove what setup made, by name, whatever exists of it. */
static int teardown_command(const WCHAR *root)
{
    SC_HANDLE manager = OpenSCManagerW(NULL, NULL, SC_MANAGER_CONNECT);
    DWORD error;
    PSID sid;
    int i;
    (void)root;
    delete_task("TEARDOWN");
    sid = account_sid(SERVICE_ACCOUNT, &error);
    if (sid != NULL) {
        error = remove_rights(sid);
        say("TEARDOWN service rights: %s", error == 0 ? "removed" : error_text(error));
    }
    if (manager != NULL) {
        SC_HANDLE service = OpenServiceW(manager, SERVICE_NAME, SERVICE_STOP | DELETE | SERVICE_QUERY_STATUS);
        if (service != NULL) {
            SERVICE_STATUS status;
            ControlService(service, SERVICE_CONTROL_STOP, &status);
            say("TEARDOWN service: %s", DeleteService(service) ? "deleted" : error_text(GetLastError()));
            CloseServiceHandle(service);
        }
        CloseServiceHandle(manager);
    }
    for (i = 0; i < SLOT_COUNT; i++) {
        WCHAR *name = slot_name(i);
        sid = account_sid(name, &error);
        if (sid == NULL)
            continue;
        error = remove_rights(sid);
        say("TEARDOWN %s rights: %s", utf8(name), error == 0 ? "removed" : error_text(error));
        error = NetUserDel(NULL, name);
        say("TEARDOWN %s: %s", utf8(name), error == NERR_Success ? "deleted" : error_text(error));
    }
    error = NetLocalGroupDel(NULL, SLOT_GROUP);
    if (error != NERR_GroupNotFound && error != ERROR_NO_SUCH_ALIAS)
        say("TEARDOWN group: %s", error == NERR_Success ? "deleted" : error_text(error));
    return 0;
}

/* Start the service for PHASE and wait for it to stop; print its log. */
static int phase_command(const WCHAR *root, const WCHAR *phase)
{
    struct layout l;
    SC_HANDLE manager;
    SC_HANDLE service;
    const WCHAR *arguments[2];
    SERVICE_STATUS status;
    ULONGLONG deadline;
    WCHAR *log_path;
    memset(&status, 0, sizeof status);
    char *log;
    DWORD error;
    layout_of(root, &l);
    log_path = join(l.service_dir, L"\\", phase, L".log", END);
    DeleteFileW(log_path);
    manager = OpenSCManagerW(NULL, NULL, SC_MANAGER_CONNECT);
    service = manager != NULL ? OpenServiceW(manager, SERVICE_NAME, SERVICE_START | SERVICE_QUERY_STATUS | SERVICE_STOP)
                              : NULL;
    if (service == NULL) {
        say("PHASE %s: the service cannot be opened, %s", utf8(phase), error_text(GetLastError()));
        return 1;
    }
    /* The service's name first, as StartServiceW's arguments start; the
     * service reads the phase as its last argument. */
    arguments[0] = SERVICE_NAME;
    arguments[1] = phase;
    if (!StartServiceW(service, 2, arguments)) {
        say("MEASURE phase %s start: %s", utf8(phase), error_text(GetLastError()));
        CloseServiceHandle(service);
        CloseServiceHandle(manager);
        return 0;
    }
    deadline = GetTickCount64() + PHASE_TIMEOUT_MS;
    do {
        Sleep(POLL_MS);
        if (!QueryServiceStatus(service, &status))
            break;
    } while (status.dwCurrentState != SERVICE_STOPPED && GetTickCount64() < deadline);
    if (status.dwCurrentState != SERVICE_STOPPED) {
        say("MEASURE phase %s: did not stop in time, stopping it", utf8(phase));
        ControlService(service, SERVICE_CONTROL_STOP, &status);
    }
    say("PHASE %s ended: state %lu, exit %lu, service exit %lu", utf8(phase), (unsigned long)status.dwCurrentState,
        (unsigned long)status.dwWin32ExitCode, (unsigned long)status.dwServiceSpecificExitCode);
    CloseServiceHandle(service);
    CloseServiceHandle(manager);
    log = read_file(log_path, &error);
    if (log == NULL)
        say("PHASE %s log: unreadable, %s", utf8(phase), error_text(error));
    else
        say_lines("SERVICE", log);
    return 0;
}

/* ------------------------------------------------------------------------
 * WFP
 * ------------------------------------------------------------------------ */

/* Open a dynamic WFP session, whose filters go when it closes. */
static HANDLE open_dynamic_engine(DWORD *error)
{
    FWPM_SESSION0 session;
    HANDLE engine = NULL;
    memset(&session, 0, sizeof session);
    session.flags = SESSION_FLAG_DYNAMIC;
    *error = FwpmEngineOpen0(NULL, RPC_C_AUTHN_WINNT, NULL, &session, &engine);
    return *error == 0 ? engine : NULL;
}

/* Add, in ENGINE, a filter at each blocking layer that blocks what a
 * process whose token holds SID sends or takes, but on loopback. */
static DWORD add_block_filters(HANDLE engine, const WCHAR *sid, const char *label)
{
    PSECURITY_DESCRIPTOR descriptor = NULL;
    FWP_BYTE_BLOB blob;
    FWPM_FILTER_CONDITION0 conditions[2];
    WCHAR *sddl = join(L"D:(A;;CC;;;", sid, L")", END);
    DWORD failed = 0;
    int i;
    if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(sddl, SDDL_REVISION_1, &descriptor, NULL))
        return GetLastError();
    blob.size = GetSecurityDescriptorLength(descriptor);
    blob.data = (UINT8 *)descriptor;
    memset(conditions, 0, sizeof conditions);
    conditions[0].fieldKey = condition_user_id;
    conditions[0].matchType = FWP_MATCH_EQUAL;
    conditions[0].conditionValue.type = FWP_SECURITY_DESCRIPTOR_TYPE;
    conditions[0].conditionValue.sd = &blob;
    conditions[1].fieldKey = condition_flags;
    conditions[1].matchType = FWP_MATCH_FLAGS_NONE_SET;
    conditions[1].conditionValue.type = FWP_UINT32;
    conditions[1].conditionValue.uint32 = FWP_CONDITION_FLAG_IS_LOOPBACK;
    for (i = 0; i < BLOCK_LAYER_COUNT; i++) {
        FWPM_FILTER0 filter;
        UINT64 id = 0;
        DWORD error;
        LARGE_INTEGER started = now_counter();
        memset(&filter, 0, sizeof filter);
        filter.displayData.name = L"nnprobe block";
        filter.layerKey = *block_layers[i];
        filter.action.type = FWP_ACTION_BLOCK;
        filter.weight.type = FWP_EMPTY;
        filter.numFilterConditions = 2;
        filter.filterCondition = conditions;
        error = FwpmFilterAdd0(engine, &filter, NULL, &id);
        say("MEASURE %s filter %s: %s, %.2f ms", label, block_layer_names[i], error == 0 ? "added" : error_text(error),
            elapsed_ms(started));
        if (error != 0)
            failed = error;
    }
    LocalFree(descriptor);
    return failed;
}

/* FwpmSubLayerSetSecurityInfoByKey0, which mingw-w64's fwpmu.h does not
 * declare, looked up in fwpuclnt by name. */
typedef DWORD(WINAPI *set_sublayer_security_call)(HANDLE, const GUID *, SECURITY_INFORMATION, const SID *, const SID *,
                                                  const ACL *, const ACL *);

static DWORD set_sublayer_security(HANDLE engine, const GUID *key, const ACL *dacl)
{
    HMODULE module = GetModuleHandleW(L"fwpuclnt.dll");
    set_sublayer_security_call call =
        module != NULL ? (set_sublayer_security_call)(any_function)GetProcAddress(module, "FwpmSubLayerSetSecurityInfoByKey0")
                       : NULL;
    if (call == NULL)
        return ERROR_PROC_NOT_FOUND;
    return call(engine, key, DACL_SECURITY_INFORMATION, NULL, NULL, dacl, NULL);
}

/* Grant SID, in its DACL, the WFP rights to open a session and add
 * filters: open on the engine, add on the filters' container and add-link
 * on the blocking layers and the universal sublayer; each object's old
 * DACL goes in OLD, for revoke_filter_rights. */
#define FILTER_RIGHT_OBJECTS (2 + BLOCK_LAYER_COUNT + 1)

struct old_dacls {
    PSECURITY_DESCRIPTOR descriptors[FILTER_RIGHT_OBJECTS];
    PACL dacls[FILTER_RIGHT_OBJECTS];
};

static PACL with_entry(PACL dacl, PSID sid, DWORD rights, DWORD *error)
{
    EXPLICIT_ACCESSW access;
    PACL updated = NULL;
    memset(&access, 0, sizeof access);
    access.grfAccessPermissions = rights;
    access.grfAccessMode = GRANT_ACCESS;
    access.grfInheritance = NO_INHERITANCE;
    access.Trustee.TrusteeForm = TRUSTEE_IS_SID;
    access.Trustee.TrusteeType = TRUSTEE_IS_UNKNOWN;
    access.Trustee.ptstrName = (LPWSTR)sid;
    *error = SetEntriesInAclW(1, &access, dacl, &updated);
    return *error == 0 ? updated : NULL;
}

static DWORD grant_filter_rights(HANDLE engine, PSID sid, struct old_dacls *old)
{
    DWORD error;
    PACL updated;
    int i;
    memset(old, 0, sizeof *old);
    error = FwpmEngineGetSecurityInfo0(engine, DACL_SECURITY_INFORMATION, NULL, NULL, &old->dacls[0], NULL,
                                       &old->descriptors[0]);
    if (error == 0 && (updated = with_entry(old->dacls[0], sid, ACTRL_OPEN | ACTRL_READ | ACTRL_ENUM | ACTRL_BEGIN_READ_TXN | ACTRL_BEGIN_WRITE_TXN, &error)) != NULL)
        error = FwpmEngineSetSecurityInfo0(engine, DACL_SECURITY_INFORMATION, NULL, NULL, updated, NULL);
    say("MEASURE wfp grant engine: %s", error == 0 ? "granted" : error_text(error));
    if (error != 0)
        return error;
    error = FwpmFilterGetSecurityInfoByKey0(engine, NULL, DACL_SECURITY_INFORMATION, NULL, NULL, &old->dacls[1], NULL,
                                            &old->descriptors[1]);
    if (error == 0 && (updated = with_entry(old->dacls[1], sid, ACTRL_ADD | ACTRL_READ, &error)) != NULL)
        error = FwpmFilterSetSecurityInfoByKey0(engine, NULL, DACL_SECURITY_INFORMATION, NULL, NULL, updated, NULL);
    say("MEASURE wfp grant filters: %s", error == 0 ? "granted" : error_text(error));
    if (error != 0)
        return error;
    for (i = 0; i < BLOCK_LAYER_COUNT; i++) {
        error = FwpmLayerGetSecurityInfoByKey0(engine, block_layers[i], DACL_SECURITY_INFORMATION, NULL, NULL,
                                               &old->dacls[2 + i], NULL, &old->descriptors[2 + i]);
        if (error == 0 && (updated = with_entry(old->dacls[2 + i], sid, ACTRL_ADD_LINK | ACTRL_READ, &error)) != NULL)
            error = FwpmLayerSetSecurityInfoByKey0(engine, block_layers[i], DACL_SECURITY_INFORMATION, NULL, NULL,
                                                   updated, NULL);
        say("MEASURE wfp grant layer %s: %s", block_layer_names[i], error == 0 ? "granted" : error_text(error));
        if (error != 0)
            return error;
    }
    error = FwpmSubLayerGetSecurityInfoByKey0(engine, &sublayer_universal, DACL_SECURITY_INFORMATION, NULL, NULL,
                                              &old->dacls[2 + BLOCK_LAYER_COUNT], NULL,
                                              &old->descriptors[2 + BLOCK_LAYER_COUNT]);
    if (error == 0
        && (updated = with_entry(old->dacls[2 + BLOCK_LAYER_COUNT], sid, ACTRL_ADD_LINK | ACTRL_READ, &error)) != NULL)
        error = set_sublayer_security(engine, &sublayer_universal, updated);
    say("MEASURE wfp grant universal sublayer: %s", error == 0 ? "granted" : error_text(error));
    return error;
}

static void revoke_filter_rights(HANDLE engine, struct old_dacls *old)
{
    DWORD error;
    int i;
    if (old->descriptors[2 + BLOCK_LAYER_COUNT] != NULL) {
        error = set_sublayer_security(engine, &sublayer_universal, old->dacls[2 + BLOCK_LAYER_COUNT]);
        say("WFP restore universal sublayer: %s", error == 0 ? "restored" : error_text(error));
    }
    for (i = 0; i < BLOCK_LAYER_COUNT; i++)
        if (old->descriptors[2 + i] != NULL) {
            error = FwpmLayerSetSecurityInfoByKey0(engine, block_layers[i], DACL_SECURITY_INFORMATION, NULL, NULL,
                                                   old->dacls[2 + i], NULL);
            say("WFP restore layer %s: %s", block_layer_names[i], error == 0 ? "restored" : error_text(error));
        }
    if (old->descriptors[1] != NULL) {
        error = FwpmFilterSetSecurityInfoByKey0(engine, NULL, DACL_SECURITY_INFORMATION, NULL, NULL, old->dacls[1], NULL);
        say("WFP restore filters: %s", error == 0 ? "restored" : error_text(error));
    }
    if (old->descriptors[0] != NULL) {
        error = FwpmEngineSetSecurityInfo0(engine, DACL_SECURITY_INFORMATION, NULL, NULL, old->dacls[0], NULL);
        say("WFP restore engine: %s", error == 0 ? "restored" : error_text(error));
    }
    for (i = 0; i < FILTER_RIGHT_OBJECTS; i++)
        if (old->descriptors[i] != NULL)
            FwpmFreeMemory0((void **)&old->descriptors[i]);
}

/* Confirm each blocking layer's key names a layer, by its name. */
static void describe_layers(HANDLE engine)
{
    int i;
    for (i = 0; i < BLOCK_LAYER_COUNT; i++) {
        FWPM_LAYER0 *layer = NULL;
        DWORD error = FwpmLayerGetByKey0(engine, block_layers[i], &layer);
        say("MEASURE wfp layer %s: %s", block_layer_names[i],
            error == 0 ? utf8(layer->displayData.name) : error_text(error));
        if (layer != NULL)
            FwpmFreeMemory0((void **)&layer);
    }
}

static int wfp_command(const WCHAR *root, const WCHAR *target)
{
    struct layout l;
    char *config;
    DWORD error;
    HANDLE engine;
    char *sid;
    LARGE_INTEGER started;
    layout_of(root, &l);
    config = read_file(l.config, &error);
    if (config == NULL) {
        say("WFP configuration: %s", error_text(error));
        return 1;
    }
    sid = config_value(config, wcscmp(target, L"group") == 0 ? "group-sid" : "slot1-sid");
    started = now_counter();
    engine = open_dynamic_engine(&error);
    say("MEASURE wfp-%s administrator's dynamic session: %s, %.2f ms", utf8(target),
        engine != NULL ? "open" : error_text(error), elapsed_ms(started));
    if (engine == NULL)
        return 0;
    describe_layers(engine);
    started = now_counter();
    error = add_block_filters(engine, widen(sid), wcscmp(target, L"group") == 0 ? "wfp-group" : "wfp-slot");
    say("MEASURE wfp-%s filters for %s: %s, %.2f ms in all", utf8(target), sid, error == 0 ? "added" : error_text(error),
        elapsed_ms(started));
    phase_command(root, L"net");
    started = now_counter();
    error = FwpmEngineClose0(engine);
    say("MEASURE wfp-%s session closed, its filters with it: %s, %.2f ms", utf8(target),
        error == 0 ? "closed" : error_text(error), elapsed_ms(started));
    return 0;
}

static int wfp_self_command(const WCHAR *root)
{
    struct layout l;
    struct old_dacls old;
    char *config;
    DWORD error;
    HANDLE engine;
    PSID service;
    layout_of(root, &l);
    config = read_file(l.config, &error);
    if (config == NULL)
        return 1;
    service = sid_of(widen(config_value(config, "service-sid")));
    engine = open_dynamic_engine(&error);
    if (engine == NULL || service == NULL) {
        say("WFP administrator's session: %s", error_text(error));
        return 0;
    }
    error = grant_filter_rights(engine, service, &old);
    if (error == 0)
        phase_command(root, L"wfp-self");
    revoke_filter_rights(engine, &old);
    FwpmEngineClose0(engine);
    return 0;
}

/* ------------------------------------------------------------------------
 * The escapes, as an administrator
 * ------------------------------------------------------------------------ */

/* Delete the task an escape check makes, as an administrator. */
static void delete_task(const char *label)
{
    STARTUPINFOW startup;
    PROCESS_INFORMATION process;
    WCHAR *command = join(L"schtasks.exe /delete /tn ", TASK_NAME, L" /f", END);
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    if (CreateProcessW(NULL, command, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &startup, &process)) {
        DWORD code = 0;
        WaitForSingleObject(process.hProcess, INFINITE);
        GetExitCodeProcess(process.hProcess, &code);
        say("%s deleting the task %s: schtasks exited %lu", label, utf8(TASK_NAME), (unsigned long)code);
        CloseHandle(process.hThread);
        CloseHandle(process.hProcess);
    }
}

static int escapes_command(const WCHAR *root)
{
    struct layout l;
    char *list;
    char *at;
    DWORD error;
    layout_of(root, &l);
    list = read_file(join(l.service_dir, L"\\escaped", END), &error);
    if (list == NULL) {
        say("MEASURE escapes: no process IDs were recorded, %s", error_text(error));
    } else {
        for (at = strtok(list, "\n"); at != NULL; at = strtok(NULL, "\n")) {
            char way[64];
            unsigned long pid = 0;
            if (sscanf(at, "%63s %lu", way, &pid) != 2 || pid == 0)
                continue;
            {
                HANDLE process = OpenProcess(SYNCHRONIZE | PROCESS_TERMINATE | PROCESS_QUERY_LIMITED_INFORMATION, FALSE,
                                             pid);
                if (process == NULL) {
                    say("MEASURE escape %s process %lu after its build: gone (%s)", way, pid,
                        error_text(GetLastError()));
                    continue;
                }
                if (WaitForSingleObject(process, 0) == WAIT_TIMEOUT) {
                    say("MEASURE escape %s process %lu after its build and the service's end: still running", way, pid);
                    say("ESCAPES ending process %lu: %s", pid,
                        TerminateProcess(process, 1) ? "ended" : error_text(GetLastError()));
                } else {
                    say("MEASURE escape %s process %lu after its build: ended", way, pid);
                }
                CloseHandle(process);
            }
        }
    }
    delete_task("ESCAPES");
    return 0;
}

/* ------------------------------------------------------------------------
 * The service: starting builds
 * ------------------------------------------------------------------------ */

struct slot {
    char *name;
    char *password;
    char *sid;
    char *build;
    HANDLE token;
    PSID sid_binary;
};

struct service_context {
    char *config;
    char *helper;
    char *probe;
    char *service_only;
    char *slot_only;
    WCHAR *service_dir;
    struct slot slots[SLOT_COUNT];
    PSID own_sid;
    unsigned int runs;
    /* The service's unnamed window station, made once, for the direct
     * starts, or NULL. */
    HWINSTA station;
    WCHAR *station_name;
    struct user32 user32;
    int have_user32;
};

/* A pipe's reader, gathering what comes until the pipe ends. */
struct reader {
    HANDLE pipe;
    HANDLE thread;
    char *bytes;
    size_t used;
    size_t capacity;
    CRITICAL_SECTION lock;
};

static DWORD WINAPI read_pipe(LPVOID parameter)
{
    struct reader *r = parameter;
    char chunk[4096];
    DWORD got = 0;
    while (ReadFile(r->pipe, chunk, sizeof chunk, &got, NULL) && got > 0) {
        EnterCriticalSection(&r->lock);
        if (r->used + got + 1 > r->capacity) {
            size_t capacity = (r->used + got + 1) * 2;
            char *larger = realloc(r->bytes, capacity);
            if (larger != NULL) {
                r->bytes = larger;
                r->capacity = capacity;
            }
        }
        if (r->used + got + 1 <= r->capacity) {
            memcpy(r->bytes + r->used, chunk, got);
            r->used += got;
            r->bytes[r->used] = '\0';
        }
        LeaveCriticalSection(&r->lock);
    }
    return 0;
}

static void start_reader(struct reader *r, HANDLE pipe)
{
    r->pipe = pipe;
    r->capacity = 4096;
    r->bytes = take(r->capacity);
    r->used = 0;
    InitializeCriticalSection(&r->lock);
    r->thread = CreateThread(NULL, 0, read_pipe, r, 0, NULL);
}

static char *reader_text(struct reader *r)
{
    char *copy;
    EnterCriticalSection(&r->lock);
    copy = take(r->used + 1);
    memcpy(copy, r->bytes, r->used);
    LeaveCriticalSection(&r->lock);
    return copy;
}

/* What one build came to. */
struct build {
    DWORD start_error;
    DWORD exit_code;
    int timed_out;
    char *status;
    char *log;
    /* How long after the helper's end its log ended, or -1 when it had
     * not by LOG_END_WAIT_MS. */
    long log_end_ms;
};

/* The plan nova-nix-sandbox reads, its tokens each its length, a colon,
 * its bytes and a NUL; answers its length in LENGTH. */
static char *render_plan(const struct slot *slot, const char *desktop, const char *const *environment, size_t entries,
                         size_t *length)
{
    const char *tokens[64];
    size_t count = 0;
    char token_value[32];
    size_t i;
    size_t body_length = 0;
    char *body;
    char *plan;
    char header[64];
    snprintf(token_value, sizeof token_value, "%lu", (unsigned long)(ULONG_PTR)slot->token);
    tokens[count++] = "backend";
    tokens[count++] = "windows";
    tokens[count++] = "build-dir";
    tokens[count++] = slot->build;
    tokens[count++] = "token";
    tokens[count++] = token_value;
    tokens[count++] = "slot-sid";
    tokens[count++] = slot->sid;
    tokens[count++] = "desktop";
    tokens[count++] = desktop;
    tokens[count++] = "memory-cap";
    tokens[count++] = MEMORY_CAP_BYTES;
    for (i = 0; i < entries && count + 2 <= sizeof tokens / sizeof tokens[0]; i++) {
        tokens[count++] = "env";
        tokens[count++] = environment[i];
    }
    for (i = 0; i < count; i++)
        body_length += 24 + strlen(tokens[i]);
    body = take(body_length);
    body_length = 0;
    for (i = 0; i < count; i++) {
        int written = sprintf(body + body_length, "%lu:%s", (unsigned long)strlen(tokens[i]), tokens[i]);
        body_length += (size_t)written;
        body[body_length++] = '\0';
    }
    snprintf(header, sizeof header, "%s%lu\n", PLAN_VERSION_LINE, (unsigned long)body_length);
    plan = take(strlen(header) + body_length);
    memcpy(plan, header, strlen(header));
    memcpy(plan + strlen(header), body, body_length);
    *length = strlen(header) + body_length;
    return plan;
}

/* The environment a builder gets: Windows' directory, a PATH of System32
 * and PowerShell's, and its build directory as its temporary directory
 * and profile. */
static const char *const *builder_environment(const struct slot *slot, size_t *count)
{
    static const char *entries[8];
    char system_root[MAX_PATH] = "C:\\Windows";
    char *buffer;
    GetEnvironmentVariableA("SystemRoot", system_root, sizeof system_root);
    buffer = take(8 * 512);
    snprintf(buffer, 512, "SystemRoot=%s", system_root);
    snprintf(buffer + 512, 512, "PATH=%s\\System32;%s\\System32\\WindowsPowerShell\\v1.0", system_root, system_root);
    snprintf(buffer + 1024, 512, "TEMP=%s", slot->build);
    snprintf(buffer + 1536, 512, "TMP=%s", slot->build);
    snprintf(buffer + 2048, 512, "USERPROFILE=%s", slot->build);
    snprintf(buffer + 2560, 512, "APPDATA=%s", slot->build);
    snprintf(buffer + 3072, 512, "LOCALAPPDATA=%s", slot->build);
    entries[0] = buffer;
    entries[1] = buffer + 512;
    entries[2] = buffer + 1024;
    entries[3] = buffer + 1536;
    entries[4] = buffer + 2048;
    entries[5] = buffer + 2560;
    entries[6] = buffer + 3072;
    *count = 7;
    return entries;
}

/* Run a build through nova-nix-sandbox: COMMAND, the builder's command
 * line, in SLOT, on a window station named for the run.  The helper gets
 * the slot's token inherited, and the plan, the lifeline, the status pipe
 * and the log pipe as nova-nix would give them. */
static void through_helper(struct service_context *c, const struct slot *slot, const WCHAR *command,
                           struct build *result)
{
    SECURITY_ATTRIBUTES inheritable;
    HANDLE in_read = NULL;
    HANDLE in_write = NULL;
    HANDLE status_read = NULL;
    HANDLE status_write = NULL;
    HANDLE log_read = NULL;
    HANDLE log_write = NULL;
    HANDLE inherited[4];
    SIZE_T list_size = 0;
    LPPROC_THREAD_ATTRIBUTE_LIST list;
    STARTUPINFOEXW startup;
    PROCESS_INFORMATION process;
    struct reader status_reader;
    struct reader log_reader;
    char desktop[96];
    size_t plan_length;
    char *plan;
    size_t entries;
    const char *const *environment = builder_environment(slot, &entries);
    WCHAR *line;
    DWORD written;
    WCHAR empty_environment[2] = {0, 0};
    ULONGLONG ended;
    memset(result, 0, sizeof *result);
    result->log_end_ms = -1;
    c->runs++;
    snprintf(desktop, sizeof desktop, "nnprobe-%lu-%u\\build", (unsigned long)GetCurrentProcessId(), c->runs);
    plan = render_plan(slot, desktop, environment, entries, &plan_length);
    inheritable.nLength = sizeof inheritable;
    inheritable.lpSecurityDescriptor = NULL;
    inheritable.bInheritHandle = TRUE;
    if (!CreatePipe(&in_read, &in_write, &inheritable, 0) || !CreatePipe(&status_read, &status_write, &inheritable, 0)
        || !CreatePipe(&log_read, &log_write, &inheritable, 0)) {
        result->start_error = GetLastError();
        return;
    }
    SetHandleInformation(in_write, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(status_read, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(log_read, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(slot->token, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    inherited[0] = in_read;
    inherited[1] = status_write;
    inherited[2] = log_write;
    inherited[3] = slot->token;
    InitializeProcThreadAttributeList(NULL, 1, 0, &list_size);
    list = take(list_size);
    InitializeProcThreadAttributeList(list, 1, 0, &list_size);
    UpdateProcThreadAttribute(list, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherited, sizeof inherited, NULL, NULL);
    memset(&startup, 0, sizeof startup);
    startup.StartupInfo.cb = sizeof startup;
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdInput = in_read;
    startup.StartupInfo.hStdOutput = status_write;
    startup.StartupInfo.hStdError = log_write;
    startup.lpAttributeList = list;
    line = join(L"\"", widen(c->helper), L"\" run ", command, END);
    if (!CreateProcessW(NULL, line, NULL, NULL, TRUE,
                        CREATE_UNICODE_ENVIRONMENT | EXTENDED_STARTUPINFO_PRESENT | CREATE_NO_WINDOW,
                        empty_environment, widen(slot->build), &startup.StartupInfo, &process)) {
        result->start_error = GetLastError();
        SetHandleInformation(slot->token, HANDLE_FLAG_INHERIT, 0);
        DeleteProcThreadAttributeList(list);
        return;
    }
    SetHandleInformation(slot->token, HANDLE_FLAG_INHERIT, 0);
    DeleteProcThreadAttributeList(list);
    CloseHandle(in_read);
    CloseHandle(status_write);
    CloseHandle(log_write);
    CloseHandle(process.hThread);
    start_reader(&status_reader, status_read);
    start_reader(&log_reader, log_read);
    WriteFile(in_write, plan, (DWORD)plan_length, &written, NULL);
    if (WaitForSingleObject(process.hProcess, BUILD_TIMEOUT_MS) != WAIT_OBJECT_0) {
        result->timed_out = 1;
        CloseHandle(in_write);
        in_write = NULL;
        if (WaitForSingleObject(process.hProcess, LOG_END_WAIT_MS) != WAIT_OBJECT_0)
            TerminateProcess(process.hProcess, 1);
        WaitForSingleObject(process.hProcess, INFINITE);
    }
    ended = GetTickCount64();
    GetExitCodeProcess(process.hProcess, &result->exit_code);
    if (WaitForSingleObject(log_reader.thread, LOG_END_WAIT_MS) == WAIT_OBJECT_0)
        result->log_end_ms = (long)(GetTickCount64() - ended);
    WaitForSingleObject(status_reader.thread, LOG_END_WAIT_MS);
    result->status = reader_text(&status_reader);
    result->log = reader_text(&log_reader);
    if (in_write != NULL)
        CloseHandle(in_write);
    CloseHandle(process.hProcess);
}

/* Say what a build came to, under LABEL. */
static void say_build(const char *label, const struct build *b)
{
    if (b->start_error != 0) {
        say("MEASURE %s: the helper did not start, %s", label, error_text(b->start_error));
        return;
    }
    say("MEASURE %s: helper exit 0x%lx%s, status \"%s\", log ended %s", label, (unsigned long)b->exit_code,
        b->timed_out ? " after the build timed out" : "", b->status, b->log_end_ms >= 0 ? "with the helper" : "NOT within the wait after the helper ended");
    if (b->log_end_ms >= 0)
        say("MEASURE %s log ended %ld ms after the helper", label, b->log_end_ms);
    say_lines(label, b->log);
}

/* The builder's command line for an operation of this program's. */
static WCHAR *child_command(const char *image, const WCHAR *operation)
{
    return join(L"\"", widen(image), L"\" child ", operation, END);
}

/* ------------------------------------------------------------------------
 * The service: a direct start on its own window station
 * ------------------------------------------------------------------------ */

/* Start COMMAND in SLOT with CreateProcessAsUserW from this process, on a
 * desktop of the service's unnamed window station named for the run,
 * granting the service and the slot: the start nova-nix-sandbox would make
 * if a service could not name a window station.  It has no job and no
 * pseudoconsole; it shows only whether the builder starts and where. */
static void direct_start(struct service_context *c, const struct slot *slot, const WCHAR *command, struct build *result)
{
    PSID grantees[2];
    SECURITY_ATTRIBUTES attributes;
    HWINSTA previous;
    HDESK desktop;
    WCHAR *desktop_name;
    SECURITY_ATTRIBUTES inheritable;
    HANDLE log_read = NULL;
    HANDLE log_write = NULL;
    STARTUPINFOW startup;
    PROCESS_INFORMATION process;
    struct reader log_reader;
    memset(result, 0, sizeof *result);
    result->log_end_ms = -1;
    result->status = "";
    result->log = "";
    if (c->station == NULL) {
        result->start_error = ERROR_INVALID_HANDLE;
        return;
    }
    c->runs++;
    grantees[0] = c->own_sid;
    grantees[1] = slot->sid_binary;
    attributes.nLength = sizeof attributes;
    attributes.lpSecurityDescriptor = grant_all(grantees, 2);
    attributes.bInheritHandle = FALSE;
    desktop_name = join(L"nnprobe-direct-", decimal(c->runs), END);
    previous = c->user32.get_station();
    c->user32.set_station(c->station);
    desktop = c->user32.create_desktop(desktop_name, NULL, NULL, 0, GENERIC_ALL, &attributes);
    if (desktop == NULL)
        result->start_error = GetLastError();
    c->user32.set_station(previous);
    if (desktop == NULL)
        return;
    inheritable.nLength = sizeof inheritable;
    inheritable.lpSecurityDescriptor = NULL;
    inheritable.bInheritHandle = TRUE;
    CreatePipe(&log_read, &log_write, &inheritable, 0);
    SetHandleInformation(log_read, HANDLE_FLAG_INHERIT, 0);
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    startup.lpDesktop = join(c->station_name, L"\\", desktop_name, END);
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = NULL;
    startup.hStdOutput = log_write;
    startup.hStdError = log_write;
    if (!CreateProcessAsUserW(slot->token, NULL, (LPWSTR)join(command, END), NULL, NULL, TRUE, DETACHED_PROCESS, NULL,
                              widen(slot->build), &startup, &process)) {
        result->start_error = GetLastError();
        CloseHandle(log_write);
        CloseHandle(log_read);
        c->user32.close_desktop(desktop);
        return;
    }
    CloseHandle(log_write);
    start_reader(&log_reader, log_read);
    if (WaitForSingleObject(process.hProcess, BUILD_TIMEOUT_MS) != WAIT_OBJECT_0) {
        result->timed_out = 1;
        TerminateProcess(process.hProcess, 1);
    }
    GetExitCodeProcess(process.hProcess, &result->exit_code);
    if (WaitForSingleObject(log_reader.thread, LOG_END_WAIT_MS) == WAIT_OBJECT_0)
        result->log_end_ms = 0;
    result->log = reader_text(&log_reader);
    CloseHandle(process.hThread);
    CloseHandle(process.hProcess);
    c->user32.close_desktop(desktop);
}

/* ------------------------------------------------------------------------
 * The service: its phases
 * ------------------------------------------------------------------------ */

static int read_context(struct service_context *c, const WCHAR *root)
{
    struct layout l;
    DWORD error;
    int i;
    layout_of(root, &l);
    c->service_dir = l.service_dir;
    c->config = read_file(l.config, &error);
    say("MEASURE service reads its configuration, which grants it alone besides SYSTEM and Administrators: %s",
        c->config != NULL ? "read" : error_text(error));
    if (c->config == NULL)
        return 0;
    c->helper = config_value(c->config, "helper");
    c->probe = config_value(c->config, "probe");
    c->service_only = config_value(c->config, "service-only");
    c->slot_only = config_value(c->config, "slot-only");
    for (i = 0; i < SLOT_COUNT; i++) {
        char key[32];
        struct slot *s = &c->slots[i];
        snprintf(key, sizeof key, "slot%d-name", i + 1);
        s->name = config_value(c->config, key);
        snprintf(key, sizeof key, "slot%d-password", i + 1);
        s->password = config_value(c->config, key);
        snprintf(key, sizeof key, "slot%d-sid", i + 1);
        s->sid = config_value(c->config, key);
        snprintf(key, sizeof key, "slot%d-build", i + 1);
        s->build = config_value(c->config, key);
        if (s->name == NULL || s->password == NULL || s->sid == NULL || s->build == NULL)
            return 0;
        s->sid_binary = sid_of(widen(s->sid));
    }
    return 1;
}

/* Log each slot's account on as a batch job, as the service would. */
static int log_slots_on(struct service_context *c)
{
    int i;
    int all = 1;
    for (i = 0; i < SLOT_COUNT; i++) {
        struct slot *s = &c->slots[i];
        if (!LogonUserW(widen(s->name), L".", widen(s->password), LOGON32_LOGON_BATCH, LOGON32_PROVIDER_DEFAULT,
                        &s->token)) {
            say("MEASURE service LogonUserW %s (batch): %s", s->name, error_text(GetLastError()));
            s->token = NULL;
            all = 0;
            continue;
        }
        say("MEASURE service LogonUserW %s (batch): logged on", s->name);
    }
    return all;
}

static void own_token(struct service_context *c, int describe)
{
    HANDLE token = NULL;
    union {
        TOKEN_USER user;
        BYTE bytes[sizeof(TOKEN_USER) + SECURITY_MAX_SID_SIZE];
    } user;
    DWORD size = 0;
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY | TOKEN_ADJUST_PRIVILEGES, &token)) {
        say("MEASURE service token: %s", error_text(GetLastError()));
        return;
    }
    if (describe)
        describe_token(token, "service token");
    enable_start_privileges(token, "service");
    if (GetTokenInformation(token, TokenUser, &user, sizeof user, &size)) {
        DWORD length = GetLengthSid(user.user.User.Sid);
        c->own_sid = take(length);
        CopySid(length, c->own_sid, user.user.User.Sid);
    }
    if (describe)
        describe_token(token, "service token, privileges enabled");
    CloseHandle(token);
}

/* The window station checks: a named one, made only if none of the name
 * exists, with a desktop on it; then the unnamed one, twice. */
static void station_checks(struct service_context *c)
{
    PSID grantees[3];
    SECURITY_ATTRIBUTES attributes;
    HWINSTA named;
    HWINSTA again;
    WCHAR *name = join(L"nnprobe-named-", decimal(GetCurrentProcessId()), END);
    c->have_user32 = load_user32(&c->user32);
    say("MEASURE service loads user32: %s", c->have_user32 ? "loaded" : "failed");
    if (!c->have_user32)
        return;
    grantees[0] = c->own_sid;
    grantees[1] = c->slots[0].sid_binary;
    grantees[2] = c->slots[1].sid_binary;
    attributes.nLength = sizeof attributes;
    attributes.lpSecurityDescriptor = grant_all(grantees, 3);
    attributes.bInheritHandle = FALSE;
    say("MEASURE service's own window station: %s", utf8(object_name(&c->user32, c->user32.get_station())));
    named = c->user32.create_station(name, CWF_CREATE_ONLY, WINSTA_ALL_ACCESS | READ_CONTROL, &attributes);
    say("MEASURE service CreateWindowStationW named %s: %s", utf8(name), named != NULL ? "made" : error_text(GetLastError()));
    if (named != NULL) {
        HWINSTA previous = c->user32.get_station();
        HDESK desktop;
        c->user32.set_station(named);
        desktop = c->user32.create_desktop(L"build", NULL, NULL, 0, GENERIC_ALL, &attributes);
        say("MEASURE service CreateDesktopW on the named station: %s", desktop != NULL ? "made" : error_text(GetLastError()));
        c->user32.set_station(previous);
        if (desktop != NULL)
            c->user32.close_desktop(desktop);
        c->user32.close_station(named);
    }
    c->station = c->user32.create_station(NULL, CWF_CREATE_ONLY, WINSTA_ALL_ACCESS | READ_CONTROL, &attributes);
    say("MEASURE service CreateWindowStationW unnamed, create-only: %s",
        c->station != NULL ? "made" : error_text(GetLastError()));
    if (c->station != NULL) {
        c->station_name = object_name(&c->user32, c->station);
        say("MEASURE service's unnamed window station is named %s", utf8(c->station_name));
        again = c->user32.create_station(NULL, CWF_CREATE_ONLY, WINSTA_ALL_ACCESS, &attributes);
        say("MEASURE service CreateWindowStationW unnamed again, create-only: %s",
            again != NULL ? utf8(object_name(&c->user32, again)) : error_text(GetLastError()));
        if (again != NULL)
            c->user32.close_station(again);
        again = c->user32.create_station(NULL, 0, WINSTA_ALL_ACCESS, &attributes);
        say("MEASURE service CreateWindowStationW unnamed again, not create-only: %s",
            again != NULL ? utf8(object_name(&c->user32, again)) : error_text(GetLastError()));
        if (again != NULL)
            c->user32.close_station(again);
        return;
    }
    {
        /* The unnamed window station is the one the service already runs
         * on; the fallback is to grant the slots that one. */
        WCHAR *own = object_name(&c->user32, c->user32.get_station());
        HWINSTA opened = c->user32.open_station(own, FALSE, WINSTA_ALL_ACCESS | READ_CONTROL | WRITE_DAC);
        PSECURITY_DESCRIPTOR descriptor = NULL;
        PACL dacl = NULL;
        PACL updated = NULL;
        DWORD error;
        int i;
        say("MEASURE service OpenWindowStationW its own, for WRITE_DAC: %s", opened != NULL ? "opened" : error_text(GetLastError()));
        if (opened == NULL)
            return;
        error = GetSecurityInfo(opened, SE_WINDOW_OBJECT, DACL_SECURITY_INFORMATION, NULL, NULL, &dacl, NULL, &descriptor);
        if (error == 0)
            say("MEASURE service's own window station's DACL before: %s", descriptor_text(descriptor, DACL_SECURITY_INFORMATION));
        for (i = 0; i < SLOT_COUNT && error == 0; i++) {
            updated = with_entry(dacl, c->slots[i].sid_binary, GENERIC_ALL, &error);
            dacl = updated;
        }
        if (error == 0)
            error = SetSecurityInfo(opened, SE_WINDOW_OBJECT, DACL_SECURITY_INFORMATION, NULL, NULL, dacl, NULL);
        say("MEASURE service grants the slots its own window station: %s", error == 0 ? "granted" : error_text(error));
        if (error == 0) {
            c->station = opened;
            c->station_name = own;
        }
    }
}

/* Run the helper's probe, as nova-nix would. */
static void helper_probe(struct service_context *c)
{
    SECURITY_ATTRIBUTES inheritable;
    HANDLE out_read = NULL;
    HANDLE out_write = NULL;
    STARTUPINFOW startup;
    PROCESS_INFORMATION process;
    struct reader reader;
    WCHAR empty_environment[2] = {0, 0};
    DWORD code = 0;
    inheritable.nLength = sizeof inheritable;
    inheritable.lpSecurityDescriptor = NULL;
    inheritable.bInheritHandle = TRUE;
    CreatePipe(&out_read, &out_write, &inheritable, 0);
    SetHandleInformation(out_read, HANDLE_FLAG_INHERIT, 0);
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdOutput = out_write;
    startup.hStdError = out_write;
    if (!CreateProcessW(NULL, join(L"\"", widen(c->helper), L"\" probe", END), NULL, NULL, TRUE,
                        CREATE_UNICODE_ENVIRONMENT | CREATE_NO_WINDOW, empty_environment, NULL, &startup, &process)) {
        say("MEASURE helper probe as the service: did not start, %s", error_text(GetLastError()));
        return;
    }
    CloseHandle(out_write);
    start_reader(&reader, out_read);
    WaitForSingleObject(process.hProcess, BUILD_TIMEOUT_MS);
    GetExitCodeProcess(process.hProcess, &code);
    WaitForSingleObject(reader.thread, LOG_END_WAIT_MS);
    say("MEASURE helper probe as the service: exit %lu, wrote \"%s\"", (unsigned long)code, reader_text(&reader));
    CloseHandle(process.hThread);
    CloseHandle(process.hProcess);
}

/* Record a process a build started outside its job, from the line its
 * marker holds, for the escapes command. */
static void record_escape(struct service_context *c, const char *way, const char *log)
{
    const char *marker = log != NULL ? strstr(log, "escaped pid ") : NULL;
    unsigned long pid = 0;
    char line[128];
    WCHAR *path = join(c->service_dir, L"\\escaped", END);
    DWORD error;
    char *existing;
    char *combined;
    if (marker == NULL || sscanf(marker, "escaped pid %lu", &pid) != 1)
        return;
    snprintf(line, sizeof line, "%s %lu\n", way, pid);
    existing = read_file(path, &error);
    combined = take((existing != NULL ? strlen(existing) : 0) + strlen(line) + 1);
    if (existing != NULL)
        strcpy(combined, existing);
    strcat(combined, line);
    write_file(path, combined);
}

/* Listen on a loopback port of the service's, for a build to reach. */
static SOCKET loopback_listener(unsigned short *port)
{
    struct sockaddr_in address;
    int length = sizeof address;
    SOCKET listener = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    *port = 0;
    if (listener == INVALID_SOCKET)
        return listener;
    memset(&address, 0, sizeof address);
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(listener, (struct sockaddr *)&address, sizeof address) != 0 || listen(listener, SOMAXCONN) != 0
        || getsockname(listener, (struct sockaddr *)&address, &length) != 0) {
        closesocket(listener);
        return INVALID_SOCKET;
    }
    *port = ntohs(address.sin_port);
    return listener;
}

static void net_build(struct service_context *c, const char *label)
{
    struct build b;
    unsigned short port = 0;
    SOCKET listener = loopback_listener(&port);
    char *label_copy = take(strlen(label) + 1);
    strcpy(label_copy, label);
    through_helper(c, &c->slots[0], child_command(c->probe, join(L"net ", decimal(port), END)), &b);
    say_build(label_copy, &b);
    if (listener != INVALID_SOCKET)
        closesocket(listener);
}

static void service_wfp_self(struct service_context *c, const char *label)
{
    DWORD error;
    LARGE_INTEGER started = now_counter();
    HANDLE engine = open_dynamic_engine(&error);
    char text[128];
    say("MEASURE %s service FwpmEngineOpen0 (dynamic): %s, %.2f ms", label, engine != NULL ? "open" : error_text(error),
        elapsed_ms(started));
    if (engine == NULL)
        return;
    started = now_counter();
    error = add_block_filters(engine, widen(c->slots[0].sid), label);
    say("MEASURE %s service's filters for the slot: %s, %.2f ms in all", label, error == 0 ? "added" : error_text(error),
        elapsed_ms(started));
    if (error == 0) {
        snprintf(text, sizeof text, "%s net, the service's filters", label);
        net_build(c, text);
    }
    started = now_counter();
    error = FwpmEngineClose0(engine);
    say("MEASURE %s service's session closed, its filters with it: %s, %.2f ms", label,
        error == 0 ? "closed" : error_text(error), elapsed_ms(started));
    snprintf(text, sizeof text, "%s net, after the session closed", label);
    net_build(c, text);
}

static void main_phase(struct service_context *c)
{
    struct build b;
    int helper_runs;
    const char *images[3];
    const char *image_labels[3];
    int i;
    station_checks(c);
    helper_probe(c);

    through_helper(c, &c->slots[0], child_command(c->probe, L"identity"), &b);
    say_build("helper start slot1 identity", &b);
    helper_runs = b.start_error == 0 && strstr(b.log, "identity user") != NULL;
    say("MEASURE helper starts a builder for the service: %s", helper_runs ? "yes" : "no");
    if (c->station != NULL) {
        direct_start(c, &c->slots[0], child_command(c->probe, L"identity"), &b);
        say_build("direct start slot1 identity, on the unnamed station's desktop", &b);
        direct_start(c, &c->slots[1], child_command(c->probe, L"identity"), &b);
        say_build("direct start slot2 identity, on the unnamed station's desktop", &b);
    }

    images[0] = c->probe;
    image_labels[0] = "image the slot and the service may read";
    images[1] = c->service_only;
    image_labels[1] = "image only the service may read";
    images[2] = c->slot_only;
    image_labels[2] = "image only the slot may read";
    for (i = 0; i < 3; i++) {
        char label[160];
        snprintf(label, sizeof label, "helper start, %s", image_labels[i]);
        through_helper(c, &c->slots[0], child_command(images[i], L"identity"), &b);
        say_build(label, &b);
        if (c->station != NULL) {
            snprintf(label, sizeof label, "direct start, %s", image_labels[i]);
            direct_start(c, &c->slots[0], child_command(images[i], L"identity"), &b);
            say_build(label, &b);
        }
    }

    {
        static const WCHAR *const ways[] = {L"escape-task", L"escape-wmi", L"escape-hkcu"};
        for (i = 0; i < 3; i++) {
            char label[64];
            snprintf(label, sizeof label, "helper build %s", utf8(ways[i]));
            through_helper(c, &c->slots[0], child_command(c->probe, ways[i]), &b);
            say_build(label, &b);
            record_escape(c, utf8(ways[i]), b.log);
        }
    }

    net_build(c, "net, no rule");
    service_wfp_self(c, "wfp, default rights");
}

static void run_phase(const WCHAR *root, const WCHAR *phase)
{
    struct service_context c;
    WSADATA wsa;
    memset(&c, 0, sizeof c);
    WSAStartup(MAKEWORD(2, 2), &wsa);
    say("PHASE %s as %s", utf8(phase), utf8(SERVICE_ACCOUNT));
    own_token(&c, wcscmp(phase, L"main") == 0);
    if (!read_context(&c, root) || !log_slots_on(&c))
        return;
    if (wcscmp(phase, L"main") == 0)
        main_phase(&c);
    else if (wcscmp(phase, L"net") == 0)
        net_build(&c, "net");
    else if (wcscmp(phase, L"wfp-self") == 0)
        service_wfp_self(&c, "wfp, granted rights");
    say("PHASE %s done", utf8(phase));
}

/* ------------------------------------------------------------------------
 * The service's entry
 * ------------------------------------------------------------------------ */

static SERVICE_STATUS_HANDLE service_handle;
static SERVICE_STATUS service_status;
static WCHAR *service_root;

static void report_state(DWORD state)
{
    service_status.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
    service_status.dwCurrentState = state;
    service_status.dwControlsAccepted = state == SERVICE_RUNNING ? SERVICE_ACCEPT_STOP : 0;
    service_status.dwWin32ExitCode = 0;
    service_status.dwWaitHint = 0;
    SetServiceStatus(service_handle, &service_status);
}

static DWORD WINAPI service_control(DWORD control, DWORD type, LPVOID data, LPVOID context)
{
    (void)type;
    (void)data;
    (void)context;
    if (control == SERVICE_CONTROL_STOP) {
        report_state(SERVICE_STOP_PENDING);
        ExitProcess(0);
    }
    return NO_ERROR;
}

static void WINAPI service_main(DWORD count, LPWSTR *arguments)
{
    const WCHAR *phase = count > 1 ? arguments[count - 1] : L"main";
    WCHAR *log_path;
    service_handle = RegisterServiceCtrlHandlerExW(SERVICE_NAME, service_control, NULL);
    if (service_handle == NULL)
        return;
    report_state(SERVICE_RUNNING);
    log_path = join(service_root, L"\\service\\", phase, L".log", END);
    out = CreateFileW(log_path, GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    run_phase(service_root, phase);
    if (out != INVALID_HANDLE_VALUE)
        CloseHandle(out);
    report_state(SERVICE_STOPPED);
}

static int service_command(const WCHAR *root)
{
    SERVICE_TABLE_ENTRYW table[2];
    service_root = join(root, END);
    table[0].lpServiceName = SERVICE_NAME;
    table[0].lpServiceProc = service_main;
    table[1].lpServiceName = NULL;
    table[1].lpServiceProc = NULL;
    return StartServiceCtrlDispatcherW(table) ? 0 : (int)GetLastError();
}

/* ------------------------------------------------------------------------
 * The child: a builder's operations, and an escaped process's
 * ------------------------------------------------------------------------ */

static void child_identity(void)
{
    HANDLE token = NULL;
    union {
        TOKEN_USER user;
        BYTE bytes[sizeof(TOKEN_USER) + SECURITY_MAX_SID_SIZE];
    } user;
    DWORD size = 0;
    BOOL in_job = FALSE;
    struct user32 u;
    HANDLE console;
    if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)
        && GetTokenInformation(token, TokenUser, &user, sizeof user, &size))
        say("identity user %s", utf8(sid_text(user.user.User.Sid)));
    IsProcessInJob(GetCurrentProcess(), NULL, &in_job);
    say("identity in a job %s", in_job ? "yes" : "no");
    if (load_user32(&u)) {
        say("identity station %s", utf8(object_name(&u, u.get_station())));
        say("identity desktop %s", utf8(object_name(&u, u.thread_desktop(GetCurrentThreadId()))));
    }
    console = CreateFileW(L"CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
    say("identity console %s", console != INVALID_HANDLE_VALUE ? "yes" : "no");
    if (console != INVALID_HANDLE_VALUE)
        CloseHandle(console);
}

/* The result of one network check, as a line. */
static void net_line(const char *check, int ok, int error)
{
    if (ok)
        say("net %s ok", check);
    else
        say("net %s refused, error %d", check, error);
}

/* Connect a TCP socket to ADDRESS, waiting at most NET_TIMEOUT_MS;
 * answers 1, or 0 with the Winsock error. */
static int tcp_connect(const struct sockaddr *address, int length, int *error)
{
    SOCKET s = socket(address->sa_family, SOCK_STREAM, IPPROTO_TCP);
    u_long nonblocking = 1;
    fd_set writable;
    fd_set failed;
    struct timeval wait;
    int result;
    *error = 0;
    if (s == INVALID_SOCKET) {
        *error = WSAGetLastError();
        return 0;
    }
    ioctlsocket(s, FIONBIO, &nonblocking);
    if (connect(s, address, length) == 0) {
        closesocket(s);
        return 1;
    }
    if (WSAGetLastError() != WSAEWOULDBLOCK) {
        *error = WSAGetLastError();
        closesocket(s);
        return 0;
    }
    FD_ZERO(&writable);
    FD_ZERO(&failed);
    FD_SET(s, &writable);
    FD_SET(s, &failed);
    wait.tv_sec = NET_TIMEOUT_MS / 1000;
    wait.tv_usec = 0;
    result = select(0, NULL, &writable, &failed, &wait);
    if (result == 0) {
        *error = WSAETIMEDOUT;
    } else if (FD_ISSET(s, &failed)) {
        int code = 0;
        int code_length = sizeof code;
        getsockopt(s, SOL_SOCKET, SO_ERROR, (char *)&code, &code_length);
        *error = code;
    }
    closesocket(s);
    return result > 0 && *error == 0;
}

static int tcp_to(const char *host, unsigned short port, int *error)
{
    struct sockaddr_in address;
    memset(&address, 0, sizeof address);
    address.sin_family = AF_INET;
    address.sin_port = htons(port);
    inet_pton(AF_INET, host, &address.sin_addr);
    return tcp_connect((struct sockaddr *)&address, sizeof address, error);
}

/* A DNS query for NET_NAME's A record, sent to the server over UDP;
 * answers 1 when a reply comes. */
static int udp_dns(int *error)
{
    static const unsigned char query[] = {0x12, 0x34, 0x01, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                          7,    'e',  'x',  'a',  'm',  'p',  'l',  'e',  3,    'c',  'o',  'm',
                                          0,    0x00, 0x01, 0x00, 0x01};
    struct sockaddr_in server;
    char reply[512];
    DWORD timeout = NET_TIMEOUT_MS;
    int got;
    SOCKET s = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    *error = 0;
    if (s == INVALID_SOCKET) {
        *error = WSAGetLastError();
        return 0;
    }
    setsockopt(s, SOL_SOCKET, SO_RCVTIMEO, (const char *)&timeout, sizeof timeout);
    memset(&server, 0, sizeof server);
    server.sin_family = AF_INET;
    server.sin_port = htons(NET_DNS_PORT);
    inet_pton(AF_INET, NET_ADDRESS, &server.sin_addr);
    if (sendto(s, (const char *)query, sizeof query, 0, (struct sockaddr *)&server, sizeof server) < 0) {
        *error = WSAGetLastError();
        closesocket(s);
        return 0;
    }
    got = recvfrom(s, reply, sizeof reply, 0, NULL, NULL);
    if (got <= 0)
        *error = WSAGetLastError();
    closesocket(s);
    return got > 0;
}

/* A listener and a connection to it on a loopback address of FAMILY. */
static int loopback_self(int family, int *error)
{
    struct sockaddr_storage address;
    int length;
    SOCKET listener = socket(family, SOCK_STREAM, IPPROTO_TCP);
    int ok;
    *error = 0;
    if (listener == INVALID_SOCKET) {
        *error = WSAGetLastError();
        return 0;
    }
    memset(&address, 0, sizeof address);
    if (family == AF_INET) {
        struct sockaddr_in *v4 = (struct sockaddr_in *)&address;
        v4->sin_family = AF_INET;
        v4->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        length = sizeof *v4;
    } else {
        struct sockaddr_in6 *v6 = (struct sockaddr_in6 *)&address;
        v6->sin6_family = AF_INET6;
        v6->sin6_addr.s6_addr[15] = 1;
        length = sizeof *v6;
    }
    if (bind(listener, (struct sockaddr *)&address, length) != 0 || listen(listener, 1) != 0
        || getsockname(listener, (struct sockaddr *)&address, &length) != 0) {
        *error = WSAGetLastError();
        closesocket(listener);
        return 0;
    }
    ok = tcp_connect((struct sockaddr *)&address, length, error);
    closesocket(listener);
    return ok;
}

static void child_net(const char *service_port)
{
    WSADATA wsa;
    struct addrinfo hints;
    struct addrinfo *found = NULL;
    int error = 0;
    int ok;
    int resolved;
    WSAStartup(MAKEWORD(2, 2), &wsa);
    ok = tcp_to(NET_ADDRESS, NET_TCP_PORT, &error);
    net_line("tcp " NET_ADDRESS ":443", ok, error);
    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    resolved = getaddrinfo(NET_NAME, "443", &hints, &found);
    net_line("resolve " NET_NAME, resolved == 0, resolved);
    if (resolved == 0 && found != NULL) {
        ok = tcp_connect(found->ai_addr, (int)found->ai_addrlen, &error);
        net_line("tcp " NET_NAME ":443", ok, error);
        freeaddrinfo(found);
    }
    ok = udp_dns(&error);
    net_line("udp dns " NET_ADDRESS ":53", ok, error);
    ok = loopback_self(AF_INET, &error);
    net_line("loopback own 127.0.0.1", ok, error);
    ok = loopback_self(AF_INET6, &error);
    net_line("loopback own ::1", ok, error);
    ok = tcp_to("127.0.0.1", (unsigned short)strtoul(service_port, NULL, 10), &error);
    net_line("loopback to the service's port", ok, error);
}

/* The marker an escaped process writes, beside this program's builds. */
static WCHAR *marker_path(const char *way)
{
    WCHAR directory[MAX_PATH] = L".";
    GetCurrentDirectoryW(MAX_PATH, directory);
    return join(directory, L"\\marker-", widen(way), END);
}

/* This program's command line for an escaped process: it writes MARKER
 * and duplicates the builder's log handle, then holds both. */
static WCHAR *escaped_command(const char *way)
{
    WCHAR self[MAX_PATH];
    char handle[32];
    GetModuleFileNameW(NULL, self, MAX_PATH);
    snprintf(handle, sizeof handle, "%lu", (unsigned long)(ULONG_PTR)GetStdHandle(STD_OUTPUT_HANDLE));
    return join(self, L" child escaped ", marker_path(way), L" ", decimal(GetCurrentProcessId()), L" ", widen(handle),
                END);
}

/* Run a program, its output to this one's, and answer its exit code. */
static DWORD run_program(const WCHAR *command)
{
    STARTUPINFOW startup;
    PROCESS_INFORMATION process;
    DWORD code = 0;
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    startup.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    if (!CreateProcessW(NULL, (LPWSTR)join(command, END), NULL, NULL, TRUE, 0, NULL, NULL, &startup, &process)) {
        say("escape start %s: %s", utf8(command), error_text(GetLastError()));
        return (DWORD)-1;
    }
    WaitForSingleObject(process.hProcess, ESCAPE_WAIT_MS);
    GetExitCodeProcess(process.hProcess, &code);
    CloseHandle(process.hThread);
    CloseHandle(process.hProcess);
    return code;
}

/* Wait for an escaped process's marker, and print it. */
static void await_marker(const char *way)
{
    ULONGLONG deadline = GetTickCount64() + ESCAPE_WAIT_MS;
    DWORD error = 0;
    char *marker;
    do {
        marker = read_file(marker_path(way), &error);
        if (marker != NULL)
            break;
        Sleep(POLL_MS);
    } while (GetTickCount64() < deadline);
    if (marker == NULL)
        say("escape %s: no marker within the wait, %s", way, error_text(error));
    else
        say_lines("escape marker", marker);
}

static const char base64_digits[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/* TEXT's UTF-16LE bytes in base64, as PowerShell's -EncodedCommand takes
 * a command. */
static WCHAR *encoded_command(const WCHAR *text)
{
    size_t length = wcslen(text) * 2;
    const unsigned char *bytes = (const unsigned char *)text;
    WCHAR *encoded = take(((length + 2) / 3) * 4 * sizeof(WCHAR) + sizeof(WCHAR));
    size_t i;
    size_t at = 0;
    for (i = 0; i < length; i += 3) {
        unsigned long chunk = (unsigned long)bytes[i] << 16;
        if (i + 1 < length)
            chunk |= (unsigned long)bytes[i + 1] << 8;
        if (i + 2 < length)
            chunk |= bytes[i + 2];
        encoded[at++] = (WCHAR)base64_digits[(chunk >> 18) & 63];
        encoded[at++] = (WCHAR)base64_digits[(chunk >> 12) & 63];
        encoded[at++] = i + 1 < length ? (WCHAR)base64_digits[(chunk >> 6) & 63] : L'=';
        encoded[at++] = i + 2 < length ? (WCHAR)base64_digits[chunk & 63] : L'=';
    }
    encoded[at] = 0;
    return encoded;
}

static void child_escape(const char *way)
{
    if (strcmp(way, "escape-task") == 0) {
        DWORD created = run_program(join(L"schtasks.exe /create /tn ", TASK_NAME, L" /tr \"", escaped_command(way),
                                         L"\" /sc once /st 23:59 /np /f", END));
        say("escape task schtasks /create /np exited %lu", (unsigned long)created);
        if (created == 0)
            say("escape task schtasks /run exited %lu",
                (unsigned long)run_program(join(L"schtasks.exe /run /tn ", TASK_NAME, END)));
        await_marker(way);
    } else if (strcmp(way, "escape-wmi") == 0) {
        WCHAR *script = join(L"$r = Invoke-CimMethod -ClassName Win32_Process -MethodName Create -Arguments @{CommandLine='",
                             escaped_command(way), L"'}; \"escape wmi Win32_Process.Create returned $($r.ReturnValue), process $($r.ProcessId)\"",
                             END);
        DWORD code = run_program(join(L"powershell.exe -NoProfile -NonInteractive -EncodedCommand ",
                                      encoded_command(script), END));
        say("escape wmi powershell exited %lu", (unsigned long)code);
        await_marker(way);
    } else if (strcmp(way, "escape-hkcu") == 0) {
        HKEY key;
        LSTATUS status = RegCreateKeyExW(HKEY_CURRENT_USER,
                                         L"Software\\Classes\\CLSID\\{6d1e4a2b-3c5f-4e7a-9b8c-0d1e2f3a4b5c}\\LocalServer32",
                                         0, NULL, 0, KEY_WRITE, NULL, &key, NULL);
        say("escape hkcu creating a per-user COM class's LocalServer32 key: %s",
            status == ERROR_SUCCESS ? "created" : error_text((DWORD)status));
        if (status == ERROR_SUCCESS) {
            RegCloseKey(key);
            RegDeleteTreeW(HKEY_CURRENT_USER, L"Software\\Classes\\CLSID\\{6d1e4a2b-3c5f-4e7a-9b8c-0d1e2f3a4b5c}");
        }
        {
            HANDLE token = NULL;
            union {
                TOKEN_USER user;
                BYTE bytes[sizeof(TOKEN_USER) + SECURITY_MAX_SID_SIZE];
            } user;
            DWORD size = 0;
            if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token)
                && GetTokenInformation(token, TokenUser, &user, sizeof user, &size)) {
                status = RegOpenKeyExW(HKEY_USERS, sid_text(user.user.User.Sid), 0, KEY_READ, &key);
                say("escape hkcu the account's own hive under HKEY_USERS: %s",
                    status == ERROR_SUCCESS ? "loaded" : error_text((DWORD)status));
                if (status == ERROR_SUCCESS)
                    RegCloseKey(key);
            }
        }
    }
}

/* As a process a build started: say whether it is in a job, take a copy
 * of the builder's log handle, write the marker, and hold. */
static void child_escaped(const WCHAR *marker, const WCHAR *builder_pid, const WCHAR *handle_value)
{
    BOOL in_job = FALSE;
    HANDLE builder = OpenProcess(PROCESS_DUP_HANDLE, FALSE, (DWORD)wcstoul(builder_pid, NULL, 10));
    HANDLE copy = NULL;
    char text[512];
    char dup_text[256];
    DWORD session = 0;
    IsProcessInJob(GetCurrentProcess(), NULL, &in_job);
    ProcessIdToSessionId(GetCurrentProcessId(), &session);
    if (builder == NULL)
        snprintf(dup_text, sizeof dup_text, "opening the builder failed, %s", error_text(GetLastError()));
    else if (!DuplicateHandle(builder, (HANDLE)(ULONG_PTR)wcstoul(handle_value, NULL, 10), GetCurrentProcess(), &copy, 0,
                              FALSE, DUPLICATE_SAME_ACCESS))
        snprintf(dup_text, sizeof dup_text, "duplicating its log handle failed, %s", error_text(GetLastError()));
    else
        snprintf(dup_text, sizeof dup_text, "holds a copy of the builder's log handle");
    snprintf(text, sizeof text, "escaped pid %lu\nescaped in a job %s\nescaped session %lu\nescaped %s\n",
             (unsigned long)GetCurrentProcessId(), in_job ? "yes" : "no", (unsigned long)session, dup_text);
    write_file_whole(marker, text);
    Sleep(ESCAPED_HOLD_MS);
    if (copy != NULL)
        CloseHandle(copy);
}

static int child_command_run(int count, WCHAR **arguments)
{
    const WCHAR *op = count > 2 ? arguments[2] : L"";
    out = GetStdHandle(STD_OUTPUT_HANDLE);
    if (wcscmp(op, L"identity") == 0)
        child_identity();
    else if (wcscmp(op, L"net") == 0 && count > 3)
        child_net(utf8(arguments[3]));
    else if (wcsncmp(op, L"escape-", 7) == 0)
        child_escape(utf8(op));
    else if (wcscmp(op, L"escaped") == 0 && count > 5)
        child_escaped(arguments[3], arguments[4], arguments[5]);
    else
        return 2;
    return 0;
}

/* ------------------------------------------------------------------------
 * The entry point
 * ------------------------------------------------------------------------ */

int wmain(int count, WCHAR **arguments)
{
    const WCHAR *command = count > 1 ? arguments[1] : L"";
    out = GetStdHandle(STD_OUTPUT_HANDLE);
    if (wcscmp(command, L"child") == 0)
        return child_command_run(count, arguments);
    if (wcscmp(command, L"service") == 0 && count > 2)
        return service_command(arguments[2]);
    if (wcscmp(command, L"setup") == 0 && count > 3)
        return setup_command(arguments[2], arguments[3]);
    if (wcscmp(command, L"phase") == 0 && count > 3)
        return phase_command(arguments[2], arguments[3]);
    if (wcscmp(command, L"wfp") == 0 && count > 3)
        return wfp_command(arguments[2], arguments[3]);
    if (wcscmp(command, L"wfp-self") == 0 && count > 2)
        return wfp_self_command(arguments[2]);
    if (wcscmp(command, L"escapes") == 0 && count > 2)
        return escapes_command(arguments[2]);
    if (wcscmp(command, L"teardown") == 0 && count > 2)
        return teardown_command(arguments[2]);
    say("usage: probe setup ROOT HELPER | phase ROOT PHASE | wfp ROOT slot|group | wfp-self ROOT | escapes ROOT | "
        "teardown ROOT | service ROOT | child OP ARG...");
    return 2;
}

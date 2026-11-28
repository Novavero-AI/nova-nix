/*
 * nova-nix-sandbox's Windows backend (backend.h): runs the builder as the
 * build slot's account whose primary token the helper is given, on a
 * window station and desktop of the build's own, attached to a
 * pseudoconsole the helper owns, in a job that caps each process's memory
 * and lets none of them break away.
 *
 * The helper runs as the service that holds each slot's token, which
 * hands it one slot's token as a handle it inherits and names in the
 * plan, with the slot's SID.  The token must be a primary token whose
 * user is that SID; the helper keeps it from every process it starts but
 * the builder, which runs as it.  Starting a process under another
 * logon's token takes SeAssignPrimaryTokenPrivilege and
 * SeIncreaseQuotaPrivilege, which the helper enables in its own token
 * first, and which the probe checks it can enable.
 *
 * A process the service starts would start on the service's window
 * station and desktop, which the slot's account cannot open, and a
 * process that cannot open its window station and desktop fails to
 * start.  So the helper makes a window station and a desktop for the
 * build, named as the plan says, whose DACLs grant its own user and the
 * slot's account all access and nobody else.  A window station of that
 * name that already exists is refused, not opened, since whoever made it
 * chose its DACL.  Both go with their last handle, which the helper
 * closes once the build has ended.
 *
 * A process started with a console of its own, as libiberty starts gcc's
 * subprograms when gcc has none, fails to start under a build account's
 * token, where one sharing a console its parent owns starts.  So the
 * builder is attached to a pseudoconsole the helper owns, which what it
 * starts shares.  The pseudoconsole's output is the console's screen as
 * terminal sequences, not the build log, so a thread of the helper's
 * reads it and drops it, which also keeps the console from waiting on a
 * full pipe.  The build log is the builder's standard output and
 * standard error, the helper's own standard error, as on Linux, and its
 * standard input is NUL; those two handles are all it inherits.
 *
 * The builder starts suspended, is assigned to the build's job and is
 * then resumed, so no process of the build runs outside the job.  The
 * job kills its processes when its last handle closes, the helper's, so
 * the build ends with the helper however the helper ends.  It caps the
 * memory each process may commit, ends a process at an unhandled
 * exception at once, and grants no breakaway, so no process in it can
 * break away from it.  It nests in any job the helper is in, the process
 * library's among them.  The helper waits until the job holds no
 * process, as nova-nix waits for an unsandboxed build's job, and exits
 * with the builder's exit code.  Standard input, past the plan, is the
 * lifeline: a thread of the helper's reads it, and when it ends
 * terminates the job.
 *
 * The status channel is the helper's standard output, which no process
 * of the build inherits, so a builder that starts leaves it empty.  A
 * record's error is the Win32 error code of the call that failed, but
 * for the plan's, which the core reports with errno values; a failure
 * once the builder runs goes to the build log.
 *
 * The plan's text is UTF-8, which the helper turns to UTF-16.  The
 * builder's command line is the helper's own past its program's name and
 * the run verb, as the process that started the helper wrote it, and
 * names the program as CreateProcess reads a command line: the C
 * runtime's argv holds the command line in the system's code page, which
 * loses what that cannot hold, and rebuilding a command line from it
 * could quote it differently.
 */

/* Windows 10 1809, the first with pseudoconsoles, which the headers
 * declare from that version on. */
#define _WIN32_WINNT 0x0A00
#define NTDDI_VERSION 0x0A000006

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#include <windows.h>
#include <sddl.h>

#include "backend.h"
#include "plan.h"

/* CreateWindowStationW's flag that refuses a window station that already
 * exists, which older MinGW headers lack; the value is fixed ABI. */
#ifndef CWF_CREATE_ONLY
#define CWF_CREATE_ONLY 0x00000001
#endif

/* The exit code the build's processes are ended with when the lifeline
 * ends or the helper cannot watch them: the process library's, which
 * ends an unsandboxed build's job with it. */
#define ENDED_BUILD_EXIT_CODE 1

/* The pseudoconsole's size, and the room its output pipe has. */
#define CONSOLE_COLUMNS 80
#define CONSOLE_ROWS 25
#define CONSOLE_PIPE_BYTES 65536

/* How much of the pseudoconsole's output, and of the lifeline, one read
 * takes. */
#define DRAIN_READ_BYTES 4096

/* The attributes the builder starts with: its handle list and its
 * pseudoconsole. */
#define START_ATTRIBUTE_COUNT 2

/* The memory cap is a SIZE_T in the job's limits; on every Windows this
 * backend builds for, a SIZE_T holds any cap a plan names. */
typedef char memory_cap_fits_size[sizeof(SIZE_T) >= sizeof(unsigned long long) ? 1 : -1];

/* The backend this file is. */
const enum plan_backend backend_plan = PLAN_WINDOWS;

/* The privileges a process needs to start a process under another
 * logon's token (CreateProcessAsUserW).  winnt.h spells their names
 * narrow without UNICODE, so they are spelled here. */
static const WCHAR *const start_privileges[] = {L"SeAssignPrimaryTokenPrivilege", L"SeIncreaseQuotaPrivilege"};

/* A token's user: a TOKEN_USER and the SID it points at, which follows it
 * in the buffer and is at most SECURITY_MAX_SID_SIZE bytes, so one call
 * fills it.  The union aligns the buffer for the structure. */
union token_user {
    TOKEN_USER user;
    BYTE bytes[sizeof(TOKEN_USER) + SECURITY_MAX_SID_SIZE];
};

/* The pseudoconsole and its pipes: the write end of its input, the read
 * end of its output, and the thread that reads that output and drops
 * it. */
struct console {
    HPCON console;
    HANDLE input;
    HANDLE output;
    HANDLE drain;
};

/* The build's window station and desktop, and the name a process's start
 * gives them by, STATION\DESKTOP. */
struct user_objects {
    HWINSTA station;
    HDESK desktop;
    WCHAR *name;
};

/* ------------------------------------------------------------------------
 * The channels
 * ------------------------------------------------------------------------ */

void backend_write_status(const char *bytes, size_t length)
{
    HANDLE status = GetStdHandle(STD_OUTPUT_HANDLE);
    while (length > 0) {
        DWORD written = 0;
        DWORD chunk = length > MAXDWORD ? MAXDWORD : (DWORD)length;
        if (!WriteFile(status, bytes, chunk, &written, NULL) || written == 0)
            return;
        bytes += written;
        length -= written;
    }
}

int backend_read_input(char *bytes, size_t length)
{
    HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
    size_t got = 0;
    while (got < length) {
        DWORD read_now = 0;
        DWORD want = length - got > MAXDWORD ? MAXDWORD : (DWORD)(length - got);
        if (!ReadFile(input, bytes + got, want, &read_now, NULL))
            return GetLastError() == ERROR_BROKEN_PIPE ? EPIPE : EIO;
        if (read_now == 0)
            return EPIPE;
        got += read_now;
    }
    return 0;
}

/* The status channel is standard output as the helper was given it: the
 * builder inherits only the handles its start lists, so nothing it
 * writes reaches the channel, and nothing needs moving first. */
int backend_prepare_run(void)
{
    HANDLE status = GetStdHandle(STD_OUTPUT_HANDLE);
    return status == NULL || status == INVALID_HANDLE_VALUE ? -1 : 0;
}

/* ------------------------------------------------------------------------
 * Text
 * ------------------------------------------------------------------------ */

/* TEXT, UTF-8, as UTF-16 in a buffer of its own.  A failure fails the
 * process at STAGE, for INDEX. */
static WCHAR *widened(const char *text, const char *stage, long index)
{
    int length = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text, -1, NULL, 0);
    WCHAR *wide;
    if (length <= 0)
        status_fail(stage, (int)GetLastError(), index);
    wide = malloc((size_t)length * sizeof *wide);
    if (wide == NULL)
        status_fail(stage, ERROR_NOT_ENOUGH_MEMORY, index);
    if (MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text, -1, wide, length) != length)
        status_fail(stage, (int)GetLastError(), index);
    return wide;
}

/* The plan's environment as the block CreateProcessAsUserW takes with
 * CREATE_UNICODE_ENVIRONMENT: each entry in UTF-16 and NUL-terminated, in
 * the plan's order, then a NUL that ends the block, which with no entry
 * is two NULs.  An empty entry would end the block early, so it fails, as
 * one that is not UTF-8 does. */
static WCHAR *environment_block(const struct plan *plan)
{
    size_t total = 0;
    size_t i;
    WCHAR *block;
    WCHAR *at;
    for (i = 0; i < plan->environment_count; i++) {
        int length;
        if (plan->environment[i][0] == '\0')
            status_fail("environment", ERROR_INVALID_PARAMETER, (long)i);
        length = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, plan->environment[i], -1, NULL, 0);
        if (length <= 0)
            status_fail("environment", (int)GetLastError(), (long)i);
        total += (size_t)length;
    }
    /* Zeroed, so the NULs that end the block are there already. */
    block = calloc(total + 2, sizeof *block);
    if (block == NULL)
        status_fail("environment", ERROR_NOT_ENOUGH_MEMORY, NO_INDEX);
    at = block;
    for (i = 0; i < plan->environment_count; i++) {
        int length = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, plan->environment[i], -1, at,
                                         (int)(total - (size_t)(at - block)));
        if (length <= 0)
            status_fail("environment", (int)GetLastError(), (long)i);
        at += length;
    }
    return block;
}

/* Pass over one argument of a command line, and the spaces and tabs after
 * it, as the C runtime reads a program's name: each quote opens or closes
 * a quoted part, and a space or tab outside one ends the argument.  The
 * run verb, which the core read as "run", holds no backslash, so it ends
 * where this says too. */
static const WCHAR *past_argument(const WCHAR *at)
{
    int quoted = 0;
    while (*at != L'\0' && (quoted || (*at != L' ' && *at != L'\t'))) {
        if (*at == L'"')
            quoted = !quoted;
        at++;
    }
    while (*at == L' ' || *at == L'\t')
        at++;
    return at;
}

/* The builder's command line, in a buffer of its own, which
 * CreateProcessAsUserW may write to: the helper's own past its program's
 * name and the run verb, as given. */
static WCHAR *builder_command_line(void)
{
    const WCHAR *tail = past_argument(past_argument(GetCommandLineW()));
    size_t length = wcslen(tail) + 1;
    WCHAR *copy = malloc(length * sizeof *copy);
    if (copy == NULL)
        status_fail("command-line", ERROR_NOT_ENOUGH_MEMORY, NO_INDEX);
    memcpy(copy, tail, length * sizeof *copy);
    return copy;
}

/* ------------------------------------------------------------------------
 * The tokens
 * ------------------------------------------------------------------------ */

/* Enable each of the start privileges in TOKEN, this process's.  Answers
 * 0, or the Win32 error of the step that failed: ERROR_NOT_ALL_ASSIGNED
 * for a privilege the token does not hold. */
static DWORD enable_privileges(HANDLE token)
{
    size_t i;
    for (i = 0; i < sizeof start_privileges / sizeof start_privileges[0]; i++) {
        TOKEN_PRIVILEGES wanted;
        DWORD outcome;
        wanted.PrivilegeCount = 1;
        wanted.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
        if (!LookupPrivilegeValueW(NULL, start_privileges[i], &wanted.Privileges[0].Luid))
            return GetLastError();
        if (!AdjustTokenPrivileges(token, FALSE, &wanted, 0, NULL, NULL))
            return GetLastError();
        /* The call succeeds for a privilege the token does not hold, and
         * says so in the thread's last error alone. */
        outcome = GetLastError();
        if (outcome != ERROR_SUCCESS)
            return outcome;
    }
    return 0;
}

/* The user of this process's token, the service's, with the start
 * privileges enabled in that token.  Fails the process at privileges. */
static void ready_own_token(union token_user *user)
{
    HANDLE own = NULL;
    DWORD size = 0;
    DWORD error;
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &own))
        status_fail("privileges", (int)GetLastError(), NO_INDEX);
    if ((error = enable_privileges(own)) != 0)
        status_fail("privileges", (int)error, NO_INDEX);
    if (!GetTokenInformation(own, TokenUser, user, sizeof *user, &size))
        status_fail("privileges", (int)GetLastError(), NO_INDEX);
    CloseHandle(own);
}

/* The slot's token the plan names, kept from every process this one
 * starts but the builder, once it is known to be a primary token whose
 * user is the plan's slot, whose SID goes in *SLOT for the caller to
 * release with LocalFree. */
static HANDLE slot_token(const struct plan *plan, PSID *slot)
{
    HANDLE token = (HANDLE)(ULONG_PTR)plan->token_handle;
    union token_user user;
    TOKEN_TYPE type;
    DWORD size = 0;
    WCHAR *slot_text;
    if (!SetHandleInformation(token, HANDLE_FLAG_INHERIT, 0))
        status_fail("token", (int)GetLastError(), NO_INDEX);
    if (!GetTokenInformation(token, TokenType, &type, sizeof type, &size))
        status_fail("token", (int)GetLastError(), NO_INDEX);
    if (type != TokenPrimary)
        status_fail("token", ERROR_BAD_TOKEN_TYPE, NO_INDEX);
    if (!GetTokenInformation(token, TokenUser, &user, sizeof user, &size))
        status_fail("token", (int)GetLastError(), NO_INDEX);
    slot_text = widened(plan->slot_sid, "slot-sid", NO_INDEX);
    if (!ConvertStringSidToSidW(slot_text, slot))
        status_fail("slot-sid", (int)GetLastError(), NO_INDEX);
    free(slot_text);
    if (!EqualSid(user.user.User.Sid, *slot))
        status_fail("token-user", 0, NO_INDEX);
    return token;
}

/* ------------------------------------------------------------------------
 * The window station and desktop
 * ------------------------------------------------------------------------ */

/* The DACL of the build's window station and desktop, in a buffer of its
 * own: all access for HELPER, this process's user, and for SLOT, and no
 * other entry.  Generic rights, which each object maps to its own as it
 * is made. */
static PACL grant_both(PSID helper, PSID slot)
{
    DWORD entry = (DWORD)(sizeof(ACCESS_ALLOWED_ACE) - sizeof(DWORD));
    DWORD size = (DWORD)sizeof(ACL) + entry + GetLengthSid(helper) + entry + GetLengthSid(slot);
    PACL acl = malloc(size);
    if (acl == NULL)
        status_fail("station", ERROR_NOT_ENOUGH_MEMORY, NO_INDEX);
    if (!InitializeAcl(acl, size, ACL_REVISION) || !AddAccessAllowedAce(acl, ACL_REVISION, GENERIC_ALL, helper)
        || !AddAccessAllowedAce(acl, ACL_REVISION, GENERIC_ALL, slot))
        status_fail("station", (int)GetLastError(), NO_INDEX);
    return acl;
}

/* Make the build's window station and desktop, named by the plan, which
 * grant only HELPER and SLOT.  A desktop is made on the calling process's
 * window station, so this process moves to the build's for the call and
 * back after it. */
static void make_user_objects(const struct plan *plan, PSID helper, PSID slot, struct user_objects *made, PACL *acl)
{
    SECURITY_DESCRIPTOR descriptor;
    SECURITY_ATTRIBUTES attributes;
    WCHAR *station_name;
    WCHAR *desktop_name;
    HWINSTA previous;
    DWORD error = 0;

    made->name = widened(plan->desktop, "station", NO_INDEX);
    /* On this probe branch an empty desktop makes no window station: the
     * builder starts with lpDesktop empty, on the system's for its
     * token's logon session. */
    made->station = NULL;
    made->desktop = NULL;
    if (plan->desktop[0] == '\0') {
        *acl = NULL;
        return;
    }
    station_name = widened(plan->desktop, "station", NO_INDEX);
    desktop_name = wcschr(station_name, L'\\');
    *desktop_name++ = L'\0';

    *acl = grant_both(helper, slot);
    if (!InitializeSecurityDescriptor(&descriptor, SECURITY_DESCRIPTOR_REVISION)
        || !SetSecurityDescriptorDacl(&descriptor, TRUE, *acl, FALSE))
        status_fail("station", (int)GetLastError(), NO_INDEX);
    attributes.nLength = sizeof attributes;
    attributes.lpSecurityDescriptor = &descriptor;
    attributes.bInheritHandle = FALSE;

    made->station = CreateWindowStationW(station_name, CWF_CREATE_ONLY, WINSTA_ALL_ACCESS | READ_CONTROL, &attributes);
    if (made->station == NULL)
        status_fail("station", (int)GetLastError(), NO_INDEX);
    previous = GetProcessWindowStation();
    if (previous == NULL || !SetProcessWindowStation(made->station))
        status_fail("desktop", (int)GetLastError(), NO_INDEX);
    made->desktop = CreateDesktopW(desktop_name, NULL, NULL, 0, GENERIC_ALL, &attributes);
    if (made->desktop == NULL)
        error = GetLastError();
    if (!SetProcessWindowStation(previous) && error == 0)
        error = GetLastError();
    if (error != 0)
        status_fail("desktop", (int)error, NO_INDEX);
    free(station_name);
}

/* Close the build's window station and desktop, which go with their last
 * handle.  A close that fails leaves its object to this process's end,
 * which follows. */
static void close_user_objects(struct user_objects *made)
{
    if (made->desktop != NULL)
        CloseDesktop(made->desktop);
    if (made->station != NULL)
        CloseWindowStation(made->station);
    free(made->name);
}

/* ------------------------------------------------------------------------
 * The pseudoconsole
 * ------------------------------------------------------------------------ */

/* Read the pseudoconsole's output, whose read end PARAMETER is, and drop
 * it, until the pseudoconsole closes its end, which fails the read: a
 * pipe's read answers no bytes, and succeeds, for a write of none. */
static DWORD WINAPI drain_console(LPVOID parameter)
{
    char discard[DRAIN_READ_BYTES];
    DWORD got = 0;
    while (ReadFile(parameter, discard, sizeof discard, &got, NULL))
        ;
    return 0;
}

/* The Win32 error an HRESULT carries, or ERROR_GEN_FAILURE for one that
 * carries none. */
static int hresult_error(HRESULT result)
{
    return HRESULT_FACILITY(result) == FACILITY_WIN32 ? HRESULT_CODE(result) : ERROR_GEN_FAILURE;
}

/* Make the pseudoconsole and start the thread that drains it.  Its pipes'
 * ends here are not inheritable, so no process of the build holds one. */
static void make_console(struct console *made)
{
    HANDLE input_read = NULL;
    HANDLE output_write = NULL;
    COORD size;
    HRESULT result;
    if (!CreatePipe(&input_read, &made->input, NULL, 0)
        || !CreatePipe(&made->output, &output_write, NULL, CONSOLE_PIPE_BYTES))
        status_fail("console", (int)GetLastError(), NO_INDEX);
    size.X = CONSOLE_COLUMNS;
    size.Y = CONSOLE_ROWS;
    result = CreatePseudoConsole(size, input_read, output_write, 0, &made->console);
    /* The pseudoconsole holds copies of the ends it was given. */
    CloseHandle(input_read);
    CloseHandle(output_write);
    if (FAILED(result))
        status_fail("console", hresult_error(result), NO_INDEX);
    made->drain = CreateThread(NULL, 0, drain_console, made->output, 0, NULL);
    if (made->drain == NULL)
        status_fail("console", (int)GetLastError(), NO_INDEX);
}

/* Close the pseudoconsole once the build has ended.  Before Windows 11
 * 24H2, ClosePseudoConsole waits until its last output has been read,
 * which the drain goes on reading, until the pseudoconsole closes its end
 * of the pipe.  The read end closes only then: closing a handle another
 * thread is reading would not end that read. */
static void close_console(struct console *made)
{
    CloseHandle(made->input);
    ClosePseudoConsole(made->console);
    WaitForSingleObject(made->drain, INFINITE);
    CloseHandle(made->drain);
    CloseHandle(made->output);
}

/* ------------------------------------------------------------------------
 * The job
 * ------------------------------------------------------------------------ */

/* Make the build's job and the completion port it reports on. */
static void make_job(const struct plan *plan, HANDLE *job, HANDLE *port)
{
    JOBOBJECT_ASSOCIATE_COMPLETION_PORT association;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    *job = CreateJobObjectW(NULL, NULL);
    if (*job == NULL)
        status_fail("job", (int)GetLastError(), NO_INDEX);
    *port = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 1);
    if (*port == NULL)
        status_fail("job", (int)GetLastError(), NO_INDEX);
    memset(&association, 0, sizeof association);
    association.CompletionKey = *job;
    association.CompletionPort = *port;
    memset(&limits, 0, sizeof limits);
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
                                              | JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION
                                              | JOB_OBJECT_LIMIT_PROCESS_MEMORY;
    limits.ProcessMemoryLimit = (SIZE_T)plan->memory_cap;
    if (!SetInformationJobObject(*job, JobObjectAssociateCompletionPortInformation, &association, sizeof association)
        || !SetInformationJobObject(*job, JobObjectExtendedLimitInformation, &limits, sizeof limits))
        status_fail("job", (int)GetLastError(), NO_INDEX);
}

/* Read the lifeline, standard input past the plan, until it ends, then
 * end the build, whose job PARAMETER is.  A read that answers no bytes is
 * its end, as at the end of a file.  The job's handle stays open until
 * this process ends, since this thread may end the job until then. */
static DWORD WINAPI watch_lifeline(LPVOID parameter)
{
    HANDLE input = GetStdHandle(STD_INPUT_HANDLE);
    char discard[DRAIN_READ_BYTES];
    DWORD got = 0;
    while (ReadFile(input, discard, sizeof discard, &got, NULL) && got > 0)
        ;
    TerminateJobObject(parameter, ENDED_BUILD_EXIT_CODE);
    return 0;
}

/* Wait until the job holds no process, saying in the build log of each
 * process the memory cap stopped that it did.  The port's messages wake
 * the wait, ACTIVE_PROCESS_ZERO among them, but the job's own count of
 * its processes says whether the build is over: a job a process of the
 * build makes nests in the build's, and its messages can reach the
 * build's port too.  Answers 0, or the Win32 error of a wait that
 * failed. */
static DWORD wait_for_build(HANDLE job, HANDLE port, const struct plan *plan)
{
    for (;;) {
        JOBOBJECT_BASIC_ACCOUNTING_INFORMATION accounting;
        DWORD message = 0;
        ULONG_PTR key = 0;
        LPOVERLAPPED detail = NULL;
        if (!GetQueuedCompletionStatus(port, &message, &key, &detail, INFINITE))
            return GetLastError();
        /* The message's detail is the process's ID. */
        if (key == (ULONG_PTR)job && message == JOB_OBJECT_MSG_PROCESS_MEMORY_LIMIT)
            fprintf(stderr, "nova-nix-sandbox: process %lu of the build reached its memory limit of %llu bytes\n",
                    (unsigned long)(ULONG_PTR)detail, plan->memory_cap);
        if (!QueryInformationJobObject(job, JobObjectBasicAccountingInformation, &accounting, sizeof accounting, NULL))
            return GetLastError();
        if (accounting.ActiveProcesses == 0)
            return 0;
    }
}

/* ------------------------------------------------------------------------
 * The run
 * ------------------------------------------------------------------------ */

/* The attributes the builder starts with: the handles it inherits, which
 * must stay where they are until the list is deleted, and the
 * pseudoconsole it attaches to. */
static LPPROC_THREAD_ATTRIBUTE_LIST start_attributes(HANDLE *inherited, size_t count, HPCON console)
{
    SIZE_T size = 0;
    LPPROC_THREAD_ATTRIBUTE_LIST list;
    /* Asked of no list, the call fails, answering the size a list takes. */
    InitializeProcThreadAttributeList(NULL, START_ATTRIBUTE_COUNT, 0, &size);
    list = malloc(size);
    if (list == NULL)
        status_fail("attributes", ERROR_NOT_ENOUGH_MEMORY, NO_INDEX);
    if (!InitializeProcThreadAttributeList(list, START_ATTRIBUTE_COUNT, 0, &size))
        status_fail("attributes", (int)GetLastError(), NO_INDEX);
    if (!UpdateProcThreadAttribute(list, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherited, count * sizeof *inherited,
                                   NULL, NULL)
        || !UpdateProcThreadAttribute(list, 0, PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE, console, sizeof console, NULL,
                                      NULL))
        status_fail("attributes", (int)GetLastError(), NO_INDEX);
    return list;
}

/* An inheritable handle to NUL, the builder's standard input, and an
 * inheritable copy of the build log, this process's standard error, for
 * its standard output and error. */
static void builder_handles(HANDLE *nul, HANDLE *log)
{
    SECURITY_ATTRIBUTES inheritable;
    inheritable.nLength = sizeof inheritable;
    inheritable.lpSecurityDescriptor = NULL;
    inheritable.bInheritHandle = TRUE;
    *nul = CreateFileW(L"NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, &inheritable, OPEN_EXISTING, 0, NULL);
    if (*nul == INVALID_HANDLE_VALUE)
        status_fail("stdin", (int)GetLastError(), NO_INDEX);
    if (!DuplicateHandle(GetCurrentProcess(), GetStdHandle(STD_ERROR_HANDLE), GetCurrentProcess(), log, 0, TRUE,
                         DUPLICATE_SAME_ACCESS))
        status_fail("log", (int)GetLastError(), NO_INDEX);
}

int backend_run(const struct plan *plan, char **argv)
{
    union token_user helper;
    PSID slot = NULL;
    HANDLE token;
    struct user_objects objects;
    PACL acl = NULL;
    struct console console;
    HANDLE job = NULL;
    HANDLE port = NULL;
    HANDLE inherited[2];
    WCHAR *environment;
    WCHAR *build_dir;
    WCHAR *command;
    LPPROC_THREAD_ATTRIBUTE_LIST attributes;
    STARTUPINFOEXW startup;
    PROCESS_INFORMATION process;
    HANDLE watcher;
    DWORD error;
    DWORD code = 0;
    int status;

    /* The builder's command line is read from this process's own, which
     * argv holds in the system's code page. */
    (void)argv;

    ready_own_token(&helper);
    token = slot_token(plan, &slot);
    make_user_objects(plan, helper.user.User.Sid, slot, &objects, &acl);
    make_console(&console);
    make_job(plan, &job, &port);
    builder_handles(&inherited[0], &inherited[1]);
    environment = environment_block(plan);
    build_dir = widened(plan->build_dir, "build-dir", NO_INDEX);
    command = builder_command_line();
    attributes = start_attributes(inherited, sizeof inherited / sizeof inherited[0], console.console);

    memset(&startup, 0, sizeof startup);
    memset(&process, 0, sizeof process);
    startup.StartupInfo.cb = sizeof startup;
    startup.StartupInfo.lpDesktop = objects.name;
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdInput = inherited[0];
    startup.StartupInfo.hStdOutput = inherited[1];
    startup.StartupInfo.hStdError = inherited[1];
    startup.lpAttributeList = attributes;
    if (!CreateProcessAsUserW(token, NULL, command, NULL, NULL, TRUE,
                              CREATE_SUSPENDED | CREATE_UNICODE_ENVIRONMENT | EXTENDED_STARTUPINFO_PRESENT,
                              environment, build_dir, &startup.StartupInfo, &process))
        status_fail("exec", (int)GetLastError(), NO_INDEX);
    if (!AssignProcessToJobObject(job, process.hProcess)) {
        error = GetLastError();
        TerminateProcess(process.hProcess, ENDED_BUILD_EXIT_CODE);
        status_fail("assign", (int)error, NO_INDEX);
    }
    /* From here a failure that ends this process ends the job's last
     * handle with it, and so the builder. */
    watcher = CreateThread(NULL, 0, watch_lifeline, job, 0, NULL);
    if (watcher == NULL)
        status_fail("lifeline", (int)GetLastError(), NO_INDEX);
    CloseHandle(watcher);
    if (ResumeThread(process.hThread) == (DWORD)-1)
        status_fail("resume", (int)GetLastError(), NO_INDEX);

    CloseHandle(process.hThread);
    CloseHandle(inherited[0]);
    CloseHandle(inherited[1]);
    CloseHandle(token);
    DeleteProcThreadAttributeList(attributes);
    free(attributes);
    free(command);
    free(build_dir);
    free(environment);

    error = wait_for_build(job, port, plan);
    CloseHandle(port);
    if (error != 0) {
        TerminateJobObject(job, ENDED_BUILD_EXIT_CODE);
        fprintf(stderr, "nova-nix-sandbox: cannot wait for the build's processes: Win32 error %lu\n",
                (unsigned long)error);
        status = EXIT_FAILURE;
    } else if (!GetExitCodeProcess(process.hProcess, &code)) {
        fprintf(stderr, "nova-nix-sandbox: cannot get exit status of process %lu: Win32 error %lu\n",
                (unsigned long)process.dwProcessId, (unsigned long)GetLastError());
        status = EXIT_FAILURE;
    } else {
        /* main's status reaches ExitProcess as these same 32 bits. */
        status = (int)code;
    }
    CloseHandle(process.hProcess);
    close_console(&console);
    close_user_objects(&objects);
    free(acl);
    LocalFree(slot);
    return status;
}

/* ------------------------------------------------------------------------
 * The probe
 * ------------------------------------------------------------------------ */

const char *backend_probe(int *error)
{
    HANDLE own = NULL;
    DWORD failed;
    if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &own)) {
        *error = (int)GetLastError();
        return "privileges";
    }
    failed = enable_privileges(own);
    CloseHandle(own);
    *error = (int)failed;
    return failed != 0 ? "privileges" : NULL;
}

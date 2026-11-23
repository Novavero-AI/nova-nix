/*
 * A probe of what nova-nix's Windows build sandbox rests on, run on a
 * hosted runner and on a desktop before the sandbox's backend is written.
 * The design is a build account per build slot, which a service starts
 * builds as; without the service, a restricted token is the candidate for
 * a tier above job objects alone.  The probe runs one set of checks in
 * passes, each under one kind of token in a layout of its own:
 *
 *   - the design: two local build accounts, logged on with generated
 *     passwords and started through the Secondary Logon service, as this
 *     process holds no privilege to assign another logon's token (the
 *     "account-" pass, last, and only when the probe runs elevated and not
 *     in desktop mode);
 *   - the restricted token, S-1-5-12 and the build's SID restricting (no
 *     prefix), with the checks only it gets: its own token, the present
 *     store layout, a hostile store entry, a junction, and nova-nix as a
 *     standard user;
 *   - the limited token, Users and Everyone restricting too ("lim-");
 *   - write-restricted tokens, whose restricting SIDs apply to writes
 *     alone ("wr-"), with Everyone restricting too ("wr-everyone-"), and,
 *     on what the plain one deletes and reads and on starting a process
 *     and making a file, at Low integrity ("wr-low-") and with the user's
 *     SID deny-only ("wr-deny-user-").
 *
 * Each pass answers, a line each: whether a build writes the pipe it
 * inherits for its log; what the owner of a file gets; whether the
 * proposed store layout (store directory entries for itself alone, the
 * builder's new entries taking its token's default DACL) lets a build
 * make its outputs; whether a valid store path lets a build read and
 * nothing more; whether two builds are kept from each other; whether jobs
 * cap memory and refuse breakaway; whether MSYS2's bash and a MinGW gcc
 * run, gcc detached, attached to the checks' console and attached to a
 * pseudoconsole the checks own, and the programs it builds; and what a
 * build reaches of the machine's and of the user's own: their files,
 * profile, registry hive and processes' memory, and a file in the profile
 * it might delete.
 *
 * Across every token, the "why-" lines ask why a token with restricting
 * SIDs cannot start a process: starting one from a job and from none,
 * without the code-authorization check, of a system program, through the
 * native call beneath CreateProcessW, with a new console, and sharing the
 * checks' console or a pseudoconsole; and opening what starting one, and
 * MSYS2's runtime, reads.
 *
 * Each line starts PASS or FAIL and says "required" when the design
 * depends on the outcome, or "finding" when the outcome only informs it,
 * with the error the operation met.  Only the design's pass, setup and
 * the tree checks have required lines; every other pass's lines are
 * findings.  A finding's PASS says its claim held, not that the outcome
 * is a good one.  The process exits with the number of required checks
 * that failed.
 *
 * The program links no C runtime, only kernel32 and advapi32, so a child
 * under a restricted token loads those and the system DLLs they import,
 * and no runtime of the probe's own: a child that cannot start points at
 * the token, not at a runtime the probe brought.  Whether a program built
 * against the UCRT runs under the token is check 9's to say, through gcc
 * and what it builds.  The native calls it makes, ntdll being mapped into
 * every process, and the functions only the parent calls, which a child
 * could not resolve when it starts, are looked up by name.
 *
 * Each restricted token is made as a sandbox helper would make a build's:
 * its default owner the user, but for one whose user's SID is deny-only,
 * which keeps the owner it was made with, and its default DACL naming the
 * user, SYSTEM and the build's SID.  The hosted runner's user is an
 * administrator, whose tokens would otherwise give all of these to
 * Administrators alone.
 *
 *   probe run ROOT BASH GCC [write-profile | desktop]
 *                                    every check; ROOT is the probe's own
 *                                    directory, BASH and GCC the tools to
 *                                    run under each token, or "-";
 *                                    write-profile lets the probe make,
 *                                    and remove, a file in the user's
 *                                    profile for a build to try to delete;
 *                                    desktop keeps the probe to what a
 *                                    desktop someone uses allows: it
 *                                    writes only under ROOT, opens no file
 *                                    of the user's own, no other process
 *                                    and nothing outside ROOT for writing,
 *                                    makes no account, and leaves nothing
 *                                    it cannot remove
 *   probe checks ROOT BASH GCC JOB [write-profile | desktop]
 *                                    the checks, inside the job run made,
 *                                    whose handle JOB is
 *   probe hold                       a stand-in sandbox helper, for the
 *                                    check that ends a whole tree
 *   probe trace STEP BASH GCC        one thing under the restricted
 *                                    token, for a trace of what it is
 *                                    refused: STEP is control, spawn-probe,
 *                                    spawn-cmd, bash or gcc
 *   probe child OP ARG...            one operation, under whatever token
 *                                    the process was given
 */

#define _WIN32_WINNT 0x0A00

#include <stdarg.h>
#include <stddef.h>

#include <windows.h>
#include <aclapi.h>
#include <sddl.h>
#include <winioctl.h>
#include <winternl.h>
#include <ntsecapi.h>
#include <lmcons.h>
#include <lmerr.h>
#include <lmaccess.h>

/* ------------------------------------------------------------------------
 * What a program with no C runtime needs
 * ------------------------------------------------------------------------ */

/* The compiler may call these for a structure's initialization or copy. */
void *memset(void *destination, int value, size_t length)
{
    unsigned char *at = destination;
    while (length-- > 0)
        *at++ = (unsigned char)value;
    return destination;
}

void *memcpy(void *destination, const void *source, size_t length)
{
    unsigned char *to = destination;
    const unsigned char *from = source;
    while (length-- > 0)
        *to++ = *from++;
    return destination;
}

/* The exit status when the probe's own heap cannot grow. */
#define EXIT_NO_MEMORY 250
/* The exit status of a command line the probe does not know. */
#define EXIT_USAGE 249

/* The run command's options: write-profile lets the probe make, and
 * remove, a file in the user's profile, for the check that a build cannot
 * delete one; desktop keeps the probe to what a desktop someone uses
 * allows (see struct context's desktop). */
#define WRITE_PROFILE L"write-profile"
#define DESKTOP L"desktop"

/* Every allocation comes from one private heap, destroyed on exit. */
static HANDLE arena;

static void *take(size_t length)
{
    void *bytes = HeapAlloc(arena, HEAP_ZERO_MEMORY, length > 0 ? length : 1);
    if (bytes == NULL)
        ExitProcess(EXIT_NO_MEMORY);
    return bytes;
}

static void finish(UINT status)
{
    HeapDestroy(arena);
    ExitProcess(status);
}

static size_t wide_length(const WCHAR *text)
{
    size_t length = 0;
    while (text[length] != 0)
        length++;
    return length;
}

static int wide_equal(const WCHAR *a, const WCHAR *b)
{
    while (*a != 0 && *a == *b) {
        a++;
        b++;
    }
    return *a == *b;
}

/* Ends a list of strings to join. */
#define END ((const WCHAR *)NULL)

/* The strings given, up to END, joined in a new string. */
static WCHAR *join(const WCHAR *first, ...)
{
    va_list pieces;
    const WCHAR *piece;
    size_t total = 0;
    WCHAR *joined;
    WCHAR *at;
    va_start(pieces, first);
    for (piece = first; piece != NULL; piece = va_arg(pieces, const WCHAR *))
        total += wide_length(piece);
    va_end(pieces);
    joined = take((total + 1) * sizeof(WCHAR));
    at = joined;
    va_start(pieces, first);
    for (piece = first; piece != NULL; piece = va_arg(pieces, const WCHAR *)) {
        size_t length = wide_length(piece);
        memcpy(at, piece, length * sizeof(WCHAR));
        at += length;
    }
    va_end(pieces);
    return joined;
}

/* A number in decimal, or in hexadecimal after "0x". */
static WCHAR *number(ULONGLONG value, int hexadecimal)
{
    WCHAR digits[24];
    int count = 0;
    ULONGLONG base = hexadecimal ? 16 : 10;
    do {
        ULONGLONG digit = value % base;
        digits[count++] = (WCHAR)(digit < 10 ? L'0' + digit : L'a' + digit - 10);
        value /= base;
    } while (value > 0);
    {
        WCHAR *text = take((size_t)(count + 3) * sizeof(WCHAR));
        WCHAR *at = text;
        if (hexadecimal) {
            *at++ = L'0';
            *at++ = L'x';
        }
        while (count > 0)
            *at++ = digits[--count];
        return text;
    }
}

/* A decimal or "0x" hexadecimal number; 0 for anything else. */
static DWORD parse_number(const WCHAR *text)
{
    DWORD value = 0;
    if (text[0] == L'0' && (text[1] == L'x' || text[1] == L'X')) {
        for (text += 2; *text != 0; text++) {
            WCHAR c = *text;
            DWORD digit = c >= L'0' && c <= L'9' ? (DWORD)(c - L'0')
                        : c >= L'a' && c <= L'f' ? (DWORD)(c - L'a' + 10)
                        : c >= L'A' && c <= L'F' ? (DWORD)(c - L'A' + 10) : 16;
            if (digit == 16)
                return 0;
            value = value * 16 + digit;
        }
        return value;
    }
    for (; *text != 0; text++) {
        if (*text < L'0' || *text > L'9')
            return 0;
        value = value * 10 + (DWORD)(*text - L'0');
    }
    return value;
}

/* A "0x" hexadecimal address, as number gives one; 0 for anything else. */
static ULONG_PTR parse_address(const WCHAR *text)
{
    ULONG_PTR value = 0;
    if (text[0] != L'0' || (text[1] != L'x' && text[1] != L'X'))
        return 0;
    for (text += 2; *text != 0; text++) {
        WCHAR c = *text;
        ULONG_PTR digit = c >= L'0' && c <= L'9' ? (ULONG_PTR)(c - L'0')
                        : c >= L'a' && c <= L'f' ? (ULONG_PTR)(c - L'a' + 10)
                        : c >= L'A' && c <= L'F' ? (ULONG_PTR)(c - L'A' + 10) : 16;
        if (digit == 16)
            return 0;
        value = value * 16 + digit;
    }
    return value;
}

/* A narrow copy of TEXT, which holds ASCII alone, as GetProcAddress takes
 * a function's name. */
static char *ascii(const WCHAR *text)
{
    size_t length = wide_length(text);
    char *narrow = take(length + 1);
    size_t i;
    for (i = 0; i < length; i++)
        narrow[i] = (char)(text[i] & 0x7F);
    return narrow;
}

/* A path with its backslashes turned to slashes, as MSYS2 reads one. */
static WCHAR *forward_slashes(const WCHAR *path)
{
    WCHAR *copy = join(path, END);
    WCHAR *at;
    for (at = copy; *at != 0; at++)
        if (*at == L'\\')
            *at = L'/';
    return copy;
}

/* ------------------------------------------------------------------------
 * Output
 * ------------------------------------------------------------------------ */

static void write_bytes(const char *bytes, DWORD length)
{
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD written;
    while (length > 0 && WriteFile(out, bytes, length, &written, NULL) && written > 0) {
        bytes += written;
        length -= written;
    }
}

/* One line, as UTF-8, written in one call so lines from two processes on
 * one pipe do not interleave. */
static void say(const WCHAR *line)
{
    WCHAR *full = join(line, L"\n", END);
    int length = WideCharToMultiByte(CP_UTF8, 0, full, -1, NULL, 0, NULL, NULL);
    if (length > 1) {
        char *bytes = take((size_t)length);
        WideCharToMultiByte(CP_UTF8, 0, full, -1, bytes, length, NULL, NULL);
        write_bytes(bytes, (DWORD)(length - 1));
    }
}

/* "error N (the system's text for it)". */
static WCHAR *error_text(DWORD error)
{
    WCHAR *message = NULL;
    WCHAR *text;
    DWORD length = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS
                                      | FORMAT_MESSAGE_ALLOCATE_BUFFER,
                                  NULL, error, 0, (LPWSTR)&message, 0, NULL);
    while (length > 0 && (message[length - 1] == L'\n' || message[length - 1] == L'\r'
                          || message[length - 1] == L' ' || message[length - 1] == L'.'))
        message[--length] = 0;
    text = join(L"error ", number(error, 0), length > 0 ? L" (" : L"", length > 0 ? message : L"",
                length > 0 ? L")" : L"", END);
    if (message != NULL)
        LocalFree(message);
    return text;
}

static DWORD required_failures;
static DWORD findings_failed;

#define REQUIRED 1
#define FINDING 0

/* The pass the checks run in.  Each pass runs checks under one kind of
 * token, in a layout of its own, its labels starting with the pass's
 * name.  Only in a pass of the design (and outside any pass, for setup
 * and the tree checks) does a required check count as one; in a
 * candidate's pass every line is a finding. */
static const WCHAR *pass_prefix = L"";
static int pass_counts_required = 1;

#define DESIGN_PASS 1
#define CANDIDATE_PASS 0

static void begin_pass(const WCHAR *prefix, int kind)
{
    pass_prefix = prefix;
    pass_counts_required = kind == DESIGN_PASS;
}

static void end_pass(void)
{
    pass_prefix = L"";
    pass_counts_required = 1;
}

/* Print a check's line and count it. */
static void verdict(int passed, int required, const WCHAR *label, const WCHAR *what, const WCHAR *detail)
{
    int counts = required && pass_counts_required;
    say(join(passed ? L"PASS " : L"FAIL ", counts ? L"required " : L"finding  ", pass_prefix, label, L": ", what,
             detail != NULL ? L" [" : L"", detail != NULL ? detail : L"", detail != NULL ? L"]" : L"", END));
    if (!passed) {
        if (counts)
            required_failures++;
        else
            findings_failed++;
    }
}

static void info(const WCHAR *label, const WCHAR *text)
{
    say(join(L"INFO ", pass_prefix, label, L": ", text, END));
}

/* ------------------------------------------------------------------------
 * Security descriptors and SIDs
 * ------------------------------------------------------------------------ */

/* The SID a string names, freed with LocalFree, or NULL. */
static PSID sid_of(const WCHAR *text)
{
    PSID sid = NULL;
    if (!ConvertStringSidToSidW(text, &sid))
        return NULL;
    return sid;
}

/* A SID's string, or "?" when it cannot be had. */
static WCHAR *sid_text(PSID sid)
{
    WCHAR *text = NULL;
    WCHAR *copy;
    if (sid == NULL || !ConvertSidToStringSidW(sid, &text))
        return join(L"?", END);
    copy = join(text, END);
    LocalFree(text);
    return copy;
}

#define OWNER_AND_DACL (OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION)

/* A descriptor's owner and DACL in SDDL, or why they cannot be. */
static WCHAR *descriptor_text(PSECURITY_DESCRIPTOR descriptor)
{
    WCHAR *text = NULL;
    WCHAR *copy;
    if (!ConvertSecurityDescriptorToStringSecurityDescriptorW(descriptor, SDDL_REVISION_1, OWNER_AND_DACL, &text, NULL))
        return join(L"unconvertible, ", error_text(GetLastError()), END);
    copy = join(text, END);
    LocalFree(text);
    return copy;
}

/* A path's owner and DACL in SDDL, or why they cannot be read. */
static WCHAR *sddl_of(const WCHAR *path)
{
    PSECURITY_DESCRIPTOR descriptor = NULL;
    WCHAR *text;
    DWORD error = GetNamedSecurityInfoW(path, SE_FILE_OBJECT, OWNER_AND_DACL, NULL, NULL, NULL, NULL, &descriptor);
    if (error != ERROR_SUCCESS)
        return join(L"unreadable, ", error_text(error), END);
    text = descriptor_text(descriptor);
    LocalFree(descriptor);
    return text;
}

/* A kernel object's owner and DACL in SDDL, a token's, a process's or an
 * object directory's, or why they cannot be read.  GetKernelObjectSecurity
 * asks the kernel through the handle alone; GetSecurityInfo goes through
 * ntmarta.dll, which is not a known DLL, and fails with error 127 inside a
 * restricted child. */
static WCHAR *object_sddl(HANDLE object)
{
    DWORD needed = 0;
    PSECURITY_DESCRIPTOR descriptor;
    GetKernelObjectSecurity(object, OWNER_AND_DACL, NULL, 0, &needed);
    if (needed == 0)
        return join(L"unreadable, ", error_text(GetLastError()), END);
    descriptor = take(needed);
    if (!GetKernelObjectSecurity(object, OWNER_AND_DACL, descriptor, needed, &needed))
        return join(L"unreadable, ", error_text(GetLastError()), END);
    return descriptor_text(descriptor);
}

/* A registry key's owner and DACL in SDDL, or why they cannot be read. */
static WCHAR *key_sddl(HKEY key)
{
    DWORD needed = 0;
    PSECURITY_DESCRIPTOR descriptor;
    LSTATUS status = RegGetKeySecurity(key, OWNER_AND_DACL, NULL, &needed);
    if (status != ERROR_INSUFFICIENT_BUFFER)
        return join(L"unreadable, ", error_text((DWORD)status), END);
    descriptor = take(needed);
    status = RegGetKeySecurity(key, OWNER_AND_DACL, descriptor, &needed);
    if (status != ERROR_SUCCESS)
        return join(L"unreadable, ", error_text((DWORD)status), END);
    return descriptor_text(descriptor);
}

/* A directory made with the security descriptor SDDL gives.  Answers 0 or
 * the error. */
static DWORD make_directory(const WCHAR *path, const WCHAR *sddl)
{
    SECURITY_ATTRIBUTES attributes;
    PSECURITY_DESCRIPTOR descriptor = NULL;
    DWORD error = 0;
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

/* A file holding CONTENT, made with the descriptor SDDL gives, or with
 * none when SDDL is NULL.  Answers 0 or the error. */
static DWORD make_file(const WCHAR *path, const WCHAR *sddl, const char *content)
{
    SECURITY_ATTRIBUTES attributes;
    PSECURITY_DESCRIPTOR descriptor = NULL;
    HANDLE file;
    DWORD error = 0;
    DWORD written;
    DWORD length = 0;
    if (sddl != NULL && !ConvertStringSecurityDescriptorToSecurityDescriptorW(sddl, SDDL_REVISION_1, &descriptor, NULL))
        return GetLastError();
    attributes.nLength = sizeof attributes;
    attributes.lpSecurityDescriptor = descriptor;
    attributes.bInheritHandle = FALSE;
    file = CreateFileW(path, GENERIC_WRITE, 0, sddl != NULL ? &attributes : NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE) {
        error = GetLastError();
    } else {
        while (content[length] != 0)
            length++;
        if (!WriteFile(file, content, length, &written, NULL))
            error = GetLastError();
        CloseHandle(file);
    }
    if (descriptor != NULL)
        LocalFree(descriptor);
    return error;
}

/* The first bytes of a file, as text, or NULL when it cannot be read. */
static char *read_start(const WCHAR *path)
{
    char *bytes = take(256);
    DWORD got = 0;
    HANDLE file = CreateFileW(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
    if (file == INVALID_HANDLE_VALUE)
        return NULL;
    if (!ReadFile(file, bytes, 255, &got, NULL))
        got = 0;
    CloseHandle(file);
    bytes[got] = 0;
    return bytes;
}

static int starts_with(const char *text, const char *prefix)
{
    if (text == NULL)
        return 0;
    while (*prefix != 0 && *text == *prefix) {
        text++;
        prefix++;
    }
    return *prefix == 0;
}

/* Text received from a child, as UTF-8 bytes, widened for a line. */
static WCHAR *widen(const char *bytes, DWORD length)
{
    int count;
    WCHAR *text;
    if (bytes == NULL || length == 0)
        return join(L"", END);
    count = MultiByteToWideChar(CP_UTF8, 0, bytes, (int)length, NULL, 0);
    text = take((size_t)(count + 1) * sizeof(WCHAR));
    MultiByteToWideChar(CP_UTF8, 0, bytes, (int)length, text, count);
    {
        WCHAR *at;
        for (at = text; *at != 0; at++)
            if (*at == L'\n' || *at == L'\r')
                *at = L' ';
    }
    return text;
}

/* ------------------------------------------------------------------------
 * Native calls, looked up in ntdll by name
 * ------------------------------------------------------------------------ */

/* Any function's pointer type, cast to the function's own to call it. */
typedef void (*any_function)(void);

typedef NTSTATUS(NTAPI *directory_call)(PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES);
typedef NTSTATUS(NTAPI *last_status_call)(void);
typedef ULONG(NTAPI *status_error_call)(NTSTATUS);
typedef NTSTATUS(NTAPI *create_parameters_call)(PRTL_USER_PROCESS_PARAMETERS *, PUNICODE_STRING, PUNICODE_STRING,
                                                PUNICODE_STRING, PUNICODE_STRING, PVOID, PUNICODE_STRING,
                                                PUNICODE_STRING, PUNICODE_STRING, PUNICODE_STRING);
typedef NTSTATUS(NTAPI *destroy_parameters_call)(PRTL_USER_PROCESS_PARAMETERS);
typedef NTSTATUS(NTAPI *create_user_process_call)(PUNICODE_STRING, ULONG, PRTL_USER_PROCESS_PARAMETERS,
                                                  PSECURITY_DESCRIPTOR, PSECURITY_DESCRIPTOR, HANDLE, BOOLEAN, HANDLE,
                                                  HANDLE, void *);

/* A function of a DLL this process has loaded, or NULL.  The probe
 * imports nothing past what it needs in every child: a function a
 * restricted child cannot resolve when it starts, such as one forwarding
 * to a DLL outside the known DLLs, would keep the child from starting at
 * all, so the parent looks such functions up when it uses them. */
static any_function function_in(const WCHAR *module, const char *name)
{
    HMODULE loaded = GetModuleHandleW(module);
    return loaded != NULL ? (any_function)GetProcAddress(loaded, name) : NULL;
}

/* An ntdll function, or NULL. */
static any_function native(const char *name)
{
    return function_in(L"ntdll.dll", name);
}

/* The Win32 error an NTSTATUS maps to, for a step's status. */
static DWORD status_error(NTSTATUS status)
{
    status_error_call call = (status_error_call)native("RtlNtStatusToDosError");
    return call != NULL ? (DWORD)call(status) : ERROR_GEN_FAILURE;
}

/* An NTSTATUS on standard output, where a step's status has no room for
 * it. */
static void say_status(NTSTATUS status)
{
    say(join(L"ntstatus ", number((DWORD)status, 1), END));
}

/* The last NTSTATUS this thread converted to a Win32 error, on standard
 * output, and whether it maps to ERROR, the error a call just failed with.
 * A call that sets its error without converting a status leaves an older
 * status there, which then does not map to ERROR or maps by chance. */
static void say_last_status(DWORD error)
{
    last_status_call last = (last_status_call)native("RtlGetLastNtStatus");
    status_error_call convert = (status_error_call)native("RtlNtStatusToDosErrorNoTeb");
    NTSTATUS status;
    if (last == NULL)
        return;
    status = last();
    say(join(L"last ntstatus ", number((DWORD)status, 1),
             convert == NULL              ? L""
             : convert(status) == error   ? L", which maps to the error"
                                          : L", which does not map to the error",
             END));
}

/* TEXT as a counted string, over a copy of its own. */
static UNICODE_STRING counted(const WCHAR *text)
{
    UNICODE_STRING string;
    string.Buffer = join(text, END);
    string.Length = (USHORT)(wide_length(text) * sizeof(WCHAR));
    string.MaximumLength = (USHORT)(string.Length + sizeof(WCHAR));
    return string;
}

/* STATUS_PROCEDURE_NOT_FOUND, from ntstatus.h, which windows.h leaves
 * out: what a native call answers when ntdll lacks it. */
#define NATIVE_CALL_MISSING ((NTSTATUS)0xC000007AL)

/* Open an object directory, or make one, OBJ_OPENIF opening it should it
 * be there.  Answers the NTSTATUS, the handle in DIRECTORY. */
static NTSTATUS object_directory(const WCHAR *path, ACCESS_MASK access, int making, HANDLE *directory)
{
    directory_call call = (directory_call)native(making ? "NtCreateDirectoryObject" : "NtOpenDirectoryObject");
    UNICODE_STRING name = counted(path);
    OBJECT_ATTRIBUTES attributes;
    *directory = NULL;
    if (call == NULL)
        return NATIVE_CALL_MISSING;
    InitializeObjectAttributes(&attributes, &name, OBJ_CASE_INSENSITIVE | (making ? OBJ_OPENIF : 0), NULL, NULL);
    return call(directory, access, &attributes);
}

/* ------------------------------------------------------------------------
 * Running a process in a job of its own
 * ------------------------------------------------------------------------ */

/* The job the checks run in, made by the run command as the process
 * library makes a spawned process's job, or NULL. */
static HANDLE outer_job;

/* What one run of a process came to. */
struct run {
    /* Why the process or its job could not be set up; 0 when it ran. */
    DWORD start_error;
    DWORD exit_code;
    /* Its membership right after it was assigned. */
    BOOL in_own_job;
    BOOL in_outer_job;
    /* The job's completion port reported its last process gone. */
    int active_zero;
    /* The job's completion port reported its memory limit reached. */
    int memory_limit_reached;
    int timed_out;
    char *output;
    DWORD output_length;
};

/* The most of a child's output kept. */
#define OUTPUT_MAX ((DWORD)16384)

/* How long a completion-port wait lasts before the pipe is read again. */
#define PORT_POLL_MS 50

/* How long a finished process is given to close its handles. */
#define SETTLE_MS 5000

#define SPAWN_DETACHED 1
/* The process is left in whatever job this one is in, with no job of its
 * own. */
#define SPAWN_NO_JOB 2
/* The process is attached to a pseudoconsole this process makes and owns,
 * so that it, and what it starts sharing its console, have a console
 * without one being made under their own token. */
#define SPAWN_PSEUDOCONSOLE 4

/* The pseudoconsole's size, and the room its output pipe has, so that it
 * seldom waits on a full pipe between the reads that drain it. */
#define CONSOLE_COLUMNS 80
#define CONSOLE_ROWS 25
#define CONSOLE_PIPE_BYTES 65536

typedef HRESULT(WINAPI *create_pseudoconsole_call)(COORD, HANDLE, HANDLE, DWORD, HPCON *);
typedef void(WINAPI *close_pseudoconsole_call)(HPCON);
typedef BOOL(WINAPI *create_with_token_call)(HANDLE, DWORD, LPCWSTR, LPWSTR, DWORD, LPVOID, LPCWSTR, LPSTARTUPINFOW,
                                              LPPROCESS_INFORMATION);

static WCHAR *describe(const struct run *run);

static void read_available(HANDLE pipe, struct run *result)
{
    DWORD available = 0;
    while (PeekNamedPipe(pipe, NULL, 0, NULL, &available, NULL) && available > 0) {
        char chunk[512];
        DWORD got = 0;
        DWORD want = available < sizeof chunk ? available : (DWORD)sizeof chunk;
        if (!ReadFile(pipe, chunk, want, &got, NULL) || got == 0)
            return;
        if (result->output_length + got <= OUTPUT_MAX) {
            memcpy(result->output + result->output_length, chunk, got);
            result->output_length += got;
        }
    }
}

/* Read and drop what a pipe holds: what a pseudoconsole paints. */
static void drain(HANDLE pipe)
{
    DWORD available = 0;
    while (PeekNamedPipe(pipe, NULL, 0, NULL, &available, NULL) && available > 0) {
        char chunk[512];
        DWORD got = 0;
        DWORD want = available < sizeof chunk ? available : (DWORD)sizeof chunk;
        if (!ReadFile(pipe, chunk, want, &got, NULL) || got == 0)
            return;
    }
}

/* A pseudoconsole, its input's write end in INPUT and its output's read
 * end in OUTPUT.  Answers 0 or the error, an HRESULT where
 * CreatePseudoConsole gave one. */
static DWORD make_pseudoconsole(HPCON *console, HANDLE *input, HANDLE *output)
{
    create_pseudoconsole_call create =
        (create_pseudoconsole_call)function_in(L"kernel32.dll", "CreatePseudoConsole");
    HANDLE input_read = NULL;
    HANDLE output_write = NULL;
    COORD size;
    HRESULT made;
    DWORD error = 0;
    *console = NULL;
    *input = NULL;
    *output = NULL;
    if (create == NULL)
        return ERROR_PROC_NOT_FOUND;
    if (!CreatePipe(&input_read, input, NULL, 0) || !CreatePipe(output, &output_write, NULL, CONSOLE_PIPE_BYTES)) {
        error = GetLastError();
        goto done;
    }
    size.X = CONSOLE_COLUMNS;
    size.Y = CONSOLE_ROWS;
    made = create(size, input_read, output_write, 0, console);
    if (FAILED(made)) {
        error = (DWORD)made;
        *console = NULL;
    }
done:
    /* The pseudoconsole keeps copies of the ends it was given. */
    if (input_read != NULL)
        CloseHandle(input_read);
    if (output_write != NULL)
        CloseHandle(output_write);
    if (error != 0) {
        if (*input != NULL)
            CloseHandle(*input);
        if (*output != NULL)
            CloseHandle(*output);
        *input = NULL;
        *output = NULL;
    }
    return error;
}

/* Whether TOKEN belongs to another logon session than this process's, as
 * a build account's does.  This process, holding no privilege to assign
 * such a token to a process, starts it through the Secondary Logon
 * service, as CreateProcessWithTokenW does with SeImpersonatePrivilege. */
static int other_logon(HANDLE token)
{
    TOKEN_STATISTICS theirs;
    TOKEN_STATISTICS ours;
    HANDLE own = NULL;
    DWORD size;
    int other = 0;
    if (!GetTokenInformation(token, TokenStatistics, &theirs, sizeof theirs, &size)
        || !OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &own))
        return 0;
    if (GetTokenInformation(own, TokenStatistics, &ours, sizeof ours, &size))
        other = theirs.AuthenticationId.LowPart != ours.AuthenticationId.LowPart
                || theirs.AuthenticationId.HighPart != ours.AuthenticationId.HighPart;
    CloseHandle(own);
    return other;
}

/* Run COMMAND under TOKEN (or this process's own when NULL) in a job of
 * its own, as the sandbox helper would run a builder: created suspended,
 * assigned to a job that kills its tree when closed, caps each process's
 * memory and lets an unhandled exception end a process, then resumed.
 * Standard input is NUL and standard output and error one pipe, and only
 * those two handles are inherited.  The job's completion port says when
 * its last process is gone; the wait gives up after TIMEOUT_MS and ends
 * the job.
 *
 * With SPAWN_NO_JOB the process gets no job of its own, staying in
 * whatever jobs this one is in, and the wait is on the process itself.
 * With SPAWN_PSEUDOCONSOLE it is attached to a pseudoconsole this process
 * owns, and SPAWN_DETACHED does not apply.  A token of another logon
 * session is started through the Secondary Logon service, given a
 * STARTUPINFO alone: no handle list, so the process may inherit every
 * inheritable handle this one holds, and no pseudoconsole; the
 * environment this process has; and the new console the service gives by
 * default, on this process's desktop, which the service grants the
 * token's user.  Whether it is in this process's jobs is the service's to
 * decide, and the nesting check says. */
static void spawn(HANDLE token, const WCHAR *command, const WCHAR *directory, SIZE_T memory_cap, DWORD timeout_ms,
                  int flags, struct run *result)
{
    SECURITY_ATTRIBUTES inheritable;
    HANDLE read_end = NULL;
    HANDLE write_end = NULL;
    HANDLE nul = INVALID_HANDLE_VALUE;
    HANDLE job = NULL;
    HANDLE port = NULL;
    HPCON console = NULL;
    HANDLE console_input = NULL;
    HANDLE console_output = NULL;
    HANDLE inherited[2];
    PROCESS_INFORMATION process;
    STARTUPINFOEXW startup;
    SIZE_T list_size = 0;
    LPPROC_THREAD_ATTRIBUTE_LIST list = NULL;
    int list_ready = 0;
    DWORD attribute_count = (flags & SPAWN_PSEUDOCONSOLE) ? 2 : 1;
    int through_service = token != NULL && other_logon(token);
    WCHAR *environment_block = NULL;
    JOBOBJECT_ASSOCIATE_COMPLETION_PORT association;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    WCHAR *mutable_command = join(command, END);
    DWORD creation;
    BOOL created;
    ULONGLONG deadline;

    memset(result, 0, sizeof *result);
    memset(&process, 0, sizeof process);
    memset(&startup, 0, sizeof startup);
    memset(&limits, 0, sizeof limits);
    result->output = take(OUTPUT_MAX);
    inheritable.nLength = sizeof inheritable;
    inheritable.lpSecurityDescriptor = NULL;
    inheritable.bInheritHandle = TRUE;

    if (through_service && (flags & SPAWN_PSEUDOCONSOLE)) {
        result->start_error = ERROR_NOT_SUPPORTED;
        goto done;
    }
    if (!CreatePipe(&read_end, &write_end, &inheritable, 0)) {
        result->start_error = GetLastError();
        goto done;
    }
    SetHandleInformation(read_end, HANDLE_FLAG_INHERIT, 0);
    nul = CreateFileW(L"NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, &inheritable, OPEN_EXISTING, 0, NULL);
    if (nul == INVALID_HANDLE_VALUE) {
        result->start_error = GetLastError();
        goto done;
    }
    if ((flags & SPAWN_PSEUDOCONSOLE)
        && (result->start_error = make_pseudoconsole(&console, &console_input, &console_output)) != 0)
        goto done;
    InitializeProcThreadAttributeList(NULL, attribute_count, 0, &list_size);
    list = take(list_size);
    if (!InitializeProcThreadAttributeList(list, attribute_count, 0, &list_size)) {
        result->start_error = GetLastError();
        goto done;
    }
    list_ready = 1;
    inherited[0] = nul;
    inherited[1] = write_end;
    if (!UpdateProcThreadAttribute(list, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherited, sizeof inherited, NULL, NULL)
        || (console != NULL
            && !UpdateProcThreadAttribute(list, 0, PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE, console, sizeof console, NULL,
                                          NULL))) {
        result->start_error = GetLastError();
        goto done;
    }
    startup.StartupInfo.cb = through_service ? (DWORD)sizeof startup.StartupInfo : (DWORD)sizeof startup;
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdInput = nul;
    startup.StartupInfo.hStdOutput = write_end;
    startup.StartupInfo.hStdError = write_end;
    startup.lpAttributeList = list;

    job = CreateJobObjectW(NULL, NULL);
    port = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 1);
    if (job == NULL || port == NULL) {
        result->start_error = GetLastError();
        goto done;
    }
    association.CompletionKey = job;
    association.CompletionPort = port;
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
                                              | JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION
                                              | (memory_cap > 0 ? JOB_OBJECT_LIMIT_PROCESS_MEMORY : 0);
    limits.ProcessMemoryLimit = memory_cap;
    if (!SetInformationJobObject(job, JobObjectAssociateCompletionPortInformation, &association, sizeof association)
        || !SetInformationJobObject(job, JobObjectExtendedLimitInformation, &limits, sizeof limits)) {
        result->start_error = GetLastError();
        goto done;
    }

    creation = CREATE_SUSPENDED | CREATE_UNICODE_ENVIRONMENT;
    if (through_service) {
        create_with_token_call create_with_token =
            (create_with_token_call)function_in(L"advapi32.dll", "CreateProcessWithTokenW");
        environment_block = GetEnvironmentStringsW();
        if (create_with_token == NULL) {
            result->start_error = ERROR_PROC_NOT_FOUND;
            goto done;
        }
        created = create_with_token(token, 0, NULL, mutable_command, creation, environment_block, directory,
                                    &startup.StartupInfo, &process);
    } else {
        creation |= EXTENDED_STARTUPINFO_PRESENT
                    | ((flags & SPAWN_DETACHED) && console == NULL ? DETACHED_PROCESS : 0);
        if (token != NULL)
            created = CreateProcessAsUserW(token, NULL, mutable_command, NULL, NULL, TRUE, creation, NULL, directory,
                                           &startup.StartupInfo, &process);
        else
            created = CreateProcessW(NULL, mutable_command, NULL, NULL, TRUE, creation, NULL, directory,
                                     &startup.StartupInfo, &process);
    }
    if (!created) {
        result->start_error = GetLastError();
        goto done;
    }
    if (!(flags & SPAWN_NO_JOB) && !AssignProcessToJobObject(job, process.hProcess)) {
        result->start_error = GetLastError();
        TerminateProcess(process.hProcess, 1);
        goto done;
    }
    IsProcessInJob(process.hProcess, job, &result->in_own_job);
    if (outer_job != NULL)
        IsProcessInJob(process.hProcess, outer_job, &result->in_outer_job);
    CloseHandle(write_end);
    write_end = NULL;
    ResumeThread(process.hThread);

    deadline = GetTickCount64() + timeout_ms;
    if (flags & SPAWN_NO_JOB)
        while (WaitForSingleObject(process.hProcess, PORT_POLL_MS) != WAIT_OBJECT_0) {
            read_available(read_end, result);
            if (console_output != NULL)
                drain(console_output);
            if (GetTickCount64() > deadline) {
                result->timed_out = 1;
                TerminateProcess(process.hProcess, 1);
                break;
            }
        }
    while (!(flags & SPAWN_NO_JOB) && !result->active_zero) {
        DWORD message = 0;
        ULONG_PTR key = 0;
        LPOVERLAPPED detail = NULL;
        read_available(read_end, result);
        if (console_output != NULL)
            drain(console_output);
        if (GetQueuedCompletionStatus(port, &message, &key, &detail, PORT_POLL_MS) && key == (ULONG_PTR)job) {
            if (message == JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO)
                result->active_zero = 1;
            else if (message == JOB_OBJECT_MSG_PROCESS_MEMORY_LIMIT)
                result->memory_limit_reached = 1;
        }
        if (!result->active_zero && GetTickCount64() > deadline) {
            result->timed_out = 1;
            TerminateJobObject(job, 1);
            break;
        }
    }
    WaitForSingleObject(process.hProcess, SETTLE_MS);
    read_available(read_end, result);
    GetExitCodeProcess(process.hProcess, &result->exit_code);

done:
    /* The pipes close before the pseudoconsole does: before Windows 11
     * 24H2, ClosePseudoConsole waits for its last output to be read, which
     * a closed pipe ends at once. */
    if (console_input != NULL)
        CloseHandle(console_input);
    if (console_output != NULL)
        CloseHandle(console_output);
    if (console != NULL) {
        close_pseudoconsole_call close_console =
            (close_pseudoconsole_call)function_in(L"kernel32.dll", "ClosePseudoConsole");
        if (close_console != NULL)
            close_console(console);
    }
    if (environment_block != NULL)
        FreeEnvironmentStringsW(environment_block);
    if (process.hThread != NULL)
        CloseHandle(process.hThread);
    if (process.hProcess != NULL)
        CloseHandle(process.hProcess);
    if (list_ready)
        DeleteProcThreadAttributeList(list);
    if (job != NULL)
        CloseHandle(job);
    if (port != NULL)
        CloseHandle(port);
    if (nul != INVALID_HANDLE_VALUE)
        CloseHandle(nul);
    if (write_end != NULL)
        CloseHandle(write_end);
    if (read_end != NULL)
        CloseHandle(read_end);
}

/* ------------------------------------------------------------------------
 * The child: one operation, reported through its exit status
 * ------------------------------------------------------------------------ */

/* A child's exit status for a step that failed: the step's number in the
 * top byte, kept below the 0xC0000000 an NTSTATUS has, and the error. */
#define STEP_FAILED(step, error) ((((DWORD)(step)) << 24) | ((DWORD)(error) & 0xFFFFFFu))
#define STEP_OF(status) ((status) >> 24)
#define ERROR_OF(status) ((status) & 0xFFFFFFu)

/* What the pipe check writes. */
static const char pipe_marker[] = "probe-pipe-ok\n";

/* What the memory check reads out of the checks' own process, at the
 * address that process gives; the reader holds the same text to compare. */
static const char memory_marker[] = "nova-nix probe memory marker";

/* How long the sleep operation lasts, so that nothing it leaves outlives
 * the job by more than this. */
#define SLEEP_MS 60000

/* A junction's reparse data (REPARSE_DATA_BUFFER's mount-point form,
 * which the user-mode headers do not declare). */
struct mount_point {
    DWORD tag;
    WORD data_length;
    WORD reserved;
    WORD substitute_offset;
    WORD substitute_length;
    WORD print_offset;
    WORD print_length;
    WCHAR paths[];
};

/* The fixed part of a reparse buffer before its data. */
#define REPARSE_HEADER_BYTES 8
/* The four offset and length fields before a mount point's paths. */
#define MOUNT_POINT_FIELDS_BYTES 8

static DWORD make_junction(const WCHAR *path, const WCHAR *target)
{
    WCHAR *substitute = join(L"\\??\\", target, END);
    size_t substitute_bytes = wide_length(substitute) * sizeof(WCHAR);
    size_t print_bytes = wide_length(target) * sizeof(WCHAR);
    size_t paths_bytes = substitute_bytes + sizeof(WCHAR) + print_bytes + sizeof(WCHAR);
    size_t total = REPARSE_HEADER_BYTES + MOUNT_POINT_FIELDS_BYTES + paths_bytes;
    struct mount_point *buffer = take(total + sizeof(WCHAR));
    HANDLE directory;
    DWORD returned;
    DWORD status = 0;
    if (!CreateDirectoryW(path, NULL))
        return STEP_FAILED(1, GetLastError());
    buffer->tag = IO_REPARSE_TAG_MOUNT_POINT;
    buffer->data_length = (WORD)(MOUNT_POINT_FIELDS_BYTES + paths_bytes);
    buffer->substitute_offset = 0;
    buffer->substitute_length = (WORD)substitute_bytes;
    buffer->print_offset = (WORD)(substitute_bytes + sizeof(WCHAR));
    buffer->print_length = (WORD)print_bytes;
    memcpy(buffer->paths, substitute, substitute_bytes);
    memcpy((char *)buffer->paths + substitute_bytes + sizeof(WCHAR), target, print_bytes);
    directory = CreateFileW(path, GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                            OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
    if (directory == INVALID_HANDLE_VALUE)
        return STEP_FAILED(2, GetLastError());
    if (!DeviceIoControl(directory, FSCTL_SET_REPARSE_POINT, buffer, (DWORD)total, NULL, 0, &returned, NULL))
        status = STEP_FAILED(3, GetLastError());
    CloseHandle(directory);
    return status;
}

/* What a process opens its own token with, here: enough to make restricted
 * tokens from it and to set their owner, default DACL and object DACL,
 * which a restricted token's handle inherits from this one. */
#define BASE_TOKEN_ACCESS \
    (TOKEN_DUPLICATE | TOKEN_ASSIGN_PRIMARY | TOKEN_QUERY | TOKEN_ADJUST_DEFAULT | READ_CONTROL | WRITE_DAC)

/* READ_CONTROL, TOKEN_QUERY, TOKEN_QUERY_SOURCE and TOKEN_ADJUST_DEFAULT:
 * what a build may do with its own token. */
#define RIGHTS_OWN_TOKEN L"0x20098"

/* SIDs the token shapes and layouts name. */
#define RESTRICTED_SID L"S-1-5-12"
#define OWNER_RIGHTS_SID L"S-1-3-4"
#define USERS_SID L"S-1-5-32-545"
#define EVERYONE_SID L"S-1-1-0"
#define ADMINISTRATORS_SID L"S-1-5-32-544"
#define LOW_INTEGRITY_SID L"S-1-16-4096"

/* How a build's token is made: as the restricted-token design has it, or
 * with a candidate change to it. */
struct token_shape {
    /* Restricting SIDs beside S-1-5-12 and the build's, NULL past the
     * last. */
    const WCHAR *extra_sids[2];
    /* The build SID's rights in the token's default DACL, in SDDL. */
    const WCHAR *build_rights;
    /* What the token object grants the build, in SDDL, or NULL to leave
     * its DACL as made. */
    const WCHAR *own_token_rights;
    /* CreateRestrictedToken's flags beside DISABLE_MAX_PRIVILEGE. */
    DWORD flags;
    /* The integrity level the token is lowered to, as a SID, or NULL. */
    const WCHAR *integrity;
    /* Whether the user's SID and Administrators are made deny-only, so
     * that only the user's other groups grant the token anything. */
    int user_deny_only;
};

#define BUILD_RIGHTS L"GRGWGXSD"

/* The restricted-token design: read, write, execute and delete for the
 * build's SID on what the build makes. */
static const struct token_shape design_shape = {{NULL, NULL}, BUILD_RIGHTS, RIGHTS_OWN_TOKEN, 0, NULL, 0};
/* The same, the token object's DACL left as made. */
static const struct token_shape bare_shape = {{NULL, NULL}, BUILD_RIGHTS, NULL, 0, NULL, 0};
/* Candidate: Users and Everyone restricting too, which the system's own
 * files, keys and named-object directories grant, as Chromium's
 * USER_LIMITED token has them. */
static const struct token_shape limited_shape = {{USERS_SID, EVERYONE_SID}, BUILD_RIGHTS, RIGHTS_OWN_TOKEN, 0, NULL, 0};
/* Candidates: the restricting SIDs checked for writes alone, every read
 * left to the user's own SIDs; with Everyone restricting too; lowered to
 * Low integrity; and with the user's SID deny-only. */
static const struct token_shape write_restricted_shape = {{NULL, NULL}, BUILD_RIGHTS, RIGHTS_OWN_TOKEN, WRITE_RESTRICTED,
                                                          NULL, 0};
static const struct token_shape write_restricted_everyone_shape = {
    {EVERYONE_SID, NULL}, BUILD_RIGHTS, RIGHTS_OWN_TOKEN, WRITE_RESTRICTED, NULL, 0};
static const struct token_shape write_restricted_low_shape = {
    {NULL, NULL}, BUILD_RIGHTS, RIGHTS_OWN_TOKEN, WRITE_RESTRICTED, LOW_INTEGRITY_SID, 0};
static const struct token_shape write_restricted_deny_user_shape = {
    {NULL, NULL}, BUILD_RIGHTS, RIGHTS_OWN_TOKEN, WRITE_RESTRICTED, NULL, 1};

#define RESTRICTING_MAX 4
#define DISABLED_MAX 2

/* A restricted token made as SHAPE says: every privilege but bypassing
 * traverse checks removed, and the restricting SIDs S-1-5-12, BUILD when
 * given, and the shape's own.  USER is the user's SID, which the shape
 * may make deny-only. */
static HANDLE restricted_token(HANDLE base, PSID build, const struct token_shape *shape, PSID user, DWORD *error)
{
    SID_AND_ATTRIBUTES restricting[RESTRICTING_MAX];
    SID_AND_ATTRIBUTES disabled[DISABLED_MAX];
    PSID made[RESTRICTING_MAX];
    PSID administrators = NULL;
    HANDLE token = NULL;
    DWORD count = 0;
    DWORD disabled_count = 0;
    DWORD made_count = 0;
    size_t i;
    *error = 0;
    made[made_count] = sid_of(RESTRICTED_SID);
    if (made[made_count] == NULL) {
        *error = GetLastError();
        goto done;
    }
    restricting[count].Sid = made[made_count++];
    restricting[count++].Attributes = 0;
    if (build != NULL) {
        restricting[count].Sid = build;
        restricting[count++].Attributes = 0;
    }
    for (i = 0; i < sizeof shape->extra_sids / sizeof shape->extra_sids[0] && shape->extra_sids[i] != NULL; i++) {
        made[made_count] = sid_of(shape->extra_sids[i]);
        if (made[made_count] == NULL) {
            *error = GetLastError();
            goto done;
        }
        restricting[count].Sid = made[made_count++];
        restricting[count++].Attributes = 0;
    }
    if (shape->user_deny_only) {
        administrators = sid_of(ADMINISTRATORS_SID);
        if (administrators == NULL) {
            *error = GetLastError();
            goto done;
        }
        disabled[disabled_count].Sid = user;
        disabled[disabled_count++].Attributes = 0;
        disabled[disabled_count].Sid = administrators;
        disabled[disabled_count++].Attributes = 0;
    }
    if (!CreateRestrictedToken(base, DISABLE_MAX_PRIVILEGE | shape->flags, disabled_count,
                               disabled_count > 0 ? disabled : NULL, 0, NULL, count, restricting, &token)) {
        *error = GetLastError();
        token = NULL;
    }
done:
    while (made_count > 0)
        LocalFree(made[--made_count]);
    if (administrators != NULL)
        LocalFree(administrators);
    return token;
}

/* The DACL an SDDL descriptor holds, in a descriptor freed with
 * LocalFree, or NULL. */
static PSECURITY_DESCRIPTOR dacl_of(const WCHAR *sddl, PACL *dacl)
{
    PSECURITY_DESCRIPTOR descriptor = NULL;
    BOOL present = FALSE;
    BOOL defaulted = FALSE;
    *dacl = NULL;
    if (!ConvertStringSecurityDescriptorToSecurityDescriptorW(sddl, SDDL_REVISION_1, &descriptor, NULL))
        return NULL;
    if (!GetSecurityDescriptorDacl(descriptor, &present, dacl, &defaulted) || !present) {
        LocalFree(descriptor);
        *dacl = NULL;
        return NULL;
    }
    return descriptor;
}

/* Set a token's default owner when OWNER is given, its default DACL, and,
 * when OBJECT_SDDL is given, the DACL of the token object itself.  Answers
 * 0 or the error. */
static DWORD dress_token(HANDLE token, PSID owner, const WCHAR *default_sddl, const WCHAR *object_sddl)
{
    TOKEN_OWNER owner_information;
    TOKEN_DEFAULT_DACL default_information;
    PSECURITY_DESCRIPTOR descriptor;
    PACL dacl;
    DWORD error = 0;
    owner_information.Owner = owner;
    if (owner != NULL && !SetTokenInformation(token, TokenOwner, &owner_information, sizeof owner_information))
        return GetLastError();
    descriptor = dacl_of(default_sddl, &dacl);
    if (descriptor == NULL)
        return GetLastError();
    default_information.DefaultDacl = dacl;
    if (!SetTokenInformation(token, TokenDefaultDacl, &default_information, sizeof default_information))
        error = GetLastError();
    LocalFree(descriptor);
    if (error != 0 || object_sddl == NULL)
        return error;
    descriptor = dacl_of(object_sddl, &dacl);
    if (descriptor == NULL)
        return GetLastError();
    error = SetSecurityInfo(token, SE_KERNEL_OBJECT, DACL_SECURITY_INFORMATION, NULL, NULL, dacl, NULL);
    LocalFree(descriptor);
    return error;
}

/* Lower a token to the integrity level a SID's string names.  Answers 0
 * or the error. */
static DWORD lower_integrity(HANDLE token, const WCHAR *level)
{
    TOKEN_MANDATORY_LABEL label;
    PSID sid = sid_of(level);
    DWORD error = 0;
    if (sid == NULL)
        return GetLastError();
    label.Label.Sid = sid;
    label.Label.Attributes = SE_GROUP_INTEGRITY;
    if (!SetTokenInformation(token, TokenIntegrityLevel, &label, sizeof label + GetLengthSid(sid)))
        error = GetLastError();
    LocalFree(sid);
    return error;
}

/* A build's default DACL: the user and SYSTEM in full and the build's own
 * SID the RIGHTS given, inherited below.  Generic rights, so each kind of
 * object it reaches, a file or a process, maps them to its own. */
static WCHAR *build_default_sddl(const WCHAR *user, const WCHAR *build, const WCHAR *rights)
{
    return join(L"D:(A;OICI;GA;;;", user, L")(A;OICI;GA;;;SY)(A;OICI;", rights, L";;;", build, L")", END);
}

/* A build token's own object DACL: the user and SYSTEM in full, the build
 * the RIGHTS given. */
static WCHAR *build_token_sddl(const WCHAR *user, const WCHAR *build, const WCHAR *rights)
{
    return join(L"D:(A;;GA;;;", user, L")(A;;GA;;;SY)(A;;", rights, L";;;", build, L")", END);
}

/* The steps of making a build's token, for the status it answers. */
#define TOKEN_STEP_RESTRICT 1
#define TOKEN_STEP_DRESS 2
#define TOKEN_STEP_INTEGRITY 3

/* A build's token, made and dressed as SHAPE says, or NULL with ERROR
 * holding the step that failed and its error.  BUILD is the build's SID
 * and BUILD_TEXT its string; with BUILD NULL the token restricts to
 * S-1-5-12 alone, and BUILD_TEXT names the SID its default DACL and
 * object grant.  A token whose user's SID is deny-only keeps the owner it
 * was made with. */
static HANDLE build_token(HANDLE base, PSID user, const WCHAR *user_text, PSID build, const WCHAR *build_text,
                          const struct token_shape *shape, DWORD *error)
{
    DWORD failure;
    HANDLE token = restricted_token(base, build, shape, user, &failure);
    if (token == NULL) {
        *error = STEP_FAILED(TOKEN_STEP_RESTRICT, failure);
        return NULL;
    }
    failure = dress_token(token, shape->user_deny_only ? NULL : user,
                          build_default_sddl(user_text, build_text, shape->build_rights),
                          shape->own_token_rights != NULL
                              ? build_token_sddl(user_text, build_text, shape->own_token_rights)
                              : NULL);
    if (failure != 0) {
        CloseHandle(token);
        *error = STEP_FAILED(TOKEN_STEP_DRESS, failure);
        return NULL;
    }
    if (shape->integrity != NULL && (failure = lower_integrity(token, shape->integrity)) != 0) {
        CloseHandle(token);
        *error = STEP_FAILED(TOKEN_STEP_INTEGRITY, failure);
        return NULL;
    }
    *error = 0;
    return token;
}

/* Why build_token made no token, from the status it answered. */
static WCHAR *token_failure(DWORD status)
{
    return join(STEP_OF(status) == TOKEN_STEP_RESTRICT ? L"CreateRestrictedToken failed, "
                : STEP_OF(status) == TOKEN_STEP_DRESS  ? L"setting its owner, default DACL or object DACL failed, "
                                                       : L"lowering its integrity level failed, ",
                error_text(ERROR_OF(status)), END);
}

static WCHAR *own_path(void)
{
    WCHAR *path = take(32768 * sizeof(WCHAR));
    GetModuleFileNameW(NULL, path, 32768);
    return path;
}

/* The directory holding PATH, which names a file or directory below
 * another. */
static WCHAR *parent_directory(const WCHAR *path)
{
    WCHAR *directory = join(path, END);
    WCHAR *last = NULL;
    WCHAR *at;
    for (at = directory; *at != 0; at++)
        if (*at == L'\\')
            last = at;
    if (last != NULL)
        *last = 0;
    return directory;
}

/* Under a token with no restricting SID, whose own token is a standard
 * user's stand-in: make a restricted token of its own and run the pipe
 * check under it, as nova-nix running as a standard user would. */
static DWORD standard_user_spawn(void)
{
    HANDLE base = NULL;
    HANDLE token;
    DWORD error;
    struct run result;
    if (!OpenProcessToken(GetCurrentProcess(), BASE_TOKEN_ACCESS, &base))
        return STEP_FAILED(1, GetLastError());
    token = restricted_token(base, NULL, &design_shape, NULL, &error);
    CloseHandle(base);
    if (token == NULL)
        return STEP_FAILED(2, error);
    spawn(token, join(L"\"", own_path(), L"\" child pipe", END), NULL, 0, 30000, SPAWN_DETACHED, &result);
    CloseHandle(token);
    /* The status below has room for an error, not for a grandchild's
     * whole status, so the whole of it goes to standard output. */
    say(join(L"the restricted grandchild ", describe(&result), END));
    if (result.start_error != 0)
        return STEP_FAILED(3, result.start_error);
    if (result.exit_code != 0)
        return STEP_FAILED(4, ERROR_GEN_FAILURE);
    return 0;
}

/* The root key a path starts with, HKLM\ or HKU\, and the rest of the
 * path in REST.  Answers 0 for any other. */
static int key_root(const WCHAR *path, HKEY *root, const WCHAR **rest)
{
    static const struct {
        const WCHAR *prefix;
        HKEY root;
    } roots[] = {{L"HKLM\\", HKEY_LOCAL_MACHINE}, {L"HKU\\", HKEY_USERS}};
    size_t i;
    for (i = 0; i < sizeof roots / sizeof roots[0]; i++) {
        size_t length = wide_length(roots[i].prefix);
        size_t at = 0;
        while (at < length && path[at] == roots[i].prefix[at])
            at++;
        if (at == length) {
            *root = roots[i].root;
            *rest = path + length;
            return 1;
        }
    }
    return 0;
}

/* The exit code cmd.exe is asked for, which it gives only by running the
 * command it was handed. */
#define CMD_EXIT_CODE 7

/* Start a program with CreateProcessW under this process's own token and
 * wait for it: WHAT is "probe", this program's noop, or "cmd", cmd.exe
 * exiting with CMD_EXIT_CODE.  FLAGS are CreateProcessW's; with
 * CREATE_BREAKAWAY_FROM_JOB the program is ended at once instead, since
 * no job would end it.  Where CreateProcessW fails, the thread's last
 * NTSTATUS goes to standard output; where the program ends with another
 * code than it should, that code does. */
static DWORD start_process(const WCHAR *what, DWORD flags)
{
    STARTUPINFOW startup;
    PROCESS_INFORMATION process;
    WCHAR *system = take(MAX_PATH * sizeof(WCHAR));
    WCHAR *command;
    DWORD expected;
    DWORD code = 0;
    DWORD status = 0;
    if (wide_equal(what, L"cmd")) {
        if (GetSystemDirectoryW(system, MAX_PATH) == 0)
            return STEP_FAILED(1, GetLastError());
        /* /d leaves out the AutoRun commands the registry may hold. */
        command = join(L"\"", system, L"\\cmd.exe\" /d /c exit ", number(CMD_EXIT_CODE, 0), END);
        expected = CMD_EXIT_CODE;
    } else {
        command = join(L"\"", own_path(), L"\" child noop", END);
        expected = 0;
    }
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    if (!CreateProcessW(NULL, command, NULL, NULL, FALSE, flags, NULL, NULL, &startup, &process)) {
        DWORD error = GetLastError();
        say_last_status(error);
        return STEP_FAILED(2, error);
    }
    if (flags & CREATE_BREAKAWAY_FROM_JOB) {
        /* It escaped every job, so nothing else would end it. */
        TerminateProcess(process.hProcess, 1);
    } else if (WaitForSingleObject(process.hProcess, SETTLE_MS) != WAIT_OBJECT_0) {
        TerminateProcess(process.hProcess, 1);
        status = STEP_FAILED(3, WAIT_TIMEOUT);
    } else if (!GetExitCodeProcess(process.hProcess, &code)) {
        status = STEP_FAILED(4, GetLastError());
    } else if (code != expected) {
        say(join(L"it exited with ", number(code, 1), END));
        status = STEP_FAILED(4, ERROR_GEN_FAILURE);
    }
    CloseHandle(process.hThread);
    CloseHandle(process.hProcess);
    return status;
}

/* What RtlCreateUserProcess answers (RTL_USER_PROCESS_INFORMATION): its
 * length, the process's and thread's handles and IDs, then the image's
 * SECTION_IMAGE_INFORMATION, given more room here than it takes. */
struct native_process {
    ULONG length;
    HANDLE process;
    HANDLE thread;
    CLIENT_ID client;
    BYTE image[256];
};

/* Start this program's noop with RtlCreateUserProcess, the native call
 * that creates a process and its first thread without what CreateProcessW
 * does around that in user mode: the code-authorization and compatibility
 * checks and the registration with the Windows subsystem.  The thread
 * starts suspended; resumed, the noop runs its own startup.  A failure's
 * NTSTATUS goes to standard output. */
static DWORD start_native_process(void)
{
    create_parameters_call create_parameters = (create_parameters_call)native("RtlCreateProcessParameters");
    destroy_parameters_call destroy_parameters = (destroy_parameters_call)native("RtlDestroyProcessParameters");
    create_user_process_call create_process = (create_user_process_call)native("RtlCreateUserProcess");
    UNICODE_STRING image;
    UNICODE_STRING command;
    PRTL_USER_PROCESS_PARAMETERS parameters = NULL;
    struct native_process made;
    NTSTATUS status;
    DWORD code = 0;
    DWORD result = 0;
    if (create_parameters == NULL || destroy_parameters == NULL || create_process == NULL)
        return STEP_FAILED(1, ERROR_PROC_NOT_FOUND);
    image = counted(join(L"\\??\\", own_path(), END));
    command = counted(join(L"\"", own_path(), L"\" child noop", END));
    status = create_parameters(&parameters, &image, NULL, NULL, &command, NULL, NULL, NULL, NULL, NULL);
    if (!NT_SUCCESS(status)) {
        say_status(status);
        return STEP_FAILED(2, status_error(status));
    }
    memset(&made, 0, sizeof made);
    status = create_process(&image, OBJ_CASE_INSENSITIVE, parameters, NULL, NULL, NULL, FALSE, NULL, NULL, &made);
    destroy_parameters(parameters);
    if (!NT_SUCCESS(status)) {
        say_status(status);
        return STEP_FAILED(3, status_error(status));
    }
    ResumeThread(made.thread);
    if (WaitForSingleObject(made.process, SETTLE_MS) != WAIT_OBJECT_0) {
        TerminateProcess(made.process, 1);
        result = STEP_FAILED(4, WAIT_TIMEOUT);
    } else if (GetExitCodeProcess(made.process, &code) && code != 0) {
        say(join(L"it exited with ", number(code, 1), END));
        result = STEP_FAILED(5, ERROR_GEN_FAILURE);
    }
    CloseHandle(made.thread);
    CloseHandle(made.process);
    return result;
}

static DWORD child(int count, WCHAR **arguments)
{
    const WCHAR *op = count > 2 ? arguments[2] : L"";
    const WCHAR *first = count > 3 ? arguments[3] : L"";
    const WCHAR *second = count > 4 ? arguments[4] : L"";

    if (wide_equal(op, L"pipe")) {
        DWORD written = 0;
        if (!WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), pipe_marker, sizeof pipe_marker - 1, &written, NULL))
            return STEP_FAILED(1, GetLastError());
        return 0;
    }
    if (wide_equal(op, L"open")) {
        HANDLE file = CreateFileW(first, parse_number(second), FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                                  OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT, NULL);
        if (file == INVALID_HANDLE_VALUE)
            return STEP_FAILED(1, GetLastError());
        CloseHandle(file);
        return 0;
    }
    if (wide_equal(op, L"create")) {
        DWORD written = 0;
        DWORD status = 0;
        HANDLE file = CreateFileW(first, GENERIC_WRITE, 0, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
        if (file == INVALID_HANDLE_VALUE)
            return STEP_FAILED(1, GetLastError());
        if (!WriteFile(file, "data\n", 5, &written, NULL))
            status = STEP_FAILED(2, GetLastError());
        CloseHandle(file);
        return status;
    }
    if (wide_equal(op, L"mkdir"))
        return CreateDirectoryW(first, NULL) ? 0 : STEP_FAILED(1, GetLastError());
    if (wide_equal(op, L"mkdir-sd")) {
        DWORD error = make_directory(first, second);
        return error == 0 ? 0 : STEP_FAILED(1, error);
    }
    if (wide_equal(op, L"junction"))
        return make_junction(first, second);
    if (wide_equal(op, L"delete")) {
        DWORD attributes = GetFileAttributesW(first);
        if (attributes == INVALID_FILE_ATTRIBUTES)
            return STEP_FAILED(1, GetLastError());
        if (attributes & FILE_ATTRIBUTE_DIRECTORY)
            return RemoveDirectoryW(first) ? 0 : STEP_FAILED(2, GetLastError());
        return DeleteFileW(first) ? 0 : STEP_FAILED(2, GetLastError());
    }
    if (wide_equal(op, L"rename"))
        return MoveFileExW(first, second, 0) ? 0 : STEP_FAILED(1, GetLastError());
    if (wide_equal(op, L"alloc")) {
        SIZE_T bytes = (SIZE_T)parse_number(first) * 1024 * 1024;
        void *block = VirtualAlloc(NULL, bytes, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
        if (block == NULL)
            return STEP_FAILED(1, GetLastError());
        VirtualFree(block, 0, MEM_RELEASE);
        return 0;
    }
    if (wide_equal(op, L"crash")) {
        RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 0, NULL);
        return STEP_FAILED(1, 0);
    }
    if (wide_equal(op, L"noop"))
        return 0;
    if (wide_equal(op, L"open-token")) {
        HANDLE token = NULL;
        if (!OpenProcessToken(GetCurrentProcess(), parse_number(first), &token))
            return STEP_FAILED(1, GetLastError());
        CloseHandle(token);
        return 0;
    }
    /* The owner and DACL of the token object this process runs with, on
     * standard output. */
    if (wide_equal(op, L"token-sddl")) {
        HANDLE token = NULL;
        if (!OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY | READ_CONTROL, &token))
            return STEP_FAILED(1, GetLastError());
        say(object_sddl(token));
        CloseHandle(token);
        return 0;
    }
    if (wide_equal(op, L"load")) {
        HMODULE module = LoadLibraryExW(first, NULL, 0);
        if (module == NULL)
            return STEP_FAILED(1, GetLastError());
        FreeLibrary(module);
        return 0;
    }
    /* Look a function up by name in a DLL, as a runtime that loads what it
     * calls lazily does; an export that forwards to another DLL loads that
     * DLL on the way. */
    if (wide_equal(op, L"resolve")) {
        HMODULE module = LoadLibraryExW(first, NULL, 0);
        DWORD status = 0;
        if (module == NULL)
            return STEP_FAILED(1, GetLastError());
        if (GetProcAddress(module, ascii(second)) == NULL)
            status = STEP_FAILED(2, GetLastError());
        FreeLibrary(module);
        return status;
    }
    if (wide_equal(op, L"spawn"))
        return start_process(first, parse_number(second));
    if (wide_equal(op, L"native-spawn"))
        return start_native_process();
    if (wide_equal(op, L"open-directory") || wide_equal(op, L"create-directory")) {
        HANDLE directory;
        NTSTATUS status = object_directory(first, parse_number(second), wide_equal(op, L"create-directory"), &directory);
        if (!NT_SUCCESS(status)) {
            say_status(status);
            return STEP_FAILED(1, status_error(status));
        }
        CloseHandle(directory);
        return 0;
    }
    /* A registry key opened for reading, under HKLM\ or HKU\.  The
     * user's own hive is named under HKU by its SID, since HKCU can stand
     * for the default user's hive when the user's own cannot be opened. */
    if (wide_equal(op, L"open-key")) {
        HKEY root;
        const WCHAR *path;
        HKEY key;
        LSTATUS status;
        if (!key_root(first, &root, &path))
            return STEP_FAILED(1, ERROR_INVALID_PARAMETER);
        status = RegOpenKeyExW(root, path, 0, KEY_READ, &key);
        if (status != ERROR_SUCCESS)
            return STEP_FAILED(2, (DWORD)status);
        RegCloseKey(key);
        return 0;
    }
    /* Read another process's memory: open it for PROCESS_VM_READ, as
     * reading another program's secrets out of its memory starts. */
    if (wide_equal(op, L"read-memory")) {
        HANDLE target = OpenProcess(PROCESS_VM_READ, FALSE, parse_number(first));
        char copy[sizeof memory_marker];
        SIZE_T got = 0;
        DWORD status = 0;
        if (target == NULL)
            return STEP_FAILED(1, GetLastError());
        if (!ReadProcessMemory(target, (LPCVOID)parse_address(second), copy, sizeof copy, &got))
            status = STEP_FAILED(2, GetLastError());
        else if (got != sizeof copy || !starts_with(copy, memory_marker))
            status = STEP_FAILED(3, ERROR_INVALID_DATA);
        CloseHandle(target);
        return status;
    }
    if (wide_equal(op, L"sleep")) {
        Sleep(SLEEP_MS);
        return 0;
    }
    if (wide_equal(op, L"spawn-test"))
        return standard_user_spawn();
    return STEP_FAILED(0x3F, ERROR_INVALID_PARAMETER);
}

/* ------------------------------------------------------------------------
 * The checks
 * ------------------------------------------------------------------------ */

/* What one operation in a child came to. */
struct outcome {
    /* The child started and the operation ended with a status of its own,
     * success or a step's Win32 error, not a crash or a failure to
     * start. */
    int ran;
    int succeeded;
    /* The operation ran and was refused access. */
    int denied;
    WCHAR *detail;
    struct run run;
};

/* The default memory cap of a check's job: room for any one operation. */
#define CHECK_MEMORY_CAP ((SIZE_T)256 * 1024 * 1024)
#define CHECK_TIMEOUT_MS 30000

static const WCHAR *self_path;
static const WCHAR *child_directory;

/* The status of a process that crashed or could not initialise. */
#define NTSTATUS_FLOOR 0xC0000000u

/* The highest step any operation numbers; a status naming a step past it
 * is not one an operation wrote. */
#define LAST_STEP 5

/* Whether a run ended with a status an operation wrote. */
static int operation_status(const struct run *run)
{
    DWORD step = STEP_OF(run->exit_code);
    return run->start_error == 0 && !run->timed_out
           && (run->exit_code == 0 || (step >= 1 && step <= LAST_STEP));
}

/* How a run of the probe's own child ended, its status read as a step and
 * an error. */
static WCHAR *describe(const struct run *run)
{
    if (run->start_error != 0)
        return join(L"did not start, ", error_text(run->start_error), END);
    if (run->timed_out)
        return join(L"timed out", END);
    if (run->exit_code == 0)
        return join(L"succeeded", END);
    if (!operation_status(run))
        return join(L"ended with status ", number(run->exit_code, 1), END);
    return join(L"failed at step ", number(STEP_OF(run->exit_code), 0), L", ", error_text(ERROR_OF(run->exit_code)),
                END);
}

/* How a run of another program ended, its status read as an exit code or
 * an NTSTATUS. */
static WCHAR *describe_program(const struct run *run)
{
    if (run->start_error != 0)
        return join(L"did not start, ", error_text(run->start_error), END);
    if (run->timed_out)
        return join(L"timed out", END);
    if (run->exit_code >= NTSTATUS_FLOOR)
        return join(L"ended with status ", number(run->exit_code, 1), END);
    return join(L"exited with ", number(run->exit_code, 0), END);
}

/* Run one child operation under TOKEN, or under this process's own token
 * when TOKEN is NULL, spawned with FLAGS. */
static struct outcome operation_with(HANDLE token, int flags, const WCHAR *op, const WCHAR *first, const WCHAR *second)
{
    struct outcome result;
    WCHAR *command = join(L"\"", self_path, L"\" child ", op, first != NULL ? L" \"" : L"", first != NULL ? first : L"",
                          first != NULL ? L"\"" : L"", second != NULL ? L" \"" : L"", second != NULL ? second : L"",
                          second != NULL ? L"\"" : L"", END);
    spawn(token, command, child_directory, CHECK_MEMORY_CAP, CHECK_TIMEOUT_MS, flags, &result.run);
    result.ran = operation_status(&result.run);
    result.succeeded = result.ran && result.run.exit_code == 0;
    result.denied = result.ran && result.run.exit_code != 0 && ERROR_OF(result.run.exit_code) == ERROR_ACCESS_DENIED;
    result.detail = describe(&result.run);
    if (result.run.output_length > 0)
        result.detail = join(result.detail, L", wrote \"", widen(result.run.output, result.run.output_length), L"\"", END);
    return result;
}

/* Run one child operation under TOKEN in a job of its own. */
static struct outcome operation(HANDLE token, const WCHAR *op, const WCHAR *first, const WCHAR *second)
{
    return operation_with(token, SPAWN_DETACHED, op, first, second);
}

static void expect_allowed(int required, const WCHAR *label, const WCHAR *what, struct outcome result)
{
    verdict(result.succeeded, required, label, what, result.detail);
}

/* A denial passes only when the operation ran and was refused access, so
 * neither a child that never started nor a target that is missing is
 * taken for one. */
static void expect_denied(int required, const WCHAR *label, const WCHAR *what, struct outcome result)
{
    verdict(result.denied, required, label, what, result.detail);
}

/* A check that cannot be made, since what it acts on was not made. */
static void prerequisite_missing(int required, const WCHAR *label, const WCHAR *what, const WCHAR *missing)
{
    verdict(0, required, label, what, join(L"not tried: ", missing, L" was not made", END));
}

static int exists(const WCHAR *path)
{
    return GetFileAttributesW(path) != INVALID_FILE_ATTRIBUTES;
}

/* Access masks the checks ask for (winnt.h). */
#define MASK_READ L"0x80000000"
#define MASK_WRITE L"0x40000000"
#define MASK_EXECUTE L"0x20"
#define MASK_APPEND L"0x4"
#define MASK_ADD_FILE L"0x2"
#define MASK_LIST L"0x1"
#define MASK_DELETE L"0x10000"
#define MASK_DELETE_CHILD L"0x40"
#define MASK_WRITE_ATTRIBUTES L"0x100"
#define MASK_WRITE_DAC L"0x40000"
#define MASK_WRITE_OWNER L"0x80000"

/* The rights the design grants. */
#define RIGHTS_READ_EXECUTE L"0x1200a9"
/* FILE_ADD_FILE, FILE_ADD_SUBDIRECTORY, FILE_TRAVERSE,
 * FILE_READ_ATTRIBUTES and SYNCHRONIZE: create an entry, nothing more. */
#define RIGHTS_CREATE_ONLY L"0x1000a6"
/* Read, write and execute, without DELETE, FILE_DELETE_CHILD,
 * WRITE_DAC or WRITE_OWNER. */
#define RIGHTS_NO_DELETE L"0x1201bf"

/* Token rights a process asks of its own token. */
#define MASK_TOKEN_QUERY L"0x8"
#define MASK_TOKEN_ADJUST_DEFAULT L"0x80"

/* CreateProcessW's flags for a child's own children: DETACHED_PROCESS,
 * and with CREATE_BREAKAWAY_FROM_JOB or CREATE_PRESERVE_CODE_AUTHZ_LEVEL,
 * which leaves out the code-authorization (Safer) check; and
 * CREATE_NO_WINDOW, a new console of the child's own, as libiberty asks
 * for gcc's subprograms when gcc has none. */
#define FLAGS_DETACHED L"0x8"
#define FLAGS_BREAKAWAY L"0x1000008"
#define FLAGS_PRESERVE_AUTHZ L"0x2000008"
#define FLAGS_NEW_CONSOLE L"0x8000000"
/* No flags: the child shares the console of the process starting it. */
#define FLAGS_SHARED_CONSOLE L"0x0"

/* Object directory rights (the DDK's wdm.h): DIRECTORY_QUERY and
 * DIRECTORY_TRAVERSE; and those with DIRECTORY_CREATE_OBJECT,
 * DIRECTORY_CREATE_SUBDIRECTORY and READ_CONTROL, what MSYS2's runtime
 * asks of the object directory it makes for itself in
 * \BaseNamedObjects. */
#define MASK_DIRECTORY_LOOK L"0x3"
#define MASK_DIRECTORY_MSYS L"0x2000f"

/* The tools' jobs get more room than an operation's. */
#define TOOL_MEMORY_CAP ((SIZE_T)1024 * 1024 * 1024)
#define TOOL_TIMEOUT_MS 180000

/* The memory check's job cap, and an allocation inside it and past it. */
#define CAP_MEMORY ((SIZE_T)128 * 1024 * 1024)
#define UNDER_CAP_MB L"64"
#define OVER_CAP_MB L"400"

static WCHAR *environment(const WCHAR *name)
{
    WCHAR *value = take(32768 * sizeof(WCHAR));
    if (GetEnvironmentVariableW(name, value, 32768) == 0)
        return NULL;
    return value;
}

/* What every check works with: the tokens, the SIDs' strings, and the
 * layout's paths. */
struct context {
    /* The first and second builds' tokens, made as the helper would make
     * them. */
    HANDLE build_one;
    HANDLE build_two;
    /* The first build's token with its object's DACL left as made. */
    HANDLE build_bare;
    /* A standard user's stand-in for nova-nix itself, or NULL. */
    HANDLE standard;
    /* The candidate tokens, NULL where one could not be made: the first
     * and second builds' limited, write-restricted, and write-restricted
     * with Everyone restricting too; and, as a second build, the
     * write-restricted token at Low integrity and with the user's SID
     * deny-only. */
    HANDLE limited;
    HANDLE limited_two;
    HANDLE write_restricted;
    HANDLE write_restricted_two;
    HANDLE write_restricted_everyone;
    HANDLE write_restricted_everyone_two;
    HANDLE write_restricted_low;
    HANDLE write_restricted_deny_user;
    /* The two build accounts' tokens, NULL when none were made. */
    HANDLE account_one;
    HANDLE account_two;
    const WCHAR *user;
    /* The first build's SID, whom the build directory grants. */
    const WCHAR *b1;
    /* The SIDs the layout grants what a build needs, NULL past the last:
     * S-1-5-12 for restricted tokens, each account's own for the build
     * accounts. */
    const WCHAR *builders[2];
    const WCHAR *tools;
    const WCHAR *store;
    const WCHAR *nogrant;
    const WCHAR *inherit;
    const WCHAR *valid;
    const WCHAR *valid_directory;
    const WCHAR *own;
    const WCHAR *ownow;
    const WCHAR *outside;
    const WCHAR *build;
    const WCHAR *b1_out;
    const WCHAR *bash;
    const WCHAR *gcc;
    const WCHAR *user_temp;
    const WCHAR *user_profile;
    const WCHAR *user_appdata;
    const WCHAR *system_root;
    const WCHAR *system_drive;
    const WCHAR *program_data;
    const WCHAR *public_directory;
    /* The object directory this session's named objects go in. */
    const WCHAR *session_objects;
    /* A file in the user's profile the user can read, or NULL. */
    const WCHAR *profile_file;
    /* Whether the run may make a file in the user's profile, for the
     * check that a build cannot delete one. */
    int profile_writes;
    /* Whether the run is on a desktop someone uses, where the probe
     * writes only under its root, opens no file of the user's own and no
     * other process, opens nothing outside the root for writing, makes no
     * account, and leaves nothing it cannot remove. */
    int desktop;
    /* This process's ID and the address of its memory marker, for the
     * check that a build cannot read another process's memory. */
    const WCHAR *checks_pid;
    const WCHAR *marker_address;
};

/* Whether TEXT holds NEEDLE. */
static int contains(const WCHAR *text, const WCHAR *needle)
{
    size_t length = wide_length(needle);
    for (; *text != 0; text++) {
        size_t i = 0;
        while (i < length && text[i] == needle[i])
            i++;
        if (i == length)
            return 1;
    }
    return 0;
}

/* The last part of a program's output, for a line of its own. */
#define OUTPUT_TAIL_CHARS 3000

static void show_output(const WCHAR *label, const struct run *run)
{
    WCHAR *text = widen(run->output, run->output_length);
    size_t length = wide_length(text);
    info(label, length > OUTPUT_TAIL_CHARS ? text + (length - OUTPUT_TAIL_CHARS) : text);
}

/* 1, and 8's nesting and completion port, from the same run. */
static void check_pipe(const struct context *c)
{
    struct outcome piped = operation(c->build_one, L"pipe", NULL, NULL);
    struct run attached;
    /* Whether a process the Secondary Logon service starts is in this
     * one's jobs is the service's to decide; the service the build
     * accounts' design rests on starts builds itself, so the nesting
     * there is a finding. */
    int through_service = other_logon(c->build_one);
    verdict(piped.succeeded && piped.run.output_length == sizeof pipe_marker - 1
                && starts_with(piped.run.output, pipe_marker),
            REQUIRED, L"pipe-log",
            L"a restricted child writes the pipe it inherits as standard output, and the parent reads it", piped.detail);
    verdict(piped.run.start_error == 0 && piped.run.in_own_job && piped.run.in_outer_job,
            through_service ? FINDING : REQUIRED, L"jobs-nesting",
            L"a restricted child started inside the checks' job is assigned a job of its own and stays in both",
            join(L"own job ", piped.run.in_own_job ? L"yes" : L"no", L", outer job ",
                 piped.run.in_outer_job ? L"yes" : L"no", L", ", piped.detail, END));
    verdict(piped.run.active_zero, REQUIRED, L"jobs-completion-port",
            L"the job's completion port reports its last process gone", piped.detail);
    spawn(c->build_one, join(L"\"", self_path, L"\" child pipe", END), child_directory, CHECK_MEMORY_CAP,
          CHECK_TIMEOUT_MS, 0, &attached);
    verdict(attached.start_error == 0 && attached.exit_code == 0 && starts_with(attached.output, pipe_marker), FINDING,
            L"pipe-log-attached", L"the same, with the child left to attach to a console rather than detached",
            describe(&attached));
}

/* A build's process opening its own token, as MSYS2's runtime does when it
 * starts. */
static void check_own_token(const struct context *c)
{
    expect_allowed(REQUIRED, L"token-self-query",
                   L"a build whose token object grants its SID query opens its own token",
                   operation(c->build_one, L"open-token", MASK_TOKEN_QUERY, NULL));
    /* Whether a token object left with the DACL it was made with still
     * lets its process open it.  The DACL each child's token object has,
     * read from inside it, says whether the object a process runs with is
     * the one the helper dressed, or another made from the default DACL. */
    expect_allowed(FINDING, L"token-self-query-undressed",
                   L"a build whose token object keeps the DACL it was made with still opens its own token for query",
                   operation(c->build_bare, L"open-token", MASK_TOKEN_QUERY, NULL));
    expect_allowed(FINDING, L"token-self-adjust-undressed",
                   L"and for adjusting its default DACL",
                   operation(c->build_bare, L"open-token", MASK_TOKEN_ADJUST_DEFAULT, NULL));
    info(L"token-child-object", operation(c->build_one, L"token-sddl", NULL, NULL).detail);
    info(L"token-child-object-undressed", operation(c->build_bare, L"token-sddl", NULL, NULL).detail);
}

/* 2.  Each file's DACL gives the restricting SID read and execute only, so
 * WRITE_DAC can come only from the owner's implicit rights. */
static void check_owner(const struct context *c)
{
    expect_denied(FINDING, L"D-owner-implicit-write-dac",
                  L"a user-owned file with no OWNER RIGHTS entry: the restricted child is refused WRITE_DAC",
                  operation(c->build_one, L"open", join(c->own, L"\\plain", END), MASK_WRITE_DAC));
    expect_denied(FINDING, L"D-owner-default-owner-write-dac",
                  L"a file owned by the runner's default owner: the restricted child is refused WRITE_DAC",
                  operation(c->build_one, L"open", join(c->own, L"\\by-default-owner", END), MASK_WRITE_DAC));
    expect_denied(REQUIRED, L"D-owner-rights-write-dac",
                  L"a user-owned file whose DACL has OWNER RIGHTS read-control: the restricted child is refused WRITE_DAC",
                  operation(c->build_one, L"open", join(c->ownow, L"\\plain", END), MASK_WRITE_DAC));
}

/* 3. */
static void check_store(const struct context *c)
{
    expect_denied(REQUIRED, L"D-no-grant",
                  L"a directory granting the restricting SIDs nothing: the restricted child cannot create in it",
                  operation(c->build_one, L"create", join(c->nogrant, L"\\x", END), NULL));
    expect_allowed(REQUIRED, L"D-create-file",
                   L"the proposed store directory: the restricted child creates a file in it and writes it",
                   operation(c->build_one, L"create", join(c->store, L"\\b1-file", END), NULL));
    expect_allowed(REQUIRED, L"D-reopen-file",
                   L"the proposed store directory: the restricted child reopens the file it made, for writing",
                   operation(c->build_one, L"open", join(c->store, L"\\b1-file", END), MASK_WRITE));
    {
        struct outcome made = operation(c->build_one, L"mkdir", c->b1_out, NULL);
        struct outcome file = operation(c->build_one, L"create", join(c->b1_out, L"\\f", END), NULL);
        struct outcome sub = operation(c->build_one, L"mkdir", join(c->b1_out, L"\\sub", END), NULL);
        struct outcome deep = operation(c->build_one, L"create", join(c->b1_out, L"\\sub\\g", END), NULL);
        verdict(made.succeeded && file.succeeded && sub.succeeded && deep.succeeded, REQUIRED, L"D-fill-directory",
                L"the proposed store directory: the restricted child makes a directory and fills it two levels deep",
                join(L"mkdir ", made.detail, L"; file ", file.detail, L"; subdirectory ", sub.detail, L"; deep file ",
                     deep.detail, END));
    }
    info(L"D-dacl-store-file", sddl_of(join(c->store, L"\\b1-file", END)));
    if (!exists(join(c->b1_out, L"\\f", END))) {
        prerequisite_missing(FINDING, L"D-default-dacl-flags", L"what a new output directory and its file are given",
                             L"the output's file");
        return;
    }
    {
        WCHAR *top = sddl_of(c->b1_out);
        WCHAR *below = sddl_of(join(c->b1_out, L"\\f", END));
        /* "OICI" on the new directory means the default DACL's entries
         * kept their inheritance flags; "ID" on the file below means it
         * inherited them, rather than taking the default DACL again. */
        int top_inherits = contains(top, L"OICI");
        int below_inherited = contains(below, L"ID;");
        info(L"D-dacl-output", top);
        info(L"D-dacl-output-file", below);
        info(L"D-dacl-output-deep-file", sddl_of(join(c->b1_out, L"\\sub\\g", END)));
        verdict(top_inherits && below_inherited, FINDING, L"D-default-dacl-flags",
                L"the default DACL's entries keep their inheritance on a new directory, and what is made in it inherits them",
                join(L"directory inheritable ", top_inherits ? L"yes" : L"no", L", file inherited ",
                     below_inherited ? L"yes" : L"no", END));
    }
}

/* 4. */
static void check_present_layout(const struct context *c)
{
    const WCHAR *file = join(c->inherit, L"\\c-file", END);
    struct outcome made = operation(c->build_one, L"create", file, NULL);
    verdict(made.ran && (made.succeeded || STEP_OF(made.run.exit_code) == 2), FINDING, L"C-create-file",
            L"the present store layout with a create grant: the restricted child creates a file", made.detail);
    verdict(made.succeeded, FINDING, L"C-creation-handle",
            L"the present store layout: the handle that created the file is granted the write it asked for", made.detail);
    if (exists(file)) {
        info(L"C-dacl-file", sddl_of(file));
        expect_denied(FINDING, L"C-reopen-file",
                      L"the present store layout: the restricted child cannot reopen the file it made, for writing",
                      operation(c->build_one, L"open", file, MASK_WRITE));
    } else {
        prerequisite_missing(FINDING, L"C-reopen-file",
                             L"the present store layout: the restricted child cannot reopen the file it made, for writing",
                             L"the file");
    }
    {
        struct outcome directory = operation(c->build_one, L"mkdir", join(c->inherit, L"\\c-out", END), NULL);
        struct outcome inside = operation(c->build_one, L"create", join(c->inherit, L"\\c-out\\f", END), NULL);
        verdict(directory.succeeded && inside.denied, FINDING, L"C-fill-directory",
                L"the present store layout: the restricted child makes a directory but is refused filling it",
                join(L"mkdir ", directory.detail, L"; file ", inside.detail, END));
    }
}

/* 5.  The rename comes last, since it would move the file the others
 * act on should it be allowed. */
static void check_valid_path(const struct context *c)
{
    const WCHAR *file = join(c->valid, L"\\f", END);
    expect_allowed(REQUIRED, L"D-valid-read", L"a valid path: the restricted child reads a file",
                   operation(c->build_one, L"open", file, MASK_READ));
    expect_allowed(REQUIRED, L"D-valid-execute", L"a valid path: the restricted child may execute a file",
                   operation(c->build_one, L"open", file, MASK_EXECUTE));
    expect_denied(REQUIRED, L"D-valid-write", L"a valid path: writing a file is refused",
                  operation(c->build_one, L"open", file, MASK_WRITE));
    expect_denied(REQUIRED, L"D-valid-append", L"a valid path: appending to a file is refused",
                  operation(c->build_one, L"open", file, MASK_APPEND));
    expect_denied(REQUIRED, L"D-valid-attributes", L"a valid path: writing a file's attributes is refused",
                  operation(c->build_one, L"open", file, MASK_WRITE_ATTRIBUTES));
    expect_denied(REQUIRED, L"D-valid-delete", L"a valid path: deleting a file is refused",
                  operation(c->build_one, L"open", file, MASK_DELETE));
    expect_denied(REQUIRED, L"D-valid-write-dac", L"a valid path: changing a file's DACL is refused",
                  operation(c->build_one, L"open", file, MASK_WRITE_DAC));
    expect_denied(REQUIRED, L"D-valid-write-owner", L"a valid path: taking a file's ownership is refused",
                  operation(c->build_one, L"open", file, MASK_WRITE_OWNER));
    expect_denied(REQUIRED, L"D-valid-create-inside", L"a valid path: creating in its directory is refused",
                  operation(c->build_one, L"create", join(c->valid_directory, L"\\x", END), NULL));
    expect_denied(REQUIRED, L"D-valid-delete-child", L"a valid path: deleting from its directory is refused",
                  operation(c->build_one, L"open", c->valid_directory, MASK_DELETE_CHILD));
    expect_denied(REQUIRED, L"D-valid-rename", L"a valid path: renaming a file is refused",
                  operation(c->build_one, L"rename", file, join(c->valid, L"\\renamed", END)));
}

/* 6. */
static void check_isolation(const struct context *c)
{
    const WCHAR *first_file = join(c->b1_out, L"\\f", END);
    expect_allowed(REQUIRED, L"D-isolation-own", L"a second build creates its own output in the store",
                   operation(c->build_two, L"create", join(c->store, L"\\b2-file", END), NULL));
    if (!exists(first_file)) {
        prerequisite_missing(REQUIRED, L"D-isolation", L"a second build is kept from the first build's output",
                             L"the first build's output");
        return;
    }
    expect_denied(REQUIRED, L"D-isolation-fill", L"a second build cannot create in the first build's output",
                  operation(c->build_two, L"create", join(c->b1_out, L"\\x", END), NULL));
    expect_denied(REQUIRED, L"D-isolation-write", L"a second build cannot write the first build's file",
                  operation(c->build_two, L"open", first_file, MASK_WRITE));
    expect_denied(REQUIRED, L"D-isolation-delete", L"a second build cannot delete the first build's file",
                  operation(c->build_two, L"open", first_file, MASK_DELETE));
    expect_denied(FINDING, L"D-isolation-read", L"a second build cannot read the first build's file",
                  operation(c->build_two, L"open", first_file, MASK_READ));
}

/* 7: an entry whose own DACL keeps nova-nix from its contents.  Its owner
 * is the user, the build token's default owner, so only the OWNER RIGHTS
 * entry keeps the user from rewriting its DACL; and the store directory's
 * FILE_DELETE_CHILD still lets nova-nix move it aside. */
static void check_hostile_entry(const struct context *c)
{
    const WCHAR *hostile = join(c->store, L"\\hostile", END);
    const WCHAR *hostile_file = join(hostile, L"\\x", END);
    struct outcome made = operation(c->build_one, L"mkdir-sd", hostile,
                                    join(L"D:P(A;OICI;RC;;;", OWNER_RIGHTS_SID, L")(A;OICI;", RIGHTS_NO_DELETE, L";;;",
                                         c->user, L")(A;OICI;", RIGHTS_NO_DELETE, L";;;", c->b1, L")", END));
    struct outcome filled = operation(c->build_one, L"create", hostile_file, NULL);
    verdict(made.succeeded && filled.succeeded, FINDING, L"D-hostile-made",
            L"a build makes a store entry whose own DACL grants no one DELETE or WRITE_DAC, and fills it",
            join(L"mkdir ", made.detail, L"; file ", filled.detail, END));
    if (!made.succeeded || !filled.succeeded) {
        prerequisite_missing(FINDING, L"D-hostile", L"what nova-nix can do with such an entry", L"the entry");
        return;
    }
    info(L"D-hostile-dacl", sddl_of(hostile));
    if (c->standard == NULL) {
        verdict(0, FINDING, L"D-hostile", L"what nova-nix can do with such an entry",
                L"no standard-user stand-in token to try with");
        return;
    }
    expect_denied(FINDING, L"D-hostile-file-kept", L"nova-nix as a standard user is refused deleting the entry's file",
                  operation(c->standard, L"delete", hostile_file, NULL));
    expect_denied(FINDING, L"D-hostile-dacl-kept", L"nova-nix as a standard user is refused rewriting the entry's DACL",
                  operation(c->standard, L"open", hostile, MASK_WRITE_DAC));
    expect_allowed(FINDING, L"D-hostile-move-aside", L"nova-nix as a standard user moves the entry aside, freeing its name",
                   operation(c->standard, L"rename", hostile, join(hostile, L".leftover", END)));
}

/* 7: a junction a build leaves. */
static void check_junction(const struct context *c)
{
    const WCHAR *junction = join(c->store, L"\\junction", END);
    struct outcome made = operation(c->build_one, L"junction", junction, c->outside);
    verdict(made.succeeded, FINDING, L"D-junction-made", L"a build makes a junction in the store, needing no privilege",
            made.detail);
    if (!made.succeeded) {
        prerequisite_missing(REQUIRED, L"D-junction-removal",
                             L"removing the build's junction removes the link and leaves what it led to", L"the junction");
        return;
    }
    expect_denied(FINDING, L"D-junction-reach", L"a build cannot read through its junction what the token cannot reach",
                  operation(c->build_one, L"open", join(junction, L"\\sentinel", END), MASK_READ));
    {
        /* Removed as nova-nix would remove it, as the standard-user
         * stand-in, or as this process when there is none. */
        struct outcome removed = operation(c->standard, L"delete", junction, NULL);
        char *sentinel = read_start(join(c->outside, L"\\sentinel", END));
        verdict(removed.succeeded && !exists(junction) && starts_with(sentinel, "sentinel"), REQUIRED,
                L"D-junction-removal", L"removing the build's junction removes the link and leaves what it led to",
                join(L"remove ", removed.detail, L", sentinel ", sentinel != NULL ? L"intact" : L"gone", END));
    }
}

/* 8, but for the tree checks, which the run command makes. */
static void check_jobs(const struct context *c)
{
    struct run under;
    struct run over;
    struct run crashed;
    struct outcome plain;
    struct outcome breaking;
    spawn(c->build_one, join(L"\"", self_path, L"\" child alloc ", UNDER_CAP_MB, END), child_directory, CAP_MEMORY,
          CHECK_TIMEOUT_MS, SPAWN_DETACHED, &under);
    spawn(c->build_one, join(L"\"", self_path, L"\" child alloc ", OVER_CAP_MB, END), child_directory, CAP_MEMORY,
          CHECK_TIMEOUT_MS, SPAWN_DETACHED, &over);
    verdict(under.start_error == 0 && under.exit_code == 0 && operation_status(&over) && over.exit_code != 0, REQUIRED,
            L"jobs-memory-cap", L"under a 128 MB per-process cap a 64 MB commit succeeds and a 400 MB one fails",
            join(L"64 MB ", describe(&under), L"; 400 MB ", describe(&over), END));
    verdict(over.memory_limit_reached, FINDING, L"jobs-memory-message",
            L"the job's completion port reports the memory limit reached", NULL);
    plain = operation(c->build_one, L"spawn", L"probe", FLAGS_DETACHED);
    breaking = operation(c->build_one, L"spawn", L"probe", FLAGS_BREAKAWAY);
    verdict(plain.succeeded && breaking.denied, REQUIRED, L"jobs-no-breakaway",
            L"a restricted child that can start a process cannot start one that breaks away from its job",
            join(L"plain ", plain.detail, L"; breaking away ", breaking.detail, END));
    spawn(c->build_one, join(L"\"", self_path, L"\" child crash", END), child_directory, CHECK_MEMORY_CAP,
          CHECK_TIMEOUT_MS, SPAWN_DETACHED, &crashed);
    verdict(crashed.start_error == 0 && !crashed.timed_out && crashed.exit_code == EXCEPTION_ACCESS_VIOLATION, FINDING,
            L"jobs-unhandled-exception", L"an unhandled exception ends the child at once with its code",
            describe(&crashed));
}

/* PATH as the sandbox would set it for a build: the tools it was given,
 * then System32, so a DLL is never looked for where the token cannot
 * read. */
static void set_tool_path(const WCHAR *tools, const WCHAR *system_root)
{
    SetEnvironmentVariableW(L"PATH", join(tools, L"\\mingw64\\bin;", tools, L"\\msys64\\usr\\bin;",
                                          system_root != NULL ? system_root : L"C:\\Windows", L"\\System32", END));
}

/* The ways gcc is run under a build.  Detached, gcc has no console, and
 * libiberty starts each of its subprograms with CREATE_NO_WINDOW, a new
 * console each; attached to the checks' console, or to a pseudoconsole
 * the checks own, the subprograms share gcc's. */
struct console_way {
    /* What the way's labels and files end in. */
    const WCHAR *suffix;
    const WCHAR *what;
    int flags;
};

static const struct console_way console_ways[] = {
    {L"", L"detached, so its subprograms each get a new console", SPAWN_DETACHED},
    {L"-attached", L"attached to the checks' console, which its subprograms share", 0},
    {L"-pseudoconsole", L"attached to a pseudoconsole the checks own, which its subprograms share", SPAWN_PSEUDOCONSOLE},
};

/* The one way a token of another logon session runs: the Secondary
 * Logon service gives the process a new console of its own and takes no
 * pseudoconsole, and gcc's subprograms share that console. */
static const struct console_way service_ways[] = {
    {L"", L"started through the Secondary Logon service with a console of its own, which its subprograms share", 0},
};

/* The console ways a token can take, and how many in COUNT. */
static const struct console_way *console_ways_for(HANDLE token, size_t *count)
{
    if (other_logon(token)) {
        *count = sizeof service_ways / sizeof service_ways[0];
        return service_ways;
    }
    *count = sizeof console_ways / sizeof console_ways[0];
    return console_ways;
}

/* MSYS2's bash writing a file into the store, the way WAY gives it a
 * console; with FORKING, after running an MSYS2 program first, which bash
 * forks to start. */
static void run_bash(const struct context *c, const struct console_way *way, int forking, int required)
{
    const WCHAR *label = join(L"tools-bash", forking ? L"-fork" : L"", way->suffix, END);
    const WCHAR *out = join(c->store, L"\\bash-out", forking ? L"-fork" : L"", way->suffix, END);
    struct run ran;
    spawn(c->build_one,
          join(L"\"", c->bash, L"\" -c \"", forking ? L"/usr/bin/true && " : L"", L"echo ok > '", forward_slashes(out),
               L"'\"", END),
          c->build, TOOL_MEMORY_CAP, TOOL_TIMEOUT_MS, way->flags, &ran);
    verdict(ran.start_error == 0 && ran.exit_code == 0 && starts_with(read_start(out), "ok"), required, label,
            join(L"MSYS2 bash, under the token, ", forking ? L"runs an MSYS2 program and " : L"",
                 L"writes a file into the store, ", way->what, END),
            describe_program(&ran));
    if (ran.output_length > 0)
        show_output(join(label, L"-output", END), &ran);
}

/* 9.  bash, then gcc each way a console can be had, then each program gcc
 * built.  The ways past the first are findings. */
static void check_tools(const struct context *c)
{
    size_t ways;
    const struct console_way *way = console_ways_for(c->build_one, &ways);
    size_t i;
    /* As the sandbox would set them for a build: temporary files in its
     * build directory, and the tools' PATH. */
    SetEnvironmentVariableW(L"TEMP", c->build);
    SetEnvironmentVariableW(L"TMP", c->build);
    set_tool_path(c->tools, c->system_root);
    if (wide_equal(c->bash, L"-")) {
        verdict(0, REQUIRED, L"tools-bash", L"MSYS2 bash runs a command under the token", L"no bash was given");
    } else {
        run_bash(c, &way[0], 0, REQUIRED);
        run_bash(c, &way[0], 1, FINDING);
        for (i = 1; i < ways; i++)
            if (way[i].flags & SPAWN_PSEUDOCONSOLE) {
                run_bash(c, &way[i], 0, FINDING);
                run_bash(c, &way[i], 1, FINDING);
            }
    }
    if (wide_equal(c->gcc, L"-")) {
        verdict(0, REQUIRED, L"tools-gcc", L"MinGW gcc compiles under the token", L"no gcc was given");
        return;
    }
    for (i = 0; i < ways; i++) {
        const WCHAR *program = join(c->store, L"\\hello", way[i].suffix, L".exe", END);
        const WCHAR *label = join(L"tools-gcc", way[i].suffix, END);
        struct run compiled;
        spawn(c->build_one, join(L"\"", c->gcc, L"\" -v -o \"", program, L"\" \"", c->build, L"\\hello.c\"", END),
              c->build, TOOL_MEMORY_CAP, TOOL_TIMEOUT_MS, way[i].flags, &compiled);
        verdict(compiled.start_error == 0 && compiled.exit_code == 0 && exists(program), i == 0 ? REQUIRED : FINDING,
                label,
                join(L"MinGW gcc, a UCRT program, compiles under the token into the store, ", way[i].what, END),
                describe_program(&compiled));
        if (compiled.exit_code != 0 || compiled.start_error != 0)
            show_output(join(label, L"-output", END), &compiled);
    }
    for (i = 0; i < ways; i++) {
        const WCHAR *program = join(c->store, L"\\hello", way[i].suffix, L".exe", END);
        const WCHAR *label = join(L"tools-run-output", way[i].suffix, END);
        const WCHAR *what = join(L"the program gcc built ", way[i].what,
                                 L" runs under the token and writes its output", END);
        struct run hello;
        if (!exists(program)) {
            prerequisite_missing(i == 0 ? REQUIRED : FINDING, label, what, L"the program");
            continue;
        }
        spawn(c->build_one, join(L"\"", program, L"\"", END), c->build, CHECK_MEMORY_CAP, CHECK_TIMEOUT_MS,
              SPAWN_DETACHED, &hello);
        verdict(hello.start_error == 0 && hello.exit_code == 0 && starts_with(hello.output, "hello"),
                i == 0 ? REQUIRED : FINDING, label, what,
                join(describe_program(&hello), L", output \"", widen(hello.output, hello.output_length), L"\"", END));
    }
}

/* A token the comparisons run under: its name in labels, what it is, the
 * token itself, NULL for the checks' own, and why it could not be made,
 * or NULL. */
struct candidate {
    const WCHAR *name;
    const WCHAR *holds;
    HANDLE token;
    const WCHAR *failure;
};

/* FILE_ADD_SUBDIRECTORY on a directory. */
#define MASK_ADD_SUBDIRECTORY L"0x4"

/* Registry keys starting a process reads, or may: its image's execution
 * options, the application compatibility settings and cache, the
 * code-authorization (Safer) policy, and the Session Manager key, the
 * parent of the AppCertDlls list.  And the roots every key under HKLM or
 * HKU is opened through, so that a refusal there is not taken for the
 * key's own. */
#define REGISTRY_IFEO L"HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Image File Execution Options"
#define REGISTRY_APPCOMPAT_FLAGS L"HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\AppCompatFlags"
#define REGISTRY_APPCOMPAT_CACHE L"HKLM\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\AppCompatCache"
#define REGISTRY_SAFER L"HKLM\\SOFTWARE\\Policies\\Microsoft\\Windows\\Safer\\CodeIdentifiers"
#define REGISTRY_SESSION_MANAGER L"HKLM\\SYSTEM\\CurrentControlSet\\Control\\Session Manager"
#define REGISTRY_MACHINE L"HKLM\\"
#define REGISTRY_USERS L"HKU\\"

#define KNOWN_DLLS L"\\KnownDlls"
#define GLOBAL_OBJECTS L"\\BaseNamedObjects"

/* The user's own registry hive, by its SID, as a build would reach it. */
static WCHAR *user_hive(const struct context *c)
{
    return join(L"HKU\\", c->user, L"\\Software", END);
}

typedef HWINSTA(WINAPI *window_station_call)(void);
typedef HDESK(WINAPI *thread_desktop_call)(DWORD);
typedef BOOL(WINAPI *object_security_call)(HANDLE, PSECURITY_INFORMATION, PSECURITY_DESCRIPTOR, DWORD, LPDWORD);
typedef BOOL(WINAPI *object_information_call)(HANDLE, int, PVOID, DWORD, LPDWORD);

/* The room a window station's or desktop's name is given. */
#define USER_OBJECT_NAME_CHARS 256

/* A window station's or desktop's name, owner and DACL, read through
 * user32, which only this process loads: a child that imported it would
 * need its desktop to start. */
static WCHAR *user_object_text(HMODULE user32, HANDLE object)
{
    object_security_call security = (object_security_call)(any_function)GetProcAddress(user32, "GetUserObjectSecurity");
    object_information_call information =
        (object_information_call)(any_function)GetProcAddress(user32, "GetUserObjectInformationW");
    WCHAR *name = take(USER_OBJECT_NAME_CHARS * sizeof(WCHAR));
    SECURITY_INFORMATION wanted = OWNER_AND_DACL;
    PSECURITY_DESCRIPTOR descriptor;
    DWORD needed = 0;
    if (security == NULL || information == NULL || object == NULL)
        return join(L"unreadable, ", error_text(ERROR_PROC_NOT_FOUND), END);
    if (!information(object, UOI_NAME, name, USER_OBJECT_NAME_CHARS * sizeof(WCHAR), &needed))
        name = join(L"?", END);
    security(object, &wanted, NULL, 0, &needed);
    if (needed == 0)
        return join(name, L" unreadable, ", error_text(GetLastError()), END);
    descriptor = take(needed);
    if (!security(object, &wanted, descriptor, needed, &needed))
        return join(name, L" unreadable, ", error_text(GetLastError()), END);
    return join(name, L" ", descriptor_text(descriptor), END);
}

/* The name, owner and DACL of this process's window station and desktop,
 * which a new console opens, labelled WHEN.  user32 stays loaded until
 * the process ends, as it is not unloaded once a thread has used it. */
static void describe_user_objects(const WCHAR *when)
{
    HMODULE user32 = LoadLibraryExW(L"user32.dll", NULL, LOAD_LIBRARY_SEARCH_SYSTEM32);
    window_station_call station;
    thread_desktop_call desktop;
    if (user32 == NULL) {
        info(join(L"window-station-", when, END), join(L"user32 did not load, ", error_text(GetLastError()), END));
        return;
    }
    station = (window_station_call)(any_function)GetProcAddress(user32, "GetProcessWindowStation");
    desktop = (thread_desktop_call)(any_function)GetProcAddress(user32, "GetThreadDesktop");
    info(join(L"window-station-", when, END), user_object_text(user32, station != NULL ? (HANDLE)station() : NULL));
    info(join(L"desktop-", when, END),
         user_object_text(user32, desktop != NULL ? (HANDLE)desktop(GetCurrentThreadId()) : NULL));
}

/* The owner and DACL of each object directory and key the spawning
 * comparisons open, and of the window station and desktop a new console
 * opens, read by the checks' own token. */
static void describe_objects(const struct context *c)
{
    const WCHAR *directories[] = {KNOWN_DLLS, GLOBAL_OBJECTS,
                                  c->session_objects != NULL ? c->session_objects : GLOBAL_OBJECTS};
    const WCHAR *keys[] = {REGISTRY_MACHINE,         REGISTRY_USERS,           REGISTRY_IFEO,
                           REGISTRY_APPCOMPAT_FLAGS, REGISTRY_APPCOMPAT_CACHE, REGISTRY_SAFER,
                           REGISTRY_SESSION_MANAGER, user_hive(c)};
    size_t i;
    for (i = 0; i < sizeof directories / sizeof directories[0]; i++) {
        HANDLE directory;
        NTSTATUS status = object_directory(directories[i], READ_CONTROL, 0, &directory);
        if (!NT_SUCCESS(status)) {
            info(L"object-dacl", join(directories[i], L" unopened, ntstatus ", number((DWORD)status, 1), END));
            continue;
        }
        info(L"object-dacl", join(directories[i], L" ", object_sddl(directory), END));
        CloseHandle(directory);
    }
    for (i = 0; i < sizeof keys / sizeof keys[0]; i++) {
        HKEY root;
        const WCHAR *path;
        HKEY key;
        LSTATUS status;
        if (!key_root(keys[i], &root, &path))
            continue;
        status = RegOpenKeyExW(root, path, 0, READ_CONTROL, &key);
        if (status != ERROR_SUCCESS) {
            info(L"key-dacl", join(keys[i], L" unopened, ", error_text((DWORD)status), END));
            continue;
        }
        info(L"key-dacl", join(keys[i], L" ", key_sddl(key), END));
        RegCloseKey(key);
    }
    describe_user_objects(L"at-start");
}

/* Where a spawning step's arguments come from: the table, or the
 * context, which knows the session's named-object directory, this
 * process's ID and its memory marker's address. */
#define ARGUMENTS_FIXED 0
#define ARGUMENTS_SESSION_OBJECTS 1
#define ARGUMENTS_MSYS_DIRECTORY 2
#define ARGUMENTS_MEMORY 3

/* The operations check_spawning runs under each token. */
static const struct {
    const WCHAR *name;
    int flags;
    const WCHAR *op;
    const WCHAR *first;
    const WCHAR *second;
    int arguments;
    const WCHAR *what;
} spawning_steps[] = {
    {L"token-object", SPAWN_DETACHED, L"token-sddl", NULL, NULL, ARGUMENTS_FIXED,
     L"reads its own token object's owner and DACL"},
    {L"spawn-probe", SPAWN_DETACHED, L"spawn", L"probe", FLAGS_DETACHED, ARGUMENTS_FIXED,
     L"starts the probe with CreateProcessW from a job of its own, and it runs"},
    {L"spawn-probe-no-job", SPAWN_DETACHED | SPAWN_NO_JOB, L"spawn", L"probe", FLAGS_DETACHED, ARGUMENTS_FIXED,
     L"the same from no job but the one the checks run in"},
    {L"spawn-probe-preserve-authz", SPAWN_DETACHED, L"spawn", L"probe", FLAGS_PRESERVE_AUTHZ, ARGUMENTS_FIXED,
     L"the same with CREATE_PRESERVE_CODE_AUTHZ_LEVEL, leaving out the code-authorization check"},
    {L"spawn-cmd", SPAWN_DETACHED, L"spawn", L"cmd", FLAGS_DETACHED, ARGUMENTS_FIXED,
     L"starts System32's cmd.exe with CreateProcessW, and it runs its command"},
    {L"native-spawn", SPAWN_DETACHED, L"native-spawn", NULL, NULL, ARGUMENTS_FIXED,
     L"starts the probe with RtlCreateUserProcess, beneath CreateProcessW, and it runs"},
    {L"new-console", SPAWN_DETACHED, L"spawn", L"probe", FLAGS_NEW_CONSOLE, ARGUMENTS_FIXED,
     L"starts the probe with a new console of its own, as libiberty starts gcc's subprograms, and it runs"},
    {L"shared-console", 0, L"spawn", L"probe", FLAGS_SHARED_CONSOLE, ARGUMENTS_FIXED,
     L"attached to the checks' console, starts the probe sharing it, as gcc's subprograms share gcc's, and it runs"},
    {L"pseudoconsole", SPAWN_PSEUDOCONSOLE, L"spawn", L"probe", FLAGS_SHARED_CONSOLE, ARGUMENTS_FIXED,
     L"attached to a pseudoconsole the checks own, starts the probe sharing it, and it runs"},
    {L"known-dlls", SPAWN_DETACHED, L"open-directory", KNOWN_DLLS, MASK_DIRECTORY_LOOK, ARGUMENTS_FIXED,
     L"opens \\KnownDlls to look a known DLL up"},
    {L"session-objects", SPAWN_DETACHED, L"open-directory", NULL, MASK_DIRECTORY_LOOK, ARGUMENTS_SESSION_OBJECTS,
     L"opens its session's named-object directory"},
    {L"msys-directory", SPAWN_DETACHED, L"create-directory", NULL, MASK_DIRECTORY_MSYS, ARGUMENTS_MSYS_DIRECTORY,
     L"makes an object directory in \\BaseNamedObjects, as MSYS2's runtime makes its own"},
    {L"key-ifeo", SPAWN_DETACHED, L"open-key", REGISTRY_IFEO, NULL, ARGUMENTS_FIXED,
     L"reads the Image File Execution Options key"},
    {L"key-appcompat-flags", SPAWN_DETACHED, L"open-key", REGISTRY_APPCOMPAT_FLAGS, NULL, ARGUMENTS_FIXED,
     L"reads the AppCompatFlags key"},
    {L"key-appcompat-cache", SPAWN_DETACHED, L"open-key", REGISTRY_APPCOMPAT_CACHE, NULL, ARGUMENTS_FIXED,
     L"reads the AppCompatCache key"},
    {L"key-safer", SPAWN_DETACHED, L"open-key", REGISTRY_SAFER, NULL, ARGUMENTS_FIXED,
     L"reads the code-authorization (Safer) policy key"},
    {L"key-session-manager", SPAWN_DETACHED, L"open-key", REGISTRY_SESSION_MANAGER, NULL, ARGUMENTS_FIXED,
     L"reads the Session Manager key"},
    {L"system-function", SPAWN_DETACHED, L"resolve", L"advapi32.dll", L"SystemFunction036", ARGUMENTS_FIXED,
     L"resolves advapi32's SystemFunction036, which forwards to cryptbase.dll, as MSYS2's runtime does"},
    {L"load-cryptbase", SPAWN_DETACHED, L"load", L"cryptbase.dll", NULL, ARGUMENTS_FIXED,
     L"loads cryptbase.dll, which is not a known DLL"},
    {L"memory-read", SPAWN_DETACHED, L"read-memory", NULL, NULL, ARGUMENTS_MEMORY,
     L"reads the checks' own process's memory"},
};

/* Why a build under a token with restricting SIDs could start no process,
 * one operation at a time under one token, the checks' own and the
 * standard user's stand-in the controls: starting a process from a job
 * of its own and from none of the checks', without the code-authorization
 * check, of a system program, through the native call beneath
 * CreateProcessW, with a new console, and sharing the checks' console or
 * a pseudoconsole; opening what starting a process and MSYS2's runtime
 * read; loading cryptbase.dll; and reading this process's memory.  Each
 * claim is that the operation succeeds.  A row starts by reading the DACL
 * of the token object its process runs with, which also makes every later
 * step in the row the second or later process made from that token. */
static void spawning_row(const struct context *c, const struct candidate *row)
{
    size_t j;
    if (row->failure != NULL) {
        verdict(0, FINDING, join(L"why-", row->name, END), join(L"make ", row->holds, END), row->failure);
        return;
    }
    for (j = 0; j < sizeof spawning_steps / sizeof spawning_steps[0]; j++) {
        const WCHAR *label = join(L"why-", row->name, L"-", spawning_steps[j].name, END);
        const WCHAR *what = join(L"under ", row->holds, L", a process ", spawning_steps[j].what, END);
        const WCHAR *first = spawning_steps[j].first;
        const WCHAR *second = spawning_steps[j].second;
        switch (spawning_steps[j].arguments) {
        case ARGUMENTS_SESSION_OBJECTS:
            first = c->session_objects;
            break;
        case ARGUMENTS_MSYS_DIRECTORY:
            first = join(GLOBAL_OBJECTS, L"\\nova-nix-probe-", c->checks_pid, END);
            break;
        case ARGUMENTS_MEMORY:
            first = c->checks_pid;
            second = c->marker_address;
            break;
        default:
            break;
        }
        if (spawning_steps[j].arguments == ARGUMENTS_MEMORY && c->desktop)
            info(label, L"not tried: on a desktop the probe opens no other process for reading");
        else if (spawning_steps[j].arguments == ARGUMENTS_SESSION_OBJECTS && first == NULL)
            verdict(0, FINDING, label, what, L"not tried: the session's directory is unknown");
        else if (!(spawning_steps[j].flags & SPAWN_DETACHED) && row->token != NULL && other_logon(row->token))
            verdict(0, FINDING, label, what,
                    L"not tried: the Secondary Logon service gives the process a console of its own, and takes no "
                    L"pseudoconsole");
        else
            expect_allowed(FINDING, label, what,
                           operation_with(row->token, spawning_steps[j].flags, spawning_steps[j].op, first, second));
    }
}

/* Every token's row but the build account's, which runs once the accounts
 * are made, last. */
static void check_spawning(const struct context *c)
{
    const struct candidate rows[] = {
        {L"own", L"the checks' own token", NULL, NULL},
        {L"standard", L"the standard user's stand-in, no privilege and no restricting SID", c->standard,
         c->standard == NULL ? L"no stand-in token was made" : NULL},
        {L"design", L"the restricted-token design's token", c->build_one, NULL},
        {L"limited", L"S-1-5-12, the build's SID, Users and Everyone restricting", c->limited,
         c->limited == NULL ? L"the token was not made" : NULL},
        {L"write-restricted", L"the design's restricting SIDs for writes alone", c->write_restricted,
         c->write_restricted == NULL ? L"the token was not made" : NULL},
        {L"wr-everyone", L"S-1-5-12, the build's SID and Everyone restricting writes", c->write_restricted_everyone,
         c->write_restricted_everyone == NULL ? L"the token was not made" : NULL},
        {L"wr-low", L"the write-restricted token at Low integrity", c->write_restricted_low,
         c->write_restricted_low == NULL ? L"the token was not made" : NULL},
        {L"wr-deny-user", L"the write-restricted token with the user's SID deny-only", c->write_restricted_deny_user,
         c->write_restricted_deny_user == NULL ? L"the token was not made" : NULL},
    };
    size_t i;
    for (i = 0; i < sizeof rows / sizeof rows[0]; i++)
        spawning_row(c, &rows[i]);
}

/* What the pass's first build token reaches elsewhere on the machine:
 * writes where every user may, a listing of the user's profile, and a
 * read of a system DLL as a file. */
static void check_exposure(const struct context *c)
{
    const struct {
        const WCHAR *name;
        const WCHAR *path;
        const WCHAR *access;
        const WCHAR *what;
    } places[] = {
        {L"drive-root", c->system_drive != NULL ? join(c->system_drive, L"\\", END) : NULL, MASK_ADD_SUBDIRECTORY,
         L"make a directory at the system drive's root"},
        {L"programdata", c->program_data, MASK_ADD_SUBDIRECTORY, L"make a directory in ProgramData"},
        {L"public", c->public_directory, MASK_ADD_FILE, L"create in the Public profile"},
        {L"windows-temp", c->system_root != NULL ? join(c->system_root, L"\\Temp", END) : NULL, MASK_ADD_FILE,
         L"create in the Windows directory's Temp"},
        {L"user-temp", c->user_temp, MASK_ADD_FILE, L"create in the user's TEMP"},
        {L"profile-list", c->user_profile, MASK_LIST, L"list the user's profile"},
    };
    size_t i;
    if (c->desktop)
        info(L"exposure-writes", L"not tried: on a desktop the probe opens nothing outside its root for writing");
    for (i = 0; i < sizeof places / sizeof places[0]; i++)
        if (places[i].path != NULL && (!c->desktop || wide_equal(places[i].access, MASK_LIST)))
            expect_denied(FINDING, join(L"exposure-", places[i].name, END),
                          join(L"a build cannot ", places[i].what, END),
                          operation(c->build_one, L"open", places[i].path, places[i].access));
    if (c->system_root != NULL)
        expect_allowed(FINDING, L"exposure-system-read", L"a build reads kernel32.dll as a file",
                       operation(c->build_one, L"open", join(c->system_root, L"\\System32\\kernel32.dll", END),
                                 MASK_READ));
}

/* What a build can do to the user's own: read a file whose DACL grants
 * the user alone, a file in the user's profile, the user's registry hive
 * and another of the user's processes' memory; and delete a file in the
 * user's profile, which the probe makes there when the run allows it and
 * removes itself should the build not.  Another build's outputs are
 * check 6's. */
static void check_read_exposure(const struct context *c)
{
    expect_denied(REQUIRED, L"read-user-only-file", L"a build cannot read a file whose DACL grants the user alone",
                  operation(c->build_one, L"open", join(c->outside, L"\\sentinel", END), MASK_READ));
    if (c->desktop)
        info(L"read-profile-file", L"not tried: on a desktop the probe opens no file of the user's own");
    else if (c->profile_file == NULL)
        verdict(0, FINDING, L"read-profile-file", L"a build cannot read a file in the user's profile",
                L"not tried: no file there the user can read was found");
    else
        expect_denied(REQUIRED, L"read-profile-file",
                      join(L"a build cannot read a file in the user's profile, ", c->profile_file, END),
                      operation(c->build_one, L"open", c->profile_file, MASK_READ));
    expect_denied(REQUIRED, L"read-user-hive", L"a build cannot read the user's registry hive",
                  operation(c->build_one, L"open-key", user_hive(c), NULL));
    if (c->desktop)
        info(L"read-process-memory", L"not tried: on a desktop the probe opens no other process for reading");
    else
        expect_denied(REQUIRED, L"read-process-memory",
                      L"a build cannot open another of the user's processes, the checks', to read its memory",
                      operation(c->build_one, L"read-memory", c->checks_pid, c->marker_address));
    if (!c->profile_writes) {
        info(L"delete-profile-file", L"not tried: the run was not given write-profile");
        return;
    }
    if (c->user_profile == NULL) {
        info(L"delete-profile-file", L"not tried: USERPROFILE is not set");
        return;
    }
    {
        const WCHAR *planted =
            join(c->user_profile, L"\\nova-nix-probe-", c->checks_pid, L"-", pass_prefix, L"planted.txt", END);
        DWORD error = make_file(planted, NULL, "planted by the sandbox probe\n");
        if (error != 0) {
            verdict(0, REQUIRED, L"delete-profile-file", L"a build cannot delete a file in the user's profile",
                    join(L"not tried: making the file failed, ", error_text(error), END));
            return;
        }
        expect_denied(REQUIRED, L"delete-profile-file", L"a build cannot delete a file in the user's profile",
                      operation(c->build_one, L"delete", planted, NULL));
        if (exists(planted) && !DeleteFileW(planted))
            info(L"delete-profile-file-cleanup",
                 join(L"the probe could not remove ", planted, L", ", error_text(GetLastError()), END));
    }
}

/* 10. */
static void check_user_directories(const struct context *c)
{
    if (c->desktop)
        info(L"tier-b-writes", L"not tried: on a desktop the probe opens nothing outside its root for writing");
    if (c->user_temp != NULL && !c->desktop)
        expect_denied(REQUIRED, L"tier-b-temp", L"the restricted child cannot create in the user's TEMP",
                      operation(c->build_one, L"open", c->user_temp, MASK_ADD_FILE));
    if (c->user_profile != NULL) {
        expect_denied(REQUIRED, L"tier-b-profile-list", L"the restricted child cannot list the user's profile",
                      operation(c->build_one, L"open", c->user_profile, MASK_LIST));
        if (!c->desktop)
            expect_denied(REQUIRED, L"tier-b-profile-write", L"the restricted child cannot create in the user's profile",
                          operation(c->build_one, L"open", c->user_profile, MASK_ADD_FILE));
    }
    if (c->user_appdata != NULL && !c->desktop)
        expect_denied(REQUIRED, L"tier-b-appdata", L"the restricted child cannot create in the user's APPDATA",
                      operation(c->build_one, L"open", c->user_appdata, MASK_ADD_FILE));
    if (c->system_root != NULL) {
        expect_denied(FINDING, L"tier-b-system-read", L"the restricted child cannot read kernel32.dll as a file",
                      operation(c->build_one, L"open", join(c->system_root, L"\\System32\\kernel32.dll", END), MASK_READ));
        expect_denied(FINDING, L"tier-b-ucrt-read", L"the restricted child cannot read ucrtbase.dll as a file",
                      operation(c->build_one, L"open", join(c->system_root, L"\\System32\\ucrtbase.dll", END), MASK_READ));
    }
}

/* nova-nix running as a standard user, not as the runner's administrator. */
static void check_standard_user(const struct context *c)
{
    if (c->standard == NULL) {
        verdict(0, FINDING, L"standard-user-spawn", L"no standard-user stand-in token to try with", NULL);
        return;
    }
    expect_allowed(FINDING, L"standard-user-spawn",
                   L"a process with no privilege but bypassing traverse checks starts a restricted child of its own",
                   operation(c->standard, L"spawn-test", NULL, NULL));
}

/* Whether a token's groups hold the SID a string names. */
static int token_has_group(HANDLE token, const WCHAR *group)
{
    DWORD needed = 0;
    TOKEN_GROUPS *groups;
    PSID wanted = sid_of(group);
    DWORD i;
    int found = 0;
    if (wanted == NULL)
        return 0;
    GetTokenInformation(token, TokenGroups, NULL, 0, &needed);
    groups = take(needed > 0 ? needed : 1);
    if (needed > 0 && GetTokenInformation(token, TokenGroups, groups, needed, &needed))
        for (i = 0; i < groups->GroupCount && !found; i++)
            found = EqualSid(groups->Groups[i].Sid, wanted);
    LocalFree(wanted);
    return found;
}

typedef BOOL(WINAPI *lookup_sid_call)(LPCWSTR, PSID, LPWSTR, LPDWORD, LPWSTR, LPDWORD, PSID_NAME_USE);

/* The room an account's or domain's name is given. */
#define ACCOUNT_NAME_CHARS 256

/* An account's name, DOMAIN\NAME, or why it cannot be had. */
static WCHAR *account_name(PSID sid)
{
    lookup_sid_call lookup = (lookup_sid_call)function_in(L"advapi32.dll", "LookupAccountSidW");
    WCHAR *name = take(ACCOUNT_NAME_CHARS * sizeof(WCHAR));
    WCHAR *domain = take(ACCOUNT_NAME_CHARS * sizeof(WCHAR));
    DWORD name_size = ACCOUNT_NAME_CHARS;
    DWORD domain_size = ACCOUNT_NAME_CHARS;
    SID_NAME_USE use;
    if (lookup == NULL || !lookup(NULL, sid, name, &name_size, domain, &domain_size, &use))
        return join(L"unknown, ", error_text(lookup == NULL ? ERROR_PROC_NOT_FOUND : GetLastError()), END);
    return join(domain, L"\\", name, END);
}

/* The kinds of logon a token's groups say it comes from: an sshd session
 * is a network or service logon, not the interactive desktop's. */
static WCHAR *logon_kinds(HANDLE token)
{
    static const struct {
        const WCHAR *sid;
        const WCHAR *kind;
    } kinds[] = {{L"S-1-5-4", L" interactive"}, {L"S-1-5-2", L" network"}, {L"S-1-5-3", L" batch"},
                 {L"S-1-5-6", L" service"},     {L"S-1-2-1", L" console"}, {L"S-1-5-14", L" remote-interactive"}};
    WCHAR *found = join(L"", END);
    size_t i;
    for (i = 0; i < sizeof kinds / sizeof kinds[0]; i++)
        if (token_has_group(token, kinds[i].sid))
            found = join(found, kinds[i].kind, END);
    return found[0] != 0 ? found + 1 : join(L"none of the logon groups", END);
}

/* Report the runner's context, and answer this process's user SID. */
static WCHAR *describe_context(HANDLE base)
{
    DWORD needed = 0;
    WCHAR *user = NULL;
    GetTokenInformation(base, TokenUser, NULL, 0, &needed);
    {
        TOKEN_USER *token_user = take(needed);
        if (GetTokenInformation(base, TokenUser, token_user, needed, &needed)) {
            user = sid_text(token_user->User.Sid);
            info(L"context-account", account_name(token_user->User.Sid));
        }
    }
    info(L"context-logon", logon_kinds(base));
    needed = 0;
    GetTokenInformation(base, TokenOwner, NULL, 0, &needed);
    {
        TOKEN_OWNER *owner = take(needed);
        if (GetTokenInformation(base, TokenOwner, owner, needed, &needed))
            info(L"context-owner", join(L"the runner's new objects are owned by ", sid_text(owner->Owner), END));
    }
    needed = 0;
    GetTokenInformation(base, TokenIntegrityLevel, NULL, 0, &needed);
    {
        TOKEN_MANDATORY_LABEL *label = take(needed);
        if (GetTokenInformation(base, TokenIntegrityLevel, label, needed, &needed))
            info(L"context-integrity", sid_text(label->Label.Sid));
    }
    {
        TOKEN_ELEVATION elevation;
        DWORD size = sizeof elevation;
        if (GetTokenInformation(base, TokenElevation, &elevation, size, &size))
            info(L"context-elevated", elevation.TokenIsElevated ? L"yes" : L"no");
    }
    info(L"context-token-object", object_sddl(base));
    return user;
}

/* An access entry for each SID the layout grants what a build needs, with
 * the inheritance FLAGS and the RIGHTS given, in SDDL. */
static WCHAR *builder_entries(const struct context *c, const WCHAR *flags, const WCHAR *rights)
{
    WCHAR *entries = join(L"", END);
    size_t i;
    for (i = 0; i < sizeof c->builders / sizeof c->builders[0] && c->builders[i] != NULL; i++)
        entries = join(entries, L"(A;", flags, L";", rights, L";;;", c->builders[i], L")", END);
    return entries;
}

/* Make the layout's directories and files.  Answers whether all were
 * made. */
static int lay_out(const struct context *c)
{
    const WCHAR *owner_only = join(L"O:", c->user, END);
    /* Each directory, made in this order, with its owner and DACL. */
    const struct {
        const WCHAR *path;
        const WCHAR *sddl;
    } directories[] = {
        /* The proposed store directory: entries for itself alone, the
         * builders allowed to create an entry and nothing else. */
        {c->store, join(L"O:", c->user, L"D:P(A;;FA;;;", c->user, L")(A;;FA;;;SY)(A;;FA;;;BA)(A;;", RIGHTS_READ_EXECUTE,
                        L";;;BU)", builder_entries(c, L"", RIGHTS_CREATE_ONLY), END)},
        /* A directory that grants the builders nothing. */
        {c->nogrant, join(L"O:", c->user, L"D:P(A;OICI;FA;;;", c->user, L")", END)},
        /* The store's present layout: the user's entries inherited
         * below, with the same create grant as the proposal. */
        {c->inherit, join(L"O:", c->user, L"D:P(A;OICI;FA;;;", c->user, L")(A;OICI;FA;;;SY)",
                          builder_entries(c, L"", RIGHTS_CREATE_ONLY), END)},
        /* A valid store path's proposed DACL, protected and inherited
         * below: the builders read and execute, and OWNER RIGHTS takes
         * the owner's implicit WRITE_DAC away. */
        {c->valid, join(L"O:", c->user, L"D:P(A;OICI;FA;;;", c->user, L")(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;",
                        RIGHTS_READ_EXECUTE, L";;;BU)", builder_entries(c, L"OICI", RIGHTS_READ_EXECUTE),
                        L"(A;OICI;RC;;;", OWNER_RIGHTS_SID, L")", END)},
        {c->valid_directory, owner_only},
        /* A user-owned tree the builders may read, without and with an
         * OWNER RIGHTS entry. */
        {c->own, join(L"O:", c->user, L"D:P(A;OICI;FA;;;", c->user, L")",
                      builder_entries(c, L"OICI", RIGHTS_READ_EXECUTE), END)},
        {c->ownow, join(L"O:", c->user, L"D:P(A;OICI;FA;;;", c->user, L")",
                        builder_entries(c, L"OICI", RIGHTS_READ_EXECUTE), L"(A;OICI;RC;;;", OWNER_RIGHTS_SID, L")",
                        END)},
        /* Somewhere a junction could lead, the user's alone. */
        {c->outside, join(L"O:", c->user, L"D:P(A;OICI;FA;;;", c->user, L")", END)},
        /* The first build's build directory, its SID in full. */
        {c->build, join(L"O:", c->user, L"D:P(A;OICI;FA;;;", c->user, L")(A;OICI;FA;;;SY)(A;OICI;FA;;;", c->b1, L")",
                        END)},
    };
    size_t i;
    DWORD error;
    for (i = 0; i < sizeof directories / sizeof directories[0]; i++) {
        error = make_directory(directories[i].path, directories[i].sddl);
        if (error != 0) {
            verdict(0, REQUIRED, L"setup-layout", join(L"make ", directories[i].path, END), error_text(error));
            return 0;
        }
    }
    if ((error = make_file(join(c->valid, L"\\f", END), owner_only, "store data\n")) != 0
        || (error = make_file(join(c->own, L"\\plain", END), owner_only, "owned\n")) != 0
        || (error = make_file(join(c->own, L"\\by-default-owner", END), NULL, "owned\n")) != 0
        || (error = make_file(join(c->ownow, L"\\plain", END), owner_only, "owned\n")) != 0
        || (error = make_file(join(c->outside, L"\\sentinel", END), NULL, "sentinel\n")) != 0
        || (error = make_file(join(c->build, L"\\hello.c", END), NULL,
                              "#include <stdio.h>\nint main(void)\n{\n    puts(\"hello\");\n    return 0;\n}\n"))
               != 0) {
        verdict(0, REQUIRED, L"setup-files", L"make the layout's files", error_text(error));
        return 0;
    }
    info(L"layout-store", sddl_of(c->store));
    info(L"layout-valid-file", sddl_of(join(c->valid, L"\\f", END)));
    info(L"layout-by-default-owner", sddl_of(join(c->own, L"\\by-default-owner", END)));
    return 1;
}

/* The standard-user stand-in: the Administrators group deny-only, no
 * privilege but bypassing traverse checks, and the owner, default DACL
 * and object DACL a standard user's token has, naming the user. */
static HANDLE standard_token(HANDLE base, PSID administrators, PSID user, const WCHAR *user_text, DWORD *error)
{
    SID_AND_ATTRIBUTES disabled;
    HANDLE token = NULL;
    WCHAR *dacl = join(L"D:(A;;GA;;;", user_text, L")(A;;GA;;;SY)", END);
    disabled.Sid = administrators;
    disabled.Attributes = 0;
    if (!CreateRestrictedToken(base, DISABLE_MAX_PRIVILEGE, 1, &disabled, 0, NULL, 0, NULL, &token)) {
        *error = GetLastError();
        return NULL;
    }
    *error = dress_token(token, user, dacl, dacl);
    if (*error != 0) {
        CloseHandle(token);
        return NULL;
    }
    return token;
}

/* How deep below the user's profile the search for a readable file goes. */
#define PROFILE_SEARCH_DEPTH 3

/* A regular file at most DEPTH directories below DIRECTORY that this
 * process can read, junctions and links passed over, or NULL. */
static WCHAR *readable_file_below(const WCHAR *directory, int depth)
{
    WIN32_FIND_DATAW found;
    HANDLE search = FindFirstFileW(join(directory, L"\\*", END), &found);
    WCHAR *answer = NULL;
    if (search == INVALID_HANDLE_VALUE)
        return NULL;
    do {
        WCHAR *path;
        if (wide_equal(found.cFileName, L".") || wide_equal(found.cFileName, L"..")
            || (found.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT))
            continue;
        path = join(directory, L"\\", found.cFileName, END);
        if (found.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
            if (depth > 0)
                answer = readable_file_below(path, depth - 1);
        } else if (read_start(path) != NULL) {
            answer = path;
        }
    } while (answer == NULL && FindNextFileW(search, &found));
    FindClose(search);
    return answer;
}

/* The layout's paths under ROOT. */
static void place(struct context *c, const WCHAR *root)
{
    c->store = join(root, L"\\store", END);
    c->nogrant = join(root, L"\\nogrant", END);
    c->inherit = join(root, L"\\inherit", END);
    c->valid = join(root, L"\\valid", END);
    c->valid_directory = join(c->valid, L"\\d", END);
    c->own = join(root, L"\\own", END);
    c->ownow = join(root, L"\\ownow", END);
    c->outside = join(root, L"\\outside", END);
    c->build = join(root, L"\\build", END);
    c->b1_out = join(c->store, L"\\b1-out", END);
}

/* A candidate token, made as SHAPE says for the build BUILD names, or
 * NULL with why it was not in FAILURE, which is also reported under
 * LABEL. */
static HANDLE candidate_token(HANDLE base, PSID user, const WCHAR *user_text, PSID build, const WCHAR *build_text,
                              const struct token_shape *shape, const WCHAR *label, const WCHAR **failure)
{
    DWORD error;
    HANDLE token = build_token(base, user, user_text, build, build_text, shape, &error);
    *failure = NULL;
    if (token == NULL) {
        *failure = token_failure(error);
        info(label, *failure);
    }
    return token;
}

/* The checks a build's token is judged by, in a layout of the pass's own
 * under ROOT\NAME, with ONE and TWO as the first and second builds'
 * tokens.  KIND says whether the pass is the design's. */
static void full_pass(const struct context *c, const WCHAR *root, const WCHAR *name, const WCHAR *about, int kind,
                      HANDLE one, HANDLE two)
{
    struct context w = *c;
    const WCHAR *pass_root = join(root, L"\\", name, END);
    begin_pass(join(name, L"-", END), kind);
    info(L"pass", about);
    if (one == NULL || two == NULL) {
        verdict(0, REQUIRED, L"setup-tokens", L"make the pass's tokens", NULL);
    } else if (!CreateDirectoryW(pass_root, NULL)) {
        verdict(0, REQUIRED, L"setup-root", join(L"make ", pass_root, END), error_text(GetLastError()));
    } else {
        w.build_one = one;
        w.build_two = two;
        place(&w, pass_root);
        if (lay_out(&w)) {
            check_pipe(&w);
            check_owner(&w);
            check_store(&w);
            check_valid_path(&w);
            check_isolation(&w);
            check_jobs(&w);
            check_tools(&w);
            check_user_directories(&w);
            check_exposure(&w);
            check_read_exposure(&w);
        }
    }
    end_pass();
}

/* What a variant of the write-restricted token, VARIANT, still does of
 * what the plain one, MAKER, does, in a layout of the pass's own under
 * ROOT\NAME: MAKER, as the first build, makes an output; VARIANT, as the
 * second, starts a process, makes a file of its own, and is tried on the
 * valid path, on that output, and on the user's own. */
static void targeted_pass(const struct context *c, const WCHAR *root, const WCHAR *name, const WCHAR *about,
                          HANDLE maker, HANDLE variant)
{
    struct context w = *c;
    const WCHAR *pass_root = join(root, L"\\", name, END);
    begin_pass(join(name, L"-", END), CANDIDATE_PASS);
    info(L"pass", about);
    if (maker == NULL || variant == NULL) {
        verdict(0, REQUIRED, L"setup-tokens", L"make the pass's tokens", NULL);
    } else if (!CreateDirectoryW(pass_root, NULL)) {
        verdict(0, REQUIRED, L"setup-root", join(L"make ", pass_root, END), error_text(GetLastError()));
    } else {
        place(&w, pass_root);
        if (lay_out(&w)) {
            struct outcome made = operation(maker, L"mkdir", w.b1_out, NULL);
            struct outcome filled = operation(maker, L"create", join(w.b1_out, L"\\f", END), NULL);
            verdict(made.succeeded && filled.succeeded, FINDING, L"setup-first-output",
                    L"the plain write-restricted token, as the first build, makes an output",
                    join(L"mkdir ", made.detail, L"; file ", filled.detail, END));
            w.build_one = variant;
            w.build_two = variant;
            expect_allowed(FINDING, L"spawn-probe", L"a build starts a process with CreateProcessW, and it runs",
                           operation(variant, L"spawn", L"probe", FLAGS_DETACHED));
            expect_allowed(FINDING, L"D-create-file", L"a build creates a file in the store and writes it",
                           operation(variant, L"create", join(w.store, L"\\variant-file", END), NULL));
            check_valid_path(&w);
            check_isolation(&w);
            check_read_exposure(&w);
        }
    }
    end_pass();
}

/* ------------------------------------------------------------------------
 * Build accounts, standing in for the per-slot accounts a service runs
 * builds as: two local accounts, each granted the right to log on as a
 * batch job and logged on with a password generated for it, for a
 * primary token of its own.  Made only when this process is elevated, and
 * removed again, logon right first, whatever came of the checks.  netapi32
 * and these advapi32 functions are looked up here, in the parent alone.
 * ------------------------------------------------------------------------ */

#define ACCOUNT_COUNT 2
#define ACCOUNT_NAME_PREFIX L"nnprobe"
/* An upper-case letter, a lower-case letter, a digit and a symbol, which
 * the password complexity policy asks a password to hold; random letters
 * and digits follow. */
#define ACCOUNT_PASSWORD_PREFIX L"Aa1!"
#define ACCOUNT_RANDOM_CHARS 20
#define BATCH_LOGON_RIGHT L"SeBatchLogonRight"
#define DOMAIN_TEXT_MAX 256

typedef NET_API_STATUS(WINAPI *user_add_call)(LPCWSTR, DWORD, LPBYTE, LPDWORD);
typedef NET_API_STATUS(WINAPI *user_delete_call)(LPCWSTR, LPCWSTR);
typedef BOOL(WINAPI *lookup_name_call)(LPCWSTR, LPCWSTR, PSID, LPDWORD, LPWSTR, LPDWORD, PSID_NAME_USE);
typedef BOOL(WINAPI *logon_call)(LPCWSTR, LPCWSTR, LPCWSTR, DWORD, DWORD, PHANDLE);
typedef NTSTATUS(NTAPI *policy_open_call)(PLSA_UNICODE_STRING, PLSA_OBJECT_ATTRIBUTES, ACCESS_MASK, PLSA_HANDLE);
typedef NTSTATUS(NTAPI *rights_add_call)(LSA_HANDLE, PSID, PLSA_UNICODE_STRING, ULONG);
typedef NTSTATUS(NTAPI *rights_remove_call)(LSA_HANDLE, PSID, BOOLEAN, PLSA_UNICODE_STRING, ULONG);
typedef NTSTATUS(NTAPI *policy_close_call)(LSA_HANDLE);
typedef BOOLEAN(WINAPI *random_call)(PVOID, ULONG);

struct account {
    WCHAR *name;
    PSID sid;
    WCHAR *sid_text;
    HANDLE token;
    /* What has been done, so that removal undoes that much. */
    int made;
    int right_granted;
};

struct accounts {
    struct account slot[ACCOUNT_COUNT];
    HMODULE netapi;
    LSA_HANDLE policy;
};

/* A password for a build account, or NULL. */
static WCHAR *account_password(void)
{
    static const WCHAR alphabet[] = L"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    random_call random = (random_call)function_in(L"advapi32.dll", "SystemFunction036");
    unsigned char bytes[ACCOUNT_RANDOM_CHARS];
    size_t prefix = wide_length(ACCOUNT_PASSWORD_PREFIX);
    WCHAR *password;
    size_t i;
    if (random == NULL || !random(bytes, sizeof bytes))
        return NULL;
    password = take((prefix + ACCOUNT_RANDOM_CHARS + 1) * sizeof(WCHAR));
    memcpy(password, ACCOUNT_PASSWORD_PREFIX, prefix * sizeof(WCHAR));
    for (i = 0; i < ACCOUNT_RANDOM_CHARS; i++)
        password[prefix + i] = alphabet[bytes[i] % (sizeof alphabet / sizeof alphabet[0] - 1)];
    return password;
}

/* Make the build accounts and log each on, its token's default DACL
 * dressed as a build's.  Answers NULL, or why they could not all be
 * made; what was made is in ACCOUNTS either way, for remove_accounts. */
static const WCHAR *make_accounts(struct accounts *accounts, const struct context *c)
{
    user_add_call user_add;
    lookup_name_call lookup_name = (lookup_name_call)function_in(L"advapi32.dll", "LookupAccountNameW");
    logon_call logon = (logon_call)function_in(L"advapi32.dll", "LogonUserW");
    policy_open_call policy_open = (policy_open_call)function_in(L"advapi32.dll", "LsaOpenPolicy");
    rights_add_call rights_add = (rights_add_call)function_in(L"advapi32.dll", "LsaAddAccountRights");
    LSA_OBJECT_ATTRIBUTES attributes;
    UNICODE_STRING right_text = counted(BATCH_LOGON_RIGHT);
    LSA_UNICODE_STRING right;
    WCHAR *password;
    NTSTATUS status;
    size_t i;
    memset(&attributes, 0, sizeof attributes);
    right.Length = right_text.Length;
    right.MaximumLength = right_text.MaximumLength;
    right.Buffer = right_text.Buffer;
    accounts->netapi = LoadLibraryExW(L"netapi32.dll", NULL, LOAD_LIBRARY_SEARCH_SYSTEM32);
    if (accounts->netapi == NULL)
        return join(L"netapi32 did not load, ", error_text(GetLastError()), END);
    user_add = (user_add_call)(any_function)GetProcAddress(accounts->netapi, "NetUserAdd");
    if (user_add == NULL || lookup_name == NULL || logon == NULL || policy_open == NULL || rights_add == NULL)
        return join(L"a function was not found, ", error_text(ERROR_PROC_NOT_FOUND), END);
    status = policy_open(NULL, &attributes, POLICY_CREATE_ACCOUNT | POLICY_LOOKUP_NAMES, &accounts->policy);
    if (!NT_SUCCESS(status)) {
        accounts->policy = NULL;
        return join(L"LsaOpenPolicy failed, ", error_text(status_error(status)), END);
    }
    password = account_password();
    if (password == NULL)
        return join(L"no password could be generated, ", error_text(GetLastError()), END);
    for (i = 0; i < ACCOUNT_COUNT; i++) {
        struct account *slot = &accounts->slot[i];
        USER_INFO_1 user;
        DWORD bad_field = 0;
        DWORD sid_size = SECURITY_MAX_SID_SIZE;
        DWORD domain_size = DOMAIN_TEXT_MAX;
        WCHAR *domain = take(DOMAIN_TEXT_MAX * sizeof(WCHAR));
        SID_NAME_USE use;
        NET_API_STATUS added;
        DWORD error;
        slot->name = join(ACCOUNT_NAME_PREFIX, c->checks_pid, L"s", number(i + 1, 0), END);
        memset(&user, 0, sizeof user);
        user.usri1_name = slot->name;
        user.usri1_password = password;
        user.usri1_priv = USER_PRIV_USER;
        user.usri1_flags = UF_SCRIPT | UF_DONT_EXPIRE_PASSWD;
        added = user_add(NULL, 1, (LPBYTE)&user, &bad_field);
        if (added != NERR_Success)
            return join(L"NetUserAdd ", slot->name, L" failed, ", error_text(added), END);
        slot->made = 1;
        slot->sid = take(SECURITY_MAX_SID_SIZE);
        if (!lookup_name(NULL, slot->name, slot->sid, &sid_size, domain, &domain_size, &use))
            return join(L"LookupAccountNameW ", slot->name, L" failed, ", error_text(GetLastError()), END);
        slot->sid_text = sid_text(slot->sid);
        status = rights_add(accounts->policy, slot->sid, &right, 1);
        if (!NT_SUCCESS(status))
            return join(L"LsaAddAccountRights ", slot->name, L" failed, ", error_text(status_error(status)), END);
        slot->right_granted = 1;
        if (!logon(slot->name, L".", password, LOGON32_LOGON_BATCH, LOGON32_PROVIDER_DEFAULT, &slot->token)) {
            slot->token = NULL;
            return join(L"LogonUserW ", slot->name, L" failed, ", error_text(GetLastError()), END);
        }
        error = dress_token(slot->token, NULL, build_default_sddl(c->user, slot->sid_text, BUILD_RIGHTS), NULL);
        if (error != 0)
            return join(L"setting ", slot->name, L"'s default DACL failed, ", error_text(error), END);
        info(join(L"build-account-", number(i + 1, 0), END),
             join(slot->name, L" ", slot->sid_text, L", in Users ", token_has_group(slot->token, USERS_SID) ? L"yes" : L"no",
                  L", token object ", object_sddl(slot->token), END));
    }
    return NULL;
}

/* Remove what make_accounts made: each token, logon right and account. */
static void remove_accounts(struct accounts *accounts)
{
    rights_remove_call rights_remove = (rights_remove_call)function_in(L"advapi32.dll", "LsaRemoveAccountRights");
    policy_close_call policy_close = (policy_close_call)function_in(L"advapi32.dll", "LsaClose");
    user_delete_call user_delete =
        accounts->netapi != NULL ? (user_delete_call)(any_function)GetProcAddress(accounts->netapi, "NetUserDel") : NULL;
    size_t i;
    for (i = 0; i < ACCOUNT_COUNT; i++) {
        struct account *slot = &accounts->slot[i];
        if (slot->token != NULL)
            CloseHandle(slot->token);
        if (slot->right_granted && rights_remove != NULL) {
            NTSTATUS status = rights_remove(accounts->policy, slot->sid, TRUE, NULL, 0);
            if (!NT_SUCCESS(status))
                info(L"build-account-cleanup", join(L"LsaRemoveAccountRights ", slot->name, L" failed, ",
                                              error_text(status_error(status)), END));
        }
        if (slot->made) {
            NET_API_STATUS deleted = user_delete != NULL ? user_delete(NULL, slot->name) : ERROR_PROC_NOT_FOUND;
            info(L"build-account-cleanup", deleted == NERR_Success ? join(L"removed ", slot->name, END)
                                                              : join(L"NetUserDel ", slot->name, L" failed, ",
                                                                     error_text(deleted), END));
        }
    }
    if (accounts->policy != NULL && policy_close != NULL)
        policy_close(accounts->policy);
    if (accounts->netapi != NULL)
        FreeLibrary(accounts->netapi);
}

/* Whether a token is elevated. */
static int elevated(HANDLE token)
{
    TOKEN_ELEVATION elevation;
    DWORD size = sizeof elevation;
    return GetTokenInformation(token, TokenElevation, &elevation, size, &size) && elevation.TokenIsElevated;
}

/* The design: the checks under the two build accounts' own tokens, the
 * layout granting each account what a build needs. */
static void account_pass(const struct context *c, const WCHAR *root, const struct accounts *accounts)
{
    struct context a = *c;
    a.builders[0] = accounts->slot[0].sid_text;
    a.builders[1] = accounts->slot[1].sid_text;
    a.b1 = accounts->slot[0].sid_text;
    full_pass(&a, root, L"account",
              L"the design: the checks under two build accounts' own tokens, started through the Secondary Logon "
              L"service",
              DESIGN_PASS, c->account_one, c->account_two);
}

static void checks(const WCHAR *root, const WCHAR *bash, const WCHAR *gcc, const WCHAR *option)
{
    struct context c;
    struct accounts accounts;
    HANDLE base = NULL;
    PSID user_sid = NULL;
    PSID build_one_sid = NULL;
    PSID build_two_sid = NULL;
    PSID administrators = NULL;
    DWORD error;
    DWORD session = 0;
    DWORD session_error = 0;
    DWORD seed = GetTickCount() ^ GetCurrentProcessId();
    WCHAR *b2;

    memset(&c, 0, sizeof c);
    memset(&accounts, 0, sizeof accounts);
    c.bash = bash;
    c.gcc = gcc;
    c.profile_writes = option != NULL && wide_equal(option, WRITE_PROFILE);
    c.desktop = option != NULL && wide_equal(option, DESKTOP);
    c.user_temp = environment(L"TEMP");
    c.user_profile = environment(L"USERPROFILE");
    c.user_appdata = environment(L"APPDATA");
    c.system_root = environment(L"SystemRoot");
    c.system_drive = environment(L"SystemDrive");
    c.program_data = environment(L"ProgramData");
    c.public_directory = environment(L"PUBLIC");
    c.tools = parent_directory(child_directory);
    c.checks_pid = number(GetCurrentProcessId(), 0);
    c.marker_address = number((ULONG_PTR)memory_marker, 1);
    if (!ProcessIdToSessionId(GetCurrentProcessId(), &session))
        session_error = GetLastError();
    else
        c.session_objects = session == 0 ? GLOBAL_OBJECTS
                                         : join(L"\\Sessions\\", number(session, 0), L"\\BaseNamedObjects", END);
    c.profile_file = c.user_profile != NULL && !c.desktop ? readable_file_below(c.user_profile, PROFILE_SEARCH_DEPTH)
                                                           : NULL;

    if (!OpenProcessToken(GetCurrentProcess(), BASE_TOKEN_ACCESS, &base)) {
        verdict(0, REQUIRED, L"setup-token", L"open this process's token", error_text(GetLastError()));
        return;
    }
    c.user = describe_context(base);
    if (c.user == NULL) {
        verdict(0, REQUIRED, L"setup-user", L"read this process's user SID", error_text(GetLastError()));
        CloseHandle(base);
        return;
    }
    info(L"context-user", c.user);
    info(L"context-paths", join(L"TEMP=", c.user_temp ? c.user_temp : L"?", L" USERPROFILE=",
                                c.user_profile ? c.user_profile : L"?", L" APPDATA=",
                                c.user_appdata ? c.user_appdata : L"?", END));
    info(L"context-session", session_error != 0
                                 ? join(L"unknown, ", error_text(session_error), END)
                                 : join(number(session, 0), L", its named objects in ", c.session_objects, END));
    info(L"context-mode", c.desktop ? L"desktop: writes only under the root, opens no file of the user's own, no "
                                      L"other process and nothing outside the root for writing, and makes no account"
                                    : L"runner");
    info(L"context-profile-file", c.desktop ? L"not looked for on a desktop"
                                  : c.profile_file != NULL ? c.profile_file
                                                           : L"none found");
    info(L"context-profile-writes",
         c.profile_writes ? L"allowed" : L"not allowed: the run was not given write-profile");

    c.b1 = join(L"S-1-9-", number(seed, 0), L"-", number(GetCurrentProcessId(), 0), L"-1", END);
    b2 = join(L"S-1-9-", number(seed, 0), L"-", number(GetCurrentProcessId(), 0), L"-2", END);
    info(L"context-build-sids", join(c.b1, L" and ", b2, END));
    user_sid = sid_of(c.user);
    build_one_sid = sid_of(c.b1);
    build_two_sid = sid_of(b2);
    administrators = sid_of(ADMINISTRATORS_SID);
    if (user_sid == NULL || build_one_sid == NULL || build_two_sid == NULL || administrators == NULL) {
        verdict(0, REQUIRED, L"setup-sids", L"make the SIDs", error_text(GetLastError()));
        goto cleanup;
    }
    c.build_one = build_token(base, user_sid, c.user, build_one_sid, c.b1, &design_shape, &error);
    if (c.build_one == NULL) {
        verdict(0, REQUIRED, L"setup-token-build-one", L"make the first build's token", token_failure(error));
        goto cleanup;
    }
    c.build_two = build_token(base, user_sid, c.user, build_two_sid, b2, &design_shape, &error);
    if (c.build_two == NULL) {
        verdict(0, REQUIRED, L"setup-token-build-two", L"make the second build's token", token_failure(error));
        goto cleanup;
    }
    c.build_bare = build_token(base, user_sid, c.user, build_one_sid, c.b1, &bare_shape, &error);
    if (c.build_bare == NULL) {
        verdict(0, REQUIRED, L"setup-token-build-bare", L"make the first build's token without its object grant",
                token_failure(error));
        goto cleanup;
    }
    /* The candidate tokens.  One that cannot be made is reported, with
     * the step that failed, and its comparisons say so. */
    {
        const WCHAR *failure;
        c.limited = candidate_token(base, user_sid, c.user, build_one_sid, c.b1, &limited_shape,
                                    L"candidate-limited-token", &failure);
        c.limited_two = candidate_token(base, user_sid, c.user, build_two_sid, b2, &limited_shape,
                                        L"candidate-limited-two-token", &failure);
        c.write_restricted = candidate_token(base, user_sid, c.user, build_one_sid, c.b1, &write_restricted_shape,
                                             L"candidate-write-restricted-token", &failure);
        c.write_restricted_two = candidate_token(base, user_sid, c.user, build_two_sid, b2, &write_restricted_shape,
                                                 L"candidate-write-restricted-two-token", &failure);
        c.write_restricted_everyone =
            candidate_token(base, user_sid, c.user, build_one_sid, c.b1, &write_restricted_everyone_shape,
                            L"candidate-wr-everyone-token", &failure);
        c.write_restricted_everyone_two =
            candidate_token(base, user_sid, c.user, build_two_sid, b2, &write_restricted_everyone_shape,
                            L"candidate-wr-everyone-two-token", &failure);
        c.write_restricted_low = candidate_token(base, user_sid, c.user, build_two_sid, b2, &write_restricted_low_shape,
                                                 L"candidate-wr-low-token", &failure);
        c.write_restricted_deny_user =
            candidate_token(base, user_sid, c.user, build_two_sid, b2, &write_restricted_deny_user_shape,
                            L"candidate-wr-deny-user-token", &failure);
    }
    c.standard = standard_token(base, administrators, user_sid, c.user, &error);
    if (c.standard == NULL)
        info(L"context-standard-user", join(L"no stand-in token, ", error_text(error), END));
    info(L"token-build-object", object_sddl(c.build_one));
    info(L"token-build-bare-object", object_sddl(c.build_bare));
    if (c.standard != NULL)
        info(L"token-standard-object", object_sddl(c.standard));
    describe_objects(&c);

    /* The restricted token's checks, a candidate's: every line a
     * finding. */
    begin_pass(L"", CANDIDATE_PASS);
    info(L"pass", L"the restricted-token checks, S-1-5-12 and the build's SID restricting; every line a finding");
    c.builders[0] = RESTRICTED_SID;
    place(&c, root);
    if (lay_out(&c)) {
        check_pipe(&c);
        check_own_token(&c);
        check_owner(&c);
        check_store(&c);
        check_present_layout(&c);
        check_valid_path(&c);
        check_isolation(&c);
        /* A hostile entry stays in the store, which the user cannot
         * remove; on a desktop the probe leaves nothing it cannot
         * remove. */
        if (c.desktop)
            info(L"D-hostile", L"not tried: on a desktop the probe leaves nothing it cannot remove");
        else
            check_hostile_entry(&c);
        check_junction(&c);
        check_jobs(&c);
        check_tools(&c);
        check_user_directories(&c);
        check_exposure(&c);
        check_read_exposure(&c);
        check_standard_user(&c);
    }
    check_spawning(&c);
    end_pass();

    full_pass(&c, root, L"lim", L"the checks under the limited token: S-1-5-12, the build's SID, Users and Everyone "
                                L"restricting; every line a finding",
              CANDIDATE_PASS, c.limited, c.limited_two);
    full_pass(&c, root, L"wr", L"the checks under write-restricted tokens; every line a finding", CANDIDATE_PASS,
              c.write_restricted, c.write_restricted_two);
    full_pass(&c, root, L"wr-everyone",
              L"the checks under write-restricted tokens with Everyone restricting too; every line a finding",
              CANDIDATE_PASS, c.write_restricted_everyone, c.write_restricted_everyone_two);
    targeted_pass(&c, root, L"wr-low",
                  L"the write-restricted token at Low integrity, on what the plain one deletes and reads; every line a "
                  L"finding",
                  c.write_restricted, c.write_restricted_low);
    targeted_pass(&c, root, L"wr-deny-user",
                  L"the write-restricted token with the user's SID deny-only, on what the plain one deletes and reads; "
                  L"every line a finding",
                  c.write_restricted, c.write_restricted_deny_user);
    /* The design last, so that the accounts and their logon right exist
     * for as short a time as they can. */
    if (c.desktop) {
        info(L"build-account-setup", L"not run: on a desktop the probe makes no account");
    } else if (!elevated(base)) {
        info(L"build-account-setup", L"not run: this process is not elevated, and only an administrator makes "
                                     L"accounts");
    } else {
        const WCHAR *failure = make_accounts(&accounts, &c);
        if (failure != NULL) {
            verdict(0, REQUIRED, L"build-account-setup", L"make two build accounts and log each on", failure);
        } else {
            const struct candidate row = {L"account", L"a build account's own token", accounts.slot[0].token, NULL};
            c.account_one = accounts.slot[0].token;
            c.account_two = accounts.slot[1].token;
            account_pass(&c, root, &accounts);
            begin_pass(L"", CANDIDATE_PASS);
            spawning_row(&c, &row);
            end_pass();
            /* The Secondary Logon service grants each account the window
             * station and desktop it starts it on; what it leaves there. */
            describe_user_objects(L"after-accounts");
        }
    }

cleanup:
    remove_accounts(&accounts);
    {
        HANDLE tokens[] = {c.standard,
                           c.write_restricted_deny_user,
                           c.write_restricted_low,
                           c.write_restricted_everyone_two,
                           c.write_restricted_everyone,
                           c.write_restricted_two,
                           c.write_restricted,
                           c.limited_two,
                           c.limited,
                           c.build_bare,
                           c.build_two,
                           c.build_one};
        size_t i;
        for (i = 0; i < sizeof tokens / sizeof tokens[0]; i++)
            if (tokens[i] != NULL)
                CloseHandle(tokens[i]);
    }
    if (administrators != NULL)
        LocalFree(administrators);
    if (build_two_sid != NULL)
        LocalFree(build_two_sid);
    if (build_one_sid != NULL)
        LocalFree(build_one_sid);
    if (user_sid != NULL)
        LocalFree(user_sid);
    CloseHandle(base);
}

/* ------------------------------------------------------------------------
 * The stand-in helper, and the run command
 * ------------------------------------------------------------------------ */

/* This process's user, as a SID and as text, or NULL. */
static PSID own_user(HANDLE base, WCHAR **text)
{
    DWORD needed = 0;
    TOKEN_USER *token_user;
    GetTokenInformation(base, TokenUser, NULL, 0, &needed);
    token_user = take(needed);
    if (!GetTokenInformation(base, TokenUser, token_user, needed, &needed))
        return NULL;
    *text = sid_text(token_user->User.Sid);
    return sid_of(*text);
}

/* The build SID the stand-in helper's child runs with. */
#define HOLD_BUILD_SID L"S-1-9-1-3"

/* Start a restricted child, made as the helper would make a build, in a
 * job of its own nested in whatever job this process is in; report its
 * process ID; and wait, as the sandbox helper waits on a build. */
static DWORD hold(void)
{
    HANDLE base = NULL;
    HANDLE token = NULL;
    HANDLE job = NULL;
    PSID user = NULL;
    PSID build = sid_of(HOLD_BUILD_SID);
    WCHAR *user_text = NULL;
    DWORD error;
    STARTUPINFOW startup;
    PROCESS_INFORMATION process;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    WCHAR *command = join(L"\"", self_path, L"\" child sleep", END);
    DWORD status = 0;
    if (!OpenProcessToken(GetCurrentProcess(), BASE_TOKEN_ACCESS, &base)) {
        status = STEP_FAILED(1, GetLastError());
        goto done;
    }
    user = own_user(base, &user_text);
    if (user == NULL || build == NULL) {
        status = STEP_FAILED(1, GetLastError());
        goto done;
    }
    token = build_token(base, user, user_text, build, HOLD_BUILD_SID, &design_shape, &error);
    if (token == NULL) {
        status = STEP_FAILED(2, ERROR_OF(error));
        goto done;
    }
    job = CreateJobObjectW(NULL, NULL);
    memset(&limits, 0, sizeof limits);
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (job == NULL || !SetInformationJobObject(job, JobObjectExtendedLimitInformation, &limits, sizeof limits)) {
        status = STEP_FAILED(3, GetLastError());
        goto done;
    }
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    if (!CreateProcessAsUserW(token, NULL, command, NULL, NULL, FALSE, CREATE_SUSPENDED | DETACHED_PROCESS, NULL,
                              child_directory, &startup, &process)) {
        status = STEP_FAILED(4, GetLastError());
        goto done;
    }
    if (!AssignProcessToJobObject(job, process.hProcess)) {
        status = STEP_FAILED(5, GetLastError());
        TerminateProcess(process.hProcess, 1);
    } else {
        ResumeThread(process.hThread);
        say(join(L"pid ", number(process.dwProcessId, 0), END));
        WaitForSingleObject(process.hProcess, SLEEP_MS);
    }
    CloseHandle(process.hThread);
    CloseHandle(process.hProcess);
done:
    if (job != NULL)
        CloseHandle(job);
    if (token != NULL)
        CloseHandle(token);
    if (base != NULL)
        CloseHandle(base);
    if (user != NULL)
        LocalFree(user);
    if (build != NULL)
        LocalFree(build);
    return status;
}

/* How long the run command waits for the checks, and for a tree to end. */
#define CHECKS_TIMEOUT_MS 1500000
#define TREE_TIMEOUT_MS 10000
/* How long the process library waits on each process of a job in turn. */
#define POLL_MS 200
/* The most process IDs read from a job at once. */
#define PROCESS_LIST_MAX 64

/* Room for this many process IDs from a job at once. */
#define PROCESS_LIST_BYTES ((DWORD)(sizeof(JOBOBJECT_BASIC_PROCESS_ID_LIST) + sizeof(ULONG_PTR) * PROCESS_LIST_MAX))

/* Read a job's processes into LIST, which holds PROCESS_LIST_BYTES; a job
 * holding more gives the first of them.  Answers 0 or the error. */
static DWORD list_processes(HANDLE job, JOBOBJECT_BASIC_PROCESS_ID_LIST *list)
{
    if (!QueryInformationJobObject(job, JobObjectBasicProcessIdList, list, PROCESS_LIST_BYTES, NULL)
        && GetLastError() != ERROR_MORE_DATA)
        return GetLastError();
    return 0;
}

/* Open each of a job's processes with SYNCHRONIZE, as the process
 * library's waitForJobCompletion opens them to wait on, a process already
 * gone passed over as it passes it over.  Answers 0, or the first error
 * that would stop that wait. */
static DWORD open_each(const JOBOBJECT_BASIC_PROCESS_ID_LIST *list)
{
    DWORD i;
    for (i = 0; i < list->NumberOfProcessIdsInList; i++) {
        HANDLE process = OpenProcess(SYNCHRONIZE, FALSE, (DWORD)list->ProcessIdList[i]);
        DWORD error;
        if (process != NULL) {
            CloseHandle(process);
            continue;
        }
        error = GetLastError();
        if (error != ERROR_INVALID_PARAMETER && error != ERROR_INVALID_HANDLE)
            return error;
    }
    return 0;
}

/* Wait until a job holds no process, as the process library's
 * waitForJobCompletion waits: list its processes, open each with
 * SYNCHRONIZE and wait on it a while, and list again.  Answers 0 once the
 * job is empty, the error that would stop the library's wait, or
 * WAIT_TIMEOUT. */
static DWORD library_wait(HANDLE job, DWORD timeout_ms)
{
    ULONGLONG deadline = GetTickCount64() + timeout_ms;
    JOBOBJECT_BASIC_PROCESS_ID_LIST *list = take(PROCESS_LIST_BYTES);
    for (;;) {
        DWORD i;
        DWORD error = list_processes(job, list);
        if (error != 0)
            return error;
        if (list->NumberOfProcessIdsInList == 0)
            return 0;
        for (i = 0; i < list->NumberOfProcessIdsInList; i++) {
            HANDLE process = OpenProcess(SYNCHRONIZE, FALSE, (DWORD)list->ProcessIdList[i]);
            if (process == NULL) {
                error = GetLastError();
                if (error != ERROR_INVALID_PARAMETER && error != ERROR_INVALID_HANDLE)
                    return error;
                continue;
            }
            WaitForSingleObject(process, POLL_MS);
            CloseHandle(process);
        }
        if (GetTickCount64() > deadline)
            return WAIT_TIMEOUT;
    }
}

/* A job like the process library's for a spawned process: it kills its
 * tree when closed and sets nothing else. */
static HANDLE library_job(BOOL inheritable)
{
    SECURITY_ATTRIBUTES attributes;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    HANDLE job;
    attributes.nLength = sizeof attributes;
    attributes.lpSecurityDescriptor = NULL;
    attributes.bInheritHandle = inheritable;
    job = CreateJobObjectW(&attributes, NULL);
    if (job == NULL)
        return NULL;
    memset(&limits, 0, sizeof limits);
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation, &limits, sizeof limits)) {
        CloseHandle(job);
        return NULL;
    }
    return job;
}

/* End the whole tree from the outer job: a stand-in helper in a job like
 * the process library's starts a restricted child in a job of its own;
 * terminating the outer job must end the child too, and the outer job
 * must then hold no process. */
static void tree_checks(void)
{
    HANDLE job = library_job(FALSE);
    SECURITY_ATTRIBUTES inheritable;
    HANDLE read_end = NULL;
    HANDLE write_end = NULL;
    STARTUPINFOW startup;
    PROCESS_INFORMATION process;
    struct run collected;
    HANDLE grandchild = NULL;
    DWORD pid = 0;
    ULONGLONG deadline;
    memset(&process, 0, sizeof process);
    memset(&collected, 0, sizeof collected);
    collected.output = take(OUTPUT_MAX);
    inheritable.nLength = sizeof inheritable;
    inheritable.lpSecurityDescriptor = NULL;
    inheritable.bInheritHandle = TRUE;
    if (job == NULL || !CreatePipe(&read_end, &write_end, &inheritable, 0)) {
        verdict(0, REQUIRED, L"jobs-terminate-tree", L"set up the outer job and its pipe", error_text(GetLastError()));
        goto done;
    }
    SetHandleInformation(read_end, HANDLE_FLAG_INHERIT, 0);
    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    startup.dwFlags = STARTF_USESTDHANDLES;
    startup.hStdInput = NULL;
    startup.hStdOutput = write_end;
    startup.hStdError = write_end;
    if (!CreateProcessW(NULL, join(L"\"", self_path, L"\" hold", END), NULL, NULL, TRUE, CREATE_SUSPENDED, NULL,
                        child_directory, &startup, &process)) {
        verdict(0, REQUIRED, L"jobs-terminate-tree", L"start the stand-in helper", error_text(GetLastError()));
        goto done;
    }
    if (!AssignProcessToJobObject(job, process.hProcess)) {
        verdict(0, REQUIRED, L"jobs-terminate-tree", L"assign the stand-in helper to the outer job",
                error_text(GetLastError()));
        TerminateProcess(process.hProcess, 1);
        goto done;
    }
    CloseHandle(write_end);
    write_end = NULL;
    ResumeThread(process.hThread);
    deadline = GetTickCount64() + TREE_TIMEOUT_MS * 3;
    while (GetTickCount64() < deadline) {
        const char *at;
        read_available(read_end, &collected);
        collected.output[collected.output_length < OUTPUT_MAX ? collected.output_length : OUTPUT_MAX - 1] = 0;
        /* Only a whole line counts, so a number still arriving is not
         * taken for the process ID. */
        for (at = collected.output; *at != 0; at++)
            if (starts_with(at, "pid ")) {
                const char *digit = at + 4;
                DWORD read_pid = 0;
                while (*digit >= '0' && *digit <= '9')
                    read_pid = read_pid * 10 + (DWORD)(*digit++ - '0');
                if (*digit == '\n')
                    pid = read_pid;
                break;
            }
        if (pid != 0 || WaitForSingleObject(process.hProcess, POLL_MS) == WAIT_OBJECT_0)
            break;
    }
    if (pid == 0) {
        struct run helper;
        memset(&helper, 0, sizeof helper);
        if (WaitForSingleObject(process.hProcess, 0) == WAIT_OBJECT_0)
            GetExitCodeProcess(process.hProcess, &helper.exit_code);
        else
            helper.timed_out = 1;
        verdict(0, REQUIRED, L"jobs-terminate-tree", L"the stand-in helper reports its restricted child",
                join(L"the helper ", describe(&helper), L", wrote \"", widen(collected.output, collected.output_length),
                     L"\"", END));
        goto done;
    }
    grandchild = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_LIMITED_INFORMATION | READ_CONTROL, FALSE, pid);
    if (grandchild == NULL) {
        verdict(0, REQUIRED, L"jobs-terminate-tree", L"open the restricted child", error_text(GetLastError()));
        goto done;
    }
    info(L"jobs-child-process-object", object_sddl(grandchild));
    {
        /* While the restricted child lives: the process library's wait
         * opens every process in the job with SYNCHRONIZE, and fails on
         * any it is refused. */
        JOBOBJECT_BASIC_PROCESS_ID_LIST *list = take(PROCESS_LIST_BYTES);
        DWORD error = list_processes(job, list);
        if (error == 0)
            error = open_each(list);
        verdict(error == 0 && list->NumberOfProcessIdsInList >= 2, REQUIRED, L"jobs-library-wait-open",
                L"the process library's wait can open every process of the outer job, the restricted child's included",
                join(number(list->NumberOfProcessIdsInList, 0), L" processes", error != 0 ? L", " : L"",
                     error != 0 ? error_text(error) : L"", END));
    }
    if (!TerminateJobObject(job, 1)) {
        verdict(0, REQUIRED, L"jobs-terminate-tree", L"terminate the outer job", error_text(GetLastError()));
        goto done;
    }
    verdict(WaitForSingleObject(grandchild, TREE_TIMEOUT_MS) == WAIT_OBJECT_0, REQUIRED, L"jobs-terminate-tree",
            L"terminating the outer job ends the restricted child in the job nested in it", NULL);
    {
        DWORD waited = library_wait(job, TREE_TIMEOUT_MS);
        verdict(waited == 0, REQUIRED, L"jobs-drain",
                L"the outer job then holds no process, and a wait on it as the process library's waits returns",
                waited != 0 ? error_text(waited) : NULL);
    }
done:
    if (grandchild != NULL)
        CloseHandle(grandchild);
    if (process.hThread != NULL)
        CloseHandle(process.hThread);
    if (process.hProcess != NULL)
        CloseHandle(process.hProcess);
    if (write_end != NULL)
        CloseHandle(write_end);
    if (read_end != NULL)
        CloseHandle(read_end);
    if (job != NULL)
        CloseHandle(job);
}

/* Run the checks in a job like the process library's, as nova-nix would
 * run the sandbox helper, then the tree checks.  Answers the required
 * failures of both. */
static DWORD run(const WCHAR *root, const WCHAR *bash, const WCHAR *gcc, const WCHAR *option)
{
    HANDLE job = library_job(TRUE);
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    HANDLE err = GetStdHandle(STD_ERROR_HANDLE);
    HANDLE inherited[3];
    DWORD inherited_count = 0;
    STARTUPINFOEXW startup;
    SIZE_T list_size = 0;
    LPPROC_THREAD_ATTRIBUTE_LIST list = NULL;
    PROCESS_INFORMATION process;
    DWORD checks_failures = 0;
    BOOL started;
    memset(&process, 0, sizeof process);
    if (job == NULL) {
        verdict(0, REQUIRED, L"setup-outer-job", L"make the checks' job", error_text(GetLastError()));
        return required_failures;
    }
    /* The checks inherit their job and the standard handles alone, so
     * that nothing else this process holds can pass on to a build. */
    inherited[inherited_count++] = job;
    if (out != NULL && out != INVALID_HANDLE_VALUE) {
        SetHandleInformation(out, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
        inherited[inherited_count++] = out;
    }
    if (err != NULL && err != INVALID_HANDLE_VALUE && err != out) {
        SetHandleInformation(err, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
        inherited[inherited_count++] = err;
    }
    memset(&startup, 0, sizeof startup);
    startup.StartupInfo.cb = sizeof startup;
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdInput = NULL;
    startup.StartupInfo.hStdOutput = out;
    startup.StartupInfo.hStdError = err;
    InitializeProcThreadAttributeList(NULL, 1, 0, &list_size);
    list = take(list_size);
    if (!InitializeProcThreadAttributeList(list, 1, 0, &list_size)) {
        verdict(0, REQUIRED, L"setup-checks", L"make the checks' attribute list", error_text(GetLastError()));
        CloseHandle(job);
        return required_failures;
    }
    startup.lpAttributeList = list;
    started = UpdateProcThreadAttribute(list, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherited,
                                        inherited_count * sizeof inherited[0], NULL, NULL)
              && CreateProcessW(NULL,
                                join(L"\"", self_path, L"\" checks \"", root, L"\" \"", bash, L"\" \"", gcc,
                                     L"\" ", number((ULONG_PTR)job, 1), option != NULL ? L" " : L"",
                                     option != NULL ? option : L"", END),
                                NULL, NULL, TRUE, CREATE_SUSPENDED | EXTENDED_STARTUPINFO_PRESENT, NULL,
                                child_directory, &startup.StartupInfo, &process);
    DeleteProcThreadAttributeList(list);
    if (!started) {
        verdict(0, REQUIRED, L"setup-checks", L"start the checks", error_text(GetLastError()));
        CloseHandle(job);
        return required_failures;
    }
    if (!AssignProcessToJobObject(job, process.hProcess)) {
        verdict(0, REQUIRED, L"setup-checks", L"assign the checks to their job", error_text(GetLastError()));
        TerminateProcess(process.hProcess, 1);
    } else {
        ResumeThread(process.hThread);
        DWORD waited = library_wait(job, CHECKS_TIMEOUT_MS);
        if (waited != 0) {
            verdict(0, REQUIRED, L"setup-checks", L"the checks finish, waited for as the process library waits",
                    error_text(waited));
            TerminateJobObject(job, 1);
        }
        WaitForSingleObject(process.hProcess, SETTLE_MS);
        GetExitCodeProcess(process.hProcess, &checks_failures);
        /* A crash's status is not a count of failures. */
        if (checks_failures >= NTSTATUS_FLOOR) {
            verdict(0, REQUIRED, L"setup-checks", L"the checks end on their own",
                    join(L"they ended with status ", number(checks_failures, 1), END));
            checks_failures = 0;
        }
    }
    CloseHandle(process.hThread);
    CloseHandle(process.hProcess);
    CloseHandle(job);
    tree_checks();
    say(join(L"SUMMARY required failures: ", number(checks_failures + required_failures, 0), END));
    return checks_failures + required_failures;
}

/* The build SID the trace command's token is made with. */
#define TRACE_BUILD_SID L"S-1-9-1-4"

/* Run one thing under a token made as the design makes a build's, for the
 * workflow's trace of what the token is refused, and say how it ended.
 * STEP is control, a child reading kernel32.dll as a file, which the
 * token is refused, so that the trace shows it records a refusal;
 * spawn-probe or spawn-cmd, a child starting the probe or cmd.exe with
 * CreateProcessW; or bash or gcc, MSYS2's bash running "exit 0" or gcc
 * preprocessing an empty file, which starts cc1.exe.  Answers 0 when the
 * thing was run, whatever came of it. */
static DWORD trace(const WCHAR *step, const WCHAR *bash, const WCHAR *gcc)
{
    HANDLE base = NULL;
    HANDLE token = NULL;
    PSID user = NULL;
    PSID build = NULL;
    WCHAR *user_text = NULL;
    WCHAR *command = NULL;
    BOOL in_job = FALSE;
    DWORD error;
    DWORD status = 0;
    int ours = 0;
    struct run ran;
    if (wide_equal(step, L"control")) {
        WCHAR *system = take(MAX_PATH * sizeof(WCHAR));
        if (GetSystemDirectoryW(system, MAX_PATH) != 0)
            command = join(L"\"", self_path, L"\" child open \"", system, L"\\kernel32.dll\" ", MASK_READ, END);
        ours = 1;
    } else if (wide_equal(step, L"spawn-probe") || wide_equal(step, L"spawn-cmd")) {
        command = join(L"\"", self_path, L"\" child spawn ", wide_equal(step, L"spawn-cmd") ? L"cmd" : L"probe", L" ",
                       FLAGS_DETACHED, END);
        ours = 1;
    } else if (wide_equal(step, L"bash") && !wide_equal(bash, L"-")) {
        command = join(L"\"", bash, L"\" -c \"exit 0\"", END);
    } else if (wide_equal(step, L"gcc") && !wide_equal(gcc, L"-")) {
        command = join(L"\"", gcc, L"\" -E -x c NUL", END);
    }
    if (command == NULL) {
        say(join(L"TRACE ", step, L": nothing to run", END));
        return EXIT_USAGE;
    }
    if (!OpenProcessToken(GetCurrentProcess(), BASE_TOKEN_ACCESS, &base) || (user = own_user(base, &user_text)) == NULL
        || (build = sid_of(TRACE_BUILD_SID)) == NULL) {
        say(join(L"TRACE ", step, L": no token, ", error_text(GetLastError()), END));
        status = 1;
        goto done;
    }
    token = build_token(base, user, user_text, build, TRACE_BUILD_SID, &design_shape, &error);
    if (token == NULL) {
        say(join(L"TRACE ", step, L": no token, ", token_failure(error), END));
        status = 1;
        goto done;
    }
    IsProcessInJob(GetCurrentProcess(), NULL, &in_job);
    set_tool_path(parent_directory(child_directory), environment(L"SystemRoot"));
    spawn(token, command, child_directory, TOOL_MEMORY_CAP, TOOL_TIMEOUT_MS, SPAWN_DETACHED, &ran);
    say(join(L"TRACE ", step, L": ", ours ? describe(&ran) : describe_program(&ran), L", wrote \"",
             widen(ran.output, ran.output_length), L"\"; the trace ran ", in_job ? L"in a job" : L"in no job", END));
done:
    if (token != NULL)
        CloseHandle(token);
    if (base != NULL)
        CloseHandle(base);
    if (user != NULL)
        LocalFree(user);
    if (build != NULL)
        LocalFree(build);
    return status;
}

/* ------------------------------------------------------------------------
 * The entry point
 * ------------------------------------------------------------------------ */

/* The command line split as CommandLineToArgvW splits a plain one: at
 * spaces, with double quotes around an argument that holds any.  The
 * probe passes no argument holding a quote. */
static WCHAR **split_arguments(const WCHAR *line, int *count)
{
    size_t length = wide_length(line);
    WCHAR **arguments = take((length / 2 + 2) * sizeof(WCHAR *));
    WCHAR *copy = join(line, END);
    WCHAR *at = copy;
    *count = 0;
    for (;;) {
        WCHAR *start;
        while (*at == L' ' || *at == L'\t')
            at++;
        if (*at == 0)
            break;
        if (*at == L'"') {
            start = ++at;
            while (*at != 0 && *at != L'"')
                at++;
        } else {
            start = at;
            while (*at != 0 && *at != L' ' && *at != L'\t')
                at++;
        }
        arguments[(*count)++] = start;
        if (*at == 0)
            break;
        *at++ = 0;
    }
    return arguments;
}

/* No error box for a process that fails to load or crashes: the probe
 * starts many that do, and every process it starts inherits this. */
#define PROBE_ERROR_MODE (SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX | SEM_NOOPENFILEERRORBOX)

void probe_entry(void)
{
    int count;
    WCHAR **arguments;
    UINT status;
    SetErrorMode(PROBE_ERROR_MODE);
    arena = HeapCreate(0, 0, 0);
    if (arena == NULL)
        ExitProcess(EXIT_NO_MEMORY);
    arguments = split_arguments(GetCommandLineW(), &count);
    self_path = own_path();
    /* Children start in the directory holding the probe, which the
     * restricting SID may read. */
    child_directory = parent_directory(self_path);
    if (count >= 2 && wide_equal(arguments[1], L"child"))
        finish(child(count, arguments));
    if (count >= 2 && wide_equal(arguments[1], L"hold"))
        finish(hold());
    if (count >= 6 && wide_equal(arguments[1], L"checks")) {
        const WCHAR *option = count >= 7 ? arguments[6] : NULL;
        if (option != NULL && !wide_equal(option, WRITE_PROFILE) && !wide_equal(option, DESKTOP)) {
            say(join(L"unknown option: ", option, END));
            finish(EXIT_USAGE);
        }
        outer_job = (HANDLE)parse_address(arguments[5]);
        /* Inherited from run, and to be inherited by nothing: a build
         * started through the Secondary Logon service, which takes no
         * handle list, would otherwise receive them, the step's output
         * pipe among them. */
        SetHandleInformation(outer_job, HANDLE_FLAG_INHERIT, 0);
        SetHandleInformation(GetStdHandle(STD_OUTPUT_HANDLE), HANDLE_FLAG_INHERIT, 0);
        SetHandleInformation(GetStdHandle(STD_ERROR_HANDLE), HANDLE_FLAG_INHERIT, 0);
        checks(arguments[2], arguments[3], arguments[4], option);
        status = required_failures;
        say(join(L"CHECKS required failures: ", number(required_failures, 0), L", findings whose claim did not hold: ",
                 number(findings_failed, 0), END));
        finish(status);
    }
    if (count >= 5 && wide_equal(arguments[1], L"run")) {
        const WCHAR *option = count >= 6 ? arguments[5] : NULL;
        if (option != NULL && !wide_equal(option, WRITE_PROFILE) && !wide_equal(option, DESKTOP)) {
            say(join(L"unknown option: ", option, END));
            finish(EXIT_USAGE);
        }
        finish(run(arguments[2], arguments[3], arguments[4], option));
    }
    if (count >= 5 && wide_equal(arguments[1], L"trace"))
        finish(trace(arguments[2], arguments[3], arguments[4]));
    say(join(L"usage: probe run ROOT BASH GCC [" WRITE_PROFILE L" | " DESKTOP L"], or probe trace STEP BASH GCC", END));
    finish(EXIT_USAGE);
}

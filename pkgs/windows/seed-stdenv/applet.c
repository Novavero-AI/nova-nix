/*
 * One of busybox-w32's applets as a command of its own.  A copy of this
 * program named for an applet starts the busybox.exe beside it with the
 * command line it was given, unchanged, so busybox runs the applet that
 * line's first word names, and exits with busybox's status.  w64devkit
 * installs busybox-w32's applets the same way (src/busybox-alias.c at
 * 667d0d3); busybox --install makes hard links instead, which a store
 * path's NAR holds as one copy of busybox.exe each.
 */
#include <string.h>
#include <windows.h>

/* The status a shell gives a command it found and could not run. */
#define CANNOT_RUN 126

static const wchar_t busybox[] = L"busybox.exe";

/* busybox shares this console and takes Ctrl-C itself; the launcher waits
   for its status rather than exiting first. */
static BOOL WINAPI ignore_interrupt(DWORD event)
{
    (void)event;
    return TRUE;
}

static void fail(const char *message)
{
    DWORD written;

    WriteFile(GetStdHandle(STD_ERROR_HANDLE), message, (DWORD)strlen(message), &written, NULL);
    ExitProcess(CANNOT_RUN);
}

int main(void)
{
    wchar_t path[MAX_PATH];
    DWORD length = GetModuleFileNameW(NULL, path, MAX_PATH);
    STARTUPINFOW startup;
    PROCESS_INFORMATION child;
    DWORD status;

    if (length == 0 || length == MAX_PATH)
        fail("applet: cannot read the launcher's own path\n");
    while (length > 0 && path[length - 1] != L'\\' && path[length - 1] != L'/')
        length--;
    if (length + sizeof busybox / sizeof busybox[0] > MAX_PATH)
        fail("applet: busybox.exe's path is too long\n");
    memcpy(path + length, busybox, sizeof busybox);

    memset(&startup, 0, sizeof startup);
    startup.cb = sizeof startup;
    SetConsoleCtrlHandler(ignore_interrupt, TRUE);
    if (!CreateProcessW(path, GetCommandLineW(), NULL, NULL, TRUE, 0, NULL, NULL, &startup, &child))
        fail("applet: cannot start busybox.exe\n");
    CloseHandle(child.hThread);
    WaitForSingleObject(child.hProcess, INFINITE);
    if (!GetExitCodeProcess(child.hProcess, &status))
        fail("applet: cannot read busybox.exe's exit status\n");
    CloseHandle(child.hProcess);
    ExitProcess(status);
}

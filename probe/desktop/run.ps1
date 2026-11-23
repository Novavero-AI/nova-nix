# Runs the sandbox probe's desktop mode under ROOT at below-normal
# priority, which the probe's own processes inherit, and writes under ROOT
# the probe's log and a description of the logon it ran under.  ROOT holds
# probe.exe on entry.  The script's own writes are all under ROOT; the
# probe's are too, in desktop mode.
param(
    [Parameter(Mandatory)]
    [string] $Root
)

$ErrorActionPreference = 'Stop'
[System.Diagnostics.Process]::GetCurrentProcess().PriorityClass = 'BelowNormal'
$Root = (Resolve-Path -LiteralPath $Root).ProviderPath

# The probe sits in ROOT\tools\bin, which grants read and execute to
# S-1-5-12, to Users and to this user, so that each token the probe makes
# can start it; the probe lays out the rest of ROOT itself.
$tools = Join-Path $Root 'tools'
$bin = Join-Path $tools 'bin'
$probe = Join-Path $bin 'probe.exe'
New-Item -ItemType Directory -Path $bin | Out-Null
Move-Item -LiteralPath (Join-Path $Root 'probe.exe') -Destination $probe
$user = [System.Security.Principal.WindowsIdentity]::GetCurrent().User.Value
icacls $tools /grant '*S-1-5-12:(OI)(CI)RX' '*S-1-5-32-545:(OI)(CI)RX' "*${user}:(OI)(CI)RX" | Out-Null
if ($LASTEXITCODE -ne 0) { throw "icacls failed with $LASTEXITCODE" }

# Read-only: the system, the logon, and the ACLs of two System32 DLLs.
$context = Join-Path $Root 'context.log'
& {
    [System.Environment]::OSVersion.VersionString
    (Get-ItemProperty 'HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion').DisplayVersion
    whoami /user /groups /priv
    icacls (Join-Path $env:SystemRoot 'System32\cryptbase.dll')
    icacls (Join-Path $env:SystemRoot 'System32\kernel32.dll')
} | Out-File -LiteralPath $context -Encoding utf8

$log = Join-Path $Root 'probe.log'
& $probe run $Root - - desktop | Out-File -LiteralPath $log -Encoding utf8
$status = $LASTEXITCODE
"probe exit status $status" | Out-File -LiteralPath $log -Encoding utf8 -Append
"probe exit status $status; its log is $log"
exit $status

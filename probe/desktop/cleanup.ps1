# Removes everything under ROOT but this script, which the next step
# removes with ROOT, and lists whatever is left.  ROOT must be a
# sandbox-probe-N directory, so that a wrong argument removes nothing
# else.  A junction at ROOT's top is removed as a link.
param(
    [Parameter(Mandatory)]
    [string] $Root
)

$ErrorActionPreference = 'Stop'
$Root = (Resolve-Path -LiteralPath $Root).ProviderPath
if ((Split-Path -Leaf $Root) -notmatch '^sandbox-probe-\d+$') {
    throw "refusing to clean $Root, which is not a sandbox-probe-N directory"
}

$failed = @()
foreach ($item in Get-ChildItem -LiteralPath $Root -Force) {
    if ($item.FullName -eq $PSCommandPath) { continue }
    try {
        if ($item.Attributes -band [System.IO.FileAttributes]::ReparsePoint) {
            [System.IO.Directory]::Delete($item.FullName)
        } elseif ($item.PSIsContainer) {
            # The one junction the probe makes leads under ROOT, and the
            # probe removes it itself.
            cmd /c rmdir /s /q $item.FullName
        } else {
            Remove-Item -LiteralPath $item.FullName -Force
        }
    } catch {
        $failed += "could not remove $($item.FullName): $($_.Exception.Message)"
    }
}

# What could not be removed or listed is reported with what is left.
$left = @(Get-ChildItem -LiteralPath $Root -Force -Recurse -ErrorAction SilentlyContinue -ErrorVariable unlisted |
          Where-Object { $_.FullName -ne $PSCommandPath })
if ($left.Count -eq 0 -and $unlisted.Count -eq 0 -and $failed.Count -eq 0) {
    "nothing but this script is left under $Root"
} else {
    "left under ${Root}:"
    $left | ForEach-Object { $_.FullName }
    $failed
    $unlisted | ForEach-Object { "could not list: $($_.TargetObject): $($_.Exception.Message)" }
    exit 1
}

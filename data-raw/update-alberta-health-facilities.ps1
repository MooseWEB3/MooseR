param(
    [string]$OutputPath = (Join-Path $PSScriptRoot "..\inst\extdata\alberta-health-facilities.txt")
)

$ErrorActionPreference = "Stop"
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

$sourceUrl = "https://www.albertahealthservices.ca/findhealth/search.aspx"
$html = (Invoke-WebRequest -UseBasicParsing -Uri $sourceUrl).Content
$select = [regex]::Match(
    $html,
    '(?is)<select\b[^>]*id="MainPlaceHolder_FacilityNameDropDownList"[^>]*>.*?</select>'
)

if (-not $select.Success) {
    throw "The AHS facility-name list was not found."
}

$optionMatches = [regex]::Matches(
    $select.Value,
    '(?is)<option\b[^>]*value="([^"]*)"[^>]*>.*?</option>'
)
$facilities = foreach ($option in $optionMatches) {
    [System.Net.WebUtility]::HtmlDecode($option.Groups[1].Value).Trim()
}
$facilities = @($facilities | Where-Object { $_ } | Select-Object -Unique)

if ($facilities.Count -lt 1000) {
    throw "Only $($facilities.Count) facilities were found; refusing to replace the registry."
}

$outputDirectory = Split-Path -Parent $OutputPath
New-Item -ItemType Directory -Force $outputDirectory | Out-Null
[System.IO.File]::WriteAllLines(
    [System.IO.Path]::GetFullPath($OutputPath),
    $facilities,
    [System.Text.UTF8Encoding]::new($false)
)

Write-Output "Saved $($facilities.Count) AHS facility names to $OutputPath"

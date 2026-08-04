param(
    [string]$OutputPath = (Join-Path $PSScriptRoot "..\inst\extdata\alberta-schools.txt")
)

$ErrorActionPreference = "Stop"
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

$sourceUrl = "https://education.alberta.ca/media/1626669/authority_and_school.xlsx"
$tempFile = [System.IO.Path]::GetTempFileName()

try {
    Invoke-WebRequest -UseBasicParsing -Uri $sourceUrl -OutFile $tempFile

    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $archive = [System.IO.Compression.ZipFile]::OpenRead($tempFile)

    try {
        function Read-ZipXml([string]$EntryName, [bool]$Required = $true) {
            $entry = $archive.GetEntry($EntryName)

            if ($null -eq $entry) {
                if ($Required) {
                    throw "The workbook entry '$EntryName' was not found."
                }

                return $null
            }

            $reader = [System.IO.StreamReader]::new($entry.Open())

            try {
                return [xml]$reader.ReadToEnd()
            }
            finally {
                $reader.Dispose()
            }
        }

        $sharedStringsXml = Read-ZipXml "xl/sharedStrings.xml" $false
        $sharedStrings = @()

        if ($null -ne $sharedStringsXml) {
            $sharedStrings = @(
                foreach ($item in $sharedStringsXml.SelectNodes(
                    "//*[local-name()='si']"
                )) {
                    $textNodes = $item.SelectNodes(".//*[local-name()='t']")
                    ($textNodes | ForEach-Object { $_.InnerText }) -join ""
                }
            )
        }
        $sheetXml = Read-ZipXml "xl/worksheets/sheet1.xml"
        $names = foreach ($row in $sheetXml.SelectNodes(
            "//*[local-name()='sheetData']/*[local-name()='row']"
        )) {
            if ([int]$row.GetAttribute("r") -lt 4) {
                continue
            }

            $cell = $row.SelectSingleNode("./*[local-name()='c'][starts-with(@r, 'B')]")

            if ($null -eq $cell) {
                continue
            }

            $cellType = $cell.GetAttribute("t")
            $value = if ($cellType -eq "inlineStr") {
                $textNodes = $cell.SelectNodes(".//*[local-name()='t']")
                ($textNodes | ForEach-Object { $_.InnerText }) -join ""
            }
            else {
                $valueNode = $cell.SelectSingleNode("./*[local-name()='v']")

                if ($null -eq $valueNode) {
                    continue
                }

                if ($cellType -eq "s") {
                    $sharedStrings[[int]$valueNode.InnerText]
                }
                else {
                    $valueNode.InnerText
                }
            }

            $value.Trim()
        }
    }
    finally {
        $archive.Dispose()
    }
}
finally {
    Remove-Item -LiteralPath $tempFile -Force -ErrorAction SilentlyContinue
}

$seen = [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase
)
$uniqueNames = foreach ($name in $names) {
    if ($name -and $seen.Add($name)) {
        $name
    }
}

if ($uniqueNames.Count -lt 2500) {
    throw "Only $($uniqueNames.Count) school names were found; refusing to replace the registry."
}

$outputDirectory = Split-Path -Parent $OutputPath
New-Item -ItemType Directory -Force $outputDirectory | Out-Null
[System.IO.File]::WriteAllLines(
    [System.IO.Path]::GetFullPath($OutputPath),
    $uniqueNames,
    [System.Text.UTF8Encoding]::new($false)
)

Write-Output "Saved $($uniqueNames.Count) Alberta school names to $OutputPath"

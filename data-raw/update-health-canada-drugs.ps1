param(
    [string]$OutputPath = (Join-Path $PSScriptRoot "..\inst\extdata\health-canada-drugs.txt")
)

$ErrorActionPreference = "Stop"
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

$baseUrl = "https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database"
$files = @(
    "drug.zip",
    "drug_ap.zip",
    "ingred.zip",
    "ingred_ap.zip"
)
$drugHeaders = @(
    "DRUG_CODE",
    "PRODUCT_CATEGORIZATION",
    "CLASS",
    "DIN",
    "BRAND_NAME",
    "DESCRIPTOR",
    "PEDIATRIC_FLAG",
    "ACCESSION_NUMBER",
    "NUMBER_OF_AIS",
    "LAST_UPDATE_DATE",
    "AI_GROUP_NO",
    "CLASS_F",
    "BRAND_NAME_F",
    "DESCRIPTOR_F"
)
$ingredientHeaders = @(
    "DRUG_CODE",
    "ACTIVE_INGREDIENT_CODE",
    "INGREDIENT",
    "INGREDIENT_SUPPLIED_IND",
    "STRENGTH",
    "STRENGTH_UNIT",
    "STRENGTH_TYPE",
    "DOSAGE_VALUE",
    "BASE",
    "DOSAGE_UNIT",
    "NOTES",
    "INGREDIENT_F",
    "STRENGTH_UNIT_F",
    "STRENGTH_TYPE_F",
    "DOSAGE_UNIT_F"
)

$tempDirectory = Join-Path (
    [System.IO.Path]::GetTempPath()
) ("MooseR-dpd-" + [guid]::NewGuid().ToString("N"))
New-Item -ItemType Directory -Path $tempDirectory -Force | Out-Null

try {
    foreach ($file in $files) {
        $archive = Join-Path $tempDirectory $file
        & curl.exe -L --fail --retry 3 --silent --show-error `
            --output $archive "$baseUrl/$file"
        if ($LASTEXITCODE -ne 0) {
            throw "Download failed for $file with curl exit code $LASTEXITCODE."
        }
        Expand-Archive -LiteralPath $archive -DestinationPath (
            Join-Path $tempDirectory ([System.IO.Path]::GetFileNameWithoutExtension($file))
        ) -Force
    }

    $drugs = @(
        (Import-Csv (Join-Path $tempDirectory "drug\drug.txt") -Header $drugHeaders) +
        (Import-Csv (Join-Path $tempDirectory "drug_ap\drug_ap.txt") -Header $drugHeaders)
    ) | Where-Object { $_.CLASS -eq "Human" }

    $humanDrugCodes = [System.Collections.Generic.HashSet[string]]::new(
        [System.StringComparer]::OrdinalIgnoreCase
    )
    foreach ($drug in $drugs) {
        [void]$humanDrugCodes.Add($drug.DRUG_CODE)
    }

    $ingredients = @(
        (Import-Csv (Join-Path $tempDirectory "ingred\ingred.txt") -Header $ingredientHeaders) +
        (Import-Csv (Join-Path $tempDirectory "ingred_ap\ingred_ap.txt") -Header $ingredientHeaders)
    ) | Where-Object { $humanDrugCodes.Contains($_.DRUG_CODE) }

    $names = @(
        $drugs.BRAND_NAME
        $drugs.BRAND_NAME_F
        $ingredients.INGREDIENT
        $ingredients.INGREDIENT_F
    ) | ForEach-Object {
        if ($null -ne $_) {
            ([regex]::Replace($_.Trim(), "\s+", " "))
        }
    } | Where-Object {
        $_ -and [regex]::IsMatch($_, "\p{L}")
    } | Sort-Object -Unique

    if ($names.Count -lt 9000) {
        throw "Only $($names.Count) human drug names were found; refusing to replace the registry."
    }

    $outputDirectory = Split-Path -Parent $OutputPath
    New-Item -ItemType Directory -Force $outputDirectory | Out-Null
    [System.IO.File]::WriteAllLines(
        [System.IO.Path]::GetFullPath($OutputPath),
        $names,
        [System.Text.UTF8Encoding]::new($false)
    )

    Write-Output (
        "Saved $($names.Count) marketed or approved human drug brand and " +
        "active-ingredient names to $OutputPath"
    )
}
finally {
    if (Test-Path -LiteralPath $tempDirectory) {
        Remove-Item -LiteralPath $tempDirectory -Recurse -Force
    }
}

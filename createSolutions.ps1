$rootDirectory = "C:\code\advent2023"

1..24 | ForEach-Object {
    $dayFolderName = "day_$_"
    New-Item -Path $rootDirectory -Name "${dayFolderName}" -ItemType Directory
    $srcFolderPath = Join-Path -Path $rootDirectory -ChildPath $dayFolderName
    cd $srcFolderPath 
    dotnet new xunit -lang F#
    New-Item -Path $srcFolderPath -Name "input1.txt" -ItemType File
    New-Item -Path $srcFolderPath -Name "input2.txt" -ItemType File
} 

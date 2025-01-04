#!/usr/bin/env pwsh
##############################################################################################################

Function Show-Usage {
    "
vagrant  = 'it-gro/win10-ltsc-eval'
download = 'https://microsoft.com/en-us/evalcenter'
package  = 'https://learn.microsoft.com/en-us/mem/configmgr/develop/apps/how-to-create-the-windows-installer-file-msi'
shell    = 'https://learn.microsoft.com/en-us/powershell'

Usage: pwsh -File $($PSCommandPath) [OPTIONS]
Options:
    build
    lint
" | Out-Host
}

Function Build-Project {
    New-Variable -Option Constant -Name VAR -Value @{
        Src = 'Examples'
        Use = 'Package'
        Pkg = 'Package\components.txt'
    }
    If (! (Test-Path -Path $Var.Src)) {
        "$([char]27)[31m.... Source do not find!$([char]27)[0m" | Out-Host
        Exit 1
    }
    If (Test-Path -Path '.gitmodules') {
        & git submodule update --init --recursive --force --remote | Out-Host
        "$([char]27)[32m.... [[$($LastExitCode)]] git submodule update$([char]27)[0m" | Out-Host
    }
    @(
        @{
            Cmd = 'lazbuild'
            Url = 'https://fossies.org/windows/misc/lazarus-3.6-fpc-3.2.2-win64.exe'
            Path = "C:\Lazarus"
        }
    ) | Where-Object { ! (Test-Path -Path $_.Path) } |
        ForEach-Object {
            $_.Url | Request-File | Install-Program
            $Env:PATH+=";$($_.Path)"
            (Get-Command $_.Cmd).Source | Out-Host
        }
    If (Test-Path -Path $VAR.Use) {
        If (Test-Path -Path $VAR.Pkg) {
            Get-Content -Path $VAR.Pkg |
                Where-Object {
                    ! (Test-Path -Path "$($VAR.Use)\$($_)") &&
                    ! (& lazbuild --verbose-pkgsearch $_ ) &&
                    ! (& lazbuild --add-package $_)
                } | ForEach-Object {
                    Return @{
                        Uri = "https://packages.lazarus-ide.org/$($_).zip"
                        Path = "$($VAR.Use)\$($_)"
                        OutFile = (New-TemporaryFile).FullName
                    }
                } | ForEach-Object -Parallel {
                    Invoke-WebRequest -OutFile $_.OutFile -Uri $_.Uri
                    Expand-Archive -Path $_.OutFile -DestinationPath $_.Path
                    Remove-Item $_.OutFile
                    Return "$([char]27)[32m.... download $($_.Uri)$([char]27)[0m"
                } | Out-Host
        }
        (Get-ChildItem -Filter '*.lpk' -Recurse -File –Path $VAR.Use).FullName |
            ForEach-Object {
                & lazbuild --add-package-link $_ | Out-Null
                Return "$([char]27)[32m.... [$($LastExitCode)] add package link $($_)$([char]27)[0m"
            } | Out-Host
    }
    Exit (
        (Get-ChildItem -Filter '*.lpi' -Recurse -File –Path $Var.Src).FullName |
            Sort-Object |
            ForEach-Object {
                $Output = (& lazbuild --build-all --recursive --no-write-project --build-mode='release' $_)
                $Result = @("$([char]27)[32m.... [$($LastExitCode)] build project $($_)$([char]27)[0m")
                $exitCode = Switch ($LastExitCode) {
                    0 {
                        $Result += $Output | Select-String -Pattern 'Linking'
                        0
                    }
                    Default {
                        $Result += $Output | Select-String -Pattern 'Error:', 'Fatal:'
                        1
                    }
                }
                $Result | Out-Host
                Return $exitCode
            } | Measure-Object -Sum
    ).Sum
}

Function Request-File {
    While ($Input.MoveNext()) {
        New-Variable -Option Constant -Name VAR -Value @{
            Uri = $Input.Current
            OutFile = (Split-Path -Path $Input.Current -Leaf).Split('?')[0]
        }
        Invoke-WebRequest @VAR
        Return $VAR.OutFile
    }
}

Function Install-Program {
    While ($Input.MoveNext()) {
        Switch ((Split-Path -Path $Input.Current -Leaf).Split('.')[-1]) {
            'msi' {
                & msiexec /passive /package $Input.Current | Out-Null
            }
            Default {
                & ".\$($Input.Current)" /SP- /VERYSILENT /SUPPRESSMSGBOXES /NORESTART | Out-Null
            }
        }
        Remove-Item $Input.Current
    }
}

Function Request-URL([Switch] $Post) {
    $VAR = Switch ($Post) {
        True {
            Return @{
                Method = 'POST'
                Headers = @{
                    ContentType = 'application/json'
                }
                Uri = 'https://postman-echo.com/post'
                Body = @{
                    One = '1'
                } | ConvertTo-Json
            }
        }
        False {
            Return @{
                Uri = 'https://postman-echo.com/get'
            }
        }
    }
    Return (Invoke-WebRequest @VAR | ConvertFrom-Json).Headers
}

Function Switch-Action {
    $ErrorActionPreference = 'stop'
    Set-PSDebug -Strict #-Trace 1
    Invoke-ScriptAnalyzer -EnableExit -Path $PSCommandPath
    If ($args.count -gt 0) {
        Switch ($args[0]) {
            'lint' {
                Invoke-ScriptAnalyzer -EnableExit -Recurse -Path '.'
                (Get-ChildItem -Filter '*.ps1' -Recurse -Path '.').FullName |
                    ForEach-Object {
                        Invoke-Formatter -ScriptDefinition $(Get-Content -Path $_ | Out-String) |
                            Set-Content -Path $_
                    }
            }
            'build' {
                Build-Project
            }
            Default {
                Show-Usage
            }
        }
    } Else {
        Show-Usage
    }
}

##############################################################################################################
Switch-Action @args

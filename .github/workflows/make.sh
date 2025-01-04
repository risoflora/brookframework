#!/usr/bin/env bash
##############################################################################################################

function priv_clippit
(
    cat <<EOF
https://google.github.io/styleguide/shellguide.html
https://guide.bash.academy
https://devhints.io/bash
https://tldr.sh

Usage: bash ${0} [OPTIONS]
Options:
    build   Build program
EOF
)

function priv_lazbuild
(
    declare -rA VAR=(
        [src]='Examples'
        [use]='Package'
        [pkg]='Package/components.txt'
    )
    if ! [[ -d "${VAR[src]}" ]]; then
        printf '\x1b[31m\tSource do not find!\x1b[0m\n'
        exit 1
    fi
    if [[ -f '.gitmodules' ]]; then
        git submodule update --init --recursive --force --remote &
    fi
    if ! (command -v lazbuild); then
        source '/etc/os-release'
        case ${ID:?} in
            debian | ubuntu)
                sudo apt-get update
                sudo apt-get install -y lazarus{-ide-qt5,} &
                ;;
        esac
    fi
    wait
    if [[ -d "${VAR[use]}" ]]; then
        if [[ -f "${VAR[pkg]}" ]]; then
            while read -r; do
                if [[ -n "${REPLY}" ]] &&
                    ! [[ -d "${VAR[use]}/${REPLY}" ]] &&
                    ! (lazbuild --verbose-pkgsearch "${REPLY}") &&
                    ! (lazbuild --add-package "${REPLY}"); then
                        (
                            declare -A TMP=(
                                [url]="https://packages.lazarus-ide.org/${REPLY}.zip"
                                [dir]="${VAR[use]}/${REPLY}"
                                [out]=$(mktemp)
                            )
                            wget --quiet --output-document "${TMP[out]}" "${TMP[url]}"
                            unzip -o "${TMP[out]}" -d "${TMP[dir]}"
                            rm --verbose "${TMP[out]}"
                        ) &
                    fi
            done < "${VAR[pkg]}"
            wait
        fi
        find "${VAR[use]}" -type 'f' -name '*.lpk' -printf '\033[32m\tadd package link\t%p\033[0m\n' -exec \
            lazbuild --add-package-link {} + 1>&2
    fi
    declare -i errors=0
    while read -r; do
        declare -A TMP=(
            [out]=$(mktemp)
        )
        if (lazbuild --build-all --recursive --no-write-project "${REPLY}" > "${TMP[out]}"); then
            printf '\x1b[32m\t[%s]\t%s\x1b[0m\n' "${?}" "${REPLY}"
            grep --color='always' 'Linking' "${TMP[out]}"
        else
            printf '\x1b[31m\t[%s]\t%s\x1b[0m\n' "${?}" "${REPLY}"
            grep --color='always' --extended-regexp '(Error|Fatal):' "${TMP[out]}"
            ((errors+=1))
        fi 1>&2
        rm "${TMP[out]}"
    done < <(find "${VAR[src]}" -type 'f' -name '*.lpi' | sort)
    exit "${errors}"
)

function priv_main
(
    set -euo pipefail
    if ((${#})); then
        case ${1} in
            build) priv_lazbuild ;;
            *) priv_clippit ;;
        esac
    else
        priv_clippit
    fi
)

##############################################################################################################
priv_main "${@}" >/dev/null

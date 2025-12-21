#!/bin/sh

default_prefix=/usr/local
prefix="$default_prefix"

prev_option=
for option
do
    if test -n "$prev_option"
    then
        eval "$prev_option=\$option"
        prev_option=
        continue
    fi

    # split --foo=bar to --foo bar
    case "$option" in
        *=?*) optarg="${option#*=}" ;;
    esac

    case "$option" in
        --prefix) prev_option="prefix" ;;
        --prefix=*) prefix="$optarg";;

        -*)
            echo "Unrecognized option: $option"
            ;;
        *)
            echo "Something went wrong!"
            exit 1
    esac
done

bin_dir="${prefix}/bin"
info_dir="${prefix}/share/info"
doc_dir="${prefix}/doc/__PROJECT-NAME__"

if test -e __PROJECT-NAME__
then
    install -d -v "$bin_dir"
    install -v __PROJECT-NAME__ "${bin_dir}/__PROJECT-NAME__"
fi

if test -e __PROJECT-NAME__.info
then
    install -d -v "$info_dir"
    install -v -m 644 __PROJECT-NAME__.info "${info_dir}/__PROJECT-NAME__.info"
    install-info "${info_dir}/__PROJECT-NAME__.info" "${info_dir}/dir"
fi

if test -e __PROJECT-NAME__.html
then
    install -d -v "$doc_dir"
    install -v -m 644 __PROJECT-NAME__.html "${doc_dir}/__PROJECT-NAME__.hmtl"
fi

if test -e README.org
then
    install -d -v "$doc_dir"
    install -v -m 644 README.org "${doc_dir}/README.org"
fi

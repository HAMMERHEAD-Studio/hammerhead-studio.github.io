#!/bin/sh

# ============================================================
# Render evenry page under SRC with template.awk
#
# usage: build.sh [SRC [DIST]]    (defaults: src, dist)
# env:   DEFAULT_LANG             language of pages without a ".<lang>"
#                                 suffix (default: en)
#   src/index.html         -> dist/index.html        lang=$DEFAULT_LANG  root=
#   src/index.ja.html      -> dist/ja/index.html     lang=ja             root=/ja
#   src/blog/post.ja.html  -> dist/ja/blog/post.html lang=ja             root=/ja
# ============================================================
# Skipped: src/resources/ (copied to dist as is), and evenry file or directory
# whose name starts with "." or "_" (layouts, includes).
# DIST is deleted and rebuild on evenry run, and deleted again if build fails.
# ============================================================

die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

abort() {
  printf 'error: %s\n' "$1" >&2
  if [ -n "${2:-}" ]; then
    printf '%s\n' "$2" | while IFS= read -r line; do
      printf '  >> %s\n' "$line" >&2
    done
  fi
  rm -rf "$DIST"
  printf 'cleanup: %s\n' "$DIST"
  exit 1
}

resolve_link() {
  path=$1
  n=0
  while [ -L "$path" ]; do
    n=$((n + 1))
    [ "$n" -gt 32 ] && die "too many levels of symbolic links: $1"
    target=$(ls -ld -- "$path") || die "cannot read link: $path"
    target=${target#* -> }
    case $target in
      /*) path=$target ;;
      *) path=$(dirname -- "$path")/$target ;;
    esac
  done
  printf '%s\n' "$path"
}

# ------------------------------------------------------------

SRC="${1:-src}"
DIST="${2:-dist}"
DEFAULT_LANG="${DEFAULT_LANG:-en}"


while [ "$SRC" != "${SRC%/}" ]; do SRC=${SRC%/}; done
while [ "$DIST" != "${DIST%/}" ]; do DIST=${DIST%/}; done

script=$(resolve_link "$0") || exit 1
script_dir=$(cd "$(dirname -- "$script")" && pwd -P) || exit 1
AWK_SCRIPT="$script_dir/template.awk"
[ -f "$AWK_SCRIPT" ] || die "template.awk not found: $AWK_SCRIPT"

[ -d "$SRC" ] || die "source directory not found: $SRC"

dist_existed=0
[ -d "$DIST" ] && dist_existed=1
mkdir -p "$DIST" || die "cannot create output directory: $DIST"

src_abs=$(cd "$SRC" && pwd -P) || exit 1
dist_abs=$(cd "$DIST" && pwd -P) || exit 1

nested=0
case "$dist_abs/" in "$src_abs"/*) nested=1 ;; esac
case "$src_abs/" in "$dist_abs"/*) nested=1 ;; esac
if [ "$nested" -eq 1 ]; then
  [ "$dist_existed" -eq 0 ] && rmdir "$DIST" 2>/dev/null
  die "source and output directories must not be identical or nested: $SRC, $DIST"
fi

rm -rf "$DIST"
if [ "$dist_existed" -eq 1 ]; then
  rm -rf "$DIST"
  printf 'remove: %s\n' "$DIST"
fi
mkdir -p "$DIST" || die "cannot create output directory: $DIST"

if [ -d "$SRC/resources" ]; then
  cp -R "$SRC/resources" "$DIST/" || abort "cannot copy resources"
  printf 'copy: %s/resources -> %s/resources\n' "$SRC" "$DIST"
fi

find "$SRC" -type f -name '*.html' | sort | {
  count=0
  while IFS= read -r filepath; do
    relpath=${filepath#"$SRC"/}

    case $relpath in
      resources/*) continue ;;
    esac
    case /$relpath in
      */.* | */_*) continue ;;
    esac

    reldir=
    case $relpath in
      */*) reldir=${relpath%/*} ;;
    esac
    filename=${relpath##*/}
    stem=${filename%.*}

    case $stem in
      *.*)
        lang=${stem##*.}
        name=${stem%.*}
        case $lang in
          "" | *[!ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-]*)
            abort "invalid language suffix in file name: $filename"
            ;;
        esac
        out_dir=$DIST/$lang
        root=/$lang/
        ;;
      *)
        lang=$DEFAULT_LANG
        name=$stem
        out_dir=$DIST
        root=/
        ;;
    esac
    if [ -n "$reldir" ]; then
      out_dir=$out_dir/$reldir
      url=$root$reldir/$name.html
      path=/$reldir/$name.html
    else
      url=$root$name.html
      path=/$name.html
    fi

    out=$out_dir/$name.html
    [ -e "$out" ] && abort "output collision: $out (from $filepath)"
    mkdir -p "$out_dir" || abort "cannot create directory: $out_dir"

    if err=$(awk -f "$AWK_SCRIPT" "$filepath" "lang=$lang" "root=$root" "url=$url" "path=$path" 2>&1 > "$out"); then
      printf 'build: %s -> %s\n' "$filepath" "$out"
      count=$((count + 1))
    else
      abort "$filepath" "$err"
    fi
  done
  printf 'done: %s page(s)\n' "$count"
} || exit 1

#!/bin/bash -x

set -e pipefail

configure_ffmpeg() {
  emconfigure ./configure \
    --disable-x86asm \
    --disable-inline-asm \
    --disable-doc \
    --disable-stripping \
    --disable-ffprobe \
    --nm="llvm-nm -g" \
    --ar=emar \
    --cc=emcc \
    --cxx=em++ \
    --objcc=emcc \
    --dep-cc=emcc
}

make_ffmpeg() {
  NPROC=$(grep -c ^processor /proc/cpuinfo)
  emmake make -j${NPROC}
}

build_ffmpegjs() {
  emcc \
    -I. \
    -Llibavcodec -Llibavdevice -Llibavfilter -Llibavformat -Llibavresample -Llibavutil -Llibpostproc -Llibswscale -Llibswresample \
    -Qunused-arguments -Oz \
    -o javascript/ffmpeg-core.js fftools/ffmpeg_opt.o fftools/ffmpeg_filter.o fftools/ffmpeg_hw.o fftools/cmdutils.o fftools/ffmpeg.o \
    -lavdevice -lavfilter -lavformat -lavcodec -lswresample -lswscale -lavutil -lm \
    -s MODULARIZE=1 \
    -s EXPORTED_FUNCTIONS="[_ffmpeg]" \
    -s EXTRA_EXPORTED_RUNTIME_METHODS="[cwrap, FS, getValue, setValue]" \
    -s TOTAL_MEMORY=33554432 \
    -s ALLOW_MEMORY_GROWTH=1
}

main() {
  configure_ffmpeg
  make_ffmpeg
  build_ffmpegjs
}

main "$@"

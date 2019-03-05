today=`date +%Y%m%d_%H%M%S`
# ./ffmpeg -i ./testsfiles/1R.mp4 -i ./testsfiles/1L.mp4 -filter_complex "[0:v]fisheye[a];[1:v]fisheye[b];[a][b]hstack=inputs=2[w];[w]fisheye=merge=1:width=90" "./testsfiles/1LR_complex_$today.mp4"
# ./ffmpeg -i ./testsfiles/1R.mp4 -i ./testsfiles/1L.mp4 -filter_complex "[0:v]fisheye[a];[1:v]fisheye[b];[a][b]hstack=inputs=2" "./testsfiles/1LR_complex_wide.mp4"
./ffmpeg -i ./testsfiles/1LR_complex_wide.mp4 -vf "fisheye=merge=1:width=90" "./testsfiles/1LR_complex_$today.mp4"

# ./ffmpeg -i ./testsfiles/1R.mp4 -i ./testsfiles/1L.mp4 -filter_complex "[0:v]fisheye[a];[1:v]fisheye[b];[a][b]hstack=inputs=2[c];[c]fisheye=merge=1:width=90" ./testsfiles/1LR_complex_0002.mp4no
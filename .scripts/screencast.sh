TIMESTAMP=$(date "+%m-%d-%y")
ffmpeg -f x11grab \
       -video_size 1920x1080 \
       -framerate 60 \
       -i $DISPLAY \
       -f alsa \
       -i default \
       -r 30 \
       -s 1280x720 \
       -c:v libx264 \
       -preset:v veryfast \
       -b:v 2000k \
       -c:a libopus \
       -b:a 128k \
       "$HOME/screencast-$TIMESTAMP.mkv"

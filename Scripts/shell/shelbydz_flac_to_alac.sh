#!/usr/bin/env bash

# $1 is the fist argument from the cli
MUSIC_DIR="$1"
if [ ! -d "$MUSIC_DIR" ]
then
	echo "Invalid argument"
	echo "Please enter a valid album directory"
	exit

fi

#strip down to the new directory  name 
echo $MUSIC_DIR
NEW_DIR=$(basename "$MUSIC_DIR")
echo $NEW_DIR

if [ ! -d ~/Music ] ; then
	mkdir ~/Music
	echo "Made a Music Dir"
else
	echo "Music directory exists"
fi

echo "Creating new folder"

if [ -d ~/Music/"$NEW_DIR" ]
then
	echo deleting folder "$NEW_DIR"
	rm -rf ~/Music/"$NEW_DIR"
fi	

mkdir ~/Music/"$NEW_DIR"


#looping through the audio files:
for file in "$MUSIC_DIR"*.flac
do
	#echo "Processing file $file"
	NEW_SONG=$(basename "$file" .flac)".m4a"
	#echo $NEW_SONG
	#echo "writing new file: /home/$USER/Music/$NEW_DIR/$NEW_SONG"
	echo "$file"
	ffmpeg -i "$file" -c:a alac -c:v copy -map_metadata 0:s:0 "/home/$USER/Music/$NEW_DIR/$NEW_SONG"

done	
echo "Done!"

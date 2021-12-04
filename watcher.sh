#while inotifywait -q -m -e modify src/dactyl.clj
until inotifywait -e modify ./src/dactyl_keyboard/dactyl.clj
do 
	lein run a
done

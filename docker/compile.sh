#!/bin/bash
cd ..
if [ $# -eq 0 ]; then
	echo "Missing input file relative path, run again."
else
	[ ! -f out.js ] || rm out.js
	docker run -ti --rm --net=host -v $(pwd):/funkadelic:rw funkadelic bash -c "cd ../funkadelic ; stack run ${1} ; useradd -d /home/$USER -m -s /bin/bash $USER && echo "$USER:$USER" | chpasswd && adduser $USER sudo ; [ ! -f out.js ] || chown $USER:$USER out.js ; [ ! -f out.js ] || chmod +760 out.js ; chown $USER:$USER .stack-work"
	if [ ! -f out.js ]; then
		echo "Compilation failed."
	else
        	echo "Compilation successful. Saved to: out.js"
	fi
fi


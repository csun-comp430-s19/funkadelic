docker run -ti --rm --net=host -v $(pwd):/funkadelic:rw  funkadelic bash -c "TAR_OPTIONS=--no-same-owner; mkdir .stack ; cd funkadelic; stack build ; stack test --coverage"

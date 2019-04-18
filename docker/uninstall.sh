docker stop $(docker ps -a -q --filter ancestor=funkadelic)
docker rmi funkadelic:latest

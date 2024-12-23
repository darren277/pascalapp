include .env

PORT ?= 5678
PG_HOST ?= 172.18.0.21
PG_PORT ?= 5432
PG_USER ?= myusername
PG_PASS ?= mypassword
PG_DB ?= 

API_BASE_URL=http://localhost:$(PORT)/api

build:
	docker build --platform linux/arm --pull=false -t pascal-app1:1 .

run:
	docker run -d -it --rm --security-opt seccomp=unconfined --name pascal-app1 pascal-app1:1

cli:
	docker exec -it pascal-app1 sh

rm:
	docker kill pascal-app1


compile:
	fpc src/pascalapp.pas

clean:
	rm src/*.o src/*.ppu src/pascalapp

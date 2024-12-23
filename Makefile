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

init-db:
	PGPASSWORD=$(PG_PASS) psql -U $(PG_USER) -d $(PG_DB) -c "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, email VARCHAR NOT NULL);"

# -ooutput/src/pascalapp
compile:
	fpc -dPORT -dPG_HOST -dPG_PORT -dPG_USER -dPG_PASS -dPG_DB src/pascalapp.pas

run:
	PORT=$(PORT) PG_HOST=$(PG_HOST) PG_PORT=$(PG_PORT) PG_USER=$(PG_USER) PG_PASS=$(PG_PASS) PG_DB=$(PG_DB) ./src/pascalapp

clean:
	rm src/*.o src/*.ppu src/pascalapp

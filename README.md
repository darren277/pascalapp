# pascalapp

## About

## Usage

### Postgres Initialization

Download and install Postgres.

Create a user.

Modify `pg_hba.conf` (replace `13` with whatever version you have).

```shell
sudo nano /etc/postgresql/13/main/pg_hba.conf
```

```
# "local" is for Unix domain socket connections only
#local   all             all                                    peer
local    all             all                                    md5
```

```shell
sudo systemctl reload postgresql
```

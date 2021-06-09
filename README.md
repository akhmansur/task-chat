## Install client deps
```$ npm install```

# If you dont have node.js and npm
for Ubuntu/Debian:
```$ sudo apt install nodejs```
```$ sudo apt install npm```

## Start client

```$ npm start```

## Install rebar

```$ make rebar```

## Install dependencies

```$ make deps```

## Run tests

```$ make test```

## Run docs

```$ make docs```

## Start the server

```$ make server```


## Инструкция по командам.
Сначала запросите список комнат сообщением [{type: chroom_list},{msg: any}]
При заходе в комнату отправьте ник.
Сообщения серверу отправляются в формате [{type: Type},{msg: Message}]. 

|     Type          |    Message    |
| ----------------- | ------------- |
| chroom_list       | Any           |
| room_members_list | Room name     |
| enter_room        | Room name     |
| username          | User name     |
| terminate         | Any           |
| chat              | Message       |

Сообщения от сервера отправляются в формате [{type: Type},{msg: Message}, сообщения других пользователей в текущую комнату отправляются в формате [[{From1:Message1}],[{From2:Message2}]].

|     Type          |    Message                |
| ----------------- | ------------------------- |
| chroom_list       | List of rooms             |
| room_members_list | List of roommates         |
| username_error    | Busy username, try another|
| error             | Errors                    |
| last_messages     | List of last room         |
|                   | messages                  |
| enter_username    | Send your username        |
|                   | with the following        |
|                   | message                   |
| other             | Messages from other       |
|                   | users                     |

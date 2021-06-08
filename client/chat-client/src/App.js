import './App.css';
import Login from "./containers/Login/Login";
import Chat from "./containers/Chat/Chat";
import {useCallback, useEffect, useRef, useState} from "react";

const App = () => {
  const [rooms, setRooms] = useState([]);
  const [, updateState] = useState();
  const forceUpdate = useCallback(() => updateState({}), []);
  const [roommates, setRoommates] = useState();
  const messgs = useRef([]);
  const websocket = useRef(0)
  const [name, setName] = useState();
  let data
  useEffect(() => {
    const wsHost = "ws://localhost:6060/websocket";
    const socket = new WebSocket(wsHost)

    socket.onopen = function (evt) {
      console.log("open")
      sendCom('chroom_list', 'rooms')
    };
    socket.onclose = function (evt) {
      console.log("close")
    };
    socket.onmessage = (evt) => {
      data = JSON.parse(evt.data);
      switch(data.type) {
        case "chroom_list":
          setRooms(data.msg)
          break;
        case "room_members_list":
          setRoommates(data.msg)
          break;
        case "last_messages":
          messgs.current=data.msg
          break;
        case "username_allowed":
          break;
        case "username_error":
          break;
        case "enter_username":
          break;
        case "error":
          break;
        default:
          let mess = {}
          mess[data.type] = data.msg
          messgs.current.push([mess])
          forceUpdate()
          break;
      }
    };
    socket.onerror = function (evt) {
      console.log("error")
    };
    if(socket) websocket.current = socket;
  }, []);

  const sendCom = (type, msg) => {
    if(websocket.readyState === websocket.OPEN && type){
      const txt = [{"type":type},{"msg":msg}]
      websocket.current.send(JSON.stringify(txt));
    }
  };

  const [logged, setLogged] = useState(true);
  return (
    <div className="App">
      {
        logged
          ? <Chat rooms={rooms} roommates={roommates} messages={messgs}
                  sendCom={sendCom} name={name} setLogged={setLogged}/>
          : <Login sendCom={sendCom} logged={setLogged} setName={setName}/>
      }
    </div>
  )
}

export default App;

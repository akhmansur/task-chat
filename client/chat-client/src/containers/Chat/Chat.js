import React, {useEffect, useState} from 'react';
import './Chat.css'

const Chat = ({rooms, roommates, messages, sendCom, name, setLogged}) => {
  const [input, setInput] = useState('');
  const [currentRoom, setCurrentRoom] = useState(null);
  return (
    <div className="Chat">
      <div className='chrooms'>
        <p style={{fontSize: "20px"}}>Rooms:</p>
          {rooms ? getRooms(rooms, sendCom, currentRoom, setCurrentRoom, setLogged) : ''}
      </div>
      <div className='messages'>
        <span className="roomdes">Messages</span>
        <div>
          {messages ? getMessages(messages.current) : ''}
        </div>
        <div className="send-group">
          <input
            className='input-chat'
            value={input}
            onChange={event => setInput(event.target.value)}/>
          <button
            className='button-send'
            onClick={() => {
              setInput('')
              let mess = {}
              mess[name] = input
              messages.current=[...messages.current, [mess]];
              sendCom('chat', input)
            }
            }>Send
          </button>
        </div>
      </div>
      <div className='chrooms'>
        <p style={{fontSize: "20px"}}>Users:</p>
          {roommates ? getUsers(roommates) : ''}
      </div>
    </div>
  )
}

function getRooms(rooms, sendCom, currentRoom, setCurrentRoom, setLogged) {
  return rooms.map((elem, index) => {
      const isActive = currentRoom === elem
        ? " active"
        : ""
      return <button className={"roomButton" + isActive}
                     key={index + 1}
                     onClick = {() => {
                       sendCom('enter_room', elem);
                       setCurrentRoom(elem);
                       setLogged(false)
                     }
                     }
      >{elem}</button>
    }
  )
};

function getMessages(messages) {
  if (messages !== "" && messages) {
    return messages.map((elem, index) => {
        const Key = Object.keys(elem[0])
        return (<p className='message' key={index}>{Key}: {elem[0][Key]}</p>)
      }
    )
  } else {
    const roomName = Object.keys(messages)
    return (
      <span className='message'>{roomName}: {messages[roomName]}</span>)
  }
}

function getUsers(users) {
  return users.map((elem, index) => {
    return <span className='user' key={index}>{elem}</span>
  })
}

export default Chat;

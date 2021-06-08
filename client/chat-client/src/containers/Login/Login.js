import React, {useState} from 'react';
import './Login.css'

const Login = ({sendCom, logged, setName}) => {
  const [input, setInput] = useState('');
  return (
    <div className="login">
      <div className='login-group'>
        <input
          className='input'
          value={input}
          onChange={event => setInput(event.target.value)}/>
        <button
          className='button-send'
          onClick={() => {
            setInput('')
            sendCom('username', input)
            setName(input)
            logged(true)
          }
          }>
          Login
        </button>
      </div>
    </div>
  )
}

export default Login;

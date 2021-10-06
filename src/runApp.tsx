import { StrictMode } from 'react';
import ReactDOM from 'react-dom';
import './scss/index.scss';
import App from './App';

export const runApp = () => {
  ReactDOM.render(
    <StrictMode>
      <App />
    </StrictMode>,
    document.getElementById('root')
  );
}

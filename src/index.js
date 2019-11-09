import './sol-lewitt.css'
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

Elm.Main.init({
  node: document.getElementById('root')
});

const storageKey = "store";
const flags = localStorage.getItem(storageKey);
Elm.Main.init({flags});

serviceWorker.unregister();

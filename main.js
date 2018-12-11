// local storage library
window.localStore = (namespace) => {
  let store
  try {
    store = JSON.parse(localStorage.getItem(namespace)) || {}
  } catch (e) {
    console.warn(e)
    store = {}
  }

  const set = (name, value) => {
    if (name) {
      store[name] = value
    } else {
      store = value
    }
    localStorage.setItem(namespace, JSON.stringify(store))
  }

  const get = (name) => name ? store[name] : store

  return { set, get }
}

// mount Elm app
(() => {
  const store = localStore('dick.budget')

  const app = Elm.Main.init({
    node: document.getElementById('root'),
  })

  app.ports.portIntoElm.send(store.get())

  app.ports.portOutOfElm.subscribe(x => {
    store.set(null, x)
  })
})()

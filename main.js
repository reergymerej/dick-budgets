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
    store[name] = value
    localStorage.setItem(namespace, JSON.stringify(store))
  }

  const get = (name) => name ? store[name] : store

  return { set, get }
}

// mount Elm app
(() => {
  const store = localStore('dick.budget')
  console.log(store.get())

  const app = Elm.Main.init({
    node: document.getElementById('root'),
  })
})()

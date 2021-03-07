var flags = {
  user: JSON.parse(localStorage.getItem('user')) || null
}

var app = Elm.Main.init({ flags: flags })

app.ports.outgoing.subscribe(({ tag, data }) => {
  switch (tag) {
    case 'saveUser':
      return localStorage.setItem('user', JSON.stringify(data))
    case 'clearUser':
      return localStorage.removeItem('user')
    default:
      return console.warn(`Unrecognized Port`, tag)
  }
})

/*https://github.com/ryannhg/elm-spa-realworld*/
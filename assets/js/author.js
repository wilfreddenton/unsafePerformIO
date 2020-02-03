function trimLines(s) {
  return s.split('\n').map(l => l.trimEnd()).join('\n')
}

async function main() {
  const openpgp = window.openpgp
  openpgp.initWorker({path: '/static/js/openpgp.worker.min.js'})

  const publicKeyArmored = window.PUBLIC_KEY
  try {
    const {err, keys} = await openpgp.key.readArmored(publicKeyArmored)
    if (err) {
      throw `Failed to read public key with ${err[0]}`
    }

    run(openpgp, keys[0])
  } catch (error) {
    alert(error)
  }
}

function run(openpgp, publicKey) {
  const refs = {
    submits: document.querySelectorAll('.button'),
    privateKey: document.getElementById('private-key'),
    passphrase: document.getElementById('passphrase'),
    toggles: document.querySelectorAll('.toggle')
  }

  const sign = async clearText => {
    const privateKeyArmored = refs.privateKey.value
    const passphrase = refs.passphrase.value
    const message = openpgp.cleartext.fromText(clearText)

    const {err, keys} = await openpgp.key.readArmored(privateKeyArmored)
    if (err) {
      throw `Failed to read private key with ${err[0]}`
    }

    const privateKey = keys[0]
    const success = await privateKey.decrypt(passphrase)
    if (!success) {
      throw 'Failed to decrypt private key with provided passphrase'
    }

    const signed = await openpgp.sign({
      message: message,
      privateKeys: [privateKey],
      detached: true
    })

    const detachedSignature = signed.signature
    const signature = await openpgp.signature.readArmored(detachedSignature)
    const verified = await openpgp.verify({
      message: message,
      signature: signature,
      publicKeys: [publicKey]
    })

    if (!verified.signatures[0].valid) {
      throw 'Failed to verify signature. Are you using the right private key?'
    }

    return detachedSignature
  }

  const send = async (method, endpoint, signature, data) => {
    const response = await fetch(endpoint, {
      method: method,
      body: JSON.stringify({signature, data}),
      headers: {
        'Content-Type': 'application/json',
      }
    })

    if (!response.ok) {
      const body = await response.json()
      throw `request to ${endpoint} failed: ${response.status} - ${body['error']['message']}`
    }
  }

  const signAndSend = async (endpoint, clearText, data, method) => {
    try {
      const detachedSignature = await sign(clearText)
      await send((typeof method === 'undefined') ? 'POST' : method, endpoint, detachedSignature, data)
      alert('The request was signed and sent successfully!')
    } catch (error) {
      alert(error)
    }
  }

  const formHandlers = {
    'post': async form => {
      const title = form.title.value.trim()
      const body = trimLines(form.body.value)
      await signAndSend('/posts', title + body, {'title': title, 'body': body})
    },
    'editpost': async form => {
      const id = form.postId.value.trim()
      const title = form.title.value.trim()
      const body = trimLines(form.body.value)
      await signAndSend(`/posts/${id}`, title + body, {'title': title, 'body': body}, 'PUT')
    },
    'deletepost': async form => {
      const id = form.postId.value.trim()
      const b = confirm('Confirm post deletion.')
      if (b) {
        await signAndSend(`/posts/${id}`, id, null, 'DELETE')
      }
    },
    'about': async form => {
      const title = form.title.value.trim()
      const body = trimLines(form.body.value)
      await signAndSend('/about', title + body, {'title': title, 'body': body})
    },
    'contact': async form => {
      const location = form.location.value.trim()
      const email = form.email.value.trim()
      const linkedIn = form.linkedIn.value.trim()
      const facebookMessenger = form.facebookMessenger.value.trim()
      const instagram = form.instagram.value.trim()
      await signAndSend('/contact', location + email + linkedIn + facebookMessenger + instagram, {
        'location': location,
        'email': email,
        'linked_in': linkedIn,
        'facebook_messenger': facebookMessenger,
        'instagram': instagram
      })
    }
  }

  refs.toggles.forEach(toggle => {
    toggle.addEventListener('click', e => {
      e.target.nextSibling.classList.toggle('hidden')
    })
  })

  refs.submits.forEach(submit => {
    submit.addEventListener('click', async e => {
      e.preventDefault()
      const form = e.target.parentElement
      let key = form.id.split('-')[0]
      if (e.target.innerHTML === 'delete') {
        key = 'deletepost'
      }
      await formHandlers[key](form)
    })
  })
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main)
} else {
  main()
}
